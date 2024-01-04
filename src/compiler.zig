const std = @import("std");

const Token = @import("./token.zig");
const Stmt = @import("./ast/stmt.zig").Stmt;
const Expr = @import("./ast/expr.zig").Expr;

const Environment = @import("./environment.zig");
const Globals = @import("./globals.zig");

pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
const Err = ErrorReporter(CompilerError);

pub const CompilerError = error{
    UnknownVariable,
    UnknownConstant,
    VariableInitializationInGlobalScope,
    VariableAssignationInGlobalScope,
    OutOfMemory,
    FunctionCallInGlobalScope,
};

allocator: std.mem.Allocator,

ast: []*Expr,

module: c.BinaryenModuleRef,

environments: std.ArrayList(Environment),

current_env: ?*Environment,

globals: Globals,

blocks_children: std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)),

args: std.AutoArrayHashMap(*const Expr, []c.BinaryenExpressionRef),

pub fn init(allocator: std.mem.Allocator, ast: []*Expr) @This() {
    var environments = std.ArrayList(Environment).init(allocator);

    var globals = Globals.init(allocator);

    var blocks_children = std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)).init(allocator);

    var args = std.AutoArrayHashMap(*const Expr, []c.BinaryenExpressionRef).init(allocator);

    var module = c.BinaryenModuleCreate();

    return .{
        .allocator = allocator,
        .ast = ast,
        .module = module,
        .environments = environments,
        .globals = globals,
        .current_env = null,
        .blocks_children = blocks_children,
        .args = args,
    };
}

pub fn deinit(self: *@This()) void {
    for (self.blocks_children.items) |child_exprs| {
        child_exprs.deinit();
    }

    self.blocks_children.deinit();

    for (self.environments.items) |*env| {
        env.deinit();
    }

    for (self.args.values()) |args| {
        self.allocator.free(args);
    }

    self.args.deinit();

    self.environments.deinit();

    self.globals.deinit();

    c.BinaryenModuleDispose(self.module);
}

pub fn compile(self: *@This()) !void {
    for (self.ast) |expr| {
        _ = try self.codegen(expr);
    }

    try self.write();
}

fn codegen(self: *@This(), expr: *Expr) !c.BinaryenExpressionRef {
    return try self.expression(expr);
}

fn expression(self: *@This(), expr: *const Expr) !c.BinaryenExpressionRef {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            const value = try self.expression(const_init.initializer);
            const name = self.to_c_string(const_init.name.lexeme);
            // @todo find a way to know if the expression is a function directly ?
            const is_function = if (c.BinaryenGetFunction(self.module, @ptrCast(name)) != null)
                true
            else
                false;

            // it's a global function
            if (is_function) {
                try self.globals.add_function(const_init.name.lexeme);
                // @todo implement funcrefs
            } else {
                try self.globals.add_variable(const_init.name.lexeme);
                _ = c.BinaryenAddGlobal(self.module, @ptrCast(name), c.BinaryenTypeInt32(), false, value);
            }

            return value;
        },
        .VarInit => |*var_init| {
            if (self.current_env) |current_env| {
                const idx = if (current_env.last_index == 0) 0 else current_env.last_index + 1;
                try current_env.set(var_init.name, idx);
                try current_env.add_local_type(c.BinaryenTypeInt32());

                const value = try self.expression(var_init.initializer);

                return c.BinaryenLocalSet(self.module, @intCast(idx), value);
            }

            return CompilerError.VariableInitializationInGlobalScope;
        },
        .Function => |func| {
            var env: *Environment = try self.environments.addOne();
            env.* = Environment.init(self.allocator);

            const prev_env = self.current_env;
            defer self.current_env = prev_env;

            self.current_env = env;

            var params: ?c.BinaryenType = null;

            if (func.args) |args| {
                var binaryen_args = try self.allocator.alloc(c.BinaryenType, @intCast(args.len));
                //@todo dfree

                for (args, 0..) |arg, i| {
                    binaryen_args[i] = c.BinaryenTypeInt32();
                    try env.set(arg, i);
                }

                params = c.BinaryenTypeCreate(
                    @ptrCast(binaryen_args),
                    @intCast(args.len),
                );
            }

            const body = if (func.body) |body| try self.expression(body) else null;

            const name = if (func.name) |name| name.lexeme else "anonymous_func";

            // horror museum @todo clean this crap up @todo free ?
            const name_ptr: [*:0]const u8 = @ptrCast(self.allocator.dupeZ(u8, name) catch unreachable);

            var var_types = self.current_env.?.local_types.items;

            _ = c.BinaryenAddFunction(
                self.module,
                name_ptr,
                params orelse 0, // ?
                c.BinaryenTypeInt32(),
                var_types.ptr,
                @intCast(var_types.len),
                body,
            );

            _ = c.BinaryenAddFunctionExport(
                self.module,
                name_ptr,
                name_ptr,
            );

            return body;
        },
        .Binary => |binary| {
            var left = try self.expression(binary.left);
            var right = try self.expression(binary.right);

            const op = switch (binary.op.type) {
                .PLUS => c.BinaryenAddInt32(),
                .MINUS => c.BinaryenSubInt32(),
                .STAR => c.BinaryenMulInt32(),
                .SLASH => c.BinaryenDivSInt32(), // div s ? div u ?,
                .EQUAL_EQUAL => c.BinaryenEqInt32(),
                .BANG_EQUAL => c.BinaryenNeInt32(),
                .GREATER => c.BinaryenGtSInt32(),
                .GREATER_EQUAL => c.BinaryenGeSInt32(),
                .LESS => c.BinaryenLtSInt32(),
                .LESS_EQUAL => c.BinaryenLeSInt32(),

                else => unreachable,
            };

            return c.BinaryenBinary(self.module, op, left, right);
        },
        .Variable => |variable| {
            if (!self.in_global_scope()) {
                if (self.current_env.?.get_index_by_name(variable.name.lexeme)) |idx| {
                    return c.BinaryenLocalGet(
                        self.module,
                        @intCast(idx),
                        c.BinaryenTypeInt32(),
                    );
                }
            }

            if (self.globals.has_variable(variable.name.lexeme)) {
                return c.BinaryenGlobalGet(
                    self.module,
                    self.to_c_string(variable.name.lexeme),
                    c.BinaryenTypeAuto(),
                );
            }

            if (self.globals.has_function(variable.name.lexeme)) {
                //@todo implement funcrefs
                return c.BinaryenNop(self.module);
            }

            return Err.raise(
                variable.name,
                CompilerError.UnknownVariable,
                @errorName(CompilerError.UnknownVariable),
            );
        },
        .Literal => |literal| {
            const value = switch (literal.value) {
                .Boolean => |boolean| c.BinaryenLiteralInt32(if (boolean) @as(i32, 1) else @as(i32, 0)),
                .Integer => |integer| c.BinaryenLiteralInt32(integer),
                .Float => |float| c.BinaryenLiteralFloat32(float),
                // .String => |string| return c.BinaryenStringConst(self.module, @ptrCast(string)),
                else => unreachable,
            };
            return c.BinaryenConst(self.module, value);
        },
        // https://openhome.cc/eGossip/WebAssembly/Block.html ?? investigate
        .Block => |block| {
            var block_exprs = self.blocks_children.addOne() catch return CompilerError.OutOfMemory;

            block_exprs.* = std.ArrayList(c.BinaryenExpressionRef).init(self.allocator);

            for (block.exprs, 0..) |child_expr, i| {
                _ = i;
                const binaryen_expr = try self.expression(child_expr);
                block_exprs.append(binaryen_expr) catch return CompilerError.OutOfMemory;
            }

            return c.BinaryenBlock(
                self.module,
                null,
                @ptrCast(block_exprs.items),
                @intCast(block_exprs.items.len),
                c.BinaryenTypeAuto(),
            );
        },
        .Unary => |unary| {
            const value = try self.expression(unary.right);

            return switch (unary.op.type) {
                //@todo bang
                .MINUS, .BANG => c.BinaryenBinary(
                    self.module,
                    c.BinaryenMulInt32(),
                    value,
                    c.BinaryenConst(
                        self.module,
                        c.BinaryenLiteralInt32(
                            @as(i32, -1),
                        ),
                    ),
                ),
                else => unreachable,
            };
        },
        .Grouping => |grouping| {
            return try self.expression(grouping.expr);
        },
        .Logical => |logical| {
            const left = try self.expression(logical.left);
            const right = try self.expression(logical.right);

            const result = switch (logical.op.type) {
                .AND => .{ .if_true = right, .if_false = left },
                .OR => .{ .if_true = left, .if_false = right },
                else => unreachable,
            };

            return c.BinaryenSelect(
                self.module,
                left,
                result.if_true,
                result.if_false,
                c.BinaryenTypeAuto(),
            );
        },
        .Assign => |assign| {
            if (self.current_env) |current_env| {
                const idx = current_env.get_index_by_name(assign.name.lexeme) orelse return CompilerError.UnknownVariable;
                const value = try self.expression(assign.value);
                return c.BinaryenLocalSet(self.module, @intCast(idx), value);
            }

            return CompilerError.VariableAssignationInGlobalScope;
        },
        .Call => |call| {
            if (self.current_env) |current_env| {
                _ = current_env;
                const callee = try self.expression(call.callee);
                _ = callee;

                var args = try self.allocator.alloc(c.BinaryenExpressionRef, call.args.len);

                for (call.args, 0..) |arg_expr, i| {
                    args[i] = try self.expression(arg_expr);
                }

                try self.args.put(call.callee, args);

                return c.BinaryenCall(
                    self.module,
                    self.to_c_string(call.callee.Variable.name.lexeme), //@implement funcref ?
                    @ptrCast(args),
                    @intCast(args.len),
                    c.BinaryenTypeInt32(),
                );
            }

            return CompilerError.FunctionCallInGlobalScope;
        },
        // else => {
        //     std.debug.print("\n Compiler : expression type not implemented for {any}\n", .{expr});
        //     unreachable;
        // },
    };
}

fn write(self: *@This()) !void {
    c.BinaryenModulePrint(self.module);

    var buffer: [10_000]u8 = undefined;

    const out = c.BinaryenModuleAllocateAndWriteText(self.module);

    const code = try std.fmt.bufPrintZ(&buffer, "{s}", .{out});

    const file_path = "./out.wat";

    const file = try std.fs.cwd().createFile(
        file_path,
        .{ .read = true },
    );

    defer file.close();

    _ = try file.writeAll(code);
}

fn to_c_string(self: *@This(), str: []const u8) [*:0]const u8 {
    //@todo horror museum + memory leak
    return @ptrCast(self.allocator.dupeZ(u8, str) catch unreachable);
}

inline fn in_global_scope(self: *@This()) bool {
    return self.current_env == null;
}
