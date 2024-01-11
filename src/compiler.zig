const std = @import("std");
const floatMax = std.math.floatMax;
const maxInt = std.math.maxInt;

const Codegen = @import("./codegen.zig");

const Token = @import("./token.zig");
const Expr = @import("./ast/expr.zig").Expr;

const Environment = @import("./environment.zig");
const Globals = @import("./globals.zig");

const Context = @import("./context.zig").Context;

const Type = @import("./types.zig").Type;
const Sems = @import("./semer.zig").Sems;

pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
const Err = ErrorReporter(CompilerError);

pub const CompilerError = error{
    UnknownVariable,
    UnknownConstant,
    VariableAssignationInGlobalScope,
    OutOfMemory,
    FunctionCallInGlobalScope,
    ConstantAssignation,
    InvalidType,
    CannotInferType,
};

allocator: std.mem.Allocator,

ast: []*const Expr,

sems: *const Sems,

module: c.BinaryenModuleRef,

environments: std.ArrayList(Environment),

current_env: ?*Environment,

globals: Globals,

blocks_children: std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)),

args: std.AutoArrayHashMap(*const Expr, []c.BinaryenExpressionRef),

m: Codegen,

ctx: Context(ExpressionFrame),

const ExpressionFrame = struct {
    expr: *const Expr,
    expr_type: Type,
};

pub fn init(allocator: std.mem.Allocator, ast: []*const Expr, sems: *const Sems) @This() {
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
        .m = Codegen.init(module.?, allocator),
        .ctx = Context(ExpressionFrame).init(allocator),
        .sems = sems,
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

    self.m.deinit();

    self.ctx.deinit();

    c.BinaryenModuleDispose(self.module);
}

pub fn compile(self: *@This()) !void {
    for (self.ast) |expr| {
        _ = try self.compile_expr(expr);
    }

    try self.write();
}

fn compile_expr(self: *@This(), expr: *const Expr) !c.BinaryenExpressionRef {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            try self.ctx.push_frame(.{
                .expr = expr,
                .expr_type = if (const_init.type) |const_type|
                    try Type.from_str(const_type.lexeme)
                else
                    try infer_type(const_init.initializer) orelse .i32, // @implement more types
            });
            defer _ = self.ctx.pop_frame();

            const value = try self.compile_expr(const_init.initializer);
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
                try self.globals.add_constant(const_init.name.lexeme);

                const @"type" = blk: {
                    if (const_init.type) |t| break :blk try get_binaryen_type(t);

                    if (try infer_type(const_init.initializer)) |t| break :blk Type.to_binaryen_type(t);

                    break :blk c.BinaryenTypeInt32();
                };

                _ = c.BinaryenAddGlobal(
                    self.module,
                    @ptrCast(name),
                    @"type",
                    false,
                    value,
                );
            }

            return value;
        },
        .VarInit => |*var_init| {
            const var_type = self.sems.get(expr).?.type;
            try self.ctx.push_frame(
                .{
                    .expr = expr,
                    .expr_type = var_type,
                },
            );
            defer _ = self.ctx.pop_frame();

            if (self.current_env) |current_env| {
                const idx = current_env.increment_index();
                try current_env.set(var_init.name, idx);
                try current_env.add_local_type(Type.to_binaryen_type(var_type));

                const value = try self.compile_expr(var_init.initializer);

                return c.BinaryenLocalSet(self.module, @intCast(idx), value);
            } else {
                const lexeme = var_init.name.lexeme;
                try self.globals.add_variable(lexeme);
                _ = c.BinaryenAddGlobal(
                    self.module,
                    self.to_c_string(lexeme),
                    Type.to_binaryen_type(var_type),
                    true,
                    try self.compile_expr(var_init.initializer),
                );
                return null;
            }
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
                    binaryen_args[i] = Type.to_binaryen_type(try Type.from_str(arg.type.lexeme));
                    try env.set(arg.name, @intCast(i));
                }

                params = c.BinaryenTypeCreate(
                    @ptrCast(binaryen_args),
                    @intCast(args.len),
                );
            }

            const body = if (func.body) |body| try self.compile_expr(body) else null;

            const name = if (func.name) |name| name.lexeme else "anonymous_func";

            // horror museum @todo clean this crap up @todo free ?
            const name_ptr: [*:0]const u8 = @ptrCast(self.allocator.dupeZ(u8, name) catch unreachable);

            var var_types = self.current_env.?.local_types.items;

            const func_ref = c.BinaryenAddFunction(
                self.module,
                name_ptr,
                params orelse 0, // ?
                try get_binaryen_type(func.type),
                var_types.ptr,
                @intCast(var_types.len),
                body,
            );

            if (std.mem.eql(u8, name, "main")) {
                c.BinaryenSetStart(self.module, func_ref);
            }

            // if (std.mem.eql(u8, name, "main")) {
            //     c.BinaryenSetStart(self.module, func_ref);
            // }

            _ = c.BinaryenAddFunctionExport(
                self.module,
                name_ptr,
                name_ptr,
            );

            return body;
        },
        .Binary => |binary| {
            const left = try self.compile_expr(binary.left);
            const right = try self.compile_expr(binary.right);

            const left_type = c.BinaryenExpressionGetType(left);
            const right_type = c.BinaryenExpressionGetType(right);

            if (left_type != right_type) {
                return CompilerError.InvalidType;
            }

            const op = switch (binary.op.type) {
                .PLUS => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenAddInt32(),
                    .i64 => c.BinaryenAddInt64(),
                    .f32 => c.BinaryenAddFloat32(),
                    .f64 => c.BinaryenAddFloat64(),
                    else => unreachable,
                },
                .MINUS => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenSubInt32(),
                    .i64 => c.BinaryenSubInt64(),
                    .f32 => c.BinaryenSubFloat32(),
                    .f64 => c.BinaryenSubFloat64(),
                    else => unreachable,
                },
                .STAR => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenMulInt32(),
                    .i64 => c.BinaryenMulInt64(),
                    .f32 => c.BinaryenMulFloat32(),
                    .f64 => c.BinaryenMulFloat64(),
                    else => unreachable,
                },
                .SLASH => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenDivSInt32(), // div s ? div u ?,
                    .i64 => c.BinaryenDivSInt64(),
                    .f32 => c.BinaryenDivFloat32(),
                    .f64 => c.BinaryenDivFloat64(),
                    else => unreachable,
                },
                .EQUAL_EQUAL => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenEqInt32(),
                    .i64 => c.BinaryenEqInt64(),
                    .f32 => c.BinaryenEqFloat32(),
                    .f64 => c.BinaryenEqFloat64(),
                    else => unreachable,
                },
                .BANG_EQUAL => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenNeInt32(),
                    .i64 => c.BinaryenNeInt64(),
                    .f32 => c.BinaryenNeFloat32(),
                    .f64 => c.BinaryenNeFloat64(),
                    else => unreachable,
                },
                .GREATER => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenGtSInt32(),
                    .i64 => c.BinaryenGtSInt64(),
                    .f32 => c.BinaryenGtFloat32(),
                    .f64 => c.BinaryenGtFloat64(),
                    else => unreachable,
                },
                .GREATER_EQUAL => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenGeSInt32(),
                    .i64 => c.BinaryenGeSInt64(),
                    .f32 => c.BinaryenGeFloat32(),
                    .f64 => c.BinaryenGeFloat64(),
                    else => unreachable,
                },
                .LESS => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenLtSInt32(),
                    .i64 => c.BinaryenLtSInt64(),
                    .f32 => c.BinaryenLtFloat32(),
                    .f64 => c.BinaryenLtFloat64(),
                    else => unreachable,
                },
                .LESS_EQUAL => switch (Type.from_binaryen(left_type)) {
                    .i32 => c.BinaryenLeSInt32(),
                    .i64 => c.BinaryenLeSInt64(),
                    .f32 => c.BinaryenLeFloat32(),
                    .f64 => c.BinaryenLeFloat64(),
                    else => unreachable,
                },

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

            if (self.globals.has_variable(variable.name.lexeme) or self.globals.has_constant(variable.name.lexeme)) {
                return c.BinaryenGlobalGet(
                    self.module,
                    self.to_c_string(variable.name.lexeme),
                    c.BinaryenTypeInt32(),
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
                .Boolean => |boolean| c.BinaryenLiteralInt32(
                    if (boolean) @as(i32, 1) else @as(i32, 0),
                ),
                .Number => |number| switch (self.sems.get(expr).?.type) {
                    .f32 => c.BinaryenLiteralFloat32(@floatCast(number)),
                    .f64 => c.BinaryenLiteralFloat64(number),
                    .i32 => c.BinaryenLiteralInt32(@intFromFloat(number)),
                    .i64 => c.BinaryenLiteralInt64(@intFromFloat(number)),
                    else => unreachable,
                },
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
                const binaryen_expr = try self.compile_expr(child_expr);
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
            const value = try self.compile_expr(unary.right);

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
            return try self.compile_expr(grouping.expr);
        },
        .Logical => |logical| {
            const left = try self.compile_expr(logical.left);
            const right = try self.compile_expr(logical.right);

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
                if (current_env.get_index_by_name(assign.name.lexeme)) |idx| {
                    const value = try self.compile_expr(assign.value);
                    return c.BinaryenLocalSet(self.module, @intCast(idx), value);
                } else if (self.globals.has_variable(assign.name.lexeme)) {
                    const value = try self.compile_expr(assign.value);
                    return c.BinaryenGlobalSet(self.module, self.to_c_string(assign.name.lexeme), value);
                } else if (self.globals.has_constant(assign.name.lexeme)) {
                    return CompilerError.ConstantAssignation;
                } else {
                    return CompilerError.UnknownVariable;
                }
                // orelse
            }

            return CompilerError.VariableAssignationInGlobalScope;
        },
        .OperationAssign => |op_assign| {
            if (self.current_env) |current_env| {
                const value = try self.compile_expr(op_assign.value);
                const op = switch (op_assign.op.type) {
                    .PLUS_EQUAL => c.BinaryenAddInt32(),
                    .MINUS_EQUAL => c.BinaryenSubInt32(),
                    .STAR_EQUAL => c.BinaryenMulInt32(),
                    .SLASH_EQUAL => c.BinaryenDivSInt32(), // div s ? div u ?,
                    else => unreachable,
                };

                if (current_env.get_index_by_name(op_assign.name.lexeme)) |idx| {
                    return c.BinaryenLocalSet(self.module, @intCast(idx), c.BinaryenBinary(
                        self.module,
                        op,
                        c.BinaryenLocalGet(self.module, @intCast(idx), c.BinaryenTypeAuto()),
                        value,
                    ));
                }
                if (self.globals.has_variable(op_assign.name.lexeme)) {
                    const name = self.to_c_string(op_assign.name.lexeme);
                    return c.BinaryenGlobalSet(self.module, name, c.BinaryenBinary(
                        self.module,
                        op,
                        c.BinaryenGlobalGet(self.module, name, c.BinaryenTypeInt32()),
                        value,
                    ));
                }
                return CompilerError.UnknownVariable;
            }

            return CompilerError.VariableAssignationInGlobalScope;
        },
        .Call => |call| {
            if (!self.in_global_scope()) {
                var args = try self.allocator.alloc(c.BinaryenExpressionRef, call.args.len);

                for (call.args, 0..) |arg_expr, i| {
                    args[i] = try self.compile_expr(arg_expr);
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
        .If => |if_expr| {
            return c.BinaryenIf(
                self.module,
                try self.compile_expr(if_expr.condition),
                try self.compile_expr(if_expr.then_branch),
                if (if_expr.else_branch) |else_branch|
                    try self.compile_expr(else_branch)
                else
                    null,
            );
        },
        .While => |while_expr| {
            const body = try self.compile_expr(while_expr.body);
            const condition = try self.compile_expr(while_expr.condition);

            try self.m.begin_block("while", c.BinaryenTypeAuto());
            {
                try self.m.begin_block("inner", c.BinaryenTypeAuto());
                {
                    _ = try self.m.expr(
                        c.BinaryenBreak(
                            self.module,
                            "while",
                            c.BinaryenBinary(
                                self.module,
                                c.BinaryenNeInt32(),
                                condition,
                                c.BinaryenConst(
                                    self.module,
                                    c.BinaryenLiteralInt32(
                                        @as(i32, 1),
                                    ),
                                ),
                            ),
                            null,
                        ),
                    );
                    _ = try self.m.expr(body);
                    _ = try self.m.expr(
                        c.BinaryenBreak(
                            self.module,
                            "loop",
                            null,
                            null,
                        ),
                    );
                }
                _ = try self.m.end_block_as_loop("loop");
            }

            return try self.m.end_block();
        },
        .Break => |break_expr| {
            return c.BinaryenBreak(
                self.module,
                self.to_c_string("while"),
                null,
                if (break_expr.value) |value| try self.compile_expr(value) else null,
            );
        },
        .Continue => |_| {
            return c.BinaryenBreak(
                self.module,
                self.to_c_string("loop"),
                null,
                null,
            );
        },
        .Import => |import| {
            c.BinaryenAddFunctionImport(self.module, self.to_c_string(import.member.lexeme), self.to_c_string(import.namespace.lexeme), self.to_c_string(import.member.lexeme), c.BinaryenTypeInt32(), c.BinaryenTypeInt32());
            try self.globals.add_function(import.member.lexeme);
            return null;
            // c.BinaryenGetFunction(module: BinaryenModuleRef, name: [*c]const u8)
        },
        .Return => |return_expr| {
            //@todo return in webassembly is not like return in other prog languges
            return c.BinaryenReturn(self.module, if (return_expr.value) |value| try self.compile_expr(value) else null);
            // c.BinaryenGetFunction(module: BinaryenModuleRef, name: [*c]const u8)
        },
        else => {
            std.debug.print("\n Compiler : expression type not implemented for {any}\n", .{expr});
            unreachable;
        },
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

fn get_binaryen_type(token: *const Token) CompilerError!c.BinaryenType {
    const @"type" = Type.from_str(token.lexeme) catch return CompilerError.InvalidType;

    return switch (@"type") {
        .i32, .bool => c.BinaryenTypeInt32(),
        .i64 => c.BinaryenTypeInt64(),
        .f32 => c.BinaryenTypeFloat32(),
        .f64 => c.BinaryenTypeFloat64(),
        .void => c.BinaryenTypeNone(),
        else => unreachable,
    };
}

inline fn in_global_scope(self: *@This()) bool {
    return self.current_env == null;
}

fn infer_type(expr: *const Expr) !?Type {
    return switch (expr.*) {
        .Literal => |literal| {
            return switch (literal.value) {
                .Number => |number| {
                    const is_float = @rem(number, 1) != 0;
                    if (is_float) {
                        return if (number > floatMax(f32)) .f64 else .f32;
                    } else {
                        return if (number > maxInt(i32)) .i64 else .i32;
                    }
                },
                else => CompilerError.CannotInferType,
            };
        },
        .Function => null,
        else => CompilerError.CannotInferType,
    };
}
