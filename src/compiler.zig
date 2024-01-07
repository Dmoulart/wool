const std = @import("std");

const Codegen = @import("./codegen.zig");

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
    VariableAssignationInGlobalScope,
    OutOfMemory,
    FunctionCallInGlobalScope,
    ConstantAssignation,
};

allocator: std.mem.Allocator,

ast: []*Expr,

module: c.BinaryenModuleRef,

environments: std.ArrayList(Environment),

current_env: ?*Environment,

globals: Globals,

blocks_children: std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)),

args: std.AutoArrayHashMap(*const Expr, []c.BinaryenExpressionRef),

m: Codegen,

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
        .m = Codegen.init(module.?, allocator),
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

    c.BinaryenModuleDispose(self.module);
}

pub fn compile(self: *@This()) !void {
    for (self.ast) |expr| {
        _ = try self.expression(expr);
    }

    try self.write();
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
                try self.globals.add_constant(const_init.name.lexeme);
                _ = c.BinaryenAddGlobal(self.module, @ptrCast(name), c.BinaryenTypeInt32(), false, value);
            }

            return value;
        },
        .VarInit => |*var_init| {
            if (self.current_env) |current_env| {
                const idx = current_env.increment_index();
                try current_env.set(var_init.name, idx);
                try current_env.add_local_type(c.BinaryenTypeInt32());

                const value = try self.expression(var_init.initializer);

                return c.BinaryenLocalSet(self.module, @intCast(idx), value);
            } else {
                const lexeme = var_init.name.lexeme;
                try self.globals.add_variable(lexeme);
                _ = c.BinaryenAddGlobal(
                    self.module,
                    self.to_c_string(lexeme),
                    c.BinaryenTypeInt32(),
                    true,
                    try self.expression(var_init.initializer),
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
                    binaryen_args[i] = c.BinaryenTypeInt32();
                    try env.set(arg, @intCast(i));
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

            const func_ref = c.BinaryenAddFunction(
                self.module,
                name_ptr,
                params orelse 0, // ?
                c.BinaryenTypeInt32(),
                var_types.ptr,
                @intCast(var_types.len),
                body,
            );
            _ = func_ref;

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
            const left = try self.expression(binary.left);
            const right = try self.expression(binary.right);

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
                if (current_env.get_index_by_name(assign.name.lexeme)) |idx| {
                    const value = try self.expression(assign.value);
                    return c.BinaryenLocalSet(self.module, @intCast(idx), value);
                } else if (self.globals.has_variable(assign.name.lexeme)) {
                    const value = try self.expression(assign.value);
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
                const value = try self.expression(op_assign.value);
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
        .If => |if_expr| {
            return c.BinaryenIf(
                self.module,
                try self.expression(if_expr.condition),
                try self.expression(if_expr.then_branch),
                if (if_expr.else_branch) |else_branch|
                    try self.expression(else_branch)
                else
                    null,
            );
        },
        .While => |while_expr| {
            const body = try self.expression(while_expr.body);
            const condition = try self.expression(while_expr.condition);

            try self.m.begin_block("while", c.BinaryenTypeAuto());
            try self.m.begin_block("inner", c.BinaryenTypeAuto());
            // const brk_condition =
            // _ = try self.m.expr(c.BinaryenBrOn(self.module, c.BinaryenNeInt32(), "while", condition, c.BinaryenTypeInt32()));
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
            _ = try self.m.expr(c.BinaryenBreak(self.module, "loop", null, null));
            _ = try self.m.end_block_as_loop("loop");

            return try self.m.end_block();

            // const body = try self.expression(while_expr.body);
            // const condition = try self.expression(while_expr.condition);
            // const refs = try self.blocks_children.addOne();
            // refs.* = std.ArrayList(c.BinaryenExpressionRef).init(self.allocator);

            // const break_expr = c.BinaryenBreak(
            //     self.module,
            //     self.to_c_string("loop"),
            //     condition,
            //     null,
            // );

            // const loop = c.BinaryenLoop(self.module, "loop", body);
            // _ = loop;

            // try refs.append(break_expr);
            // try refs.append(body);

            // const block = c.BinaryenBlock(
            //     self.module,
            //     "loop_block",
            //     @ptrCast(refs.items),
            //     2,
            //     c.BinaryenTypeAuto(),
            // );

            // return c.BinaryenLoop(self.module, "loop", block);

            // const body = try self.expression(while_expr.body);
            // const condition = try self.expression(while_expr.condition);
            // const block = c.BinaryenBlock(self.module, "block", children: [*c]BinaryenExpressionRef, 2, c.BinaryenTypeAuto());

            // const loop_expr = c.BinaryenLoop(self.module, "loop", try self.expression(loop.body));
            // c.BinaryenBrOn(self.module, op: BinaryenOp, "loop", ref: BinaryenExpressionRef, castType: BinaryenType)

            // const block_children = ;

            // c.RelooperAddBranch(from: RelooperBlockRef, to: RelooperBlockRef, condition: BinaryenExpressionRef, code: BinaryenExpressionRef)
            // c.RelooperAddBlock(relooper, try self.expression(loop.body));
            // const loop_expr = c.BinaryenLoop(self.module, "loop", try self.expression(loop.body));
            // return loop_expr;

            // const relooper = c.RelooperCreate(self.module);
            // const block = c.RelooperAddBlock(relooper, try self.expression(while_expr.body));
            // const condition = try self.expression(while_expr.condition);
            // _ = condition;
            // // c.RelooperAddBranch(block, block, condition, null);
            // // c.Relooper
            // const body = c.RelooperRenderAndDispose(relooper, block, 0);

            // return body;
        },
        // .Loop => |loop| {
        //     // const loop_expr = c.BinaryenLoop(self.module, "loop", try self.expression(loop.body));
        //     // c.BinaryenBrOn(self.module, op: BinaryenOp, "loop", ref: BinaryenExpressionRef, castType: BinaryenType)

        //     // const block = c.BinaryenBlock(self.module, "loop_inside_block", children: [*c]BinaryenExpressionRef, 2, c.BinaryenTypeAuto());
        //     // const block_children = ;

        //     const relooper = c.RelooperCreate(self.module);
        //     const block = c.RelooperAddBlock(relooper, try self.expression(loop.body));
        //     c.RelooperAddBranch(block, block, null, null);
        //     // c.Relooper
        //     const body = c.RelooperRenderAndDispose(relooper, block, 0);

        //     return body;

        //     // c.RelooperAddBranch(from: RelooperBlockRef, to: RelooperBlockRef, condition: BinaryenExpressionRef, code: BinaryenExpressionRef)
        //     // c.RelooperAddBlock(relooper, try self.expression(loop.body));
        //     // const loop_expr = c.BinaryenLoop(self.module, "loop", try self.expression(loop.body));
        //     // return loop_expr;
        // },
        .Break => |break_expr| {
            return c.BinaryenBreak(
                self.module,
                self.to_c_string("while"),
                null,
                if (break_expr.value) |value| try self.expression(value) else null,
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
            return c.BinaryenReturn(self.module, if (return_expr.value) |value| try self.expression(value) else null);
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

inline fn in_global_scope(self: *@This()) bool {
    return self.current_env == null;
}
