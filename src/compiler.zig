const std = @import("std");

const Token = @import("./token.zig");
const Stmt = @import("./ast/stmt.zig").Stmt;
const Expr = @import("./ast/expr.zig").Expr;

const Environment = @import("./environment.zig");

pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
const Err = ErrorReporter(CompilerError);

pub const CompilerError = error{
    UnknownVariable,
    UnknownConstant,
    OutOfMemory,
};

allocator: std.mem.Allocator,

ast: []*Expr,

module: c.BinaryenModuleRef,

environments: std.ArrayList(Environment),

current_env: *Environment,

globals: *Environment,

blocks_children: std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)),

pub fn init(allocator: std.mem.Allocator, ast: []*Expr) @This() {
    var environments = std.ArrayList(Environment).init(allocator);

    var globals = environments.addOne() catch unreachable;

    globals.* = Environment.init(allocator);

    var current_env = globals;

    var blocks_children = std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)).init(allocator);

    var module = c.BinaryenModuleCreate();

    return .{
        .allocator = allocator,
        .ast = ast,
        .module = module,
        .environments = environments,
        .globals = globals,
        .current_env = current_env,
        .blocks_children = blocks_children,
    };
}

pub fn deinit(self: *@This()) void {
    for (self.blocks_children.items) |child_exprs| {
        child_exprs.deinit();
    }

    self.blocks_children.deinit();

    c.BinaryenModuleDispose(self.module);
}

pub fn compile(self: *@This()) !void {
    for (self.ast) |stmt| {
        _ = try self.codegen(stmt);
    }
    // // Create a function type for  i32 (i32, i32)
    // var ii = [2]c.BinaryenType{ c.BinaryenTypeInt32(), c.BinaryenTypeInt32() };
    // var params = c.BinaryenTypeCreate(@ptrCast(&ii), 2);
    // var results = c.BinaryenTypeInt32();

    // // Get the 0 and 1 arguments, and add them
    // var x = c.BinaryenLocalGet(module, 0, c.BinaryenTypeInt32());
    // var y = c.BinaryenLocalGet(module, 1, c.BinaryenTypeInt32());
    // var add = c.BinaryenBinary(module, c.BinaryenAddInt32(), x, y);

    // // Create the add function
    // // Note: no additional local variables
    // // Note: no basic blocks here, we are an AST. The function body is just an
    // // expression node.
    // var adder =
    //     c.BinaryenAddFunction(module, "_start", params, results, null, 0, add);
    // _ = adder;

    // _ = c.BinaryenAddFunctionExport(module, "_start", "_start");

    // c.BinaryenModulePrint(self.module);

    try self.write();
}

fn codegen(self: *@This(), expr: *Expr) !c.BinaryenExpressionRef {
    return try self.expression(expr);
}

fn expression(self: *@This(), expr: *const Expr) !c.BinaryenExpressionRef {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            return try self.expression(const_init.initializer);
        },
        .Function => |func| {
            var env: *Environment = try self.environments.addOne();
            env.* = Environment.init(self.allocator);
            self.current_env = env;

            var params: ?c.BinaryenType = null;

            if (func.args) |args| {
                var binaryen_args = try self.allocator.alloc(c.BinaryenType, @intCast(args.len));

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

            _ = c.BinaryenAddFunction(
                self.module,
                name_ptr,
                params orelse 0, // ?
                c.BinaryenTypeInt32(),
                null,
                0,
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
                .SLASH => c.BinaryenDivSInt32(), // div s ? div u ?
                else => unreachable,
            };

            return c.BinaryenBinary(self.module, op, left, right);
        },
        .Variable => |variable| {
            if (self.current_env.get_index_by_name(variable.name.lexeme)) |index| {
                return c.BinaryenLocalGet(
                    self.module,
                    @intCast(index),
                    c.BinaryenTypeInt32(),
                );
            } else {
                return Err.raise(
                    variable.name,
                    CompilerError.UnknownVariable,
                    @errorName(CompilerError.UnknownVariable),
                );
            }
        },
        .Literal => |literal| {
            const value = switch (literal.value) {
                .Boolean => |boolean| c.BinaryenLiteralInt32(if (boolean) @as(i32, 1) else @as(i32, 0)),
                .Integer => |integer| c.BinaryenLiteralInt32(integer),
                .Float => |float| c.BinaryenLiteralFloat32(float),
                else => unreachable,
            };
            return c.BinaryenConst(self.module, value);
        },
        // .Block => |block| {
        //     for (block.exprs) |child_expr| {
        //         _ = try self.expression(child_expr);
        //     }
        // },
        // -> wrong block ???!!!
        // https://openhome.cc/eGossip/WebAssembly/Block.html
        .Block => |block| {
            // var children = self.allocator.alloc(c.BinaryenExpressionRef, block.exprs.len) catch return CompilerError.OutOfMemory;
            var children = std.ArrayList(c.BinaryenExpressionRef).init(self.allocator);
            _ = children;
            var block_exprs = self.blocks_children.addOne() catch return CompilerError.OutOfMemory;

            block_exprs.* = std.ArrayList(c.BinaryenExpressionRef).init(self.allocator);

            for (block.exprs, 0..) |child_expr, i| {
                _ = i;
                const binaryen_expr = try self.expression(child_expr);
                block_exprs.append(binaryen_expr) catch return CompilerError.OutOfMemory;
            }

            // var test_children = [_]c.BinaryenExpressionRef{c.BinaryenConst(self.module, c.BinaryenLiteralInt32(@as(i32, 0)))};
            // var test_children_2: [*c]c.BinaryenExpressionRef = &test_children;

            return c.BinaryenBlock(
                self.module,
                null,
                @ptrCast(block_exprs.items),
                @intCast(block_exprs.items.len),
                c.BinaryenTypeAuto(),
            );
        },
        else => unreachable,
    };
}

fn write(self: *@This()) !void {
    // Print it out

    c.BinaryenModulePrint(self.module);

    // var buf: [10_000]u8 = std.mem.zeroes([10_000]u8);

    // var output = &buf;

    // var written_bytes = c.BinaryenModuleWrite(self.module, @ptrCast(output), 10_000);

    // const file_path = "./out.wat";

    // const file = try std.fs.cwd().createFile(
    //     file_path,
    //     .{ .read = true },
    // );

    // defer file.close();

    // _ = try file.writeAll(output[0..written_bytes]);

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
