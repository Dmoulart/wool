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
};

allocator: std.mem.Allocator,

ast: []*Expr,

module: c.BinaryenModuleRef,

environments: std.ArrayList(Environment),

current_env: *Environment,

globals: *Environment,

pub fn init(allocator: std.mem.Allocator, ast: []*Expr) @This() {
    var environments = std.ArrayList(Environment).init(allocator);

    var globals = environments.addOne() catch unreachable;

    globals.* = Environment.init(allocator);

    var current_env = globals;

    return .{
        .allocator = allocator,
        .ast = ast,
        .module = c.BinaryenModuleCreate(),
        .environments = environments,
        .globals = globals,
        .current_env = current_env,
    };
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

    c.BinaryenModulePrint(self.module);

    try write(self.module);
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

            var result = c.BinaryenTypeInt32();

            const body = if (func.body) |body| try self.expression(body) else null;

            const name = if (func.name) |name| name.lexeme else "anonymous_func";

            // horror museum @todo clean this crap up @todo free ?
            const c_name: [*:0]const u8 = @ptrCast(self.allocator.dupeZ(u8, name) catch unreachable);
            _ = c.BinaryenAddFunction(
                self.module,
                c_name,
                params orelse 0, // ?
                result,
                null,
                0,
                body,
            );

            _ = c.BinaryenAddFunctionExport(
                self.module,
                c_name,
                c_name,
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
        else => unreachable,
    };
}

fn write(module: c.BinaryenModuleRef) !void {
    // Print it out
    var buf: [10_000]u8 = undefined;

    const code_from_c = c.BinaryenModuleAllocateAndWriteText(module);

    var code = try std.fmt.bufPrintZ(&buf, "{s}", .{code_from_c});

    const file_path = "./out.wat";

    const file = try std.fs.cwd().createFile(
        file_path,
        .{ .read = true },
    );

    defer file.close();

    _ = try file.writeAll(code);

    // Clean up the module, which owns all the objects we created above
    c.BinaryenModuleDispose(module);
}
