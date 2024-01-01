const std = @import("std");

const Token = @import("./token.zig");
const Stmt = @import("./ast/stmt.zig").Stmt;
const Expr = @import("./ast/expr.zig").Expr;

pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

allocator: std.mem.Allocator,

ast: []*Stmt,

pub fn init(allocator: std.mem.Allocator, ast: []*Stmt) @This() {
    return .{
        .allocator = allocator,
        .ast = ast,
    };
}

pub fn compile(self: *@This()) !void {
    var module = c.BinaryenModuleCreate();

    for (self.ast) |stmt| {
        try self.codegen(stmt);
    }

    // Create a function type for  i32 (i32, i32)
    var ii = [2]c.BinaryenType{ c.BinaryenTypeInt32(), c.BinaryenTypeInt32() };
    var params = c.BinaryenTypeCreate(@ptrCast(&ii), 2);
    var results = c.BinaryenTypeInt32();

    // Get the 0 and 1 arguments, and add them
    var x = c.BinaryenLocalGet(module, 0, c.BinaryenTypeInt32());
    var y = c.BinaryenLocalGet(module, 1, c.BinaryenTypeInt32());
    var add = c.BinaryenBinary(module, c.BinaryenAddInt32(), x, y);

    // Create the add function
    // Note: no additional local variables
    // Note: no basic blocks here, we are an AST. The function body is just an
    // expression node.
    var adder =
        c.BinaryenAddFunction(module, "_start", params, results, null, 0, add);
    _ = adder;

    _ = c.BinaryenAddFunctionExport(module, "_start", "_start");

    try write(module);
    // c.BinaryenModulePrint(module);
}

fn codegen(self: *@This(), stmt: *Stmt) !void {
    switch (stmt.*) {
        .Expr => |expr| {
            switch (expr) {
                .ConstInit => |const_init| {
                    const maybe_func = switch (const_init.initializer.*) {
                        .Function => |func| func,
                        else => null,
                    };

                    if (maybe_func) |func| {
                        var params: ?c.BinaryenType = null;
                        if (func.args) |args| {
                            var binaryen_args = try self.allocator.alloc(c.BinaryenType, @intCast(args.len));
                            for (args, 0..) |_, i| {
                                binaryen_args[i] = c.BinaryenTypeInt32();
                            }
                            params = c.BinaryenTypeCreate(@ptrCast(&binaryen_args), @intCast(args.len));
                        }
                        var result = c.BinaryenTypeInt32();
                        _ = result;
                    }
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

fn expression(self: *@This(), expr: *Expr) !void {
    switch (expr) {
        .ConstInit => |const_init| {
            const maybe_func = switch (const_init.initializer.*) {
                .Function => |func| func,
                else => null,
            };

            if (maybe_func) |func| {
                var params: ?c.BinaryenType = null;
                if (func.args) |args| {
                    var binaryen_args = try self.allocator.alloc(c.BinaryenType, @intCast(args.len));
                    for (args, 0..) |_, i| {
                        binaryen_args[i] = c.BinaryenTypeInt32();
                    }
                    params = c.BinaryenTypeCreate(@ptrCast(&binaryen_args), @intCast(args.len));
                }
                var result = c.BinaryenTypeInt32();
                _ = result;
            }
        },
        else => unreachable,
    }
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
