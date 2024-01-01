const std = @import("std");

const Token = @import("./token.zig");
const Stmt = @import("./ast/stmt.zig").Stmt;
const Expr = @import("./ast/expr.zig").Expr;

pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

pub fn compile(ast: []*Stmt) !void {
    _ = ast;

    var module = c.BinaryenModuleCreate();

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

    // Print it out
    // c.BinaryenModulePrint(module);
    var buf: [10_000]u8 = undefined;
    const code_from_c = c.BinaryenModuleAllocateAndWriteText(module);
    std.debug.print("text {s}", .{code_from_c});
    var code = try std.fmt.bufPrintZ(&buf, "{s}", .{code_from_c});

    const file_path = "./out.wat";
    var out = std.ArrayList(u8).init(std.heap.page_allocator);
    defer out.deinit();
    const file = try std.fs.cwd().createFile(
        file_path,
        .{ .read = true },
    );
    defer file.close();

    _ = try file.writeAll(code);

    // Clean up the module, which owns all the objects we created above
    c.BinaryenModuleDispose(module);
}
