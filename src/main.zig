const std = @import("std");
const fs = std.fs;
const io = std.io;
const print = std.debug.print;
const Lexer = @import("./lexer.zig");
const Parser = @import("./parser.zig");

pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

pub fn main() !void {
    // var module = c.BinaryenModuleCreate();
    // var ret = c.BinaryenModulePrint(module);
    // print("{any}", .{ret});

    const alloc = std.heap.page_allocator;
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len > 1) {
        try runFile(args[1]);
    } else {
        try runPrompt();
    }
}

fn runFile(filepath: [:0]u8) !void {
    var file = try fs.cwd().openFile(filepath, .{});
    defer file.close();

    var allocator = std.heap.page_allocator;
    const file_size = (try file.stat()).size;
    var buf = try allocator.alloc(u8, file_size);

    try file.reader().readNoEof(buf);
    try run(buf);
}

fn runPrompt() !void {
    const stdin = std.io.getStdIn().reader();

    var buf: [1024]u8 = undefined;

    while (true) {
        print("\n> ", .{});

        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |line| {
            if (std.mem.eql(u8, line, "exit")) {
                return;
            }

            try run(line);
        }
    }
}

fn run(src: []const u8) !void {
    var lexer = Lexer.init(src, std.heap.page_allocator);
    defer lexer.deinit();

    const tokens = try lexer.scan();
    var parser = Parser.init(tokens, std.heap.page_allocator);

    const ast = try parser.parse();

    try jsonPrint(tokens, "./tokens.json");
    try jsonPrint(ast, "./ast.json");
}

pub fn jsonPrint(value: anytype, file_path: []const u8) !void {
    var out = std.ArrayList(u8).init(std.heap.page_allocator);
    defer out.deinit();

    try std.json.stringify(value, .{}, out.writer());

    const file = try std.fs.cwd().createFile(
        file_path,
        .{ .read = true },
    );
    defer file.close();

    _ = try file.writeAll(try out.toOwnedSlice());
}
