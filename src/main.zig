pub fn main() !void {
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
    try run(buf, std.heap.page_allocator);
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

            try run(line, std.heap.page_allocator);
        }
    }
}

fn run(src: []const u8, allocator: std.mem.Allocator) !void {
    var lexer = Lexer.init(src, allocator);
    defer lexer.deinit();

    const tokens = try lexer.scan();
    try jsonPrint(tokens, "./tokens.json");

    var parser = Parser.init(tokens, allocator);
    defer parser.deinit();

    const ast = try parser.parse();
    try jsonPrint(ast, "./ast.json");

    var infer = Infer.init(allocator, ast);
    const sems = try infer.infer_program();
    var ir = Ir.init(allocator);

    var program = try ir.convert_program(sems);
    try jsonPrint(program, "./ir.json");

    var compiler = Compiler.init(allocator, program);
    try compiler.compile_program();

    try compiler.write();

    // try jsonPrint(sems., "./sems.json");

    // var compiler = Compiler.init(allocator, ast, sems);
    // defer compiler.deinit();

    // try compiler.compile();

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

const std = @import("std");
const fs = std.fs;
const io = std.io;
const print = std.debug.print;

const Lexer = @import("./lexer.zig");
const Parser = @import("./parser.zig");
const Infer = @import("./infer.zig");
const Ir = @import("./ir.zig");
const Compiler = @import("./compiler.zig");
// const Compiler = @import("./compiler.zig");
