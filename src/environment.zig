local_types: std.ArrayList(c.BinaryenType),

stack: Stack(c.BinaryenExpressionRef),

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .allocator = allocator,
        .local_types = std.ArrayList(c.BinaryenType).init(allocator),
        .stack = Stack(c.BinaryenExpressionRef).init(allocator),
    };
}

pub fn deinit(self: *@This()) void {
    self.local_types.deinit();
    self.stack.deinit();
}

pub fn add_local_type(self: *@This(), local_type: c.BinaryenType) !void {
    try self.local_types.append(local_type);
}

const std = @import("std");
const Expr = @import("./ast/expr.zig").Expr;
const Token = @import("./token.zig");
const Stack = @import("./stack.zig").Stack;

pub const c = @cImport({
    @cInclude("binaryen-c.h");
});
