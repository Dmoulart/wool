local_types: std.ArrayList(c.BinaryenType),

args_nb: u32 = 0,

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .allocator = allocator,
        .local_types = std.ArrayList(c.BinaryenType).init(allocator),
    };
}

pub fn deinit(self: *@This()) void {
    self.local_types.deinit();
}

pub fn new_local(self: *@This(), ty: c.BinaryenType) !usize {
    const i = self.local_types.items.len;
    try self.local_types.append(ty);
    return i + self.args_nb;
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
