const std = @import("std");
const Expr = @import("./ast/expr.zig").Expr;
const Token = @import("./token.zig");

index_to_token: std.AutoHashMap(usize, *const Token),

name_to_index: std.StringHashMap(usize),

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .index_to_token = std.AutoHashMap(usize, *const Token).init(allocator),
        .name_to_index = std.StringHashMap(usize).init(allocator),
        .allocator = allocator,
    };
}

pub fn deinit(self: *@This()) void {
    self.index_to_token.deinit();
    self.name_to_index.deinit();
}

pub fn get_index_by_name(self: *@This(), name: []const u8) ?usize {
    return self.name_to_index.get(name);
}

pub fn set(self: *@This(), token: *const Token, index: usize) !void {
    try self.index_to_token.put(index, token);
    try self.name_to_index.put(token.lexeme, index);
}
