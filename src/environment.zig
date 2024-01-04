const std = @import("std");
const Expr = @import("./ast/expr.zig").Expr;
const Token = @import("./token.zig");

pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

index_to_token: std.AutoHashMap(usize, *const Token),

name_to_index: std.StringHashMap(usize),

last_index: usize,

local_types: std.ArrayList(c.BinaryenType),

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .index_to_token = std.AutoHashMap(usize, *const Token).init(allocator),
        .name_to_index = std.StringHashMap(usize).init(allocator),
        .allocator = allocator,
        .last_index = 0,
        .local_types = std.ArrayList(c.BinaryenType).init(allocator),
    };
}

pub fn deinit(self: *@This()) void {
    self.index_to_token.deinit();
    self.name_to_index.deinit();
    self.local_types.deinit();
}

pub fn get_index_by_name(self: *@This(), name: []const u8) ?usize {
    return self.name_to_index.get(name);
}

pub fn set(self: *@This(), token: *const Token, index: usize) !void {
    try self.index_to_token.put(index, token);
    try self.name_to_index.put(token.lexeme, index);
    self.last_index = index;
}

pub fn add_local_type(self: *@This(), local_type: c.BinaryenType) !void {
    try self.local_types.append(local_type);
}
