const std = @import("std");

variables: std.StringHashMap(void),

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .variables = std.StringHashMap(void).init(allocator),
        .allocator = allocator,
    };
}

pub fn deinit(self: *@This()) void {
    self.variables.deinit();
}
