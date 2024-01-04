const std = @import("std");

variables: std.StringHashMap(void),

functions: std.StringHashMap(void),

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .variables = std.StringHashMap(void).init(allocator),
        .functions = std.StringHashMap(void).init(allocator),
        .allocator = allocator,
    };
}

pub fn deinit(self: *@This()) void {
    self.variables.deinit();
    self.functions.deinit();
}

pub fn add_function(self: *@This(), function_name: []const u8) !void {
    try self.functions.put(function_name, {});
}

pub fn has_function(self: *@This(), function_name: []const u8) bool {
    return self.functions.get(function_name) != null;
}

pub fn add_variable(self: *@This(), variable_name: []const u8) !void {
    try self.variables.put(variable_name, {});
}

pub fn has_variable(self: *@This(), variable_name: []const u8) bool {
    return self.variables.get(variable_name) != null;
}
