pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

const std = @import("std");

module: *c.BinaryenModule,

current_block_type: ?c.BinaryenType = null,

current_block_name: ?[]const u8 = null,

current_block_exprs: std.ArrayList(c.BinaryenExpressionRef),

exprs: std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)),

allocator: std.mem.Allocator,

pub fn init(module: *c.BinaryenModule, allocator: std.mem.Allocator) @This() {
    return .{
        .module = module,
        .current_block_exprs = std.ArrayList(c.BinaryenExpressionRef).init(allocator),
        .exprs = std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)).init(allocator),
        .allocator = allocator,
    };
}

pub fn deinit(self: *@This()) void {
    self.current_block_exprs.deinit();
    for (self.exprs.items) |exprs| {
        exprs.deinit();
    }
    self.exprs.deinit();
}

pub fn begin_block(self: *@This(), name: []const u8, @"type": c.BinaryenType) void {
    self.current_block_name = name;
    self.current_block_type = @"type";
}

pub fn expr(self: *@This(), ref: c.BinaryenExpressionRef) !void {
    var new_ref = try self.current_block_exprs.addOne();
    new_ref.* = ref;
}

pub fn end_block(self: *@This()) !c.BinaryenExpressionRef {
    var exprs: *std.ArrayList(c.BinaryenExpressionRef) = try self.exprs.addOne();
    exprs.* = std.ArrayList(c.BinaryenExpressionRef).init(self.allocator);
    try exprs.appendSlice(try self.current_block_exprs.toOwnedSlice());

    return c.BinaryenBlock(
        self.module,
        self.to_c_string(self.current_block_name.?),
        @ptrCast(exprs.items),
        @intCast(exprs.items.len),
        self.current_block_type.?,
    );
}

fn to_c_string(self: *@This(), str: []const u8) [*:0]const u8 {
    //@todo horror museum + memory leak
    return @ptrCast(self.allocator.dupeZ(u8, str) catch unreachable);
}
