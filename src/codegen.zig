pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

const std = @import("std");

module: *c.BinaryenModule,

exprs: std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)),

allocator: std.mem.Allocator,

blocks: std.ArrayList(Block),

const Block = struct {
    block_type: c.BinaryenType,

    block_name: []const u8,

    block_exprs: std.ArrayList(c.BinaryenExpressionRef),

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, @"type": c.BinaryenType) @This() {
        return .{
            .block_name = name,
            .block_exprs = std.ArrayList(c.BinaryenExpressionRef).init(allocator),
            .block_type = @"type",
            .allocator = allocator,
        };
    }
    pub fn deinit(self: *Block) void {
        self.block_exprs.deinit();
    }
};

pub fn init(module: *c.BinaryenModule, allocator: std.mem.Allocator) @This() {
    return .{
        .module = module,
        .exprs = std.ArrayList(std.ArrayList(c.BinaryenExpressionRef)).init(allocator),
        .allocator = allocator,
        .blocks = std.ArrayList(Block).init(allocator),
    };
}

pub fn deinit(self: *@This()) void {
    for (self.blocks.items) |*block| {
        block.deinit();
    }
    self.blocks.deinit();

    for (self.exprs.items) |exprs| {
        exprs.deinit();
    }
    self.exprs.deinit();
}

pub fn begin_block(self: *@This(), name: []const u8, @"type": c.BinaryenType) !void {
    try self.blocks.append(Block.init(self.allocator, name, @"type"));
}

pub fn expr(self: *@This(), ref: c.BinaryenExpressionRef) !c.BinaryenExpressionRef {
    if (self.current_block()) |block| {
        try block.block_exprs.append(ref);
        return ref;
    } else {
        unreachable;
    }
}

pub fn end_block(self: *@This()) !c.BinaryenExpressionRef {
    var exprs: *std.ArrayList(c.BinaryenExpressionRef) = try self.exprs.addOne();
    exprs.* = std.ArrayList(c.BinaryenExpressionRef).init(self.allocator);
    try exprs.appendSlice(try self.current_block().?.block_exprs.toOwnedSlice());

    const block = self.blocks.orderedRemove(self.blocks.items.len - 1);
    const block_ref = c.BinaryenBlock(
        self.module,
        self.to_c_string(block.block_name),
        @ptrCast(exprs.items),
        @intCast(exprs.items.len),
        block.block_type,
    );

    return if (self.current_block()) |_| try self.expr(block_ref) else block_ref;
}

fn to_c_string(self: *@This(), str: []const u8) [*:0]const u8 {
    //@todo horror museum + memory leak
    return @ptrCast(self.allocator.dupeZ(u8, str) catch unreachable);
}

fn current_block(self: *@This()) ?*Block {
    if (self.blocks.items.len == 0) return null;
    return @constCast(&self.blocks.items[self.blocks.items.len - 1]);
}

fn Stack(comptime T: type) type {
    _ = T;
    return struct {};
}
