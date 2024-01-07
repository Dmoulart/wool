const std = @import("std");
const FieldEnum = std.meta.FieldEnum;
const activeTag = std.meta.activeTag;
const Tag = std.meta.Tag;
const Stack = @import("./stack.zig").Stack;
const Expr = @import("./ast/expr.zig").Expr;

const Type = @import("./types.zig").Type;

frames: Stack(ExpressionFrame),

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) @This() {
    return .{
        .frames = Stack(ExpressionFrame).init(allocator),
        .allocator = allocator,
    };
}

pub fn deinit(self: *@This()) void {
    self.frames.deinit();
}

pub fn in(self: *@This(), comptime expr_types: []const []const u8) bool {
    if (self.current_frame()) |frame| {
        const active_expr = @tagName(activeTag(frame.expr.*));

        for (expr_types) |expr_type| {
            if (std.mem.eql(u8, active_expr, expr_type)) return true;
        }
    }

    return false;
}

pub fn current_frame(self: *@This()) ?*ExpressionFrame {
    return self.frames.maybe_top_ptr();
}

pub fn push_frame(self: *@This(), frame: ExpressionFrame) !void {
    try self.frames.push(frame);
}

pub fn pop_frame(self: *@This()) ?ExpressionFrame {
    return self.frames.pop();
}

pub const ExpressionFrame = struct {
    expr: *const Expr,
    expr_type: Type,
};
