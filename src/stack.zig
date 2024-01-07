const std = @import("std");

pub fn Stack(comptime T: type) type {
    return struct {
        items: []T,
        len: u32,
        capacity: usize,
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Stack(T) {
            return .{
                .items = &[_]T{},
                .len = 0,
                .capacity = 0,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Stack(T)) void {
            self.allocator.free(self.items);
        }

        pub fn push(self: *Stack(T), item: T) !void {
            const count = self.len + 1;

            if (count > self.capacity) {
                try self.grow();
            }

            self.items[self.len] = item;

            self.len = count;
        }

        pub fn pop(self: *Stack(T)) T {
            const last = self.items[self.len - 1];
            self.len -= 1;
            return last;
        }

        pub fn top(self: *Stack(T)) T {
            return self.items[self.len - 1];
        }

        pub fn maybe_top_ptr(self: *Stack(T)) ?*T {
            if (self.len > 0) {
                return &self.items[self.len - 1];
            }
            return null;
        }

        pub fn maybe_top(self: *Stack(T)) ?T {
            return self.items[self.len - 1];
        }

        inline fn grow(self: *Stack(T)) !void {
            self.items = try self.allocator.realloc(self.items, (self.capacity + 1) * 2);
            self.capacity = self.items.len;
        }
    };
}

test Stack {
    var stack = Stack(i32).init(std.testing.allocator);
    defer stack.deinit();

    try stack.push(1);
    try stack.push(2);
    try stack.push(3);

    try std.testing.expect(stack.top() == 3);
    try std.testing.expect(stack.pop() == 3);
    try std.testing.expect(stack.pop() == 2);
    try std.testing.expect(stack.pop() == 1);
}
