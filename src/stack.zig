const std = @import("std");
// @todo do the unmanaged version
pub fn Stack(comptime T: type) type {
    return struct {
        items: []T,
        count: u32,
        capacity: usize,
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Stack(T) {
            return .{
                .items = &[_]T{},
                .count = 0,
                .capacity = 0,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Stack(T)) void {
            self.allocator.free(self.items);
        }

        pub fn push(self: *Stack(T), item: T) !u32 {
            if (self.count > self.capacity) {
                try self.grow();
            }

            const index = self.count;

            self.items[index] = item;

            self.count += 1;

            return index;
        }

        pub fn pop(self: *Stack(T)) T {
            const last = self.items[self.count - 1];
            self.count -= 1;
            return last;
        }

        pub fn top(self: *Stack(T)) T {
            return self.items[self.count - 1];
        }

        pub fn maybe_top_ptr(self: *Stack(T)) ?*T {
            if (self.count == 0) {
                return null;
            }
            return &self.items[self.count - 1];
        }

        pub fn maybe_top(self: *Stack(T)) ?T {
            if (self.count == 0) {
                return null;
            }
            return self.items[self.count - 1];
        }

        pub fn clear(self: *Stack(T)) !void {
            self.items = try self.allocator.realloc(self.items, 1);
            self.capacity = self.items.len;
            self.count = 0;
        }

        fn grow(self: *Stack(T)) !void {
            self.items = try self.allocator.realloc(self.items, (self.capacity + 1) * 2);
            self.capacity = self.items.len;
        }
    };
}

test Stack {
    var stack = Stack(i32).init(std.testing.allocator);
    defer stack.deinit();

    _ = try stack.push(1);
    _ = try stack.push(2);
    _ = try stack.push(3);

    try std.testing.expect(stack.top() == 3);
    try std.testing.expect(stack.pop() == 3);
    try std.testing.expect(stack.pop() == 2);
    try std.testing.expect(stack.pop() == 1);
}
