loop_depth: u32 = 0,

const Self = @This();

pub fn begin_loop_scope(self: *Self) u32 {
    self.loop_depth += 1;
    return self.loop_depth;
}

pub fn current_loop_scope(self: *Self) u32 {
    return self.loop_depth;
}

pub fn in_loop_scope(self: *Self) bool {
    return self.loop_depth > 0;
}

pub fn end_loop_scope(self: *Self) u32 {
    self.loop_depth -= 1;
    return self.loop_depth;
}
