block_depth: u32 = 0,

const Self = @This();

pub fn begin_block_scope(self: *Self) u32 {
    self.block_depth += 1;
    return self.block_depth;
}

pub fn current_block_scope(self: *Self) u32 {
    return self.block_depth;
}

pub fn in_block_scope(self: *Self) bool {
    return self.block_depth > 0;
}

pub fn end_block_scope(self: *Self) u32 {
    self.block_depth -= 1;
    return self.block_depth;
}
