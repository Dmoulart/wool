src: []const u8,
path: ?[]const u8,
lines: []u32,

pub fn get_line_start(self: *const @This(), line: u32) u32 {
    return self.lines[line - 2];
}

pub fn get_line_end(self: *const @This(), line: u32) usize {
    const next_line_index = line - 1;

    return if (self.lines.len <= next_line_index) self.src.len else self.lines[next_line_index];
}

pub fn get_line(self: *const @This(), line: u32) []const u8 {
    const col_start = self.get_line_start(line);
    const col_end = self.get_line_end(line);

    return self.src[col_start..col_end];
}

pub fn get_line_offset(self: *const @This(), line: u32, col: u32) usize {
    return col - self.get_line_start(line);
}
