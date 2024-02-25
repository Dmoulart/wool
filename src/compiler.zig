allocator: std.mem.Allocator,
ir: []Ir.Inst,
module: c.BinaryenModuleRef,

const CompileError = error{NotImplemented};

pub fn init(allocator: std.mem.Allocator, ir: []Ir.Inst) Compiler {
    return .{
        .allocator = allocator,
        .ir = ir,
        .module = c.BinaryenModuleCreate(),
    };
}

pub fn compile_program(self: *Compiler) !void {
    for (self.ir) |*instruction| {
        try self.compile(instruction);
    }
}

pub fn compile(self: *Compiler, inst: *Ir.Inst) !void {
    switch (inst.*) {
        // .push_i32 => |*push_i32| c.BinaryenConst(self.module, c.BinaryenLiteralInt32(push_i32)),
        .global_i32 => |*global_i32| {
            _ = c.BinaryenAddGlobal(
                self.module,
                self.to_c_str(global_i32.name),
                c.BinaryenTypeInt32(),
                false,
                c.BinaryenConst(self.module, c.BinaryenLiteralInt32(global_i32.value)),
            );
        },
        else => return CompileError.NotImplemented,
    }
}

pub fn write(self: *Compiler) !void {
    c.BinaryenModulePrint(self.module);

    var buffer: [10_000]u8 = undefined;

    const out = c.BinaryenModuleAllocateAndWriteText(self.module);

    const code = try std.fmt.bufPrintZ(&buffer, "{s}", .{out});

    const file_path = "./out.wat";

    const file = try std.fs.cwd().createFile(
        file_path,
        .{ .read = true },
    );

    defer file.close();

    _ = try file.writeAll(code);
}

fn to_c_str(self: *Compiler, str: []const u8) [*:0]const u8 {
    //@todo horror museum + memory leak
    return @ptrCast(self.allocator.dupeZ(u8, str) catch unreachable);
}

const std = @import("std");
const c = @cImport({
    @cInclude("binaryen-c.h");
});
const Compiler = @This();
const Ir = @import("./ir.zig");
