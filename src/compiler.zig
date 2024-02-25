allocator: std.mem.Allocator,
ir: []Ir.Inst,
binaryen: Binaryen,

const CompileError = error{NotImplemented};

pub fn init(allocator: std.mem.Allocator, ir: []Ir.Inst) Compiler {
    return .{
        .allocator = allocator,
        .ir = ir,
        .binaryen = Binaryen.init(
            c.BinaryenModuleCreate(),
        ),
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
            _ = self.binaryen.add_immutable_global(
                self.to_c_str(global_i32.name),
                .i32,
                global_i32.value,
            );
        },
        else => return CompileError.NotImplemented,
    }
}

pub fn write(self: *Compiler) !void {
    c.BinaryenModulePrint(self.binaryen.module);

    var buffer: [10_000]u8 = undefined;

    const out = c.BinaryenModuleAllocateAndWriteText(self.binaryen.module);

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

const Binaryen = struct {
    module: c.BinaryenModuleRef,

    pub fn init(module: c.BinaryenModuleRef) Binaryen {
        return .{
            .module = module,
        };
    }

    pub fn constant(self: *Binaryen, comptime tid: Infer.TypeID, value: anytype) c.BinaryenExpressionRef {
        return c.BinaryenConst(self.module, self.literal(tid, value));
        // return c.BinaryenAddGlobal(
        //     self.module,
        //     self.to_c_str(global_i32.name),
        //     Bin.ty(.i32),
        //     false,
        //     c.BinaryenConst(self.module, Bin.literal(.i32, global_i32.value)),
        // );
    }

    pub fn add_immutable_global(
        self: *Binaryen,
        name: [*:0]const u8,
        comptime tid: Infer.TypeID,
        value: anytype,
    ) c.BinaryenGlobalRef {
        return c.BinaryenAddGlobal(
            self.module,
            name,
            self.ty(tid),
            false,
            self.constant(tid, value),
        );
    }

    pub fn add_mutable_global(
        self: *Binaryen,
        name: [*:0]const u8,
        comptime tid: Infer.TypeID,
        value: anytype,
    ) c.BinaryenExpressionRef {
        return c.BinaryenAddGlobal(
            self.module,
            name,
            self.ty(tid),
            true,
            self.constant(tid, value),
        );
    }

    pub fn ty(self: *Binaryen, comptime tid: Infer.TypeID) c.BinaryenType {
        _ = self;
        return switch (tid) {
            .i32, .bool => c.BinaryenTypeInt32(),
            .i64 => c.BinaryenTypeInt64(),
            .f32 => c.BinaryenTypeFloat32(),
            .f64 => c.BinaryenTypeFloat64(),
            .void => c.BinaryenTypeNone(),
            else => unreachable,
        };
    }

    pub fn literal(self: *Binaryen, comptime tid: Infer.TypeID, value: anytype) c.BinaryenLiteral {
        _ = self;

        return switch (tid) {
            .i32, .bool => c.BinaryenLiteralInt32(value),
            .i64 => c.BinaryenLiteralInt64(value),
            .f32 => c.BinaryenLiteralFloat32(value),
            .f64 => c.BinaryenLiteralFloat64(value),
            else => unreachable,
        };
    }
};

const std = @import("std");
const c = @cImport({
    @cInclude("binaryen-c.h");
});
const Compiler = @This();
const Infer = @import("./infer.zig");
const Ir = @import("./ir.zig");
