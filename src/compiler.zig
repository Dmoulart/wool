allocator: std.mem.Allocator,
ir: []Ir.Inst,
binaryen: Binaryen,
instruction_cursor: usize,
current_env: ?*Environment,
environments: std.ArrayListUnmanaged(Environment),

const CompileError = error{NotImplemented};

pub fn init(allocator: std.mem.Allocator, ir: []Ir.Inst) Compiler {
    return .{
        .allocator = allocator,
        .ir = ir,
        .binaryen = Binaryen.init(
            c.BinaryenModuleCreate(),
        ),
        .instruction_cursor = 1,
        .environments = .{},
        .current_env = null,
    };
}

pub fn compile_program(self: *Compiler) !void {
    while (self.next_instruction()) |instruction| {
        _ = try self.compile(@constCast(instruction));
    }
}

pub fn compile_until(self: *Compiler, tag: Tag(Ir.Inst)) ![]c.BinaryenExpressionRef {
    var refs: std.ArrayListUnmanaged(c.BinaryenExpressionRef) = .{};

    while (!self.is_at_end() and !self.next_instruction_is(tag)) {
        var inst = self.next_instruction().?;

        if (try self.compile(inst)) |ref| {
            try refs.append(self.allocator, ref);
        }
    }

    return refs.items;
}

pub fn compile(self: *Compiler, inst: *Ir.Inst) !?c.BinaryenExpressionRef {
    if (is_global_instruction(inst)) {
        try self.compile_global(inst);
        return null;
    } else if (is_value_instruction(inst)) {
        try self.compile_value(inst);
        return null;
    } else if (is_no_return_instruction(inst)) {
        return null;
    }
    return try self.compile_expr(inst);
}

pub fn compile_global(self: *Compiler, inst: *Ir.Inst) !void {
    switch (inst.*) {
        // @todo: factorize this in a good way ?
        .global_i32 => |*global| {
            _ = self.binaryen.add_immutable_global(
                self.to_c_str(global.name),
                .i32,
                global.value,
            );
        },
        .global_i64 => |*global| {
            _ = self.binaryen.add_immutable_global(
                self.to_c_str(global.name),
                .i64,
                global.value,
            );
        },
        .global_f32 => |*global| {
            _ = self.binaryen.add_immutable_global(
                self.to_c_str(global.name),
                .f32,
                global.value,
            );
        },
        .global_f64 => |*global| {
            _ = self.binaryen.add_immutable_global(
                self.to_c_str(global.name),
                .f64,
                global.value,
            );
        },
        .begin_func => |*begin_func| {
            try self.use_new_environment();
            // either a block or a single expression ?
            const body = try self.compile_expr(self.next_instruction().?);

            _ = c.BinaryenAddFunction(
                self.binaryen.module,
                self.to_c_str(begin_func.name),
                try self.binaryen.arguments(&self.allocator, begin_func.args), // ?
                self.binaryen.primitive(begin_func.ret),
                self.current_env.?.local_types.items.ptr,
                @intCast(self.current_env.?.local_types.items.len),
                body,
            );
        },
        else => {
            std.debug.print("hello {any}", .{inst});
            return CompileError.NotImplemented;
        },
    }
}

pub fn compile_expr(self: *Compiler, inst: *Ir.Inst) anyerror!c.BinaryenExpressionRef {
    const expr = switch (inst.*) {
        .local_i32 => blk: {
            const index = self.current_env.?.stack.count - 1;
            const value = self.current_env.?.stack.pop();
            break :blk c.BinaryenLocalSet(self.binaryen.module, index, value);
        },
        .begin_block => blk: {
            var refs = try self.compile_until(.end_block);
            break :blk c.BinaryenBlock(
                self.binaryen.module,
                null,
                @ptrCast(refs),
                @intCast(refs.len),
                c.BinaryenTypeAuto(),
            );
        },
        else => {
            std.debug.print("not impl {any}", .{inst});
            return CompileError.NotImplemented;
        },
    };

    return expr;
}

pub fn compile_value(self: *Compiler, inst: *Ir.Inst) anyerror!void {
    switch (inst.*) {
        .push_i32 => |*push_i32| {
            const constant = self.binaryen.constant(.i32, push_i32.*);
            _ = try self.current_env.?.stack.push(constant);
        },
        else => {
            std.debug.print("not impl {any}", .{inst});
            return CompileError.NotImplemented;
        },
    }
}

pub fn next_instruction(self: *Compiler) ?*Ir.Inst {
    self.instruction_cursor += 1;

    if (self.is_at_end()) {
        return null;
    }

    return &self.ir[self.instruction_cursor - 2];
}

pub fn next_instruction_is(self: *Compiler, tag: Tag(Ir.Inst)) bool {
    if (self.is_at_end()) {
        return false;
    }

    return activeTag(self.ir[self.instruction_cursor - 2]) == tag;
}

pub fn is_at_end(self: *Compiler) bool {
    return self.instruction_cursor - 2 >= self.ir.len;
}

pub fn use_new_environment(self: *Compiler) !void {
    var new_env = try self.environments.addOne(self.allocator);
    new_env.* = Environment.init(self.allocator);
    self.current_env = new_env;
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

    pub fn add_immutable_global(
        self: *Binaryen,
        name: [*:0]const u8,
        comptime tid: Infer.TypeID,
        value: anytype,
    ) c.BinaryenGlobalRef {
        return c.BinaryenAddGlobal(
            self.module,
            name,
            self.primitive(tid),
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
            self.primitive(tid),
            true,
            self.constant(tid, value),
        );
    }

    pub fn constant(self: *Binaryen, comptime tid: Infer.TypeID, value: anytype) c.BinaryenExpressionRef {
        return c.BinaryenConst(
            self.module,
            self.literal(tid, value),
        );
    }

    pub fn primitive(self: *Binaryen, tid: Infer.TypeID) c.BinaryenType {
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

    pub fn arguments(self: *Binaryen, allocator: *std.mem.Allocator, args_tid: []Infer.TypeID) !c.BinaryenType {
        //@todo free
        var args = try allocator.alloc(c.BinaryenType, args_tid.len);
        for (args_tid, 0..) |tid, i| {
            args[i] = self.primitive(tid);
        }
        return c.BinaryenTypeCreate(
            @ptrCast(args),
            @intCast(args.len),
        );
    }
};

const global_instructions = blk: {
    var set = std.EnumSet(Tag(Ir.Inst)).initEmpty();

    set.insert(.global_i32);
    set.insert(.global_i64);
    set.insert(.global_f32);
    set.insert(.global_f64);
    set.insert(.begin_func);

    break :blk set;
};

fn is_global_instruction(inst: *Ir.Inst) bool {
    return global_instructions.contains(activeTag(inst.*));
}

const no_return_instructions = blk: {
    var set = std.EnumSet(Tag(Ir.Inst)).initEmpty();

    set.insert(.end_block);
    set.insert(.end_func);

    break :blk set;
};

fn is_no_return_instruction(inst: *Ir.Inst) bool {
    return no_return_instructions.contains(activeTag(inst.*));
}

const value_instructions = blk: {
    var set = std.EnumSet(Tag(Ir.Inst)).initEmpty();

    set.insert(.push_i32);

    break :blk set;
};
fn is_value_instruction(inst: *Ir.Inst) bool {
    return value_instructions.contains(activeTag(inst.*));
}

const std = @import("std");
const Tag = std.meta.Tag;
const activeTag = std.meta.activeTag;
const c = @cImport({
    @cInclude("binaryen-c.h");
});
const Compiler = @This();
const Infer = @import("./infer.zig");
const Ir = @import("./ir.zig");
const Environment = @import("./environment.zig");
