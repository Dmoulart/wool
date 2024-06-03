allocator: std.mem.Allocator,

ir: []*Ir.Inst,

module: c.BinaryenModuleRef,

instruction_cursor: usize,

current_env: ?*Environment,

environments: std.ArrayListUnmanaged(Environment),

memory_is_created: bool,

const CompileError = error{ NotImplemented, ExpectedExpression };

pub fn init(allocator: std.mem.Allocator, ir: []*Ir.Inst) Compiler {
    return .{
        .allocator = allocator,
        .ir = ir,
        .module = c.BinaryenModuleCreate(),
        .instruction_cursor = 1,
        .environments = .{},
        .current_env = null,
        .memory_is_created = false,
    };
}

pub fn compile_program(self: *Compiler) !void {
    for (self.ir) |instruction| {
        _ = try self.compile(instruction);
    }
}

pub fn compile(self: *Compiler, inst: *Ir.Inst) !?c.BinaryenExpressionRef {
    if (is_global_instruction(inst)) {
        try self.compile_global(inst);
        return null;
    }

    return try self.compile_expr(inst);
}

pub fn compile_global(self: *Compiler, inst: *Ir.Inst) !void {
    switch (inst.*) {
        .global_bool => |*global| {
            _ = self.add_immutable_global(
                self.to_c_str(global.name),
                .bool,
                global.value,
            );
        },
        .global_i32 => |*global| {
            _ = self.add_immutable_global(
                self.to_c_str(global.name),
                .i32,
                global.value,
            );
        },
        .global_i64 => |*global| {
            _ = self.add_immutable_global(
                self.to_c_str(global.name),
                .i64,
                global.value,
            );
        },
        .global_f32 => |*global| {
            _ = self.add_immutable_global(
                self.to_c_str(global.name),
                .f32,
                global.value,
            );
        },
        .global_f64 => |*global| {
            _ = self.add_immutable_global(
                self.to_c_str(global.name),
                .f64,
                global.value,
            );
        },
        .func => |*func| {
            try self.use_new_environment();
            self.current_env.?.args_nb = @intCast(func.args.len);

            // either a block or a single expression ?
            const body = try self.compile_expr(func.body);

            const func_ref = c.BinaryenAddFunction(
                self.module,
                self.to_c_str(func.name),
                try self.arguments(func.args), // ?
                primitive(func.ret),
                self.current_env.?.local_types.items.ptr,
                @intCast(self.current_env.?.local_types.items.len),
                body,
            );

            if (std.mem.eql(u8, func.name, "main")) {
                c.BinaryenSetStart(self.module, func_ref);
            }
        },
        .extern_func => |*extern_func| {
            c.BinaryenAddFunctionImport(
                self.module,
                self.to_c_str(extern_func.member),
                self.to_c_str(extern_func.namespace),
                self.to_c_str(extern_func.member),
                c.BinaryenTypeInt32(),
                c.BinaryenTypeNone(),
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
        .value_bool => |value_bool| {
            return self.constant(.bool, value_bool);
        },
        .value_i32 => |value_i32| {
            return self.constant(.i32, value_i32);
        },
        .value_i64 => |value_i64| {
            return self.constant(.i64, value_i64);
        },
        .value_f32 => |value_f32| {
            return self.constant(.f32, value_f32);
        },
        .value_f64 => |value_f64| {
            return self.constant(.f64, value_f64);
        },
        .local_bool => |local| {
            return self.declare_local(.bool, local);
        },
        .local_i32 => |local| {
            return try self.declare_local(.i32, local);
        },
        .local_i64 => |local| {
            return try self.declare_local(.i64, local);
        },
        .local_f32 => |local| {
            return try self.declare_local(.f32, local);
        },
        .local_f64 => |local| {
            return try self.declare_local(.f64, local);
        },
        .add_i32 => |bin| {
            return try self.binary(.i32, .PLUS, bin);
        },
        .add_i64 => |bin| {
            return try self.binary(.i64, .PLUS, bin);
        },
        .add_f32 => |bin| {
            return try self.binary(.f32, .PLUS, bin);
        },
        .add_f64 => |bin| {
            return try self.binary(.f64, .PLUS, bin);
        },
        .sub_i32 => |bin| {
            return try self.binary(.i32, .MINUS, bin);
        },
        .sub_i64 => |bin| {
            return try self.binary(.i64, .MINUS, bin);
        },
        .sub_f32 => |bin| {
            return try self.binary(.f32, .MINUS, bin);
        },
        .sub_f64 => |bin| {
            return try self.binary(.f64, .MINUS, bin);
        },
        .mul_i32 => |bin| {
            return try self.binary(.i32, .STAR, bin);
        },
        .mul_i64 => |bin| {
            return try self.binary(.i64, .STAR, bin);
        },
        .mul_f32 => |bin| {
            return try self.binary(.f32, .STAR, bin);
        },
        .mul_f64 => |bin| {
            return try self.binary(.f64, .STAR, bin);
        },
        .div_i32 => |bin| {
            return try self.binary(.i32, .SLASH, bin);
        },
        .div_i64 => |bin| {
            return try self.binary(.i64, .SLASH, bin);
        },
        .div_f32 => |bin| {
            return try self.binary(.f32, .SLASH, bin);
        },
        .div_f64 => |bin| {
            return try self.binary(.f64, .SLASH, bin);
        },
        .eq_bool => |bin| {
            return try self.binary(.bool, .EQUAL_EQUAL, bin);
        },
        .eq_i32 => |bin| {
            return try self.binary(.i32, .EQUAL_EQUAL, bin);
        },
        .eq_i64 => |bin| {
            return try self.binary(.i64, .EQUAL_EQUAL, bin);
        },
        .eq_f32 => |bin| {
            return try self.binary(.f32, .EQUAL_EQUAL, bin);
        },
        .eq_f64 => |bin| {
            return try self.binary(.f64, .EQUAL_EQUAL, bin);
        },
        .neq_bool => |bin| {
            return try self.binary(.bool, .BANG_EQUAL, bin);
        },
        .neq_i32 => |bin| {
            return try self.binary(.i32, .BANG_EQUAL, bin);
        },
        .neq_i64 => |bin| {
            return try self.binary(.i64, .BANG_EQUAL, bin);
        },
        .neq_f32 => |bin| {
            return try self.binary(.f32, .BANG_EQUAL, bin);
        },
        .neq_f64 => |bin| {
            return try self.binary(.f64, .BANG_EQUAL, bin);
        },
        .gt_i32 => |bin| {
            return try self.binary(.i32, .GREATER, bin);
        },
        .gt_i64 => |bin| {
            return try self.binary(.i64, .GREATER, bin);
        },
        .gt_f32 => |bin| {
            return try self.binary(.f32, .GREATER, bin);
        },
        .gt_f64 => |bin| {
            return try self.binary(.f64, .GREATER, bin);
        },
        .ge_i32 => |bin| {
            return try self.binary(.i32, .GREATER_EQUAL, bin);
        },
        .ge_i64 => |bin| {
            return try self.binary(.i64, .GREATER_EQUAL, bin);
        },
        .ge_f32 => |bin| {
            return try self.binary(.f32, .GREATER_EQUAL, bin);
        },
        .ge_f64 => |bin| {
            return try self.binary(.f64, .GREATER_EQUAL, bin);
        },
        .lt_i32 => |bin| {
            return try self.binary(.i32, .LESS, bin);
        },
        .lt_i64 => |bin| {
            return try self.binary(.i64, .LESS, bin);
        },
        .lt_f32 => |bin| {
            return try self.binary(.f32, .LESS, bin);
        },
        .lt_f64 => |bin| {
            return try self.binary(.f64, .LESS, bin);
        },
        .le_i32 => |bin| {
            return try self.binary(.i32, .LESS_EQUAL, bin);
        },
        .le_i64 => |bin| {
            return try self.binary(.i64, .LESS_EQUAL, bin);
        },
        .le_f32 => |bin| {
            return try self.binary(.f32, .LESS_EQUAL, bin);
        },
        .le_f64 => |bin| {
            return try self.binary(.f64, .LESS_EQUAL, bin);
        },
        .select_bool => |*sel| {
            return try self.select(.bool, sel);
        },
        .select_i32 => |*sel| {
            return try self.select(.i32, sel);
        },
        .select_i64 => |*sel| {
            return try self.select(.i64, sel);
        },
        .select_f32 => |*sel| {
            return try self.select(.f32, sel);
        },
        .select_f64 => |*sel| {
            return try self.select(.f64, sel);
        },
        .load_i32 => |*load_inst| {
            return try self.load(.i32, load_inst);
        },
        .load_i64 => |*load_inst| {
            return try self.load(.i64, load_inst);
        },
        .load_f32 => |*load_inst| {
            return try self.load(.f32, load_inst);
        },
        .load_f64 => |*load_inst| {
            return try self.load(.f64, load_inst);
        },
        .store_i32 => |*store_inst| {
            return try self.store(.i32, store_inst);
        },
        .store_i64 => |*store_inst| {
            return try self.store(.i64, store_inst);
        },
        .store_f32 => |*store_inst| {
            return try self.store(.f32, store_inst);
        },
        .store_f64 => |*store_inst| {
            return try self.store(.f64, store_inst);
        },
        .local_ref => |local_ref| {
            return c.BinaryenLocalGet(
                self.module,
                local_ref.identifier,
                primitive(local_ref.tid),
            );
        },
        .global_ref => |global_ref| {
            return c.BinaryenGlobalGet(
                self.module,
                self.to_c_str(global_ref),
                c.BinaryenTypeAuto(),
            );
        },
        .block => |*block| {
            //@todo:mem dealloc
            const refs = try self.allocator.alloc(c.BinaryenExpressionRef, block.insts.len);
            for (block.insts, 0..) |child_inst, i| {
                refs[i] = try self.compile_expr(child_inst);
            }
            return c.BinaryenBlock(
                self.module,
                null,
                @ptrCast(refs),
                @intCast(refs.len),
                primitive(block.return_type),
            );
        },
        .@"if" => |*if_inst| {
            return c.BinaryenIf(
                self.module,
                try self.compile_expr(if_inst.condition),
                try self.compile_expr(if_inst.then_branch),
                if (if_inst.else_branch) |else_branch| try self.compile_expr(else_branch) else null,
            );
        },
        .call => |*call| {
            const args = try self.allocator.alloc(c.BinaryenExpressionRef, call.args.len);

            for (call.args, 0..) |arg, i| {
                args[i] = try self.compile_expr(arg);
            }

            return c.BinaryenCall(
                self.module,
                self.to_c_str(call.callee),
                @ptrCast(args),
                @intCast(args.len),
                c.BinaryenTypeAuto(),
            );
        },
        .local_assign => |*local_assign| {
            return c.BinaryenLocalSet(
                self.module,
                local_assign.ident,
                try self.compile_expr(local_assign.value),
            );
        },
        .loop => |*loop| {
            //                         pub extern fn BinaryenLoop(module: BinaryenModuleRef, in: [*c]const u8, body: BinaryenExpressionRef) BinaryenExpressionRef;
            // pub extern fn BinaryenBreak(module: BinaryenModuleRef, name: [*c]const u8, condition: BinaryenExpressionRef, value: BinaryenExpressionRef) BinaryenExpressionRef;
            const refs = try self.allocator.alloc(c.BinaryenExpressionRef, 2);
            const inner = try self.compile_expr(loop.body);

            const buf = try self.allocator.alloc(u8, 15);
            const inner_loop_label = try std.fmt.bufPrint(buf, "loop_{d}", .{loop.id});
            const branch_to_loop = c.BinaryenBreak(self.module, self.to_c_str(inner_loop_label), null, null);

            refs[0] = inner;
            refs[1] = branch_to_loop;

            const block =
                c.BinaryenBlock(
                self.module,
                null,
                @ptrCast(refs),
                @intCast(refs.len),
                c.BinaryenTypeAuto(),
            );

            const loop_ref_ptr = try self.allocator.create(c.BinaryenExpressionRef);
            const loop_ref = c.BinaryenLoop(self.module, self.to_c_str(inner_loop_label), block);
            loop_ref_ptr.* = loop_ref;

            const buf_2 = try self.allocator.alloc(u8, 15);
            const outer_loop_label = try std.fmt.bufPrint(buf_2, "outer_{d}", .{loop.id});

            return c.BinaryenBlock(
                self.module,
                self.to_c_str(outer_loop_label),
                @ptrCast(loop_ref_ptr),
                1,
                c.BinaryenTypeAuto(),
            );
        },
        .brk => |*brk| {
            const buf = try self.allocator.alloc(u8, 15);
            const from_label = try std.fmt.bufPrint(buf, "outer_{d}", .{brk.id});

            return c.BinaryenBreak(self.module, self.to_c_str(from_label), null, null);
        },
        .break_if => |break_if| {
            const buf = try self.allocator.alloc(u8, 15);
            const from_label = try std.fmt.bufPrint(buf, "outer_{d}", .{break_if.from});

            return c.BinaryenBreak(
                self.module,
                self.to_c_str(from_label),
                try self.compile_expr(break_if.condition),
                null,
            );
        },
        else => {
            std.debug.print("not impl {any}", .{inst});
            return CompileError.NotImplemented;
        },
    };

    return expr;
}

fn declare_local(
    self: *Compiler,
    comptime tid: Infer.TypeID,
    local: Ir.Inst.Local,
) !c.BinaryenExpressionRef {
    const index = try self.current_env.?.new_local(primitive(tid));
    const value = try self.compile_expr(local.value) orelse return CompileError.ExpectedExpression; // Could fail ?
    return c.BinaryenLocalSet(self.module, @intCast(index), value);
}

fn select(self: *Compiler, comptime tid: Infer.TypeID, select_inst: *Ir.Inst.Select) !c.BinaryenExpressionRef {
    return c.BinaryenSelect(
        self.module,
        try self.compile_expr(select_inst.condition), // try self.compile_expr(select_inst.condition),
        try self.compile_expr(select_inst.then_branch),
        if (select_inst.else_branch) |else_branch|
            try self.compile_expr(else_branch)
        else
            null,
        primitive(tid),
    );
}

fn load(self: *Compiler, comptime tid: Infer.TypeID, load_inst: *Ir.Inst.Load) !c.BinaryenExpressionRef {
    if (!self.memory_is_created) {
        try self.create_memory();

        self.memory_is_created = true;
    }

    return c.BinaryenLoad(
        self.module,
        32,
        true,
        0, // offset
        0,
        primitive(tid),
        try self.compile_expr(load_inst.index),
        "0",
    );
}

fn store(self: *Compiler, comptime tid: Infer.TypeID, store_inst: *Ir.Inst.Store) !c.BinaryenExpressionRef {
    if (!self.memory_is_created) {
        try self.create_memory();

        self.memory_is_created = true;
    }

    return c.BinaryenStore(
        self.module,
        32,
        0,
        0, // offset
        try self.compile_expr(store_inst.index),
        try self.compile_expr(store_inst.value),
        primitive(tid),
        "0",
    );
}

fn binary(
    self: *Compiler,
    comptime tid: Infer.TypeID,
    comptime operator: Token.Type,
    bin: Ir.Inst.Binary,
) !c.BinaryenExpressionRef {
    const right = try self.compile_expr(bin.left);
    const left = try self.compile_expr(bin.right);
    return c.BinaryenBinary(
        self.module,
        op(tid, operator),
        right,
        left,
    );
}

fn current_function(self: *Compiler) ?*Environment {
    return if (self.current_env) |env| env else null;
}

pub fn use_new_environment(self: *Compiler) !void {
    const new_env = try self.environments.addOne(self.allocator);
    new_env.* = Environment.init(self.allocator);
    self.current_env = new_env;
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

pub fn add_immutable_global(
    self: *Compiler,
    name: [*:0]const u8,
    comptime tid: Infer.TypeID,
    value: anytype,
) c.BinaryenGlobalRef {
    return c.BinaryenAddGlobal(
        self.module,
        name,
        primitive(tid),
        false,
        self.constant(tid, value),
    );
}

pub fn add_mutable_global(
    self: *Compiler,
    name: [*:0]const u8,
    comptime tid: Infer.TypeID,
    value: anytype,
) c.BinaryenExpressionRef {
    return c.BinaryenAddGlobal(
        self.module,
        name,
        primitive(tid),
        true,
        self.constant(tid, value),
    );
}

pub fn op(comptime tid: Infer.TypeID, comptime operator: Token.Type) c.BinaryenOp {
    return switch (operator) {
        .PLUS => switch (tid) {
            .i32 => c.BinaryenAddInt32(),
            .i64 => c.BinaryenAddInt64(),
            .f32 => c.BinaryenAddFloat32(),
            .f64 => c.BinaryenAddFloat64(),
            else => unreachable,
        },
        .MINUS => switch (tid) {
            .i32 => c.BinaryenSubInt32(),
            .i64 => c.BinaryenSubInt64(),
            .f32 => c.BinaryenSubFloat32(),
            .f64 => c.BinaryenSubFloat64(),
            else => unreachable,
        },
        .STAR => switch (tid) {
            .i32 => c.BinaryenMulInt32(),
            .i64 => c.BinaryenMulInt64(),
            .f32 => c.BinaryenMulFloat32(),
            .f64 => c.BinaryenMulFloat64(),
            else => unreachable,
        },
        .SLASH => switch (tid) {
            .i32 => c.BinaryenDivSInt32(), // div s ? div u ?,
            .i64 => c.BinaryenDivSInt64(),
            .f32 => c.BinaryenDivFloat32(),
            .f64 => c.BinaryenDivFloat64(),
            else => unreachable,
        },
        .EQUAL_EQUAL => switch (tid) {
            .i32, .bool => c.BinaryenEqInt32(),
            .i64 => c.BinaryenEqInt64(),
            .f32 => c.BinaryenEqFloat32(),
            .f64 => c.BinaryenEqFloat64(),
            else => unreachable,
        },
        .BANG_EQUAL => switch (tid) {
            .i32, .bool => c.BinaryenNeInt32(),
            .i64 => c.BinaryenNeInt64(),
            .f32 => c.BinaryenNeFloat32(),
            .f64 => c.BinaryenNeFloat64(),
            else => unreachable,
        },
        .GREATER => switch (tid) {
            .i32 => c.BinaryenGtSInt32(),
            .i64 => c.BinaryenGtSInt64(),
            .f32 => c.BinaryenGtFloat32(),
            .f64 => c.BinaryenGtFloat64(),
            else => unreachable,
        },
        .GREATER_EQUAL => switch (tid) {
            .i32 => c.BinaryenGeSInt32(),
            .i64 => c.BinaryenGeSInt64(),
            .f32 => c.BinaryenGeFloat32(),
            .f64 => c.BinaryenGeFloat64(),
            else => unreachable,
        },
        .LESS => switch (tid) {
            .i32 => c.BinaryenLtSInt32(),
            .i64 => c.BinaryenLtSInt64(),
            .f32 => c.BinaryenLtFloat32(),
            .f64 => c.BinaryenLtFloat64(),
            else => unreachable,
        },
        .LESS_EQUAL => switch (tid) {
            .i32 => c.BinaryenLeSInt32(),
            .i64 => c.BinaryenLeSInt64(),
            .f32 => c.BinaryenLeFloat32(),
            .f64 => c.BinaryenLeFloat64(),
            else => unreachable,
        },

        else => unreachable,
    };
}

pub fn constant(self: *Compiler, comptime tid: Infer.TypeID, value: anytype) c.BinaryenExpressionRef {
    return c.BinaryenConst(
        self.module,
        literal(tid, value),
    );
}

pub fn primitive(tid: Infer.TypeID) c.BinaryenType {
    return switch (tid) {
        .i32, .bool, .number => c.BinaryenTypeInt32(),
        .i64 => c.BinaryenTypeInt64(),
        .f32 => c.BinaryenTypeFloat32(),
        .f64 => c.BinaryenTypeFloat64(),
        .void => c.BinaryenTypeNone(),
        else => unreachable,
    };
}

pub fn literal(comptime tid: Infer.TypeID, value: anytype) c.BinaryenLiteral {
    return switch (tid) {
        .bool => if (value == 1) c.BinaryenLiteralInt32(1) else c.BinaryenLiteralInt32(0),
        .i32 => c.BinaryenLiteralInt32(value),
        .i64 => c.BinaryenLiteralInt64(value),
        .f32 => c.BinaryenLiteralFloat32(value),
        .f64 => c.BinaryenLiteralFloat64(value),
        else => unreachable,
    };
}

pub fn arguments(self: *Compiler, args_tid: []Infer.TypeID) !c.BinaryenType {
    //@todo free
    var args = try self.allocator.alloc(c.BinaryenType, args_tid.len);
    for (args_tid, 0..) |tid, i| {
        args[i] = primitive(tid);
    }
    return c.BinaryenTypeCreate(
        @ptrCast(args),
        @intCast(args.len),
    );
}
// const segment_names: [2][]const u8 = [2][]const u8{ "0", "1" };
// // const segment_datas: [1][]const u8 = [_][]const u8{""};
// const segment_passives: [2]bool = [_]bool{ true, false };

// const segment_sizes: [2]u32 = [_]u32{ 12, 12 };

pub fn create_memory(self: *Compiler) !void {
    var segments = try self.allocator.alloc([]const u8, 1);
    segments[0] = "seg0";
    // segments[1] = "seg1";

    var offsets = try self.allocator.alloc(?c.BinaryenExpressionRef, 1);
    offsets[0] = self.constant(.i32, 10);
    // offsets[1] = null;

    var passives = try self.allocator.alloc(bool, 1);
    passives[0] = true;
    // passives[1] = false;

    var sizes = try self.allocator.alloc(c.BinaryenIndex, 1);
    sizes[0] = 12;
    // sizes[1] = 24;

    _ = c.BinaryenSetMemory(
        self.module,
        1,
        256,
        "mem",
        @ptrCast(segments),
        @ptrCast(passives),
        @ptrCast(offsets),
        @ptrCast(sizes),
        1,
        false,
        false, // use i32 mem for now
        "0",
    );
}

fn to_c_str(self: *Compiler, str: []const u8) [*:0]const u8 {
    //@todo horror museum + memory leak
    return @ptrCast(self.allocator.dupeZ(u8, str) catch unreachable);
}
const global_instructions = blk: {
    var set = std.EnumSet(Tag(Ir.Inst)).initEmpty();

    set.insert(.global_bool);
    set.insert(.global_i32);
    set.insert(.global_i64);
    set.insert(.global_f32);
    set.insert(.global_f64);
    set.insert(.extern_func);
    set.insert(.func);

    break :blk set;
};

fn is_global_instruction(inst: *Ir.Inst) bool {
    return global_instructions.contains(activeTag(inst.*));
}

const std = @import("std");
const Tag = std.meta.Tag;
const activeTag = std.meta.activeTag;
const c = @cImport({
    @cInclude("binaryen-c.h");
});
const Token = @import("./token.zig");
const Infer = @import("./infer.zig");
const Ir = @import("./ir.zig");
const Environment = @import("./environment.zig");

const Compiler = @This();
