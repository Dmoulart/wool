allocator: std.mem.Allocator,

instructions: std.ArrayListUnmanaged(Inst),

program: std.ArrayListUnmanaged(*Inst),

function_variables: Stack([]const u8),

const Ir = @This();

pub const Inst = union(enum) {
    value_i32: i32,
    value_i64: i64,
    value_f32: f32,
    value_f64: f64,

    local_i32: Local,
    local_i64: Local,
    local_f32: Local,
    local_f64: Local,

    global_i32: Inst.Global(i32),
    global_i64: Inst.Global(i64),
    global_f32: Inst.Global(f32),
    global_f64: Inst.Global(f64),

    add_i32: Binary,
    add_i64: Binary,
    add_f32: Binary,
    add_f64: Binary,

    func: Func,

    block: struct {
        insts: []*Inst,
    },

    pub fn Global(comptime T: type) type {
        return struct {
            name: []const u8,
            value: T,
        };
    }

    pub const Local = struct {
        ident: u32,
        value: *Inst,
    };

    pub const Binary = struct {
        left: *Inst,
        right: *Inst,
    };

    pub const Func = struct {
        name: []const u8,
        args: []Infer.TypeID,
        ret: Infer.TypeID,
        body: *Inst,
    };

    pub fn type_of(inst: Inst) Infer.TypeID {
        return switch (inst) {
            .value_i32, .local_i32, .global_i32, .add_i32 => .i32,
            .value_i64, .local_i64, .global_i64, .add_i64 => .i64,
            .value_f32, .local_f32, .global_f32, .add_f32 => .f32,
            .value_f64, .local_f64, .global_f64, .add_f64 => .f64,
            else => unreachable,
        };
    }
};

const IrError = error{NotImplemented};

pub fn init(allocator: std.mem.Allocator) Ir {
    return .{
        .allocator = allocator,
        .instructions = .{},
        .program = .{},
        .function_variables = Stack([]const u8).init(allocator),
    };
}

pub fn convert_program(self: *Ir, sems: []*Infer.Sem) ![]*Inst {
    for (sems) |sem| {
        try self.emit(try self.convert(sem));
    }

    return self.program.items;
}

pub fn emit(self: *Ir, inst: *Inst) !void {
    try self.program.append(self.allocator, inst);
}

pub fn convert(self: *Ir, sem: *Infer.Sem) anyerror!*Inst {
    switch (sem.*) {
        .Literal => |*literal| return switch (get_sem_tid(sem)) {
            //temporary, Number should be narrowed in Infer phase or an error should be thrown
            .number, .i32 => try self.create_inst(.{
                .value_i32 = try std.fmt.parseInt(i32, literal.orig_expr.Literal.value.Number, 10),
            }),
            .i64 => try self.create_inst(.{
                .value_i64 = try std.fmt.parseInt(i64, literal.orig_expr.Literal.value.Number, 10),
            }),
            .float, .f32 => try self.create_inst(.{
                .value_f32 = try std.fmt.parseFloat(f32, literal.orig_expr.Literal.value.Number),
            }),
            .f64 => try self.create_inst(.{
                .value_f64 = try std.fmt.parseFloat(f64, literal.orig_expr.Literal.value.Number),
            }),
            else => IrError.NotImplemented,
        },
        .VarInit => |*var_init| {
            const name = var_init.orig_expr.VarInit.name.lexeme;
            const local_ident = try self.function_variables.push(name);

            const value = try self.convert(as_sem(var_init.initializer));

            const local_value = .{
                .ident = local_ident,
                .value = value,
            };

            return try self.create_inst(switch (get_sem_tid(as_sem(var_init.initializer))) {
                .i32 => .{
                    .local_i32 = local_value,
                },
                .i64 => .{
                    .local_i64 = local_value,
                },
                .f32 => .{
                    .local_f32 = local_value,
                },
                .f64 => .{
                    .local_f64 = local_value,
                },
                else => return IrError.NotImplemented,
            });
        },
        .ConstInit => |*const_init| {
            const name = const_init.orig_expr.ConstInit.name.lexeme;
            return switch (get_sem_tid(as_sem(const_init.initializer))) {
                .number, .i32 => try self.create_inst(.{
                    .global_i32 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), i32),
                    },
                }),
                .i64 => try self.create_inst(.{
                    .global_i64 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), i64),
                    },
                }),
                .float, .f32 => try self.create_inst(.{
                    .global_f32 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), f32),
                    },
                }),
                .f64 => try self.create_inst(.{
                    .global_f64 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), f64),
                    },
                }),
                .func => try self.convert(as_sem(const_init.initializer)),

                else => IrError.NotImplemented,
            };
        },
        .Function => |*function| {
            try self.function_variables.clear();
            var args = try self.allocator.alloc(Infer.TypeID, function.type_node.function.args.len);

            for (function.type_node.function.args, 0..) |arg, i| {
                args[i] = arg.get_tid();
            }

            const name = if (function.orig_expr.Function.name) |name|
                name.lexeme
            else
                "anonymous";

            return try self.create_inst(.{
                .func = .{
                    .name = name,
                    .ret = function.type_node.function.return_type.get_tid(),
                    .args = args,
                    .body = try self.convert(as_sem(function.body)),
                },
            });
        },
        .Block => |*block| {
            //@todo:mem
            const insts = try self.allocator.alloc(*Inst, block.exprs.len);
            for (block.exprs, 0..) |expr, i| {
                insts[i] = try self.convert(as_sem(expr));
            }
            return try self.create_inst(
                .{
                    .block = .{ .insts = insts },
                },
            );
        },
        .Binary => |*binary| {
            const left = try self.convert(as_sem(binary.left));
            const right = try self.convert(as_sem(binary.right));

            const op = binary.orig_expr.Binary.op.type;

            return switch (op) {
                .PLUS => switch (get_sem_tid(as_sem(binary))) {
                    .number, .i32 => try self.create_inst(
                        .{
                            .add_i32 = .{
                                .left = left,
                                .right = right,
                            },
                        },
                    ),
                    .i64 => try self.create_inst(
                        .{
                            .add_i64 = .{
                                .left = left,
                                .right = right,
                            },
                        },
                    ),
                    .float, .f32 => try self.create_inst(
                        .{
                            .add_f32 = .{
                                .left = left,
                                .right = right,
                            },
                        },
                    ),
                    .f64 => try self.create_inst(
                        .{
                            .add_f64 = .{
                                .left = left,
                                .right = right,
                            },
                        },
                    ),
                    else => IrError.NotImplemented,
                },
                else => IrError.NotImplemented,
            };
        },
        // switch (binary.orig_expr.Binary.op.type) {
        //     .PLUS => switch (get_sem_tid(as_sem(binary))) {
        //         .i32 => c.BinaryenAddInt32(),
        //         .i64 => c.BinaryenAddInt64(),
        //         .f32 => c.BinaryenAddFloat32(),
        //         .f64 => c.BinaryenAddFloat64(),
        //         else => unreachable,
        //     },
        // .MINUS => switch (get_sem_tid(as_sem(binary))) {
        //     .i32 => c.BinaryenSubInt32(),
        //     .i64 => c.BinaryenSubInt64(),
        //     .f32 => c.BinaryenSubFloat32(),
        //     .f64 => c.BinaryenSubFloat64(),
        //     else => unreachable,
        // },
        // .STAR => switch (get_sem_tid(as_sem(binary))) {
        //     .i32 => c.BinaryenMulInt32(),
        //     .i64 => c.BinaryenMulInt64(),
        //     .f32 => c.BinaryenMulFloat32(),
        //     .f64 => c.BinaryenMulFloat64(),
        //     else => unreachable,
        // },
        // .SLASH => switch (get_sem_tid(as_sem(binary))) {
        //     .i32 => c.BinaryenDivSInt32(), // div s ? div u ?,
        //     .i64 => c.BinaryenDivSInt64(),
        //     .f32 => c.BinaryenDivFloat32(),
        //     .f64 => c.BinaryenDivFloat64(),
        //     else => unreachable,
        // },
        // .EQUAL_EQUAL => switch (get_sem_tid(as_sem(binary))) {
        //     .i32 => c.BinaryenEqInt32(),
        //     .i64 => c.BinaryenEqInt64(),
        //     .f32 => c.BinaryenEqFloat32(),
        //     .f64 => c.BinaryenEqFloat64(),
        //     else => unreachable,
        // },
        // .BANG_EQUAL => switch (get_sem_tid(as_sem(binary))) {
        //     .i32 => c.BinaryenNeInt32(),
        //     .i64 => c.BinaryenNeInt64(),
        //     .f32 => c.BinaryenNeFloat32(),
        //     .f64 => c.BinaryenNeFloat64(),
        //     else => unreachable,
        // },
        // .GREATER => switch (get_sem_tid(as_sem(binary))) {
        //     .i32 => c.BinaryenGtSInt32(),
        //     .i64 => c.BinaryenGtSInt64(),
        //     .f32 => c.BinaryenGtFloat32(),
        //     .f64 => c.BinaryenGtFloat64(),
        //     else => unreachable,
        // },
        // .GREATER_EQUAL => switch (get_sem_tid(as_sem(binary))) {
        //     .i32 => c.BinaryenGeSInt32(),
        //     .i64 => c.BinaryenGeSInt64(),
        //     .f32 => c.BinaryenGeFloat32(),
        //     .f64 => c.BinaryenGeFloat64(),
        //     else => unreachable,
        // },
        // .LESS => switch (get_sem_tid(as_sem(binary))) {
        //     .i32 => c.BinaryenLtSInt32(),
        //     .i64 => c.BinaryenLtSInt64(),
        //     .f32 => c.BinaryenLtFloat32(),
        //     .f64 => c.BinaryenLtFloat64(),
        //     else => unreachable,
        // },
        // .LESS_EQUAL => switch (get_sem_tid(as_sem(binary))) {
        //     .i32 => c.BinaryenLeSInt32(),
        //     .i64 => c.BinaryenLeSInt64(),
        //     .f32 => c.BinaryenLeFloat32(),
        //     .f64 => c.BinaryenLeFloat64(),
        //     else => unreachable,
        // },

        // else => unreachable,
        // },
        else => {
            std.debug.print("\nNot Implemented = {any}\n", .{sem});
            return IrError.NotImplemented;
        },
    }
}

fn create_inst(self: *Ir, inst: Inst) !*Inst {
    const inst_ptr = try self.instructions.addOne(self.allocator);
    inst_ptr.* = inst;
    return inst_ptr;
}

fn eval(self: *Ir, sem: *Infer.Sem, comptime ExpectedType: type) !ExpectedType {
    return switch (sem.*) {
        .Literal => |*literal| switch (ExpectedType) {
            i32, i64 => try std.fmt.parseInt(ExpectedType, literal.orig_expr.Literal.value.Number, 10),
            f32, f64 => try std.fmt.parseFloat(ExpectedType, literal.orig_expr.Literal.value.Number),
            else => IrError.NotImplemented,
        },
        .Binary => |*binary| blk: {
            const left = try self.eval(as_sem(binary.left), ExpectedType);
            const right = try self.eval(as_sem(binary.right), ExpectedType);

            break :blk switch (binary.orig_expr.Binary.op.type) {
                .PLUS => left + right,
                .MINUS => left - right,
                else => IrError.NotImplemented,
            };
        },
        else => IrError.NotImplemented,
    };
}

const std = @import("std");
const Infer = @import("./infer.zig");
const as_sem = @import("./infer.zig").as_sem;
const get_sem_tid = @import("./infer.zig").get_sem_tid;

const Stack = @import("./Stack.zig").Stack;
