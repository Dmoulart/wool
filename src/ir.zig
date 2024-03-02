allocator: std.mem.Allocator,

instructions: std.ArrayListUnmanaged(Inst),

program: std.ArrayListUnmanaged(*Inst),

function_variables: Stack([]const u8),

const Ir = @This();

pub const Inst = union(enum) {
    value_bool: i32,
    value_i32: i32,
    value_i64: i64,
    value_f32: f32,
    value_f64: f64,

    local_bool: Local,
    local_i32: Local,
    local_i64: Local,
    local_f32: Local,
    local_f64: Local,

    global_bool: Global(i32),
    global_i32: Global(i32),
    global_i64: Global(i64),
    global_f32: Global(f32),
    global_f64: Global(f64),

    add_i32: Binary,
    add_i64: Binary,
    add_f32: Binary,
    add_f64: Binary,

    sub_i32: Binary,
    sub_i64: Binary,
    sub_f32: Binary,
    sub_f64: Binary,

    mul_i32: Binary,
    mul_i64: Binary,
    mul_f32: Binary,
    mul_f64: Binary,

    div_i32: Binary,
    div_i64: Binary,
    div_f32: Binary,
    div_f64: Binary,

    eq_i32: Binary,
    eq_i64: Binary,
    eq_f32: Binary,
    eq_f64: Binary,

    neq_i32: Binary,
    neq_i64: Binary,
    neq_f32: Binary,
    neq_f64: Binary,

    gt_i32: Binary,
    gt_i64: Binary,
    gt_f32: Binary,
    gt_f64: Binary,

    ge_i32: Binary,
    ge_i64: Binary,
    ge_f32: Binary,
    ge_f64: Binary,

    lt_i32: Binary,
    lt_i64: Binary,
    lt_f32: Binary,
    lt_f64: Binary,

    le_i32: Binary,
    le_i64: Binary,
    le_f32: Binary,
    le_f64: Binary,

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
            .bool => try self.create_inst(.{
                .value_bool = if (literal.orig_expr.Literal.value.Boolean) 1 else 0,
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
                .bool => .{
                    .local_bool = local_value,
                },
                else => return IrError.NotImplemented,
            });
        },
        .ConstInit => |*const_init| {
            const name = const_init.orig_expr.ConstInit.name.lexeme;
            return switch (get_sem_tid(as_sem(const_init.initializer))) {
                .bool => try self.create_inst(.{
                    .global_bool = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), bool, i32),
                    },
                }),
                .number, .i32 => try self.create_inst(.{
                    .global_i32 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), i32, i32),
                    },
                }),
                .i64 => try self.create_inst(.{
                    .global_i64 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), i64, i64),
                    },
                }),
                .float, .f32 => try self.create_inst(.{
                    .global_f32 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), f32, f32),
                    },
                }),
                .f64 => try self.create_inst(.{
                    .global_f64 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), f64, f64),
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

            const tid = get_sem_tid(as_sem(binary));

            const values = .{
                .left = left,
                .right = right,
            };

            const inst: Ir.Inst = switch (op) {
                .PLUS => switch (tid) {
                    .number, .i32 => .{
                        .add_i32 = values,
                    },
                    .i64 => .{
                        .add_i64 = values,
                    },
                    .float, .f32 => .{
                        .add_f32 = values,
                    },
                    .f64 => .{
                        .add_f64 = values,
                    },
                    else => unreachable,
                },
                .MINUS => switch (tid) {
                    .number, .i32 => .{
                        .sub_i32 = values,
                    },
                    .i64 => .{
                        .sub_i64 = values,
                    },
                    .float, .f32 => .{
                        .sub_f32 = values,
                    },
                    .f64 => .{
                        .sub_f64 = values,
                    },
                    else => unreachable,
                },
                .STAR => switch (tid) {
                    .number, .i32 => .{
                        .mul_i32 = values,
                    },
                    .i64 => .{
                        .mul_i64 = values,
                    },
                    .float, .f32 => .{
                        .mul_f32 = values,
                    },
                    .f64 => .{
                        .mul_f64 = values,
                    },
                    else => unreachable,
                },
                .SLASH => switch (tid) {
                    .number, .i32 => .{
                        .div_i32 = values,
                    },
                    .i64 => .{
                        .div_i64 = values,
                    },
                    .float, .f32 => .{
                        .div_f32 = values,
                    },
                    .f64 => .{
                        .div_f64 = values,
                    },
                    else => unreachable,
                },
                .EQUAL_EQUAL => switch (tid) {
                    .number, .i32 => .{
                        .eq_i32 = values,
                    },
                    .i64 => .{
                        .eq_i64 = values,
                    },
                    .float, .f32 => .{
                        .eq_f32 = values,
                    },
                    .f64 => .{
                        .eq_f64 = values,
                    },
                    else => unreachable,
                },
                .BANG_EQUAL => switch (tid) {
                    .number, .i32 => .{
                        .neq_i32 = values,
                    },
                    .i64 => .{
                        .neq_i64 = values,
                    },
                    .float, .f32 => .{
                        .neq_f32 = values,
                    },
                    .f64 => .{
                        .neq_f64 = values,
                    },
                    else => unreachable,
                },
                .GREATER => switch (tid) {
                    .number, .i32 => .{
                        .gt_i32 = values,
                    },
                    .i64 => .{
                        .gt_i64 = values,
                    },
                    .float, .f32 => .{
                        .gt_f32 = values,
                    },
                    .f64 => .{
                        .gt_f64 = values,
                    },
                    else => unreachable,
                },
                .GREATER_EQUAL => switch (tid) {
                    .number, .i32 => .{
                        .ge_i32 = values,
                    },
                    .i64 => .{
                        .ge_i64 = values,
                    },
                    .float, .f32 => .{
                        .ge_f32 = values,
                    },
                    .f64 => .{
                        .ge_f64 = values,
                    },
                    else => unreachable,
                },
                .LESS => switch (tid) {
                    .number, .i32 => .{
                        .lt_i32 = values,
                    },
                    .i64 => .{
                        .lt_i64 = values,
                    },
                    .float, .f32 => .{
                        .lt_f32 = values,
                    },
                    .f64 => .{
                        .lt_f64 = values,
                    },
                    else => unreachable,
                },
                .LESS_EQUAL => switch (tid) {
                    .number, .i32 => .{
                        .le_i32 = values,
                    },
                    .i64 => .{
                        .le_i64 = values,
                    },
                    .float, .f32 => .{
                        .le_f32 = values,
                    },
                    .f64 => .{
                        .le_f64 = values,
                    },
                    else => unreachable,
                },
                else => return IrError.NotImplemented,
            };

            return try self.create_inst(inst);
        },

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

fn eval(self: *Ir, sem: *Infer.Sem, comptime InputType: type, comptime ReturnType: type) !ReturnType {
    return switch (sem.*) {
        .Literal => |*literal| switch (InputType) {
            bool => if (literal.orig_expr.Literal.value.Boolean) 1 else 0,
            i32, i64 => try std.fmt.parseInt(InputType, literal.orig_expr.Literal.value.Number, 10),
            f32, f64 => try std.fmt.parseFloat(InputType, literal.orig_expr.Literal.value.Number),
            else => IrError.NotImplemented,
        },
        .Binary => |*binary| blk: {
            const left = try self.eval(as_sem(binary.left), InputType, ReturnType);
            const right = try self.eval(as_sem(binary.right), InputType, ReturnType);

            break :blk switch (binary.orig_expr.Binary.op.type) {
                .PLUS => left + right,
                .MINUS => left - right,
                .STAR => left * right,
                .SLASH => @divTrunc(left, right), //@todo divison
                .GREATER => if (left > right) 1 else 0,
                .GREATER_EQUAL => if (left >= right) 1 else 0,
                .LESS => if (left < right) 1 else 0,
                .LESS_EQUAL => if (left <= right) 1 else 0,
                .EQUAL_EQUAL => if (left == right) 1 else 0,
                .BANG_EQUAL => if (left != right) 1 else 0,
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
