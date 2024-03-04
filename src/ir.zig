allocator: std.mem.Allocator,

instructions: std.ArrayListUnmanaged(Inst),

program: std.ArrayListUnmanaged(*Inst),

globals: std.StringHashMapUnmanaged(void),

function_locals: Stack([]const u8),
// function_local_types: Stack(Infer.TypeID),

const Ir = @This();

pub const Inst = union(enum) {
    value_bool: i32,
    value_i32: i32,
    value_i64: i64,
    value_f32: f32,
    value_f64: f64,

    local_ref: Local.Ident,

    local_bool: Local,
    local_i32: Local,
    local_i64: Local,
    local_f32: Local,
    local_f64: Local,

    global_ref: []const u8,

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

    eq_bool: Binary,
    eq_i32: Binary,
    eq_i64: Binary,
    eq_f32: Binary,
    eq_f64: Binary,

    neq_bool: Binary,
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

    select_bool: Select,
    select_i32: Select,
    select_i64: Select,
    select_f32: Select,
    select_f64: Select,

    func: Func,

    extern_func: ExternFunc,

    @"if": If,

    block: struct {
        insts: []*Inst,
        return_type: Infer.TypeID, // @todo: make a block for each type ???
    },

    call: Call,

    pub fn Global(comptime T: type) type {
        return struct {
            name: []const u8,
            value: T,
        };
    }

    pub const Local = struct {
        pub const Ident = u32;

        ident: Ident,
        value: *Inst,
    };

    pub const Binary = struct {
        left: *Inst,
        right: *Inst,
    };

    pub const ExternFunc = struct {
        name: []const u8,
        args: []Infer.TypeID,
        ret: Infer.TypeID,
    };

    pub const Func = struct {
        name: []const u8,
        args: []Infer.TypeID,
        ret: Infer.TypeID,
        body: *Inst,
    };

    pub const If = struct {
        then_branch: *Inst,
        else_branch: ?*Inst,
        condition: *Inst,
    };

    pub const Select = struct {
        then_branch: *Inst,
        else_branch: ?*Inst,
        condition: *Inst,
    };

    pub const Call = struct {
        callee: []const u8,
        args: []*Inst,
    };
};

const IrError = error{ NotImplemented, CannotFindLocalVariable };

pub fn init(allocator: std.mem.Allocator) Ir {
    return .{
        .allocator = allocator,
        .instructions = .{},
        .program = .{},
        .globals = .{},
        .function_locals = Stack([]const u8).init(allocator),
        // .function_locals = Stack(Infer.TypeID).init(allocator),
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
        .Literal => |*lit| {
            const tid = get_sem_tid(sem);
            // const lit_inst = switch (tid) {
            //     .bool => try literal(tid, lit.orig_expr.Literal.value.Boolean),
            //     .number, .i32, .i64, .float, .f32, .f64 => try literal(tid, lit.orig_expr.Literal.value.Number),
            //     else => unreachable,
            // };
            return try self.create_inst(try literal(tid, lit.orig_expr.Literal.value));
        },
        .Grouping => |grouping| {
            return try self.convert(as_sem(grouping.expr));
        },
        .VarInit => |*var_init| {
            const name = var_init.orig_expr.VarInit.name.lexeme;
            const tid = get_sem_tid(as_sem(var_init.initializer));

            const local_ident = try self.function_locals.push(name);
            // _ = try self.function_local_types.push(tid);

            const value = try self.convert(as_sem(var_init.initializer));

            const local_value = .{
                .ident = local_ident,
                .value = value,
            };

            return try self.create_inst(switch (tid) {
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

            try self.globals.put(self.allocator, name, {});

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
                .func => {
                    const func_inst = try self.convert(as_sem(const_init.initializer));
                    func_inst.func.name = name; //@todo hack to get the name. Find a better way.
                    return func_inst;
                },

                else => IrError.NotImplemented,
            };
        },
        .Function => |*function| {
            try self.function_locals.clear();
            // try self.function_local_types.clear();
            var args = try self.allocator.alloc(Infer.TypeID, function.type_node.function.args.len);

            for (function.type_node.function.args, 0..) |arg, i| {
                args[i] = arg.get_tid();
                // @todo this is fucking awful !!
                _ = try self.function_locals.push(function.orig_expr.Function.args.?[i].expr.Variable.name.lexeme);
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

            const return_type: Infer.TypeID = if (block.exprs.len == 0)
                .void
            else
                get_sem_tid(as_sem(block.exprs[block.exprs.len - 1]));

            for (block.exprs, 0..) |expr, i| {
                insts[i] = try self.convert(as_sem(expr));
            }

            return try self.create_inst(
                .{
                    .block = .{ .insts = insts, .return_type = return_type },
                },
            );
        },
        .Binary => |*bin| {
            const left = try self.convert(as_sem(bin.left));
            const right = try self.convert(as_sem(bin.right));

            const op = bin.orig_expr.Binary.op.type;

            // We could have picked binary.right.
            const tid = get_sem_tid(as_sem(bin.left));

            return try self.create_inst(try binary(tid, op, left, right));
        },
        .Variable => |*variable| {
            const name = variable.orig_expr.Variable.name.lexeme;

            if (self.globals.contains(name)) {
                return try self.create_inst(.{
                    .global_ref = name,
                });
            }

            //@todo: more efficient way of handling this
            const maybe_local_ident = self.function_locals.find_index(
                variable.orig_expr.Variable.name.lexeme,
                find_str,
            );

            if (maybe_local_ident) |local_ident| {
                return try self.create_inst(.{
                    .local_ref = @intCast(local_ident),
                });
            } else {
                return IrError.CannotFindLocalVariable;
            }
        },
        .Logical => |logical| {
            const tid = get_sem_tid(sem);

            const left = try self.convert(as_sem(logical.left));
            const right = try self.convert(as_sem(logical.right));

            const op = logical.orig_expr.Logical.op.type;

            // this is not needed for i32
            const bin = try self.create_inst(try binary(
                tid,
                if (op == .AND) .BANG_EQUAL else .EQUAL_EQUAL,
                left,
                try self.create_inst(try literal(tid, if (tid == .bool) Expr.Literal.Value{
                    .Boolean = true, //@todo  this sucks
                } else Expr.Literal.Value{
                    .Number = "0",
                })),
            ));

            //@todo refacto with if
            const values = .{
                .condition = bin,
                .then_branch = right,
                .else_branch = left,
            };

            const select_inst: Inst = switch (tid) {
                .bool => .{ .select_bool = values },
                .i32 => .{ .select_i32 = values },
                .i64 => .{ .select_i64 = values },
                .f32 => .{ .select_f32 = values },
                .f64 => .{ .select_f64 = values },
                else => unreachable,
            };

            return try self.create_inst(select_inst);

            // return try self.create_inst(.{ .@"if" = values });

            // const select_inst: Inst = switch (tid) {
            //     .bool => .{ .select_bool = values },
            //     .i32 => .{ .select_i32 = values },
            //     .i64 => .{ .select_i64 = values },
            //     .f32 => .{ .select_f32 = values },
            //     .f64 => .{ .select_f64 = values },
            //     else => unreachable,
            // };

            // return try self.create_inst(select_inst);
        },
        .If => |*if_sem| {
            const tid = get_sem_tid(as_sem(if_sem));

            const condition = try self.convert(as_sem(if_sem.condition));

            const then_branch = try self.convert(as_sem(if_sem.then_branch));

            const else_branch = if (if_sem.else_branch) |else_branch|
                try self.convert(as_sem(else_branch))
            else
                null;

            // @todo real opti detection ??
            const needs_branching = tid == .void;

            if (needs_branching) {
                return try self.create_inst(
                    .{
                        .@"if" = .{
                            .then_branch = then_branch,
                            .else_branch = else_branch,
                            .condition = condition,
                        },
                    },
                );
            }

            const values: Inst.Select = .{
                .then_branch = then_branch,
                .else_branch = else_branch,
                .condition = condition,
            };

            const select_inst: Inst = switch (tid) {
                .bool => .{ .select_bool = values },
                .i32 => .{ .select_i32 = values },
                .i64 => .{ .select_i64 = values },
                .f32 => .{ .select_f32 = values },
                .f64 => .{ .select_f64 = values },
                else => unreachable,
            };

            return try self.create_inst(select_inst);
        },
        .Call => |*call| {
            // @todo dynamic calls ??
            std.debug.print("callee {any}", .{as_sem(call)});
            // @todo horror museum
            const function_name = call.orig_expr.Call.callee.Variable.name.lexeme;

            const args = try self.allocator.alloc(*Inst, call.args.len);

            for (call.args, 0..) |arg, i| {
                args[i] = try self.convert(as_sem(arg));
            }

            return try self.create_inst(
                .{
                    .call = .{ .callee = function_name, .args = args },
                },
            );
        },
        else => {
            std.debug.print("\nNot Implemented = {any}\n", .{sem});
            return IrError.NotImplemented;
        },
    }
}

fn literal(tid: Infer.TypeID, value: Expr.Literal.Value) !Inst {
    return switch (tid) {
        //temporary, Number should be narrowed in Infer phase or an error should be thrown
        .number, .i32 => .{
            .value_i32 = try std.fmt.parseInt(i32, value.Number, 10),
        },
        .i64 => .{
            .value_i64 = try std.fmt.parseInt(i64, value.Number, 10),
        },
        .float, .f32 => .{
            .value_f32 = try std.fmt.parseFloat(f32, value.Number),
        },
        .f64 => .{
            .value_f64 = try std.fmt.parseFloat(f64, value.Number),
        },
        .bool => .{
            .value_bool = if (value.Boolean) 1 else 0,
        },
        else => IrError.NotImplemented,
    };
}
fn binary(
    tid: Infer.TypeID,
    op: Token.Type,
    left: *Inst,
    right: *Inst,
) !Inst {
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
            .bool => .{
                .eq_bool = values,
            },
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
            .bool => .{
                .neq_bool = values,
            },
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

    return inst;
}

fn find_str(item: []const u8, target: []const u8) bool {
    return std.mem.eql(u8, item, target);
}

fn create_inst(self: *Ir, inst: Inst) !*Inst {
    const inst_ptr = try self.instructions.addOne(self.allocator);
    inst_ptr.* = inst;
    return inst_ptr;
}

fn eval(self: *Ir, sem: *Infer.Sem, comptime InputType: type, comptime ReturnType: type) !ReturnType {
    return switch (sem.*) {
        .Literal => |*lit| switch (InputType) {
            bool => if (lit.orig_expr.Literal.value.Boolean) 1 else 0,
            i32, i64 => try std.fmt.parseInt(InputType, lit.orig_expr.Literal.value.Number, 10),
            f32, f64 => try std.fmt.parseFloat(InputType, lit.orig_expr.Literal.value.Number),
            else => IrError.NotImplemented,
        },
        .Binary => |*bin| blk: {
            const left = try self.eval(as_sem(bin.left), InputType, ReturnType);
            const right = try self.eval(as_sem(bin.right), InputType, ReturnType);

            break :blk switch (bin.orig_expr.Binary.op.type) {
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
const meta = std.meta;
const activeTag = std.meta.activeTag;
const Token = @import("./token.zig");
const Infer = @import("./infer.zig");
const Expr = @import("./ast/expr.zig").Expr;
const as_sem = @import("./infer.zig").as_sem;
const as_sems = @import("./infer.zig").as_sems;
const get_sem_tid = @import("./infer.zig").get_sem_tid;

const Stack = @import("./Stack.zig").Stack;
