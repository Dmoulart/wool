allocator: std.mem.Allocator,

instructions: std.ArrayListUnmanaged(Inst),

const Ir = @This();

pub const Inst = union(enum) {
    push_i32: i32,
    local_i32: []const u8,

    // make one global instruction ?
    global_i32: Inst.Global(i32),
    global_i64: Inst.Global(i64),
    global_f32: Inst.Global(f32),
    global_f64: Inst.Global(f64),

    begin_func: BeginFunc,
    end_func: []const u8,

    pub fn Global(comptime T: type) type {
        return struct {
            name: []const u8,
            value: T,
        };
    }

    pub const BeginFunc = struct {
        name: []const u8,
        args: []Infer.TypeID,
        ret: Infer.TypeID,
    };
};

const IrError = error{NotImplemented};

pub fn init(allocator: std.mem.Allocator) Ir {
    return .{
        .allocator = allocator,
        .instructions = .{},
    };
}

pub fn emit_program(self: *Ir, sems: []*Infer.Sem) ![]Inst {
    for (sems) |sem| {
        try self.emit(try self.convert(sem));
    }

    return self.instructions.items;
}

// pub fn emit_instruction(self: *Ir, sem: *Infer.Sem) !void {
//     const inst = try self.convert(sem);
//     try self.emit(inst);
// }

pub fn emit(self: *Ir, inst: Inst) !void {
    try self.instructions.append(self.allocator, inst);
}

pub fn convert(self: *Ir, sem: *Infer.Sem) anyerror!Inst {
    return switch (sem.*) {
        .Literal => |*literal| switch (get_sem_tid(sem)) {
            //temporary, Number should be narrowed in Infer phase or an error should be thrown
            .number, .i32 => Inst{
                .push_i32 = try std.fmt.parseInt(i32, literal.orig_expr.Literal.value.Number, 10),
            },
            else => IrError.NotImplemented,
        },
        .VarInit => |*var_init| switch (get_sem_tid(to_sem(var_init.initializer))) {
            .i32 => Inst{
                .local_i32 = var_init.orig_expr.VarInit.name.lexeme, // stack ?
            },
            else => IrError.NotImplemented,
        },
        .ConstInit => |*const_init| switch (get_sem_tid(to_sem(const_init.initializer))) {
            .i32 => Inst{
                .global_i32 = .{
                    .name = const_init.orig_expr.ConstInit.name.lexeme, // stack ?
                    .value = try self.eval(to_sem(const_init.initializer), i32),
                },
            },
            .i64 => Inst{
                .global_i64 = .{
                    .name = const_init.orig_expr.ConstInit.name.lexeme, // stack ?
                    .value = try self.eval(to_sem(const_init.initializer), i64),
                },
            },
            .f32 => Inst{
                .global_f32 = .{
                    .name = const_init.orig_expr.ConstInit.name.lexeme, // stack ?
                    .value = try self.eval(to_sem(const_init.initializer), f32),
                },
            },
            .f64 => Inst{
                .global_f32 = .{
                    .name = const_init.orig_expr.ConstInit.name.lexeme, // stack ?
                    .value = try self.eval(to_sem(const_init.initializer), f32),
                },
            },
            else => IrError.NotImplemented,
        },
        .Function => |*function| blk: {
            var args = try self.allocator.alloc(Infer.TypeID, function.type_node.function.args.len);
            for (function.type_node.function.args, 0..) |arg, i| {
                args[i] = arg.get_tid();
            }
            const name = if (function.orig_expr.Function.name) |name| name.lexeme else "anon";
            const begin_func = Inst{
                .begin_func = .{
                    .name = name,
                    .ret = function.type_node.function.return_type.get_tid(),
                    .args = args,
                },
            };
            try self.emit(begin_func);
            // try self.instructions.append(self.allocator, begin_func);
            const body = try self.convert(to_sem(function.body));
            try self.emit(body);
            break :blk Inst{ .end_func = name };
        },
        else => {
            std.debug.print("\nNot Implemented = {any}\n", .{sem});
            return IrError.NotImplemented;
        },
        // .ConstInit => |*const_init| Inst{
        //     .global = .{
        //         .name = const_init.orig_expr.name,
        //         .ty = const_init.orig_expr.name,
        //     },
        // },
    };
}

// fn global(self: *Ir, comptime T: type, sem: *Infer.Sem) Inst.Global(T) {
//     const name = sem.orig_expr.ConstInit.name.lexeme;
//     return .{
//         .name = name,
//         .value = try self.eval(to_sem(sem.initializer), T),
//     };
// }

fn eval(self: *Ir, sem: *Infer.Sem, comptime ExpectedType: type) !ExpectedType {
    _ = self;
    return switch (sem.*) {
        .Literal => |*literal| switch (ExpectedType) {
            i32, i64 => try std.fmt.parseInt(ExpectedType, literal.orig_expr.Literal.value.Number, 10),
            f32, f64 => try std.fmt.parseFloat(ExpectedType, literal.orig_expr.Literal.value.Number),
            else => IrError.NotImplemented,
        },
        else => IrError.NotImplemented,
    };
}

const Infer = @import("./infer.zig");
const get_sem_tid = @import("./infer.zig").get_sem_tid;
const to_sem = @import("./infer.zig").to_sem;
const std = @import("std");
