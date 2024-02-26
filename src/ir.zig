allocator: std.mem.Allocator,

instructions: std.ArrayListUnmanaged(Inst),

function_variables: Stack([]const u8),

const Ir = @This();

pub const Inst = union(enum) {
    push_i32: i32,
    local_i32: u32,

    // make one global instruction ?
    global_i32: Inst.Global(i32),
    global_i64: Inst.Global(i64),
    global_f32: Inst.Global(f32),
    global_f64: Inst.Global(f64),

    begin_func: BeginFunc,
    end_func: []const u8,

    begin_block: void,
    end_block: void,

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
        .function_variables = Stack([]const u8).init(allocator),
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
        .VarInit => |*var_init| switch (get_sem_tid(as_sem(var_init.initializer))) {
            .i32 => blk: {
                const name = var_init.orig_expr.VarInit.name.lexeme;
                const local_ident = try self.function_variables.push(name);

                try self.emit(try self.convert(as_sem(var_init.initializer)));

                break :blk Inst{
                    .local_i32 = local_ident, // stack ?
                };
            },
            else => IrError.NotImplemented,
        },
        .ConstInit => |*const_init| switch (get_sem_tid(as_sem(const_init.initializer))) {
            .i32 => Inst{
                .global_i32 = .{
                    .name = const_init.orig_expr.ConstInit.name.lexeme, // stack ?
                    .value = try self.eval(as_sem(const_init.initializer), i32),
                },
            },
            .i64 => Inst{
                .global_i64 = .{
                    .name = const_init.orig_expr.ConstInit.name.lexeme, // stack ?
                    .value = try self.eval(as_sem(const_init.initializer), i64),
                },
            },
            .f32 => Inst{
                .global_f32 = .{
                    .name = const_init.orig_expr.ConstInit.name.lexeme, // stack ?
                    .value = try self.eval(as_sem(const_init.initializer), f32),
                },
            },
            .f64 => Inst{
                .global_f32 = .{
                    .name = const_init.orig_expr.ConstInit.name.lexeme, // stack ?
                    .value = try self.eval(as_sem(const_init.initializer), f32),
                },
            },
            .func => try self.convert(as_sem(const_init.initializer)),
            else => IrError.NotImplemented,
        },
        .Function => |*function| blk: {
            try self.function_variables.clear();
            var args = try self.allocator.alloc(Infer.TypeID, function.type_node.function.args.len);

            for (function.type_node.function.args, 0..) |arg, i| {
                args[i] = arg.get_tid();
            }

            const name = if (function.orig_expr.Function.name) |name|
                name.lexeme
            else
                "anonymous";

            const begin_func = Inst{
                .begin_func = .{
                    .name = name,
                    .ret = function.type_node.function.return_type.get_tid(),
                    .args = args,
                },
            };

            try self.emit(begin_func);
            const body = try self.convert(as_sem(function.body));
            try self.emit(body);

            break :blk Inst{ .end_func = name };
        },
        .Block => |*block| blk: {
            try self.emit(Inst{ .begin_block = {} });
            for (block.exprs) |expr| {
                try self.emit(try self.convert(as_sem(expr)));
            }
            break :blk Inst{ .end_block = {} };
        },
        else => {
            std.debug.print("\nNot Implemented = {any}\n", .{sem});
            return IrError.NotImplemented;
        },
    };
}

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

const std = @import("std");
const Infer = @import("./infer.zig");
const as_sem = @import("./infer.zig").as_sem;
const get_sem_tid = @import("./infer.zig").get_sem_tid;

const Stack = @import("./Stack.zig").Stack;
