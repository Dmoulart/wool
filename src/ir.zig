allocator: std.mem.Allocator,

instructions: std.ArrayListUnmanaged(Inst),

const Ir = @This();

const Op = enum(u8) {
    push_i32,
    local_i32,
    // const_init,
    // global,
};

const Inst = union(Op) {
    push_i32: i32,
    local_i32: []const u8,
    // const_init = struct {
    //     name: []const u8,
    //     ty: TypeID,
    // },
    // global = struct {
    //     name: []const u8,
    //     ty: TypeID,
    // },
};

const IrError = error{NotImplemented};

pub fn init(allocator: std.mem.Allocator) Ir {
    return .{
        .allocator = allocator,
        .instructions = .{},
    };
}

pub fn emit_program(self: *Ir, sems: []Infer.Sem) ![]Inst {
    for (sems) |*sem| {
        const inst = try self.emit(sem);

        try self.instructions.append(self.allocator, inst);
    }

    return self.instructions.items;
}

pub fn emit(self: *Ir, sem: *Infer.Sem) !Inst {
    _ = self;
    return switch (sem.*) {
        .Literal => |*literal| switch (get_sem_tid(sem)) {
            .i32 => Inst{
                .push_i32 = try std.fmt.parseInt(i32, literal.orig_expr.Literal.value.Number, 10),
            },

            else => IrError.NotImplemented,
        },
        .VarInit => |*var_init| switch (get_sem_tid(to_sem(var_init.initializer))) {
            .i32 => Inst{
                .local_i32 = var_init.orig_expr.VarInit.name.lexeme,
            },
            else => IrError.NotImplemented,
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

const Infer = @import("./infer.zig");
const TypeID = @import("./infer.zig").TypeID;
const get_sem_tid = @import("./infer.zig").get_sem_tid;
const to_sem = @import("./infer.zig").to_sem;
const std = @import("std");
