allocator: std.mem.Allocator,

ast: []*const Expr,

sems: std.AutoHashMap(*const Expr, Sem),

const TypeError = error{ InvalidType, CannotInferType, ValueMismatchDeclaredType };

pub const Sem = struct {
    type: Type,
};

pub fn init(allocator: std.mem.Allocator, ast: []*Expr) @This() {
    return .{
        .allocator = allocator,
        .ast = ast,
        .sems = std.AutoHashMap(*const Expr, Sem).init(allocator),
    };
}

pub fn analyze(self: *@This()) !*std.AutoHashMap(*const Expr, Sem) {
    for (self.ast) |expr| {
        _ = try self.analyze_expr(expr);
    }

    return &self.sems;
}

pub fn analyze_expr(self: *@This(), expr: *const Expr) !*Sem {
    return switch (expr.*) {
        .ConstInit => |const_init| {
            const sem = try self.analyze_expr(const_init.initializer);

            if (const_init.type) |const_type| {
                if (sem.type != try Type.from_str(const_type.lexeme)) {
                    return TypeError.ValueMismatchDeclaredType;
                }
            }

            return try self.create_sem(expr, .{ .type = sem.type });
        },
        .Function => |func| {
            const sem_type = try Type.from_str(func.type.lexeme);

            return self.create_sem(expr, .{ .type = sem_type });
        },
        .Literal => |literal| {
            return switch (literal.value) {
                .Number => |number| {
                    const is_float = @rem(number, 1) != 0;

                    const @"type": Type = blk: {
                        if (is_float) {
                            break :blk if (number > floatMax(f32)) .f64 else .f32;
                        } else {
                            break :blk if (number > maxInt(i32)) .i64 else .i32;
                        }
                    };

                    return try self.create_sem(
                        expr,
                        .{
                            .type = @"type",
                        },
                    );
                },
                else => {
                    return try self.create_sem(
                        expr,
                        .{
                            .type = .void,
                        },
                    );
                },
            };
        },
        else => {
            return try self.create_sem(
                expr,
                .{
                    .type = .void,
                },
            );
        },
    };
}

fn create_sem(self: *@This(), expr: *const Expr, sem: Sem) !*Sem {
    try self.sems.put(expr, sem);
    return self.sems.getPtr(expr).?;
}

fn infer_type(expr: *const Expr) !?Type {
    return switch (expr.*) {
        .Literal => |literal| {
            return switch (literal.value) {
                .Number => |number| {
                    const is_float = @rem(number, 1) != 0;
                    if (is_float) {
                        return if (number > floatMax(f32)) .f64 else .f32;
                    } else {
                        return if (number > maxInt(i32)) .i64 else .i32;
                    }
                },
                else => TypeError.CannotInferType,
            };
        },
        .Function => .func,
        else => TypeError.CannotInferType,
    };
}

const std = @import("std");

const Expr = @import("./ast/expr.zig").Expr;
const Type = @import("./types.zig").Type;
const floatMax = std.math.floatMax;
const maxInt = std.math.maxInt;
