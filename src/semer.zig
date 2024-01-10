allocator: std.mem.Allocator,

ast: []*const Expr,

sems: std.AutoHashMap(*const Expr, Sem),

ctx: Context(TypeRecord),

const TypeError = error{
    InvalidType,
    CannotInferType,
    ValueMismatchDeclaredType,
};

pub const Sem = struct {
    type: Type,
};

pub const TypeRecord = struct {
    type: ?Type, // empty sem means it must be infered
    expr: *const Expr,
};

pub fn init(allocator: std.mem.Allocator, ast: []*Expr) @This() {
    return .{
        .allocator = allocator,
        .ast = ast,
        .sems = std.AutoHashMap(*const Expr, Sem).init(allocator),
        .ctx = Context(TypeRecord).init(allocator),
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
            const maybe_declared_type = if (const_init.type) |const_type|
                try Type.from_str(const_type.lexeme)
            else
                null;

            try self.ctx.push_frame(.{
                .expr = expr,
                .type = maybe_declared_type,
            });
            defer _ = self.ctx.pop_frame();

            const sem = try self.analyze_expr(const_init.initializer);

            if (maybe_declared_type) |declared_type| {
                if (sem.type != declared_type) {
                    return TypeError.ValueMismatchDeclaredType;
                }
            }

            return try self.create_sem(expr, .{ .type = sem.type });
        },
        .VarInit => |var_init| {
            const maybe_declared_type = if (var_init.type) |var_type|
                try Type.from_str(var_type.lexeme)
            else
                null;

            try self.ctx.push_frame(.{
                .expr = expr,
                .type = maybe_declared_type,
            });
            defer _ = self.ctx.pop_frame();

            const sem = try self.analyze_expr(var_init.initializer);

            if (maybe_declared_type) |declared_type| {
                if (sem.type != declared_type) {
                    return TypeError.ValueMismatchDeclaredType;
                }
            }

            return try self.create_sem(expr, .{ .type = sem.type });
        },
        .Function => |func| {
            const return_type = try Type.from_str(func.type.lexeme);

            if (func.body) |body| {
                const body_sem = try self.analyze_expr(body);
                if (body_sem.type != return_type) {
                    return TypeError.ValueMismatchDeclaredType;
                }
            }

            return self.create_sem(expr, .{ .type = return_type });
        },
        .Block => |block| {
            const return_type = for (block.exprs, 0..) |block_expr, i| {
                const sem = try self.analyze_expr(block_expr);
                if (i == block.exprs.len - 1) {
                    break sem.type;
                }
            } else .void;

            if (self.get_target_type()) |target_type| {
                if (target_type != return_type) {
                    return TypeError.ValueMismatchDeclaredType;
                }
            }

            return try self.create_sem(expr, .{ .type = return_type });
        },
        .Return => |return_expr| {
            const return_type = if (return_expr.value) |return_value|
                (try self.analyze_expr(return_value)).type
            else
                .void;
            return try self.create_sem(expr, .{ .type = return_type });
        },
        .Literal => |literal| {
            return switch (literal.value) {
                .Number => |number| {
                    if (self.get_target_type()) |target_type| {
                        switch (target_type) {
                            .i32, .i64, .f32, .f64 => {},
                            else => return TypeError.InvalidType,
                        }

                        if (is_integer_type(target_type) and is_float_value(number)) {
                            return TypeError.ValueMismatchDeclaredType;
                        }

                        //@todo overflow checks

                        return try self.create_sem(
                            expr,
                            .{
                                .type = target_type,
                            },
                        );
                    } else {
                        return try self.create_sem(
                            expr,
                            .{
                                .type = infer_number_type(number),
                            },
                        );
                    }
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

fn get_target_type(self: *@This()) ?Type {
    if (self.ctx.in(&.{ "VarInit", "ConstInit" })) |frame| {
        if (frame.type) |frame_type| {
            return frame_type;
        }
    }
    return null;
}

// fn infer_type(expr: *const Expr) !?Type {
//     return switch (expr.*) {
//         .Literal => |literal| {
//             return switch (literal.value) {
//                 .Number => |number| {
//                     const is_float = @rem(number, 1) != 0;
//                     if (is_float) {
//                         return if (number > floatMax(f32)) .f64 else .f32;
//                     } else {
//                         return if (number > maxInt(i32)) .i64 else .i32;
//                     }
//                 },
//                 else => TypeError.CannotInferType,
//             };
//         },
//         .Function => .func,
//         else => TypeError.CannotInferType,
//     };
// }

inline fn is_float_value(number: f64) bool {
    return @rem(number, 1) != 0;
}

inline fn is_integer_type(number_type: Type) bool {
    return number_type == .i32 or number_type == .i64;
}

inline fn is_foat_type(number_type: Type) bool {
    return number_type == .f32 or number_type == .f64;
}

fn infer_number_type(number: f64) Type {
    return blk: {
        if (is_float_value(number)) {
            break :blk if (number > floatMax(f32)) .f64 else .f32;
        } else {
            break :blk if (number > maxInt(i32)) .i64 else .i32;
        }
    };
}

const std = @import("std");

const Expr = @import("./ast/expr.zig").Expr;
const Type = @import("./types.zig").Type;
const Context = @import("./context.zig").Context;
const floatMax = std.math.floatMax;
const maxInt = std.math.maxInt;
