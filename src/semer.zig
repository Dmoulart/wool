allocator: std.mem.Allocator,

ast: []*const Expr,

sems: std.AutoHashMap(*const Expr, Sem),

ctx: Context(TypeRecord),

const TypeError = error{
    InvalidType,
    CannotInferType,
    ValueMismatchDeclaredType,
    ValueMismatchFunctionReturnType,
    NonBooleanConditionInIf,
    MultipleReturnTypesForIfExpressions,
    IncompatibleTypesInBinaryExpression,
    AlreadyRegisteredSem,
};

const Err = ErrorReporter(TypeError);

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
                    return Err.raise(
                        var_init.name,
                        TypeError.ValueMismatchDeclaredType,
                        "Variable initialization value does not match declared type",
                    );
                }
            }

            return try self.create_sem(expr, .{ .type = sem.type });
        },
        .Function => |func| {
            const return_type = try Type.from_str(func.type.lexeme);

            if (func.body) |body| {
                const body_sem = try self.analyze_expr(body);
                if (body_sem.type != return_type) {
                    return Err.raise(
                        func.type,
                        TypeError.ValueMismatchFunctionReturnType,
                        "Returned value does not match function return type",
                    );
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
        .If => |if_expr| {
            //@todo then become invalid when condition is reached
            var then_branch = (try self.analyze_expr(if_expr.then_branch)).*;
            const condition = try self.analyze_expr(if_expr.condition);
            var maybe_else_branch = if (if_expr.else_branch) |else_branch| try self.analyze_expr(else_branch) else null;

            if (condition.type != .bool) return TypeError.NonBooleanConditionInIf;

            if (self.in_assignation()) {
                if (maybe_else_branch) |else_branch| {
                    if (then_branch.type != else_branch.type) {
                        if (then_branch.type.is_number() and else_branch.type.is_number()) {
                            const coerced_type = coerce_number_types(then_branch.type, else_branch.type);
                            then_branch.type = coerced_type;
                            else_branch.type = coerced_type;
                        } else {
                            return TypeError.MultipleReturnTypesForIfExpressions;
                        }
                    }
                }
            }

            if (self.get_target_type()) |target_type| {
                if (then_branch.type != target_type or maybe_else_branch != null and maybe_else_branch.?.type != target_type) {
                    return TypeError.ValueMismatchDeclaredType;
                }
            }

            return try self.create_sem(expr, .{ .type = then_branch.type });
        },
        .Binary => |binary| {
            const left = try self.analyze_expr(binary.left);
            const right = try self.analyze_expr(binary.right);

            if (is_number_type(left.type) and is_number_type(right.type)) {
                const coerced_type = coerce_number_types(left.type, right.type);
                left.type = coerced_type;
                right.type = coerced_type;
            }

            return try self.create_sem(expr, .{ .type = left.type });
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
                .Boolean => {
                    if (self.get_target_type()) |target_type| {
                        if (target_type != .bool) {
                            return TypeError.ValueMismatchDeclaredType;
                        }
                    }
                    return try self.create_sem(
                        expr,
                        .{
                            .type = .bool,
                        },
                    );
                },
                .Number => |number| {
                    if (self.get_target_type()) |target_type| {
                        if (!is_number_type(target_type)) {
                            return TypeError.InvalidType;
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
    const result = self.sems.getOrPut(expr) catch return TypeError.AlreadyRegisteredSem;
    result.value_ptr.* = sem;
    return result.value_ptr;
}

fn in_assignation(self: *@This()) bool {
    return self.ctx.in(&.{ "VarInit", "ConstInit" }) != null;
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
//@todo remove -> put this in types
inline fn is_number_type(@"type": Type) bool {
    return switch (@"type") {
        .i32, .i64, .f32, .f64 => true,
        else => false,
    };
}

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

fn coerce_number_types(a: Type, b: Type) Type {
    if (a != b) {
        const size_of_left = a.size_of();
        const size_of_right = b.size_of();

        if (size_of_left == size_of_right) {
            if (is_foat_type(a) and !is_foat_type(b)) {
                return a;
            } else if (is_foat_type(b) and !is_foat_type(a)) {
                return b;
            }
        } else {
            return if (size_of_left >= size_of_right) a else b;
        }
    }

    return a;
}

const std = @import("std");

const Expr = @import("./ast/expr.zig").Expr;
const Type = @import("./types.zig").Type;
const Context = @import("./context.zig").Context;
const floatMax = std.math.floatMax;
const maxInt = std.math.maxInt;
const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
