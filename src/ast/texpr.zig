const std = @import("std");
const StructField = std.builtin.Type.StructField;
const UnionField = std.builtin.Type.UnionField;
const Expr = @import("./expr.zig").Expr;
const TypeNode = @import("../infer.zig").TypeNode;

pub fn Typed(comptime AST: type) type {
    if (@typeInfo(AST) != .Union) {
        @compileError("TypedAST Can only decorate a union.");
    }

    var union_fields: []const UnionField = &[0]UnionField{};

    for (std.meta.fields(AST)) |expr_type| {
        var expr_fields: []const StructField = &[0]StructField{};

        var total_expr_fields: usize = 0;

        inline for (std.meta.fields(expr_type.type)) |expr_field| {
            const maybe_field_type: ?type = switch (expr_field.type) {
                *const Expr => *anyopaque,
                ?*const Expr => ?*anyopaque,
                []*const Expr => []*anyopaque,
                else => null,
            };

            if (maybe_field_type) |field_type| {
                total_expr_fields += 1;
                expr_fields = expr_fields ++ [_]StructField{
                    .{
                        .name = expr_field.name,
                        .type = field_type,
                        .default_value = null,
                        .is_comptime = false,
                        .alignment = @alignOf(field_type),
                    },
                };
            }
        }

        expr_fields = expr_fields ++ [_]StructField{
            .{
                .name = "type_node",
                .type = *TypeNode,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(*TypeNode),
            },
        };

        expr_fields = expr_fields ++ [_]StructField{
            .{
                .name = "orig_expr",
                .type = *const Expr,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(*const Expr),
            },
        };

        const decorated_type = @Type(.{
            .Struct = .{
                .layout = .Auto,
                .fields = expr_fields,
                .decls = &[_]std.builtin.Type.Declaration{},
                .is_tuple = false,
            },
        });

        union_fields = union_fields ++ [_]UnionField{
            .{
                .name = expr_type.name,
                .type = decorated_type,
                .alignment = @alignOf(decorated_type),
            },
        };
    }

    return @Type(
        .{
            .Union = .{
                .layout = .Auto,
                .tag_type = std.meta.FieldEnum(AST),
                .fields = union_fields,
                .decls = &[_]std.builtin.Type.Declaration{},
            },
        },
    );
}
