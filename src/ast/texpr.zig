const std = @import("std");
const StructField = std.builtin.Type.StructField;
const UnionField = std.builtin.Type.UnionField;
const Expr = @import("./expr.zig").Expr;
const TypeNode = @import("../infer.zig").TypeNode;

pub fn TypedAST(comptime AST: type) type {
    if (@typeInfo(AST) != .Union) {
        @compileError("TypedAST Can only decorate a union.");
    }

    var fields: []const UnionField = &[0]UnionField{};

    for (std.meta.fields(AST)) |field| {
        const field_type = Typed(field.type);
        fields = fields ++ [_]UnionField{
            .{
                .name = field.name,
                .type = field_type,
                .alignment = @alignOf(field_type),
            },
        };
    }

    return @Type(.{ .Union = .{
        .layout = .Auto,
        .tag_type = std.meta.FieldEnum(AST),
        .fields = fields,
        .decls = &[_]std.builtin.Type.Declaration{},
    } });
}

// Decorate an expression type with types
pub fn Typed(comptime E: type) type {
    var fields: []const StructField = &[0]StructField{};

    var total_expr_fields: usize = 0;

    inline for (std.meta.fields(E)) |field| {
        if (field.type == *const Expr) {
            total_expr_fields += 1;
            fields = fields ++ [_]StructField{
                .{
                    .name = field.name,
                    .type = *const TypeNode,
                    .default_value = null,
                    .is_comptime = false,
                    .alignment = @alignOf(*const TypeNode),
                },
            };
        }
    }

    return @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = fields,
            .decls = &[_]std.builtin.Type.Declaration{},
            .is_tuple = false,
        },
    });
}
