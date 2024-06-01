const Token = @import("../token.zig");
const File = @import("../file.zig");
// const Stmt = @import("./stmt.zig").Stmt;
pub const Expr = union(enum) {
    Assign: Assign,
    OperationAssign: OperationAssign,
    Binary: Binary,
    Call: Call,
    Function: Function,
    Arg: Arg,
    Grouping: Grouping,
    Block: Block,
    Literal: Literal,
    Logical: Logical,
    Unary: Unary,
    Variable: Variable,
    ConstInit: ConstInit, // statement or expression ?
    VarInit: VarInit, // statement or expression ?
    If: If,
    Loop: Loop,
    Break: Break,
    While: While,
    Continue: Continue,
    Import: Import,
    Return: Return,

    pub const Assign = struct {
        name: *const Token,
        value: *const Expr,
    };

    pub const OperationAssign = struct {
        name: *const Token,
        op: *const Token,
        value: *const Expr,
    };

    pub const Binary = struct {
        left: *const Expr,
        op: *const Token,
        right: *const Expr,
    };

    pub const Call = struct {
        callee: *const Expr,
        paren: *const Token,
        args: []*const Expr,
    };

    pub const Function = struct {
        args: ?[]Arg,
        body: *const Expr,
        name: ?*const Token,
        type: ?*const Token, // @todo type expression

    };

    pub const Arg = struct {
        expr: *const Expr, // @todo limit this to Variables ?
        type: ?*const Token,
    };

    pub const Block = struct {
        exprs: []*const Expr,
        brace: *const Token,
    };

    pub const Grouping = struct {
        expr: *const Expr,
    };

    pub const Literal = struct {
        pub const Value = union(enum) {
            String: []const u8,
            Number: []const u8,
            Boolean: bool,
            Nil: ?bool, // what type should we use to represent null values ?
        };

        value: Value,
        token: *const Token,
    };

    pub const Logical = struct {
        left: *const Expr,
        op: *const Token,
        right: *const Expr,
    };

    pub const Unary = struct {
        op: *const Token,
        right: *const Expr,
    };

    pub const Variable = struct {
        name: *const Token,
    };

    pub const ConstInit = struct {
        name: *const Token,
        initializer: *const Expr,
        type: ?*const Token, // @todo type expression
    };

    pub const VarInit = struct {
        name: *const Token,
        initializer: *const Expr,
        type: ?*const Token, // @todo type expression
    };

    pub const If = struct {
        condition: *const Expr,
        then_branch: *const Expr,
        else_branch: ?*const Expr,
    };

    pub const Loop = struct {
        body: *const Expr,
    };

    pub const Return = struct {
        keyword: *const Token,
        value: ?*const Expr,
    };

    pub const Break = struct {
        keyword: *const Token,
        value: ?*const Expr, // @todo break return value
        from: ?[]const u8,
    };

    pub const While = struct {
        condition: *const Expr,
        body: *const Expr,
        inc: ?*const Expr,
        label: ?[]const u8,
    };

    pub const Continue = struct {};

    pub const Import = struct {
        namespace: *const Token,
        member: *const Token,
    };

    pub fn get_column_start(self: *const Expr) u32 {
        return self.get_location()[0];
    }

    pub fn get_column_end(self: *const Expr) u32 {
        return self.get_location()[1];
    }

    pub fn get_location(self: *const Expr) struct { u32, u32 } {
        return switch (self.*) {
            .Literal => |*lit| .{
                lit.token.start,
                lit.token.end,
            },
            .VarInit => |*var_init| .{
                var_init.name.start,
                var_init.initializer.get_location()[1],
            },
            .Variable => |*variable| .{
                variable.name.start,
                variable.name.end,
            },
            .ConstInit => |*const_init| .{
                const_init.name.start,
                const_init.initializer.get_location()[1],
            },
            .Assign => |*assign| .{
                assign.name.start,
                assign.value.get_location()[1],
            },
            .Binary => |*binary| .{
                binary.left.get_location()[0],
                binary.right.get_location()[1],
            },
            .Call => |*call| .{
                call.callee.get_location()[0],
                call.paren.end,
            },
            .Function => |*function| {
                const start = if (function.name) |name|
                    name.start
                else if (function.args != null and function.args.?.len > 0)
                    function.args.?[0].expr.get_location()[0]
                else
                    function.body.get_location()[0];

                const end = function.body.get_location()[1];

                return .{ start, end };
            },
            .Grouping => |*grouping| {
                const start, const end = grouping.expr.get_location();
                return .{ start - 1, end + 1 }; // take ( and ) into account
            },
            .Logical => |*logical| {
                return .{
                    logical.left.get_location()[0],
                    logical.right.get_location()[1],
                };
            },
            .Block => |*block| {
                return .{
                    block.brace.start,
                    block.brace.end,
                };
            },
            .If => |*if_expr| {
                return .{
                    if_expr.then_branch.get_location()[0],
                    if_expr.then_branch.get_location()[1],
                    // if_expr.condition.get_location()[0],
                    // if_expr.condition.get_location()[1],
                    // if (if_expr.else_branch) |else_branche|
                    //     else_branche.get_location()[1]
                    // else
                    //     if_expr.then_branch.get_location()[1],
                };
            },
            else => {
                std.debug.print("\nUnimplemented expr get location {any} \n", .{self});
                unreachable;
            },
        };
    }

    pub fn get_line(self: *const Expr, file: *const File) u32 {
        const start = self.get_location()[0];

        return file.get_start_line(start);
    }

    pub fn get_text(self: *const Expr, src: []const u8) []const u8 {
        const start, const end = self.get_location();
        return src[start..end];
    }
};

const std = @import("std");
