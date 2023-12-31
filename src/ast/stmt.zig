const Token = @import("../token.zig");
const Expr = @import("expr.zig").Expr;

pub const Stmt = union(enum) {
    Block: Block,
    Expr: Expr,
    Function: Function,
    If: If,
    Print: Expr,
    Return: Return,
    Var: Var,
    While: While,
    Break: Break,
    Continue: Continue,

    pub const Block = struct {
        stmts: []*const Stmt,
    };

    pub const Function = struct {
        name: ?*const Token,
        args: []*const Token,
        body: []*const Stmt,
    };

    pub const If = struct {
        condition: *const Expr, // @todo : what is pointer what is not pointer ????
        then_branch: *const Stmt,
        else_branch: ?*const Stmt,
    };

    pub const Var = struct {
        name: *const Token,
        initializer: ?*const Expr,
    };

    pub const Return = struct {
        keyword: *const Token,
        value: ?*const Expr,
    };

    pub const While = struct {
        condition: *const Expr,
        body: *const Stmt,
        inc: ?*const Expr,
    };

    pub const Break = struct {};

    pub const Continue = struct {};
};
