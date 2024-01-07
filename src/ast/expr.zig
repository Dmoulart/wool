const Token = @import("../token.zig");
// const Stmt = @import("./stmt.zig").Stmt;

pub const Expr = union(enum) {
    Assign: Assign,
    OperationAssign: OperationAssign,
    Binary: Binary,
    Call: Call,
    Function: Function,
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
        args: ?[]*const Token,
        body: ?*const Expr,
        name: ?*const Token,
        type: *const Token, // @todo type expression
    };

    pub const Block = struct {
        exprs: []*const Expr,
    };

    pub const Grouping = struct {
        expr: *const Expr,
    };

    pub const Literal = struct {
        const Value = union(enum) {
            String: []const u8,
            Integer: i32,
            Float: f32, //@todo typpes
            Boolean: bool,
            Nil: ?bool, // what type should we use to represent null values ?
        };

        value: Value,
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
        value: ?*const Expr, // @todo break return value
    };

    pub const While = struct {
        condition: *const Expr,
        body: *const Expr,
        inc: ?*const Expr,
    };

    pub const Continue = struct {};

    pub const Import = struct {
        namespace: *const Token,
        member: *const Token,
    };
};
