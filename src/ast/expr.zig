const Token = @import("../token.zig");
const Stmt = @import("./stmt.zig").Stmt;

pub const Expr = union(enum) {
    Assign: Assign,
    Binary: Binary,
    Call: Call,
    Lambda: Lambda,
    Grouping: Grouping,
    Literal: Literal,
    Logical: Logical,
    Unary: Unary,
    Variable: Variable,

    pub const Assign = struct {
        name: *const Token,
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

    pub const Lambda = struct {
        args: []*const Token,
        body: []*const Stmt,
    };

    pub const Grouping = struct {
        expr: *const Expr,
    };

    pub const Literal = struct {
        const Value = union(enum) {
            String: []const u8,
            Integer: i64,
            Float: f64,
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
};
