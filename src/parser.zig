const std = @import("std");
const ArrayList = @import("std").ArrayList;
const Token = @import("./token.zig");
const Stmt = @import("./ast/stmt.zig").Stmt;
const Expr = @import("./ast/expr.zig").Expr;

const Self = @This();

const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
const Err = ErrorReporter(ParserError);
const ParserError = error{
    OutOfMemory,
    MissingExpression,
    MissingSemiColonAfterValue,
};

allocator: std.mem.Allocator,

stmts: ArrayList(Stmt),

exprs: ArrayList(Expr),

current: u32 = 0,

tokens: []Token,

pub fn init(tokens: []Token, allocator: std.mem.Allocator) Self {
    return Self{
        .tokens = tokens,
        .allocator = allocator,
        .stmts = std.ArrayList(Stmt).init(allocator),
        .exprs = std.ArrayList(Expr).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.stmts.deinit();
}

pub fn parse(self: *Self) ParserError![]Stmt {
    while (!self.is_at_end()) {
        _ = try self.declaration();
    }

    return self.stmts.toOwnedSlice() catch ParserError.OutOfMemory;
}

fn declaration(self: *Self) ParserError!?*Stmt {
    return try self.expression_stmt();
}

fn expression_stmt(self: *Self) ParserError!?*Stmt {
    const expr = try self.expression();

    _ = try self.consume(
        .SEMICOLON,
        ParserError.MissingSemiColonAfterValue,
        "Expect ';' after value.",
    );

    return try self.create_stmt(.{ .Expr = expr.* });
}

fn expression(self: *Self) ParserError!*Expr {
    return try self.primary();
}

fn primary(self: *Self) ParserError!*Expr {
    if (self.match(&.{.IDENTIFIER})) {
        return try self.create_expr(
            .{
                .Variable = .{
                    .name = self.previous(),
                },
            },
        );
    }

    return Err.raise(
        self.peek(),
        ParserError.MissingExpression,
        "Missing expression",
    );
}

fn create_stmt(self: *Self, stmt: Stmt) ParserError!*Stmt {
    var ptr = self.stmts.addOne() catch return ParserError.OutOfMemory;
    ptr.* = stmt;
    return ptr;
}

fn create_expr(self: *Self, expr: Expr) ParserError!*Expr {
    var ptr = self.exprs.addOne() catch return ParserError.OutOfMemory;
    ptr.* = expr;
    return ptr;
}

fn match(self: *Self, comptime types: []const Token.Types) bool {
    for (types) |token_type| {
        if (self.check(@as(Token.Types, token_type))) {
            _ = self.advance();
            return true;
        }
    }

    return false;
}

fn check(self: *Self, token_type: Token.Types) bool {
    if (self.is_at_end()) {
        return false;
    }

    return token_type == @as(Token.Types, self.peek().type);
}

fn advance(self: *Self) *Token {
    if (!self.is_at_end()) {
        self.current += 1;
    }

    return self.previous();
}

fn consume(self: *Self, token_type: Token.Types, comptime parser_error: ParserError, comptime msg: []const u8) ParserError!*Token {
    if (self.check(token_type)) {
        return self.advance();
    }
    return Err.raise(self.peek(), parser_error, msg);
}

fn is_at_end(self: *Self) bool {
    return self.peek().type == .EOF;
}

fn peek(self: *Self) *Token {
    return &self.tokens[self.current];
}

fn previous(self: *Self) *Token {
    return &self.tokens[self.current - 1];
}
