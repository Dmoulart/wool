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
    MissingClosingParen,
    InvalidAssignmentTarget,
    MissingVariableName,
    MissingSemiColonAfterVarDeclaration,
    MissingSemiColonAfterReturnValue,
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
    if (self.match(&.{.RETURN})) {
        return try self.return_stmt();
    }

    return try self.expression_stmt();
}

fn return_stmt(self: *Self) ParserError!*Stmt {
    const keyword = self.previous();

    var value: ?*Expr = if (!self.check(.SEMICOLON))
        try self.expression()
    else
        null;

    _ = try self.consume(
        .SEMICOLON,
        ParserError.MissingSemiColonAfterReturnValue,
        "Expect ';' after return value.",
    );

    return try self.create_stmt(.{
        .Return = .{
            .keyword = keyword,
            .value = value,
        },
    });
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
    return try self.const_init();
}

fn const_init(self: *Self) ParserError!*Expr {
    var expr = try self.var_init();

    if (self.match(&.{.COLON_COLON})) {
        const equals = self.previous();
        const value = try self.expression();

        return switch (expr.*) {
            .Variable => |*var_expr| {
                var name = var_expr.name;
                return try self.create_expr(.{
                    .ConstInit = .{
                        .name = name,
                        .initializer = value,
                    },
                });
            },
            else => Err.raise(
                equals,
                ParserError.InvalidAssignmentTarget,
                "Invalid assignment target.",
            ),
        };
    }

    return expr;
}

fn var_init(self: *Self) ParserError!*Expr {
    var expr = try self.assignment();

    if (self.match(&.{.COLON_EQUAL})) {
        const equals = self.previous();
        const value = try self.expression();

        return switch (expr.*) {
            .Variable => |*var_expr| {
                var name = var_expr.name;
                return try self.create_expr(.{
                    .VarInit = .{
                        .name = name,
                        .initializer = value,
                    },
                });
            },
            else => Err.raise(
                equals,
                ParserError.InvalidAssignmentTarget,
                "Invalid assignment target.",
            ),
        };
    }

    return expr;
}

fn assignment(self: *Self) ParserError!*Expr {
    const expr = try self.or_expr();

    if (self.match(&.{.EQUAL})) {
        const equals = self.previous();
        const value = try self.assignment();

        return switch (expr.*) {
            .Variable => |*var_expr| {
                var name = var_expr.name;
                return try self.create_expr(.{
                    .Assign = .{
                        .name = name,
                        .value = value,
                    },
                });
            },
            else => Err.raise(
                equals,
                ParserError.InvalidAssignmentTarget,
                "Invalid assignment target.",
            ),
        };
    }

    return expr;
}

fn or_expr(self: *Self) ParserError!*Expr {
    var expr = try self.and_expr();

    while (self.match(&.{.OR})) {
        const op = self.previous();
        const right = try self.and_expr();

        expr = try self.create_expr(
            .{
                .Logical = .{
                    .left = expr,
                    .op = op,
                    .right = right,
                },
            },
        );
    }

    return expr;
}

fn and_expr(self: *Self) ParserError!*Expr {
    var expr = try self.equality();

    while (self.match(&.{.AND})) {
        const op = self.previous();
        const right = try self.and_expr();

        expr = try self.create_expr(
            .{
                .Logical = .{
                    .left = expr,
                    .op = op,
                    .right = right,
                },
            },
        );
    }

    return expr;
}

fn equality(self: *Self) ParserError!*Expr {
    var expr = try self.comparison();

    while (self.match(&.{ .BANG_EQUAL, .EQUAL_EQUAL })) {
        expr = try self.create_expr(.{
            .Binary = .{
                .left = expr,
                .op = self.previous(),
                .right = (try self.comparison()),
            },
        });
    }

    return expr;
}

fn comparison(self: *Self) ParserError!*Expr {
    var expr = try self.term();

    while (self.match(&.{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
        expr = try self.create_expr(.{
            .Binary = .{
                .left = expr,
                .op = self.previous(),
                .right = (try self.term()),
            },
        });
    }
    return expr;
}

fn term(self: *Self) ParserError!*Expr {
    var expr = try self.factor();

    while (self.match(&.{ .MINUS, .PLUS })) {
        expr = try self.create_expr(.{
            .Binary = .{
                .left = expr,
                .op = self.previous(),
                .right = (try self.term()),
            },
        });
    }

    return expr;
}

fn factor(self: *Self) ParserError!*Expr {
    var expr = try self.unary();

    while (self.match(&.{ .SLASH, .STAR })) {
        expr = try self.create_expr(.{
            .Binary = .{
                .left = expr,
                .op = self.previous(),
                .right = (try self.term()),
            },
        });
    }
    return expr;
}

fn unary(self: *Self) ParserError!*Expr {
    if (self.match(&.{ .BANG, .MINUS })) {
        return try self.create_expr(.{
            .Unary = .{
                .op = self.previous(),
                .right = (try self.term()),
            },
        });
    }

    // return try self.call();
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

    if (self.match(&.{.FALSE})) {
        return try self.create_expr(.{
            .Literal = .{
                .value = .{ .Boolean = false },
            },
        });
    }

    if (self.match(&.{.TRUE})) {
        return try self.create_expr(.{
            .Literal = .{
                .value = .{ .Boolean = true },
            },
        });
    }

    if (self.match(&.{.NUMBER})) {
        return try self.create_expr(.{
            .Literal = .{
                .value = .{ .Float = self.previous().type.NUMBER },
            },
        });
    }

    if (self.match(&.{.STRING})) {
        return try self.create_expr(.{
            .Literal = .{
                .value = .{
                    .String = self.previous().type.STRING,
                },
            },
        });
    }

    if (self.match(&.{.LEFT_PAREN})) {
        var expr = try self.expression();
        _ = try self.consume(
            .RIGHT_PAREN,
            ParserError.MissingClosingParen,
            "Expect ) after expression",
        );

        return try self.create_expr(.{
            .Grouping = .{
                .expr = expr,
            },
        });
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
