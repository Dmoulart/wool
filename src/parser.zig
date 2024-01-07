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
    MissingArrowInFunctionExpression,
    TooMuchArguments,
    MissingParameterName,
    MissingClosingBrace,
    MissingConstInitializer,
    MissingClosingParenAfterArguments,
    MissingImport,
    MissingFunctionType,
};

allocator: std.mem.Allocator,

exprs: ArrayList(Expr),

current: u32 = 0,

tokens: []Token,

//function as key args as values
// This will be used by compiler, but maybe this is wrong ? should parser produce an ast + a list of args ?
args: std.AutoArrayHashMap(*const Expr, []*const Expr),

pub fn init(tokens: []Token, allocator: std.mem.Allocator) Self {
    return Self{
        .tokens = tokens,
        .allocator = allocator,
        .exprs = std.ArrayList(Expr).init(allocator),
        .args = std.AutoArrayHashMap(*const Expr, []*const Expr).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.exprs.deinit();

    for (self.args.values()) |args| {
        self.allocator.free(args);
    }

    self.args.deinit();
}

pub fn parse(self: *Self) ParserError![]*Expr {
    var exprs = std.ArrayList(*Expr).init(self.allocator);

    while (!self.is_at_end()) {
        var maybe_decl = try self.declaration_expression();
        if (maybe_decl) |decl| {
            exprs.append(decl) catch return ParserError.OutOfMemory;
        }
    }

    return exprs.toOwnedSlice() catch ParserError.OutOfMemory;
}

fn declaration_expression(self: *Self) ParserError!?*Expr {
    if (self.match(&.{.RETURN})) {
        return try self.return_expr();
    }

    // if (self.match(&.{.LEFT_BRACE})) {
    //     return try self.create_expr(.{
    //         .Block = .{
    //             .exprs = try self.block(),
    //         },
    //     });
    // }

    return try self.expression_stmt();
}

fn return_expr(self: *Self) ParserError!*Expr {
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

    return try self.create_expr(.{
        .Return = .{
            .keyword = keyword,
            .value = value,
        },
    });
}

fn expression_stmt(self: *Self) ParserError!?*Expr {
    const expr = try self.expression();

    // _ = try self.consume(
    //     .SEMICOLON,
    //     ParserError.MissingSemiColonAfterValue,
    //     "Expect ';' after value.",
    // );

    return try self.create_expr(expr.*);
}

fn expression(self: *Self) ParserError!*Expr {
    if (self.match(&.{.LEFT_BRACE})) {
        return try self.create_expr(.{
            .Block = .{
                .exprs = try self.block(),
            },
        });
    }

    if (self.match(&.{.IF})) {
        return try self.if_expr();
    }

    if (self.match(&.{.WHILE})) {
        return try self.while_expr();
    }

    if (self.match(&.{.LOOP})) {
        return try self.loop();
    }

    if (self.match(&.{.BREAK})) {
        return try self.break_expr();
    }

    if (self.match(&.{.CONTINUE})) {
        return try self.continue_expr();
    }

    if (self.match(&.{.FROM})) {
        return try self.from();
    }

    return try self.function();
}

fn block(self: *Self) ParserError![]*Expr {
    var exprs = std.ArrayList(*Expr).init(self.allocator);

    while (!self.check(.RIGHT_BRACE) and !self.is_at_end()) {
        if (self.declaration_expression()) |maybe_decl| {
            if (maybe_decl) |decl| {
                exprs.append(decl) catch |decl_err| switch (decl_err) {
                    error.OutOfMemory => return ParserError.OutOfMemory,
                };
            }
        } else |decl_err| {
            return decl_err;
        }
    }

    _ = try self.consume(
        .RIGHT_BRACE,
        ParserError.MissingClosingBrace,
        "Expect '}' after block.",
    );

    return exprs.toOwnedSlice();
}

fn while_expr(self: *Self) ParserError!*Expr {
    const condition = try self.expression();
    const body = try self.expression();

    return try self.create_expr(.{ .While = .{
        .condition = condition,
        .body = body,
        .inc = null,
    } });
}

fn if_expr(self: *Self) ParserError!*Expr {
    const condition = try self.expression();
    const then_branch = try self.expression();
    const else_branch = if (self.match(&.{.ELSE})) try self.expression() else null;

    return try self.create_expr(.{ .If = .{
        .condition = condition,
        .then_branch = then_branch,
        .else_branch = else_branch,
    } });
}

fn loop(self: *Self) ParserError!*Expr {
    return try self.create_expr(.{
        .Loop = .{
            .body = try self.expression(),
        },
    });
}

fn break_expr(self: *Self) ParserError!*Expr {
    return try self.create_expr(.{
        .Break = .{
            .value = null,
        },
    });
}

fn continue_expr(self: *Self) ParserError!*Expr {
    return try self.create_expr(.{
        .Continue = .{},
    });
}

fn from(self: *Self) ParserError!*Expr {
    const namespace = try self.consume(
        .IDENTIFIER,
        ParserError.MissingImport,
        "Expect namespace.",
    );

    _ = try self.consume(
        .IMPORT,
        ParserError.MissingImport,
        "Expect import after namespace.",
    );

    const member = try self.consume(
        .IDENTIFIER,
        ParserError.MissingImport,
        "Expect member",
    );

    return try self.create_expr(.{
        .Import = .{
            .namespace = namespace,
            .member = member,
        },
    });
}

fn function(self: *Self) ParserError!*Expr {
    const current = self.peek().type;
    const is_function = current == .MINUS_ARROW or
        (current == .IDENTIFIER and (self.check_next(0, .MINUS_ARROW) or self.check_next(0, .COMMA)));

    if (is_function) {
        const args_declaration = current == .IDENTIFIER;
        var args: ?std.ArrayList(*Token) = null;

        if (args_declaration) {
            args = std.ArrayList(*Token).init(self.allocator);
            errdefer args.?.deinit();

            const first_identifier = try self.consume(
                .IDENTIFIER,
                ParserError.MissingParameterName,
                "Expect parameter name.",
            );

            args.?.append(first_identifier) catch return ParserError.OutOfMemory;

            while (self.match(&.{.COMMA})) {
                // @todo: do while would have been great in this case
                if (args.?.items.len >= 255) {
                    return Err.raise(
                        self.peek(),
                        ParserError.TooMuchArguments,
                        "Functions cannot have more than 255 arguments",
                    );
                }

                const identifier = try self.consume(
                    .IDENTIFIER,
                    ParserError.MissingParameterName,
                    "Expect parameter name.",
                );

                args.?.append(identifier) catch return ParserError.OutOfMemory;
            }
        }
        var peek_token = self.peek();
        if (peek_token.type == .MINUS_ARROW) {
            _ = try self.consume(
                .MINUS_ARROW,
                ParserError.MissingArrowInFunctionExpression,
                "Expect -> before declaring function body.",
            );

            const func_type = try self.consume(
                .IDENTIFIER,
                ParserError.MissingFunctionType,
                "Expect type after '->'.",
            );

            const maybe_last_expr = self.last_expr();

            var name: ?*const Token = null;

            if (maybe_last_expr) |last_expression| {
                name = switch (last_expression.*) {
                    .ConstInit => |*const_intialization| const_intialization.name,
                    .VarInit => |var_initialization| var_initialization.name,
                    .Assign => |assignation| assignation.name,
                    .Variable => |variable| variable.name,
                    else => null,
                };
            }

            const body = try self.expression();

            return try self.create_expr(
                .{
                    .Function = .{
                        .args = if (args) |*non_empty_args|
                            non_empty_args.toOwnedSlice() catch return ParserError.OutOfMemory
                        else
                            null,
                        .body = body,
                        .name = name,
                        .type = func_type,
                    },
                },
            );
        }
        // if args were declared but no ->
        else if (args_declaration) {
            _ = try self.consume(
                .MINUS_ARROW,
                ParserError.MissingArrowInFunctionExpression,
                "Expect -> before declaring function body.",
            );
        }
    }

    return try self.const_init();
}

fn const_init(self: *Self) ParserError!*Expr {
    var expr = try self.var_init();
    // must be before declared_type check !
    const implicit_type = self.match(&.{.COLON_COLON});

    const declared_type = self.match(&.{.COLON}) and self.check_next(0, .IDENTIFIER) and self.check_next(1, .COLON);

    const is_const = declared_type or implicit_type;

    if (is_const) {
        const props: struct {
            equals: *const Token,
            value: *const Expr,
            type: ?*const Token,
        } = if (implicit_type) blk: {
            break :blk .{
                .equals = self.previous(),
                .value = try self.expression(),
                .type = @as(?*const Token, null),
            };
        } else blk: {
            const equals = self.previous();

            const @"type" = try self.consume(
                .IDENTIFIER,
                ParserError.MissingFunctionType,
                "Missing type",
            );

            // eat semicolon
            _ = self.advance();

            const value = try self.expression();

            break :blk .{
                .equals = equals,
                .type = @"type",
                .value = value,
            };
        };
        
        return switch (expr.*) {
            .Variable => |*var_expr| {
                var name = var_expr.name;
                return try self.create_expr(.{
                    .ConstInit = .{
                        .name = name,
                        .initializer = props.value,
                        .type = props.type,
                    },
                });
            },
            else => Err.raise(
                props.equals,
                ParserError.InvalidAssignmentTarget,
                "Invalid assignment target.",
            ),
        };
    }

    return expr;
}

fn var_init(self: *Self) ParserError!*Expr {
    var expr = try self.operation_assigment();

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
                        .type = null,
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

fn operation_assigment(self: *Self) ParserError!*Expr {
    var expr = try self.assignment();

    if (self.match(&.{ .PLUS_EQUAL, .MINUS_EQUAL, .STAR_EQUAL, .SLASH_EQUAL })) {
        const op = self.previous();
        const value = try self.expression();

        return switch (expr.*) {
            .Variable => |*var_expr| {
                var name = var_expr.name;
                return try self.create_expr(.{
                    .OperationAssign = .{
                        .name = name,
                        .op = op,
                        .value = value,
                    },
                });
            },
            else => Err.raise(
                op,
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
                .right = (try self.primary()), // term was before but this would not work : 2 > -2 + 13; (interpreted as 2 > -(2 + 13))
            },
        });
    }

    return try self.call();
}

fn call(self: *Self) ParserError!*Expr {
    var expr = try self.primary();

    while (true) {
        if (self.match(&.{.LEFT_PAREN})) {
            expr = try self.finish_call(expr);
        } else {
            break;
        }
    }

    return expr;
}

fn finish_call(self: *Self, callee: *const Expr) ParserError!*Expr {
    var args = std.ArrayList(*const Expr).init(self.allocator);
    errdefer args.deinit();

    if (!self.check(.RIGHT_PAREN)) {
        const first_expr = try self.expression();

        try args.append(first_expr);

        while (self.match(&.{.COMMA})) {
            const expr = try self.expression();

            try args.append(expr);
        }
    }

    const paren = try self.consume(
        .RIGHT_PAREN,
        ParserError.MissingClosingParenAfterArguments,
        "Expect ')' after arguments.",
    );

    if (args.items.len > 255) {
        return Err.raise(
            self.peek(),
            ParserError.TooMuchArguments,
            "Functions cannot have more than 255 arguments",
        );
    }

    try self.args.put(callee, try args.toOwnedSlice());

    return try self.create_expr(
        .{
            .Call = .{
                .callee = callee,
                .paren = paren,
                .args = self.args.get(callee).?,
            },
        },
    );
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
                .value = .{ .Number = self.previous().type.NUMBER },
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

fn check_next(self: *Self, step: u32, comptime token_type: Token.Types) bool {
    if (self.is_at_end()) return false;

    var curr_step: u32 = 0;

    var current_token: *Token = &self.tokens[self.current];

    while (curr_step <= step) : (curr_step += 1) {
        current_token = &self.tokens[self.current + curr_step];

        if (current_token.type == .EOF) return false;
    }

    return current_token.type == token_type;
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

fn last_expr(self: *Self) ?*const Expr {
    return if (self.exprs.getLastOrNull()) |*expr| expr else null;
}
