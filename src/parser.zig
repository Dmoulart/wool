const std = @import("std");
const Token = @import("./token.zig");

const Self = @This();

const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
const Err = ErrorReporter(ParserError);
const ParserError = error{};

allocator: std.mem.Allocator,

pub fn init(tokens: []Token, allocator: std.mem.Allocator) Self {
    return Self{
        .tokens = tokens,
        .allocator = allocator,
        // .stmts = std.ArrayList(*Stmt).init(allocator),
    };
}

pub fn parse(self: *Self) ParserError![]*Stmt {
    while (!self.is_at_end()) {
        var maybe_stmt = try self.declaration();
        if (maybe_stmt) |stmt| {
            try self.stmts.append(stmt);
        }
    }

    return self.stmts.toOwnedSlice();
}

fn check(self: *Self, token_type: Token.Types) bool {
    if (self.is_at_end()) return false;
    // if (token_type == @as(Token.Types, self.peek().type)) {
    //     std.debug.print("\nmatch : peek {any} tok {any}", .{ self.peek().type, token_type });
    // }
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
