const std = @import("std");
const report = @import("./error-reporter.zig").report;
const Token = @import("./token.zig");
const ArrayList = @import("std").ArrayList;
const ComptimeStringMap = @import("std").ComptimeStringMap;
const isAlphanumeric = std.ascii.isAlphanumeric;
const isDigit = std.ascii.isDigit;

const Self = @This();

const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
const Err = ErrorReporter(LexerError);
const LexerError = error{
    UnexpectedCharacter,
    UnterminatedCommentBlock,
    UnterminatedString,
};

last_error: ?LexerError = null,

src: []const u8,

tokens: ArrayList(Token),

start: u32 = 0,
current: u32 = 0,
line: u32 = 1,

pub fn init(src: []const u8, allocator: std.mem.Allocator) Self {
    return Self{
        .src = src,
        .tokens = ArrayList(Token).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.tokens.deinit();
}

pub fn scan(self: *Self) ![]Token {
    while (!self.is_at_end()) {
        self.start = self.current;
        try self.scan_token();
    }

    try self.tokens.append(Token{
        .type = Token.Type.EOF,
        .lexeme = "",
        .line = self.line,
    });

    return self.tokens.items;
}

fn scan_token(self: *Self) !void {
    const char: u8 = self.advance();

    const maybe_token: ?Token.Type = switch (char) {
        '(' => .LEFT_PAREN,
        ')' => .RIGHT_PAREN,
        '{' => .LEFT_BRACE,
        '}' => .RIGHT_BRACE,
        ',' => .COMMA,
        '.' => .DOT,
        '+' => .PLUS,
        ';' => .SEMICOLON,
        '*' => .STAR,
        '-' => if (self.match('>')) .MINUS_ARROW else .MINUS,
        ':' => if (self.match(':')) .COLON_COLON else if (self.match('=')) .COLON_EQUAL else .COLON,
        '/' => blk: {
            if (self.match('/')) {
                while (self.peek() != '\n' and !self.is_at_end()) {
                    _ = self.advance();
                }
                break :blk null;
            } else if (self.match('*')) {
                self.read_comment_block();
                break :blk null;
            } else {
                break :blk .SLASH;
            }
        },
        '!' => if (self.match('=')) .BANG_EQUAL else .BANG,
        '=' => if (self.match('=')) .EQUAL_EQUAL else .EQUAL,
        '<' => if (self.match('=')) .LESS_EQUAL else .LESS,
        '>' => if (self.match('=')) .GREATER_EQUAL else .GREATER,
        ' ', '\r', '\t' => null,
        '\n' => blk: {
            self.line += 1;
            break :blk null;
        },
        '"' => self.read_string(),
        '0'...'9' => try self.read_number(),
        'a'...'z', 'A'...'Z', '_' => self.read_identifier(),
        else => blk: {
            self.err(LexerError.UnexpectedCharacter);
            break :blk null;
        },
    };

    if (maybe_token) |token| {
        try self.add_token(token);
    }
}

fn add_token(self: *Self, token_type: Token.Type) !void {
    var text = self.src[self.start..self.current];

    try self.tokens.append(.{
        .type = token_type,
        .lexeme = text,
        .line = self.line,
    });
}

fn match(self: *Self, expected: u8) bool {
    if (self.is_at_end() or self.src[self.current] != expected) return false;

    self.current += 1;
    return true;
}

fn advance(self: *Self) u8 {
    self.current += 1;
    return self.src[self.current - 1];
}

fn peek(self: *Self) u8 {
    if (self.is_at_end()) return 0;
    return self.src[self.current];
}

fn peekNext(self: *Self) u8 {
    if (self.current + 1 >= self.src.len) return 0;
    return self.src[self.current + 1];
}

fn read_comment_block(self: *Self) void {
    while (!self.is_at_end()) {
        // nested comment block
        if (self.peek() == '/' and self.peekNext() == '*') {
            _ = self.advance();
            _ = self.advance();

            self.read_comment_block();
        } else if (self.peek() == '*' and self.peekNext() == '/') {
            _ = self.advance();
            _ = self.advance();

            return;
        }
        if (self.peek() == '\n') {
            self.line += 1;
        }

        if (self.current + 1 <= self.src.len) {
            _ = self.advance();
        }
    }

    self.err(LexerError.UnterminatedCommentBlock);
}

fn read_string(self: *Self) ?Token.Type {
    while (self.peek() != '"' and !self.is_at_end()) {
        if (self.peek() == '\n') {
            self.line += 1;
        }
        _ = self.advance();
    }

    if (self.is_at_end()) {
        self.err(LexerError.UnterminatedString);
        return null;
    }

    _ = self.advance();

    const value = self.src[(self.start + 1)..(self.current - 1)];

    return Token.Type{
        .STRING = value,
    };
}

fn read_number(self: *Self) !Token.Type {
    while (isDigit(self.peek())) _ = self.advance();

    // Look for a fractional part
    if (self.peek() == '.' and isDigit(self.peekNext())) {
        // Consume the "."
        _ = self.advance();

        while (isDigit(self.peek())) _ = self.advance();
    }

    return Token.Type{
        .NUMBER = try std.fmt.parseFloat(f64, self.src[self.start..self.current]),
    };
}

fn read_identifier(self: *Self) Token.Type {
    while (isAlphanumeric(self.peek())) _ = self.advance();

    const text = self.src[self.start..self.current];

    return if (Token.keyword(text)) |keyword_type| keyword_type else Token.Type{
        .IDENTIFIER = text,
    };
}

fn is_at_end(self: *Self) bool {
    return self.current >= self.src.len;
}

fn expect_token_sequence(comptime expected: []const Token.Types, comptime src: []const u8) !void {
    var lexer = init(src, std.testing.allocator);
    var tokens = try lexer.scan();
    defer lexer.deinit();
    return expect_token_sequence_from_tokens(expected, tokens);
}

fn expect_token_sequence_from_tokens(comptime expected: []const Token.Types, tokens: []Token) !void {
    if (expected.len != tokens.len) return error.TestUnexpectedResult;

    return for (expected, tokens) |expected_token, actual_token| {
        if (@as(Token.Types, actual_token.type) != expected_token) {
            break error.TestUnexpectedResult;
        }
    };
}

fn err(self: *Self, comptime lexer_error: LexerError) void {
    Err.print(self.line, self.src[self.start..self.current], @errorName(lexer_error));
    self.last_error = lexer_error;
}

const expect = std.testing.expect;

test "can scan simple code" {
    try expect_token_sequence(
        &.{ .IDENTIFIER, .EQUAL, .STRING, .SEMICOLON, .EOF },
        "ok = \"test\";",
    );
}

test "can skip single line comments" {
    try expect_token_sequence(
        &.{ .IDENTIFIER, .EQUAL, .STRING, .SEMICOLON, .EOF },
        "// Hey I'm Commenty McCommentFace \n ok =\"test\";",
    );
}

test "can skip nested multi line comments" {
    try expect_token_sequence(
        &.{ .IDENTIFIER, .EQUAL, .STRING, .SEMICOLON, .EOF },
        "/* Hey I'm Commenty /* nested comment */ McCommentFace */ \n ok =\"test\";",
    );
}
