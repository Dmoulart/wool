const std = @import("std");
const Self = @This();

type: Type,
lexeme: []const u8,
line: u32,

pub const Types = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    COLON,
    COLON_COLON,
    COLON_EQUAL,
    MINUS_ARROW,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    ELSE,
    FALSE,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    BREAK,
    CONTINUE,
    TRUE,
    WHILE,
    EOF,
};

pub const Type = union(Types) {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    COLON,
    COLON_COLON,
    COLON_EQUAL,
    MINUS_ARROW,

    // Literals.
    IDENTIFIER: []const u8,
    STRING: []const u8,
    NUMBER: f64,

    // Keywords.
    AND,
    ELSE,
    FALSE,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    BREAK,
    CONTINUE,
    TRUE,
    WHILE,
    EOF,
};

const keywords = std.ComptimeStringMap(Type, .{
    .{ "and", .AND },
    .{ "else", .ELSE },
    .{ "false", .FALSE },
    .{ "for", .FOR },
    .{ "break", .BREAK },
    .{ "continue", .CONTINUE },
    .{ "if", .IF },
    .{ "or", .OR },
    .{ "print", .PRINT },
    .{ "return", .RETURN },
    .{ "true", .TRUE },
    .{ "while", .WHILE },
});

pub fn keyword(identifier: []const u8) ?Type {
    return keywords.get(identifier);
}

pub fn debugPrint(self: *Self) void {
    std.debug.print("\n - type: {} | lexeme: {s}\n", .{ self.type, self.lexeme });
}
