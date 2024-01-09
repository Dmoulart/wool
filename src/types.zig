const std = @import("std");

pub const c = @cImport({
    @cInclude("binaryen-c.h");
});

pub const TypeError = error{
    UnknwownType,
};

pub const Type = enum {
    i32,
    i64,
    f32,
    f64,
    bool,
    void,
    func,

    pub fn from_str(str: []const u8) !Type {
        // use meta functions for enums
        if (std.mem.eql(u8, str, "i32")) {
            return .i32;
        } else if (std.mem.eql(u8, str, "i64")) {
            return .i64;
        } else if (std.mem.eql(u8, str, "f32")) {
            return .f32;
        } else if (std.mem.eql(u8, str, "f64")) {
            return .f64;
        } else if (std.mem.eql(u8, str, "void")) {
            return .void;
        } else if (std.mem.eql(u8, str, "bool")) {
            return .bool;
        } else {
            return TypeError.UnknwownType;
        }
    }

    pub fn to_binaryen_type(@"type": Type) c.BinaryenType {
        return switch (@"type") {
            .i32, .bool => c.BinaryenTypeInt32(),
            .i64 => c.BinaryenTypeInt64(),
            .f32 => c.BinaryenTypeFloat32(),
            .f64 => c.BinaryenTypeFloat64(),
            .void => c.BinaryenTypeNone(),
            else => unreachable,
        };
    }

    pub fn to_binaryen_int_literal(@"type": Type, value: anytype) c.BinaryenLiteral {
        return switch (@"type") {
            .i32 => c.BinaryenLiteralInt32(@as(i32, value)),
            .i64 => c.BinaryenLiteralInt64(@as(i64, value)),
            else => unreachable,
        };
    }

    pub fn from_binaryen(@"type": c.BinaryenType) Type {
        //  cannot switch : comptime call of extern function
        if (@"type" == c.BinaryenTypeInt32()) {
            return .i32;
        }
        if (@"type" == c.BinaryenTypeInt64()) {
            return .i64;
        }
        if (@"type" == c.BinaryenTypeFloat32()) {
            return .f32;
        }
        if (@"type" == c.BinaryenTypeFloat64()) {
            return .f64;
        }

        unreachable;
    }
    // pub fn to_binaryen_literal(@"type": Type, value: anytype) c.BinaryenLiteral {
    //     return switch (@"type") {
    //         .i32 => c.BinaryenLiteralInt32(value),
    //         .i64 => c.BinaryenLiteralInt64(value),
    //         .f32 => c.BinaryenLiteralFloat32(value),
    //         .f64 => c.BinaryenLiteralFloat64(value),
    //     };
    // }
};
