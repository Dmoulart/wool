const std = @import("std");
const Token = @import("token.zig");
const InferError = @import("infer.zig").InferError;

const al = std.heap.page_allocator;

pub fn ErrorReporter(comptime ErrorType: type) type {
    return struct {
        pub fn raise(token: *const Token, comptime raised_err: ErrorType, comptime msg: []const u8) @TypeOf(raised_err) {
            if (token.type == .EOF) {
                print(token.line, "at end of file", msg);
            } else {
                const where = std.fmt.allocPrint(al, "at '{s}'", .{token.lexeme}) catch |print_error| {
                    std.debug.print("\nError reporter cannot report error context : {s}\n", .{@errorName(print_error)});
                    return raised_err;
                };
                print(token.line, where, msg);
            }

            return raised_err;
        }

        pub fn print(line: u32, where: []const u8, comptime msg: []const u8) void {
            std.debug.print("\n[line {}] {s} {s} \n", .{ line, msg, where });
        }
    };
}

const io = std.io;

pub const Errors = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Errors {
        return .{
            .allocator = allocator,
        };
    }

    pub fn fatal(self: *Errors, token: *const Token, comptime err: anyerror) @TypeOf(err) {
        const stderr = io.getStdErr().writer();
        // std.fmt
        const msg = std.fmt.allocPrint(self.allocator, "[Line {any}] : " ++ get_error_message(err) ++ "\n", .{
            token.line,
            token.lexeme,
        }) catch |print_error| {
            std.debug.print("\nError reporter cannot report error context : {s}\n", .{@errorName(print_error)});
            return err;
        };
        // how to handle these kind of errors ?
        _ = stderr.write(msg) catch unreachable;

        return err;
    }
};

fn get_error_message(comptime err: anyerror) []const u8 {
    return switch (err) {
        InferError.AlreadyDefinedVariable => "Identifier {s} has already been defined.",
        else => "Error message not found {s}",
    };
}
