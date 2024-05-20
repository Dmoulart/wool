const std = @import("std");
const Token = @import("token.zig");

const allocator = std.heap.page_allocator;

pub fn ErrorReporter(comptime ErrorType: type) type {
    return struct {
        pub fn raise(token: *const Token, comptime raised_err: ErrorType, comptime msg: []const u8) @TypeOf(raised_err) {
            if (token.type == .EOF) {
                print(token.line, "at end of file", msg);
            } else {
                const where = std.fmt.allocPrint(allocator, "at '{s}'", .{token.lexeme}) catch |print_error| {
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
