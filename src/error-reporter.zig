const std = @import("std");
const fmt = std.fmt;
const Token = @import("token.zig");
const Expr = @import("./ast/expr.zig").Expr;
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

const ERROR_MSG_CONTEXT_SIZE: u32 = 15;
pub fn Errors(comptime E: type) type {
    return struct {
        allocator: std.mem.Allocator,
        src: []const u8,

        pub fn init(allocator: std.mem.Allocator, src: []const u8) Errors(E) {
            return .{
                .allocator = allocator,
                .src = src,
            };
        }

        pub fn fatal(self: *Errors(E), err: E, args: anytype) @TypeOf(err) {
            const stderr = io.getStdErr().writer();

            const msg = std.fmt.allocPrint(self.allocator, get_error_message(err), args) catch |print_error| {
                std.debug.print("\nError reporter cannot report error context : {s}\n", .{@errorName(print_error)});
                return err;
            };

            // how to handle these kind of errors ?
            _ = stderr.write(msg) catch unreachable;

            return err;
        }

        fn get_error_message(err: E) []const u8 {
            return switch (@TypeOf(err)) {
                InferError => switch (err) {
                    InferError.FunctionArgumentsCanOnlyBeIdentifiers => "Function arguments can only be identifiers",
                    InferError.UnknownVariable => "Unknown variable",
                    InferError.TypeMismatch => "Type mismatch.",
                    InferError.CircularReference => "Circular reference.",
                    InferError.CannotResolveType => "Cannot resolve type.",
                    InferError.AllocError => "Allocation error.",
                    InferError.AlreadyDefinedFunction => "Function has already been defined.",
                    InferError.AlreadyDefinedVariable => "Identifier {s} has already been defined.",
                    InferError.UnknownError => "Unknown error.",
                    else => "Error message not found",
                },
                else => "No found error message",
            };
        }
    };
}
