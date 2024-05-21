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

const ERROR_MSG_CONTEXT_SIZE: u32 = 20;
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

        pub fn fatal(self: *Errors(E), token: *const Token, err: E) @TypeOf(err) {
            const stderr = io.getStdErr().writer();

            const msg = std.fmt.allocPrint(self.allocator, "[Line {any}] : {s} At '{s}'.\n", .{
                token.line,
                get_error_message(err),
                token.lexeme,
            }) catch |print_error| {
                std.debug.print("\nError reporter cannot report error context : {s}\n", .{@errorName(print_error)});
                return err;
            };

            // how to handle these kind of errors ?
            _ = stderr.write(msg) catch unreachable;

            const context_start_at = if (token.start >= ERROR_MSG_CONTEXT_SIZE)
                token.start - ERROR_MSG_CONTEXT_SIZE
            else
                0;

            const context_end_at = if (token.end + ERROR_MSG_CONTEXT_SIZE <= self.src.len - 1)
                token.end + ERROR_MSG_CONTEXT_SIZE
            else
                self.src.len - 1;

            const context = self.src[context_start_at..context_end_at];
            // std.debug.print("context {any} {any}", .{ context_start_at, context_end_at });
            _ = stderr.write(context) catch unreachable;

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
                    InferError.AlreadyDefinedVariable => "Identifier has already been defined.",
                    InferError.UnknownError => "Unknown error.",
                    else => "Error message not found",
                },
                else => "No found error message",
            };
        }
    };
}
