const std = @import("std");
const fmt = std.fmt;

const File = @import("file.zig");

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
        file: *const File,

        pub fn init(allocator: std.mem.Allocator, file: *const File) Errors(E) {
            return .{
                .allocator = allocator,
                .file = file,
            };
        }

        fn ErrorPayload(comptime err: E) type {
            return switch (E) {
                InferError => {
                    return switch (err) {
                        InferError.AlreadyDefinedIdentifier => struct {
                            []const u8, // constant or variable
                            []const u8, // identifier string
                        },
                        InferError.UnknownVariable => struct { []const u8 },
                        else => void,
                    };
                },
                else => @compileError("Error payload not implemented"),
            };
        }

        fn ErrorContext(comptime err: E) type {
            return switch (E) {
                InferError => {
                    return switch (err) {
                        InferError.AlreadyDefinedIdentifier => struct {
                            []const u8,
                        },
                        InferError.UnknownVariable => struct {
                            []const u8,
                        },
                        else => void,
                    };
                },
                else => @compileError("Error context not implemented"),
            };
        }

        fn ErrorData(comptime err: E) type {
            return struct {
                line: u32,
                column: u32,
                msg: ErrorPayload(err),
                context: ErrorContext(err),
            };
        }

        pub fn fatal(self: *Errors(E), comptime err: E, data: ErrorData(err)) E {
            const template_error_location = "{s}:{d}:{d}"; // file-path:line:column

            const template_error_msg = " error: {s}\n";

            const template_error_line = "{s}";

            const template_error_cursor = "{s}";

            const template = template_error_location ++ template_error_msg ++ template_error_line ++ template_error_cursor ++ "\n";

            const line_text = self.file.get_line(data.line);

            const err_cursor_column = self.file.get_line_offset(data.line, data.column);

            const err_cursor = self.allocator.alloc(u8, err_cursor_column + 1) catch unreachable;
            defer self.allocator.free(err_cursor);
            for (err_cursor) |*char| {
                char.* = ' ';
            }
            err_cursor[err_cursor_column] = '^';

            const msg = std.fmt.allocPrint(self.allocator, get_error_message(err), data.msg) catch
                unreachable;

            const error_msg = std.fmt.allocPrint(self.allocator, template, .{
                self.file.path orelse "repl:",
                data.line,
                data.column,
                msg,
                line_text,
                err_cursor,
            }) catch |print_error| {
                std.debug.print("\nError reporter cannot report error context : {s}\n", .{@errorName(print_error)});
                return err;
            };

            const stderr = io.getStdErr().writer();
            // how to handle these kind of errors ?
            _ = stderr.write(error_msg) catch unreachable;

            return err;
        }

        fn get_error_message(comptime err: E) []const u8 {
            return switch (@TypeOf(err)) {
                InferError => switch (err) {
                    InferError.FunctionArgumentsCanOnlyBeIdentifiers => "Function arguments can only be identifiers",
                    InferError.UnknownVariable => "Unknown variable {s}",
                    InferError.TypeMismatch => "Type mismatch.",
                    InferError.CircularReference => "Circular reference.",
                    InferError.CannotResolveType => "Cannot resolve type.",
                    InferError.AllocError => "Allocation error.",
                    InferError.AlreadyDefinedFunction => "Function has already been defined.",
                    InferError.AlreadyDefinedIdentifier => "{s} '{s}' has already been defined.",
                    InferError.UnknownError => "Unknown error.",
                    else => "Error message not found",
                },
                else => "No found error message",
            };
        }

        fn get_error_context(comptime err: E) []const u8 {
            return switch (@TypeOf(err)) {
                InferError => switch (err) {
                    InferError.AlreadyDefinedIdentifier => "identifier re-declared here:\n{s} \n",
                    else => "Error message not found",
                },
                else => "No found error message",
            };
        }
    };
}
