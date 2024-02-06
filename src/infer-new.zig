allocator: std.mem.Allocator,

ast: []*const Expr,

type_nodes: std.ArrayListUnmanaged(TypeNode),

sems: std.AutoArrayHashMapUnmanaged(*const Expr, *TypeNode),

global_context: *Context,

contexts: std.ArrayListUnmanaged(Context),

pub const TypeID = enum {
    any,
    number,
    float,
    int,
    bool,
    string,
};

const NodeID = usize;
const VarID = usize;

const MonoType = struct { tid: TypeID };
const VarType = struct {
    name: []const u8,
    ref: *TypeNode,
};
pub const TypeNode = union(enum) {
    type: MonoType,
    variable: VarType,

    pub fn set_tid(self: *TypeNode, tid: TypeID) void {
        switch (self.*) {
            .type => {
                self.type.tid = tid;
            },
            .variable => {
                self.variable.ref.type.tid = tid;
            },
        }
    }

    pub fn get_tid(self: TypeNode) TypeID {
        return switch (self) {
            .type => |*monotype| {
                return monotype.tid;
            },
            .variable => |*variable| {
                return variable.ref.type.tid;
            },
        };
    }

    pub fn clone(self: TypeNode) TypeNode {
        return switch (self) {
            .type => |*monotype| {
                return .{ .type = .{ .tid = monotype.tid } };
            },
            .variable => |*variable| {
                return .{ .variable = .{ .name = variable.name, .ref = variable.ref } };
            },
        };
    }

    pub fn get_var_id(self: TypeNode) ?VarType {
        return if (tag(self) == .variable) self.variable else null;
    }
};

pub const FunType = struct {
    name: []const u8,
    args: []TypeNode,
    return_type: *TypeNode,
};

const TypeError = error{
    TypeMismatch,
    UnknwownType,
    UnknownBuiltin,
    WrongArgumentsNumber,
    AllocError,
};

const Err = ErrorReporter(TypeError);

pub fn init(allocator: std.mem.Allocator, ast: []*Expr) @This() {
    var contexts: std.ArrayListUnmanaged(Context) = .{};
    const global_context = contexts.addOne(allocator) catch unreachable;
    return .{
        .allocator = allocator,
        .ast = ast,
        .type_nodes = .{},
        .contexts = contexts,
        .global_context = global_context,
        .sems = .{},
    };
}

pub fn infer_program(self: *@This()) anyerror!*std.AutoArrayHashMapUnmanaged(*const Expr, *TypeNode) {
    for (self.ast) |expr| {
        _ = try self.infer(expr);
    }

    try self.log_sems();

    return &self.sems;
}

pub fn infer(self: *@This(), expr: *const Expr) !*TypeNode {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            var node = try self.infer(const_init.initializer);

            var type_decl = try self.create_type_node(
                .{
                    .type = .{
                        .tid = if (const_init.type) |ty|
                            try type_from_str(ty.lexeme)
                        else
                            .any,
                    },
                },
            );

            try unify(type_decl, node);

            try self.sems.put(self.allocator, expr, node);

            return node;
        },
        .Grouping => |*grouping| {
            var node = try self.infer(grouping.expr);

            try self.sems.put(self.allocator, expr, node);

            return node;
        },
        .Binary => |*binary| {
            //@todo:mem clean memory
            var args = try self.allocator.alloc(*const Expr, 2);
            args[0] = binary.left;
            args[1] = binary.right;
            defer {
                self.allocator.free(args);
            }

            const builtin_name = try switch (binary.op.type) {
                .PLUS => "+",
                .MINUS => "-",
                .SLASH => "/",
                .STAR => "*",
                .GREATER => ">",
                .GREATER_EQUAL => ">=",
                .LESS => "<",
                .LESS_EQUAL => "<=",
                .EQUAL_EQUAL => "==",
                .BANG_EQUAL => "!=",
                else => TypeError.UnknownBuiltin,
            };

            const builtin = builtins_types.get(builtin_name).?;

            const node = try self.call(
                builtin,
                args,
                expr,
            );

            try self.sems.put(self.allocator, expr, node);

            return node;
        },
        .Literal => |*literal| {
            var node = try self.create_type_node(
                .{
                    .type = .{
                        .tid = type_of(literal.value),
                    },
                },
            );
            try self.sems.put(self.allocator, expr, node);
            return node;
        },
        else => unreachable,
    };
}

fn call(self: *@This(), function: FunType, exprs_args: []*const Expr, expr: *const Expr) anyerror!*TypeNode {
    if (function.args.len != exprs_args.len) {
        return TypeError.WrongArgumentsNumber;
    }

    if (std.mem.eql(u8, function.name, "=")) {
        pretty_print(expr);
    }

    const local_ctx = try self.contexts.addOne(self.allocator);
    local_ctx.* = Context.init(self.allocator);
    defer local_ctx.deinit();

    for (exprs_args, function.args) |expr_arg, *function_arg| {
        const arg = try self.infer(expr_arg);

        const call_arg = try self.get_or_create_local_node(
            function_arg,
            arg,
            local_ctx,
        );

        try unify(
            call_arg,
            arg,
        );

        // Variable binding !
        if (tag(call_arg.*) == .variable and tag(arg.*) == .variable) {
            if (std.mem.eql(u8, function_arg.variable.name, call_arg.variable.name)) {
                arg.variable.ref = call_arg.variable.ref;
            }
        }

        try self.sems.put(
            self.allocator,
            expr_arg,
            call_arg,
        );
    }

    const node = try self.get_local_node(function.return_type, local_ctx);

    try self.sems.put(self.allocator, expr, node);

    return node;
}

fn get_or_create_local_node(self: *@This(), base_node: *TypeNode, local_node: *TypeNode, ctx: *Context) !*TypeNode {
    if (base_node.get_var_id()) |variable| {
        if (ctx.variables.get(variable.name)) |registered_variable| {
            return registered_variable;
        } else {
            const new_var = switch (local_node.*) {
                .variable => local_node,
                .type => try self.create_type_node(
                    .{
                        .variable = .{
                            .name = base_node.variable.name,
                            .ref = local_node,
                        },
                    },
                ),
            };

            return try ctx.create_var_instance(new_var);
        }
    }
    return try self.create_type_node(base_node.*);
}

fn get_local_node(self: *@This(), node: *TypeNode, ctx: *Context) !*TypeNode {
    if (node.get_var_id()) |variable| {
        if (ctx.variables.get(variable.name)) |registered_variable| {
            return registered_variable;
        }
    }
    return try self.create_type_node(node.*);
}

fn unify(a: *TypeNode, b: *TypeNode) !void {
    if (is_narrower(a.get_tid(), b.get_tid())) {
        a.set_tid(b.get_tid());
    } else if (is_narrower(b.get_tid(), a.get_tid())) {
        b.set_tid(a.get_tid());
    }

    if (!types_intersects(a.get_tid(), b.get_tid())) {
        std.debug.print(
            "\nType Mismatch --\nExpected : {}\nFound : {}\n",
            .{ a.get_tid(), b.get_tid() },
        );
        return TypeError.TypeMismatch;
    }
}

fn is_narrower(tid: TypeID, maybe_narrower: TypeID) bool {
    return is_subtype(tid, maybe_narrower);
}

fn is_subtype(supertype: TypeID, maybe_subtype: TypeID) bool {
    if (find_subtypes(supertype, &type_hierarchy)) |subtypes| {
        if (subtypes.get(maybe_subtype)) |_| {
            return true;
        }

        for (subtypes.values) |subtype| {
            if (subtype) |sub| {
                const subtype_tid_2 = switch (sub.*) {
                    .terminal => |ty| ty.tid,
                    .supertype => |ty| ty.tid,
                };
                if (is_subtype(subtype_tid_2, maybe_subtype)) {
                    return true;
                }
            }
        }
    }
    return false;
}

fn find_subtypes(target: TypeID, hierarchy: *const TypeHierarchy) ?*const Subtypes {
    return switch (hierarchy.*) {
        .terminal => null,
        .supertype => |supertype| {
            if (supertype.tid == target) return &supertype.subtypes;
            for (&supertype.subtypes.values) |m_subtype| {
                if (m_subtype) |subtype| {
                    const subtype_tid = switch (subtype.*) {
                        .terminal => |ty| ty.tid,
                        .supertype => |ty| ty.tid,
                    };

                    if (subtype_tid == target) {
                        return switch (subtype.*) {
                            .terminal => null,
                            .supertype => &subtype.supertype.subtypes,
                        };
                    }

                    if (switch (subtype.*) {
                        .terminal => false,
                        .supertype => true,
                    }) {
                        return find_subtypes(target, subtype);
                    }
                }
            }

            return null;
        },
    };
}

fn types_intersects(a: TypeID, b: TypeID) bool {
    if (a == b) {
        return true;
    }

    const a_is_terminal = find_subtypes(a, &type_hierarchy) == null;
    const b_is_terminal = find_subtypes(b, &type_hierarchy) == null;

    if (!a_is_terminal and is_subtype(a, b)) {
        return true;
    }

    if (!b_is_terminal and is_subtype(b, a)) {
        return true;
    }

    return false;
}

fn create_type_node(self: *@This(), type_node: TypeNode) !*TypeNode {
    var tn_ptr = try self.type_nodes.addOne(self.allocator);
    tn_ptr.* = type_node;
    return tn_ptr;
}

fn type_of(value: Expr.Literal.Value) TypeID {
    return switch (value) {
        .String => .string,
        .Number => |number| if (is_float_value(number)) .float else .number,
        .Boolean => .bool,
        else => unreachable,
    };
}

inline fn is_float_value(number: f64) bool {
    return @rem(number, 1) != 0;
}

pub fn jsonPrint(value: anytype, file_path: []const u8) !void {
    var out = std.ArrayList(u8).init(std.heap.page_allocator);
    defer out.deinit();

    try std.json.stringify(value, .{}, out.writer());

    const file = try std.fs.cwd().createFile(
        file_path,
        .{ .read = true },
    );
    defer file.close();

    _ = try file.writeAll(try out.toOwnedSlice());
}

pub fn type_from_str(str: []const u8) !TypeID {
    // use meta functions for enums
    if (std.mem.eql(u8, str, "i32")) {
        return .int;
    } else if (std.mem.eql(u8, str, "i64")) {
        return .int;
    } else if (std.mem.eql(u8, str, "f32")) {
        return .float;
    } else if (std.mem.eql(u8, str, "f64")) {
        return .float;
    } else if (std.mem.eql(u8, str, "void")) {
        unreachable;
    } else if (std.mem.eql(u8, str, "bool")) {
        return .bool;
    } else {
        return TypeError.UnknwownType;
    }
}
const Env = std.StringArrayHashMap(*TypeNode);

const std = @import("std");

const Expr = @import("./ast/expr.zig").Expr;
const Type = @import("./types.zig").Type;
const Token = @import("./token.zig");
// const Context = @import("./context.zig").Context;
const floatMax = std.math.floatMax;
const maxInt = std.math.maxInt;
const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
const tag = std.meta.activeTag;

fn pretty_print(data: anytype) void {
    switch (@TypeOf(data)) {
        *const Expr => switch (data.*) {
            .Literal => |lit| {
                std.debug.print("\n Lit: {} \n", .{lit.value});
            },
            .Binary => |bin| {
                std.debug.print("\n Bin: {s}\n", .{bin.op.lexeme});
                pretty_print(bin.left);
                pretty_print(bin.right);
                std.debug.print("\n", .{});
            },
            .Grouping => |group| {
                std.debug.print("\n Group: \n", .{});
                pretty_print(group.expr);
                std.debug.print("\n", .{});
            },
            else => unreachable,
        },
        *TypeNode => switch (data.*) {
            .type => |ty| {
                std.debug.print("\n [MonoType]: {} \n", .{ty});
            },
            .variable => |variable| {
                std.debug.print("\n [Variable]: {s}\n", .{variable.name});
                std.debug.print("\n TID: {} \n", .{variable.tid});
                std.debug.print("\n $PTR: {} \n", .{@intFromPtr(data)});

                std.debug.print("\n", .{});
            },
        },
        else => @compileError("Wrong type in pretty print"),
    }
    std.debug.print("\n", .{});
}

var number_node = TypeNode{ .type = .{ .tid = .number } };

var bool_node = TypeNode{ .type = .{ .tid = .bool } };

var any_node = TypeNode{ .type = .{ .tid = .any } };

var add_args = [_]TypeNode{
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
};
var add_return_type: TypeNode = .{ .variable = .{ .ref = &number_node, .name = "T" } };
var sub_args = [_]TypeNode{ .{ .variable = .{ .ref = &number_node, .name = "T" } }, .{ .variable = .{ .ref = &number_node, .name = "T" } } };
var sub_return_type: TypeNode = .{ .variable = .{ .ref = &number_node, .name = "T" } };

var mul_args = [_]TypeNode{
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
};
var mul_return_type: TypeNode = .{ .variable = .{ .ref = &number_node, .name = "T" } };

var div_args = [_]TypeNode{
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
};
var div_return_type: TypeNode = .{ .variable = .{ .ref = &number_node, .name = "T" } };

var equal_equal_args = [_]TypeNode{
    .{ .variable = .{ .ref = &any_node, .name = "T" } },
    .{ .variable = .{ .ref = &any_node, .name = "T" } },
};
var equal_equal_return_type: TypeNode = .{ .variable = .{ .ref = &bool_node, .name = "R" } };

var bang_equal_args = [_]TypeNode{
    .{ .variable = .{ .ref = &any_node, .name = "T" } },
    .{ .variable = .{ .ref = &any_node, .name = "T" } },
};
var bang_equal_return_type: TypeNode = .{ .type = .{
    .tid = .bool,
} };

var greater_args = [_]TypeNode{
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
};
var greater_return_type: TypeNode = .{ .type = .{
    .tid = .bool,
} };

var greater_equal_args = [_]TypeNode{
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
};
var greater_equal_return_type: TypeNode = .{ .type = .{
    .tid = .bool,
} };

var less_args = [_]TypeNode{
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
};
var less_return_type: TypeNode = .{ .type = .{
    .tid = .bool,
} };

var less_equal_args = [_]TypeNode{
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
    .{ .variable = .{ .ref = &number_node, .name = "T" } },
};
var less_equal_return_type: TypeNode = .{ .type = .{
    .tid = .bool,
} };

//   .GREATER => ">",
//                 .GREATER_EQUAL => ">=",
//                 .LESS => "<",
//                 .LESS_EQUAL => "<=",
//                 .EQUAL_EQUAL => "==",
//                 .BANG_EQUAL => "!=",

const builtins_types = std.ComptimeStringMap(
    FunType,
    .{
        .{
            "+", FunType{
                .name = "+",
                .args = &add_args,
                .return_type = &add_return_type,
            },
        },
        .{
            "-", FunType{
                .name = "-",
                .args = &sub_args,
                .return_type = &sub_return_type,
            },
        },
        .{
            "*", FunType{
                .name = "*",
                .args = &mul_args,
                .return_type = &mul_return_type,
            },
        },
        .{
            "/", FunType{
                .name = "/",
                .args = &div_args,
                .return_type = &div_return_type,
            },
        },
        .{
            ">", FunType{
                .name = ">",
                .args = &greater_args,
                .return_type = &greater_return_type,
            },
        },
        .{
            ">=", FunType{
                .name = ">=",
                .args = &greater_equal_args,
                .return_type = &greater_equal_return_type,
            },
        },
        .{
            "<", FunType{
                .name = "<",
                .args = &less_args,
                .return_type = &less_return_type,
            },
        },
        .{
            "<=", FunType{
                .name = "<=",
                .args = &less_equal_args,
                .return_type = &less_equal_return_type,
            },
        },
        .{
            "==", FunType{
                .name = "==",
                .args = &equal_equal_args,
                .return_type = &equal_equal_return_type,
            },
        },
        .{
            "!=", FunType{
                .name = "!=",
                .args = &bang_equal_args,
                .return_type = &bang_equal_return_type,
            },
        },
    },
);

const Subtypes = std.EnumArray(TypeID, ?*const TypeHierarchy);
const TypeHierarchy = union(enum) {
    terminal: struct { tid: TypeID },
    supertype: struct { tid: TypeID, subtypes: Subtypes },

    pub fn get_type_id(self: *TypeHierarchy) TypeID {
        return switch (self.*) {
            .terminal => |terminal| terminal.tid,
            .supertype => |supertype| supertype.tid,
        };
    }

    pub fn get_subtypes(self: *TypeHierarchy) ?*Subtypes {
        return if (tag(self.*) == .supertype) &self.supertype.subtypes else null;
    }
};

const int_type: TypeHierarchy = .{
    .terminal = .{
        .tid = .int,
    },
};
const float_type: TypeHierarchy = .{
    .terminal = .{
        .tid = .float,
    },
};
const bool_type: TypeHierarchy = .{
    .terminal = .{
        .tid = .bool,
    },
};
const string_type: TypeHierarchy = .{
    .terminal = .{
        .tid = .string,
    },
};

var number_type: TypeHierarchy = .{
    .supertype = .{
        .tid = .number,
        .subtypes = blk: {
            var number_subtypes = Subtypes.initFill(null);
            number_subtypes.set(.int, &float_type);
            number_subtypes.set(.float, &int_type);
            break :blk number_subtypes;
        },
    },
};

const any_subtypes: Subtypes = blk: {
    var subtypes = Subtypes.initFill(null);

    subtypes.set(
        .number,
        &number_type,
    );
    subtypes.set(
        .bool,
        &bool_type,
    );
    subtypes.set(
        .string,
        &string_type,
    );

    break :blk subtypes;
};

const type_hierarchy = TypeHierarchy{
    .supertype = .{
        .tid = .any,
        .subtypes = any_subtypes,
    },
};

const Context = struct {
    variables: std.StringHashMapUnmanaged(*TypeNode),

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Context {
        return .{
            .variables = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.variables.deinit(self.allocator);
    }

    pub fn create_var_instance(self: *@This(), node: *TypeNode) !*TypeNode {
        try self.variables.put(self.allocator, node.variable.name, node);
        return node;
    }
};
pub fn log_sems(self: *@This()) !void {
    var types = std.ArrayList(struct {
        type: *TypeNode,
        expr: *const Expr,
        ptr: usize,
    }).init(self.allocator);

    var iter = self.sems.iterator();
    while (iter.next()) |entry| {
        var ptr = switch (entry.value_ptr.*.*) {
            .variable => |variable| variable.ref,
            .type => entry.value_ptr.*,
        };
        try types.append(
            .{ .type = entry.value_ptr.*, .expr = entry.key_ptr.*, .ptr = @intFromPtr(
                ptr,
            ) },
        );
    }

    try jsonPrint(types.items, "./types.json");
}
