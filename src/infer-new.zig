allocator: std.mem.Allocator,

ast: []*const Expr,

constraints: std.ArrayListUnmanaged(Constraints),

type_nodes: std.ArrayListUnmanaged(TypeNode),

sems: std.AutoArrayHashMapUnmanaged(*const Expr, *TypeNode),

contexts: std.ArrayListUnmanaged(Context),

ctx: *Context,

const Constraints = std.AutoHashMapUnmanaged(*TypeNode, TypeID);

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

    pub fn as_var(self: TypeNode) ?VarType {
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
    var context = contexts.addOne(allocator) catch unreachable;
    return .{
        .allocator = allocator,
        .ast = ast,
        .type_nodes = .{},
        .constraints = .{},
        .contexts = contexts,
        .ctx = context,
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

pub fn infer(self: *@This(), expr: *const Expr) !*TypeNode {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            var node = try self.infer(const_init.initializer);

            var type_decl = try self.create_type_node(
                TypeNode{
                    .type = .{
                        .tid = if (const_init.type) |ty| try type_from_str(ty.lexeme) else .any,
                    },
                },
            );

            _ = try self.unify(type_decl, node);

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
            var builtin = builtins_types.get(builtin_name).?;
            var node = try self.call(
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
    if (std.mem.eql(u8, function.name, "-")) {
        std.debug.print("hello", .{});
    }

    // pretty_print(expr);

    var local_ctx = try self.contexts.addOne(self.allocator);
    local_ctx.* = Context.init(self.allocator);

    for (exprs_args, function.args, 0..) |expr_arg, *function_arg, i| {
        _ = i;
        // pretty_print(expr_arg);

        var arg = try self.infer(expr_arg);

        // pretty_print(call_arg);

        // var arg_type = try self.create_type_node(function_arg.*);
        var call_arg = try self.get_or_create_local_node(function_arg, arg, local_ctx);

        // pretty_print(expr_arg);

        _ = try self.unify(
            call_arg,
            arg,
        );
        if (tag(call_arg.*) == .variable and tag(arg.*) == .variable) {
            if (std.mem.eql(u8, function_arg.variable.name, call_arg.variable.name)) {
                arg.variable.ref = call_arg.variable.ref;
            }
        }

        // pretty_print(func_arg);

        // try self.sems.put(self.allocator, expr_arg, switch (call_arg.*) {
        //     .variable => |vari| vari.ref,
        //     .type => call_arg,
        // });
        if (tag(call_arg.*) == .variable) {
            pretty_print(expr_arg);
            std.debug.print("\nPUT ptr:{} in ", .{@intFromPtr(call_arg.variable.ref)});
        }
        try self.sems.put(self.allocator, expr_arg, call_arg);
    }

    var node = try self.get_local_node(function.return_type, local_ctx);

    // std.debug.print("\n-- return type {} {}  \n", .{ @intFromPtr(node), node.get_tid() });
    // pretty_print(node);

    try self.sems.put(self.allocator, expr, node);

    // try self.log_sems();

    return node;
}

fn get_or_create_local_node(self: *@This(), node: *TypeNode, or_node: *TypeNode, ctx: *Context) !*TypeNode {
    if (node.as_var()) |variable| {
        if (ctx.variables.get(variable.name)) |registered_variable| {
            return registered_variable;
        } else {
            var new_var = switch (or_node.*) {
                .variable => or_node,
                .type => try self.create_type_node(
                    .{
                        .variable = .{
                            .name = node.variable.name,
                            .ref = or_node,
                        },
                    },
                ),
            };

            return try ctx.create_var_instance(new_var);
        }
    }
    return try self.create_type_node(node.*);
}

fn get_local_node(self: *@This(), node: *TypeNode, ctx: *Context) !*TypeNode {
    if (node.as_var()) |variable| {
        if (ctx.variables.get(variable.name)) |registered_variable| {
            return registered_variable;
        }
    }
    return try self.create_type_node(node.*);
}

fn set_local_node(self: *@This(), node: *TypeNode, ctx: *Context) !void {
    if (node.as_var()) |variable| {
        try ctx.variables.put(self.allocator, variable.name, node);

        // //@todo recursive cloning
        // var ref = try self.create_type_node(variable.ref.*);

        // return try ctx.create_var_instance(try self.create_type_node(
        //     .{
        //         .variable = .{
        //             .name = variable.name,
        //             .ref = ref,
        //         },
        //     },
        // ));
    }
    return;
}

pub fn apply_constraints(
    subst: *Constraints,
    type_node: *TypeNode,
) *TypeNode {
    if (subst.get(type_node)) |tid| {
        type_node.tid = tid;
    }

    return type_node;
}

fn unify(self: *@This(), subject: *TypeNode, with: *TypeNode) !*Constraints {
    // const s = try self._unify(a, b);
    const s = try self.substitute(subject, with);

    if (!types_intersects(subject.get_tid(), with.get_tid())) {
        std.debug.print(
            "\nType Mismatch --\nExpected : {}\nFound : {}\n",
            .{ subject.get_tid(), with.get_tid() },
        );
        return TypeError.TypeMismatch;
    }

    return s;
}

fn substitute(self: *@This(), subject: *TypeNode, with: *TypeNode) !*Constraints {
    // std.debug.print("\n Before Substitution {}: {} with {}: {}\n", .{
    //     @intFromPtr(subject),
    //     subject.get_tid(),
    //     @intFromPtr(with),
    //     with.get_tid(),
    // });

    // const subject_is_var = tag(subject.*) == .variable;
    // const with_is_var = tag(with.*) == .variable;

    if (is_narrower(subject.get_tid(), with.get_tid())) {
        subject.set_tid(with.get_tid());
        // if (subject_is_var) {
        //     subject.variable.ref = if (with_is_var) with.variable.ref else with;
        //     // subject.variable.ref.* = if (with_is_var) with.variable.ref.* else with.*;
        // }
    } else if (is_narrower(with.get_tid(), subject.get_tid())) {
        with.set_tid(subject.get_tid());
        // if (with_is_var) {
        //     with.variable.ref = if (subject_is_var) subject.variable.ref else subject;
        //     // with.variable.ref.* = if (subject_is_var) subject.variable.ref.* else subject.*;
        // }
    }

    // std.debug.print("\n After Substitution {}: {} with {}: {}\n", .{
    //     @intFromPtr(subject),
    //     subject.get_tid(),
    //     @intFromPtr(with),
    //     with.get_tid(),
    // });

    return try self.create_constraint();
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

fn compose_subst(self: *@This(), s1: *Constraints, s2: *Constraints) !*Constraints {
    const result = try self.create_constraint();

    var iter_s2 = s2.iterator();
    while (iter_s2.next()) |record| {
        try result.put(
            self.allocator,
            record.key_ptr.*,
            record.value_ptr.*,
        );
    }

    var iter_s1 = s1.iterator();
    while (iter_s1.next()) |record| {
        try result.put(self.allocator, record.key_ptr.*, record.value_ptr.*);
    }

    return result;
}

fn create_type_node(self: *@This(), type_node: TypeNode) !*TypeNode {
    var tn_ptr = try self.type_nodes.addOne(self.allocator);
    tn_ptr.* = type_node;
    return tn_ptr;
}

fn create_constraint(self: *@This()) !*Constraints {
    var subst = try self.constraints.addOne(self.allocator);
    subst.* = .{};

    return subst;
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
// fn to_type_node(self: *@This(), @"type": Type, expr: *const Expr) !*TypeNode {
//     if (@"type".is_number()) {
//         return try self.create_type_node(TypeNode{ .named = .{ .expr = expr, .id = switch (@"type") {
//             .i32, .i64 => I32_ID,
//             .f32, .f64 => F32_ID,
//             else => unreachable,
//         } } });
//     } else {
//         return try self.create_type_node(TypeNode{ .named = .{ .expr = expr, .id = BOOL_ID } });
//     }
// }

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

fn ComptimeEnumMap(comptime K: type, comptime V: type) type {
    return struct {
        const Self = ComptimeEnumMap(K, V);

        values: [std.meta.fields(K).len]?V,
        indicies: [std.meta.fields(K).len]bool,

        pub fn init() Self {
            var self: Self = undefined;
            @memset(&self.values, null);
            @memset(&self.indicies, false);
            return self;
        }

        pub fn put(self: *Self, comptime key: K, comptime value: V) void {
            self.indicies[@intFromEnum(key)] = true;
            self.values[@intFromEnum(key)] = value;
        }

        pub fn get(self: *Self, comptime key: K) ?V {
            if (self.indicies[@intFromEnum(key)]) {
                return self.values[@intFromEnum(key)];
            }
            return null;
        }
    };
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
    next_var_id: u32 = 0,

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

    // pub fn get_var_instance(self: *@This(), variable: *VarType) !*VarType {
    //     if (self.variables.get(variable.name)) |type_variable| {
    //         return type_variable;
    //     }
    //     try self.variables.put(self.allocator, variable.name, variable);
    //     return variable;
    // }

    pub fn create_var_instance(self: *@This(), node: *TypeNode) !*TypeNode {
        try self.variables.put(self.allocator, node.variable.name, node);
        return node;
    }
};

// fn get_token(expr: *const Expr)*const Token{
//     return switch (expr.*) {
//         .Grouping => |group| get_token(group.expr),
//         .Literal => |lit| lit.
//         else => unreachable
//     };
// }
