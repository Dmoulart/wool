allocator: std.mem.Allocator,

ast: []*const Expr,

constraints: std.ArrayListUnmanaged(Constraints),

type_nodes: std.ArrayListUnmanaged(TypeNode),

sems: std.AutoArrayHashMapUnmanaged(*const Expr, *TypeNode),

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

    pub fn get_tid(self: TypeNode) TypeID {
        return switch (self) {
            .type => |*monotype| {
                return monotype.tid;
            },
            .variable => |*variable| {
                return variable.ref.get_tid();
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
    return .{
        .allocator = allocator,
        .ast = ast,
        .type_nodes = .{},
        .constraints = .{},
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
    var types = std.ArrayList(struct { type: TypeNode, expr: Expr }).init(self.allocator);

    var iter = self.sems.iterator();
    while (iter.next()) |entry| {
        // std.debug.print("\n {any} \n", .{.{ .type = entry.value_ptr.*.*, .expr = entry.key_ptr.*.* }});
        try types.append(.{ .type = entry.value_ptr.*.*, .expr = entry.key_ptr.*.* });
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

            var grouping_node = try self.create_type_node(node.*);
            try self.sems.put(self.allocator, expr, grouping_node);
            return grouping_node;
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

            var node = try self.call(
                builtins_types.get(builtin_name).?,
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
    if (std.mem.eql(u8, function.name, "!=")) {
        std.debug.print("hello", .{});
    }

    for (exprs_args, function.args) |expr_arg, *function_arg| {
        var expr_type = try self.infer(expr_arg);

        var arg_type = if (function_arg.as_var()) |_|
            try self.create_type_node(function_arg.*)
        else
            function_arg;

        _ = try self.unify(expr_type, arg_type);
    }

    var node = try self.create_type_node(function.return_type.*);

    try self.sems.put(self.allocator, expr, node);

    return node;
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

fn get_next_ID(self: *@This()) NodeID {
    self.next_id += 1;
    return self.next_id;
}

fn unify(self: *@This(), a: *TypeNode, b: *TypeNode) !*Constraints {
    // const s = try self._unify(a, b);
    const s = try self.substitute(a, b);

    if (!types_intersects(a.get_tid(), b.get_tid())) {
        std.debug.print(
            "\nType Mismatch --\nExpected : {}\nFound : {}\n",
            .{ a.get_tid(), b.get_tid() },
        );
        return TypeError.TypeMismatch;
    }

    return s;
}

fn substitute(self: *@This(), a: *TypeNode, b: *TypeNode) !*Constraints {
    const tid_a = a.get_tid();
    const a_is_variable = tag(a.*) == .variable;

    const tid_b = b.get_tid();
    const b_is_variable = tag(b.*) == .variable;

    if (is_narrower(tid_a, tid_b)) {
        if (a_is_variable and !b_is_variable) {
            a.variable.ref = b.variable.ref;
        } else if (!a_is_variable and b_is_variable) {
            a.type.tid = b.get_tid();
        } else if (a_is_variable and b_is_variable) {
            a.variable.ref = b.variable.ref;
        } else {
            a.type.tid = b.get_tid();
        }
    }

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
// const Context = @import("./context.zig").Context;
const floatMax = std.math.floatMax;
const maxInt = std.math.maxInt;
const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
const tag = std.meta.activeTag;

// const Context = struct {
//     next_var_id: u32 = 0,

//     variables: std.AutoHashMapUnmanaged(VarID, *TypeNode),

//     //@todo:mem deinit
//     bounded_types: std.StringArrayHashMapUnmanaged(std.ArrayListUnmanaged(*TypeNode)),

//     allocator: std.mem.Allocator,

//     pub fn init(allocator: std.mem.Allocator) Context {
//         return .{
//             .variables = .{},
//             .bounded_types = .{},
//             .allocator = allocator,
//         };
//     }

//     pub fn deinit(self: *@This()) void {
//         self.variables.deinit(self.allocator);
//         self.bounded_types.deinit(self.allocator);
//     }

//     pub fn get_var_instance(self: *@This(), type_node: *TypeNode) !*TypeNode {
//         if (type_node.is_var()) |var_id| {
//             if (self.variables.get(var_id)) |type_variable| {
//                 return type_variable;
//             }
//             try self.variables.put(self.allocator, var_id, type_node);
//             return type_node;
//         }
//         unreachable;
//     }

//     pub fn has_var_instance(self: *@This(), var_id: VarID) bool {
//         return self.variables.get(var_id) != null;
//     }

//     pub fn next_id(self: *Context) u32 {
//         self.next_var_id += 1;
//         return self.next_var_id;
//     }

//     pub fn clone(self: *Context) !Context {
//         return .{
//             .next_var_id = self.next_var_id,
//             .allocator = self.allocator,
//             .bounded_types = try self.bounded_types.clone(self.allocator),
//             .variables = try self.variables.clone(self.allocator),
//         };
//     }
// };

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
var equal_equal_return_type: TypeNode = .{ .type = .{
    .tid = .bool,
} };

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
