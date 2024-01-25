allocator: std.mem.Allocator,

ast: []*const Expr,

substs: std.ArrayListUnmanaged(Substitutions),

records: std.ArrayListUnmanaged(Record),

type_nodes: std.ArrayListUnmanaged(TypeNode),

sems: std.AutoArrayHashMapUnmanaged(*const Expr, *TypeNode),

next_id: NodeID = 0,

ctx: Context,

contexts: std.ArrayListUnmanaged(Context),

const NodeID = usize;

const TypeNode = union(enum) {
    type: MonoType,
    vartype: VarType,

    pub fn get_node_id(self: TypeNode) NodeID {
        return switch (self) {
            .type => |*ty| ty.id,
            .vartype => |*varty| varty.id,
        };
    }

    pub fn get_type_id(self: TypeNode) TypeID {
        return switch (self) {
            .type => |*ty| ty.tid,
            .vartype => |*varty| varty.tid,
        };
    }

    pub fn clone(self: TypeNode) TypeNode {
        return switch (self) {
            .type => |ty| .{
                .type = .{
                    .id = ty.id,
                    .tid = ty.tid,
                },
            },
            .vartype => |ty| .{
                .vartype = .{
                    .id = ty.id,
                    .tid = ty.tid,
                    .name = ty.name,
                },
            },
        };
    }
};

pub const TypeID = enum {
    any,
    number,
    float,
    int,
    bool,
    string,
};

pub const MonoType = struct {
    id: NodeID,
    tid: TypeID,
};

pub const VarType = struct {
    id: NodeID,
    tid: TypeID,
    name: []const u8,
};

pub const FunType = struct {
    id: NodeID,
    name: []const u8,
    args: []TypeNode,
    return_type: *TypeNode,
};

const Subtypes = std.EnumArray(TypeID, ?*const TypeHierarchy);
// const Subtypes = ComptimeEnumMap(TypeID, ?*const TypeHierarchy)
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
// var builtins_ids: u32 = 10_000; //@todo find a solution to avoid conflict with runtime ids
// fn next_builtin_id() u32 {
//     comptime builtins_ids += 1;
//     return builtins_ids;
// }
var add_args = [_]TypeNode{
    .{
        .vartype = .{
            .id = 999,
            .name = "T",
            .tid = .number,
        },
    },
    .{
        .vartype = .{
            .id = 999,
            .name = "T",
            .tid = .number,
        },
    },
};
var add_return_type = .{
    .vartype = .{
        .id = 999,
        .name = "T",
        .tid = .number,
    },
};
const builtins_types = std.ComptimeStringMap(
    FunType,
    .{
        .{
            "+", FunType{
                .id = 111, // ????
                .name = "+",
                .args = &add_args,
                .return_type = &add_return_type,
            },
        },
    },
);

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

const Substitutions = std.AutoHashMapUnmanaged(NodeID, TypeID);

const TypeError = error{
    TypeMismatch,
    UnknwownType,
    UnknownBuiltin,
    WrongArgumentsNumber,
    AllocError,
};

const Err = ErrorReporter(TypeError);

pub const Record = struct {
    node: *TypeNode,
    subst: *Substitutions,
};

pub fn init(allocator: std.mem.Allocator, ast: []*Expr) @This() {
    return .{
        .allocator = allocator,
        .ast = ast,
        .records = .{},
        .type_nodes = .{},
        .substs = .{},
        .sems = .{},
        .contexts = .{},
        .ctx = Context.init(allocator),
    };
}

pub fn infer_program(self: *@This()) anyerror!*std.AutoArrayHashMapUnmanaged(*const Expr, *TypeNode) {
    for (self.ast) |expr| {
        _ = try self.infer(expr, &self.ctx);
    }

    var types = std.ArrayList(struct { type: TypeNode, expr: Expr }).init(self.allocator);

    var iter = self.sems.iterator();
    while (iter.next()) |entry| {
        try types.append(.{ .type = entry.value_ptr.*.*, .expr = entry.key_ptr.*.* });
    }

    try jsonPrint(types.items, "./types.json");

    return &self.sems;
}

pub fn infer(self: *@This(), expr: *const Expr, ctx: *Context) !*Record {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            const record = try self.infer(const_init.initializer, ctx);
            const infered = apply_subst(record.subst, record.node);
            // var iter = self.sems.iterator();
            // _ = iter;

            const type_decl = TypeNode{
                .type = .{
                    .id = self.get_next_ID(),
                    .tid = if (const_init.type) |decl| try type_from_str(decl.lexeme) else .any,
                },
            };

            const s = try self.unify(type_decl, infered.*);

            const infered_type = apply_subst(s, infered);

            return try self.create_record_with_subst(
                TypeNode{
                    .type = .{
                        .id = type_decl.get_node_id(),
                        .tid = infered_type.get_type_id(),
                    },
                },
                s,
                expr,
            );
        },
        .Binary => |*binary| {
            var args = try self.allocator.alloc(*const Expr, 2);
            args[0] = binary.left;
            args[1] = binary.right;
            return try self.call("+", args, ctx, expr);
        },
        .Literal => |*literal| {
            const substs = try self.create_subst();
            const id = self.get_next_ID();
            try substs.put(
                self.allocator,
                id,
                type_of(literal.value),
            );

            return try self.create_record_with_subst(
                .{
                    .type = .{
                        .tid = .any,
                        .id = id,
                    },
                },
                substs,
                expr,
            );
        },
        else => unreachable,
    };
}

fn call(self: *@This(), name: []const u8, args: []*const Expr, ctx: *Context, expr: *const Expr) anyerror!*Record {
    if (builtins_types.get(name)) |function| {
        if (function.args.len != args.len) {
            return TypeError.WrongArgumentsNumber;
        }

        var call_ctx: *Context = try self.contexts.addOne(self.allocator);
        call_ctx.* = try ctx.clone();

        var substs = try self.create_subst();

        for (args, 0..) |expr_arg, i| {
            var fn_arg = try self.create_type_node(function.args[i]);

            const is_vartype = tag(fn_arg.*) == .vartype;

            if (is_vartype) {
                fn_arg = (try call_ctx.get_or_put_var(fn_arg));
            }

            var record = try self.infer(expr_arg, call_ctx);

            var infered_arg = apply_subst(record.subst, record.node);

            var s = try self.unify(infered_arg.*, fn_arg.*);

            substs = try self.compose_subst(substs, s);

            if (is_vartype) {
                try call_ctx.put_variable(apply_subst(substs, fn_arg));
            }
        }

        var return_type = try self.create_type_node(function.return_type.*);

        return try self.create_record_with_subst(
            apply_subst(substs, return_type).*,
            substs,
            expr,
        );
    } else {
        return TypeError.UnknownBuiltin;
    }
}

pub fn apply_subst(
    subst: *Substitutions,
    type_node: *TypeNode,
) *TypeNode {
    const node: TypeNode = switch (type_node.*) {
        .type => |ty| .{
            .type = .{
                .id = ty.id,
                .tid = subst.get(ty.id) orelse ty.tid,
            },
        },
        .vartype => |ty| .{
            .vartype = .{
                .id = ty.id,
                .name = ty.name,
                .tid = subst.get(ty.id) orelse ty.tid,
            },
        },
    };
    type_node.* = node;
    return type_node;
}

fn get_next_ID(self: *@This()) NodeID {
    self.next_id += 1;
    return self.next_id;
}

fn unify(self: *@This(), a: TypeNode, b: TypeNode) !*Substitutions {
    const s = try self._unify(a, b);

    if (!types_intersects(a.get_type_id(), b.get_type_id())) {
        std.debug.print(
            "\nType Mismatch --\nExpected : {}\nFound : {}\n",
            .{ a.get_type_id(), b.get_type_id() },
        );
        return TypeError.TypeMismatch;
    }

    return s;
}

fn _unify(self: *@This(), a: TypeNode, b: TypeNode) !*Substitutions {
    const s1 = try self.coerce(a, b);
    const s2 = try self.substitute(a, b);

    return try self.compose_subst(s1, s2);
}

fn coerce(self: *@This(), a: TypeNode, b: TypeNode) !*Substitutions {
    const tid_a = a.get_type_id();
    const tid_b = b.get_type_id();

    const s = try self.create_subst();

    if ((tid_a == .number or tid_a == .float) and (tid_b == .number or tid_b == .float)) {
        if ((tid_a == .float and tid_b != .float) or (tid_b == .float and tid_a != .float)) {
            try s.put(self.allocator, a.get_node_id(), .float);
            try s.put(self.allocator, b.get_node_id(), .float);
        }
    }

    return s;
}

fn substitute(self: *@This(), a: TypeNode, b: TypeNode) !*Substitutions {
    const tid_a = a.get_type_id();
    const tid_b = b.get_type_id();

    if (is_narrower(tid_a, tid_b)) {
        const s = try self.create_subst();
        try s.put(self.allocator, a.get_node_id(), b.get_type_id());
        return s;
    }

    return try self.create_subst();
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

fn compose_subst(self: *@This(), s1: *Substitutions, s2: *Substitutions) !*Substitutions {
    const result = try self.create_subst();

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
    var type_node_ptr = try self.type_nodes.addOne(self.allocator);
    type_node_ptr.* = type_node;
    return type_node_ptr;
}

fn create_record(self: *@This(), type_node: TypeNode) !*Record {
    // create node type
    var type_node_ptr = try self.create_type_node(type_node);
    // create substituions
    const subst = try self.create_subst();

    // create record
    var record = try self.records.addOne();
    record.* = Record{
        .type = type_node_ptr,
        .subst = subst,
    };

    return record;
}

fn create_subst(self: *@This()) !*Substitutions {
    var subst = try self.substs.addOne(self.allocator);
    subst.* = .{};

    return subst;
}

fn create_record_with_subst(self: *@This(), type_node: TypeNode, subst: *Substitutions, expr: *const Expr) !*Record {
    // create node type
    var type_node_ptr = try self.type_nodes.addOne(self.allocator);
    type_node_ptr.* = type_node;

    try self.sems.put(self.allocator, expr, type_node_ptr);

    // create record
    var record = try self.records.addOne(self.allocator);
    record.* = Record{
        .node = type_node_ptr,
        .subst = subst,
    };

    return record;
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

const Context = struct {
    next_var_id: u32 = 0,

    variables: std.StringHashMap(*TypeNode),

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Context {
        return .{
            .variables = std.StringHashMap(*TypeNode).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.variables.deinit();
    }

    pub fn has_variable(self: *@This(), variable_name: []const u8) bool {
        return self.variables.get(variable_name) != null;
    }

    pub fn get_variable(self: *@This(), variable_name: []const u8) ?*TypeNode {
        return self.variables.get(variable_name);
    }

    pub fn put_variable(self: *@This(), type_node: *TypeNode) !void {
        try self.variables.put(type_node.vartype.name, type_node);
    }

    pub fn get_or_put_var(self: *@This(), type_node: *TypeNode) !*TypeNode {
        if (self.get_variable(type_node.vartype.name)) |type_variable| {
            return type_variable;
        }
        try self.put_variable(type_node);
        return type_node;
    }

    pub fn next_id(self: *Context) u32 {
        self.next_var_id += 1;
        return self.next_var_id;
    }

    pub fn clone(self: *Context) !Context {
        return .{
            .next_var_id = self.next_var_id,
            .allocator = self.allocator,
            .variables = try self.variables.clone(),
        };
    }
};

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
