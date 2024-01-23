allocator: std.mem.Allocator,

ast: []*const Expr,

substs: std.ArrayListUnmanaged(Substitutions),

records: std.ArrayListUnmanaged(Record),

type_nodes: std.ArrayListUnmanaged(TypeNode),

next_id: NodeID = 0,

const NodeID = usize;

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
};

const Substitutions = std.AutoHashMapUnmanaged(NodeID, TypeID);

const TypeError = error{ TypeMismatch, UnknwownType };

const Err = ErrorReporter(TypeError);

pub const Record = struct {
    node: TypeNode,
    subst: *Substitutions,
};

pub fn init(allocator: std.mem.Allocator, ast: []*Expr) @This() {
    return .{
        .allocator = allocator,
        .ast = ast,
        .records = .{},
        .type_nodes = .{},
        .substs = .{},
        // .ctx = Context.init(allocator),
    };
}

pub fn infer_program(self: *@This()) !*std.ArrayListUnmanaged(Record) {
    for (self.ast) |expr| {
        _ = try self.infer(expr);
    }

    var types = std.ArrayList(TypeNode).init(self.allocator);

    for (self.records.items) |rec| {
        try types.append(rec.node);
    }

    try jsonPrint(types.items, "./types.json");

    return &self.records;
}

pub fn infer(self: *@This(), expr: *const Expr) !*Record {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            const record = try self.infer(const_init.initializer);
            const infered = apply_subst(record.subst, record.node);
            const type_decl = TypeNode{
                .type = .{
                    .id = self.get_next_ID(),
                    .tid = if (const_init.type) |decl| try type_from_str(decl.lexeme) else .any,
                },
            };

            const s = try self.unify(infered, type_decl);

            return try self.create_record_with_subst(
                TypeNode{
                    .type = .{
                        .id = type_decl.get_node_id(),
                        .tid = type_decl.get_type_id(),
                    },
                },
                s,
            );
        },
        .Literal => |*literal| {
            const substs = try self.create_subst();
            try substs.put(
                self.allocator,
                @intFromPtr(expr),
                type_of(literal.value),
            );

            return try self.create_record_with_subst(
                .{
                    .type = .{
                        .tid = .any,
                        .id = self.get_next_ID(),
                    },
                },
                substs,
            );
        },
        else => unreachable,
    };
}

pub fn apply_subst(
    subst: *Substitutions,
    type_node: TypeNode,
) TypeNode {
    return switch (type_node) {
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
}

fn get_next_ID(self: *@This()) NodeID {
    self.next_id += 1;
    return self.next_id;
}

fn unify(self: *@This(), a: TypeNode, b: TypeNode) !*Substitutions {
    const s = try self._unify(a, b);

    if (!types_intersects(a.get_type_id(), b.get_type_id())) {
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

// const TypeHierarchy = union(enum) {
//     terminal,
//     subtypes: std.EnumArray(TypeID, *TypeHierarchy),
// };

// const TypeHierarchy = union(enum) {
//     terminal: TypeID,
//     supertype: struct { tid: TypeID, subtypes: std.EnumArray(TypeID, *TypeHierarchy) },
// };
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
    .terminal = .{ .tid = .int },
};
const float_type: TypeHierarchy = .{
    .terminal = .{ .tid = .float },
};
const bool_type: TypeHierarchy = .{
    .terminal = .{ .tid = .bool },
};
const string_type: TypeHierarchy = .{
    .terminal = .{ .tid = .string },
};

const any_subtypes: Subtypes = blk: {
    var subtypes = Subtypes.initFill(null);

    var number_subtypes = Subtypes.initFill(null);
    number_subtypes.set(.int, &float_type);
    number_subtypes.set(.float, &int_type);

    var number_type: TypeHierarchy = .{
        .supertype = .{
            .tid = .number,
            .subtypes = number_subtypes,
        },
    };

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

fn is_narrower(tid: TypeID, maybe_narrower: TypeID) bool {
    return is_subtype(tid, maybe_narrower);
}

fn is_subtype(tid: TypeID, maybe_subtype: TypeID) bool {
    const maybe_subtypes = find_subtypes(tid, &type_hierarchy);
    if (maybe_subtypes) |subtypes| {
        if (subtypes.get(tid)) |subtype| {
            const subtype_tid = switch (subtype.*) {
                .terminal => |ty| ty.tid,
                .supertype => |ty| ty.tid,
            };

            if (subtype_tid == tid) {
                return true;
            }
            for (subtypes.values) |m_subtype| {
                if (m_subtype) |sub| {
                    const subtype_tid_2 = switch (sub.*) {
                        .terminal => |ty| ty.tid,
                        .supertype => |ty| ty.tid,
                    };
                    if (is_subtype(subtype_tid_2, maybe_subtype)) {
                        return true;
                    }
                }
            }
            return false;
        }
    }
    return false;
}

fn find_subtypes(target: TypeID, hierarchy: *const TypeHierarchy) ?*const Subtypes {
    return switch (hierarchy.*) {
        .terminal => null,
        .supertype => |supertype| {
            for (&supertype.subtypes.values) |m_subtype| {
                if (m_subtype) |subtype| {
                    const subtype_tid = switch (subtype.*) {
                        .terminal => |ty| ty.tid,
                        .supertype => |ty| ty.tid,
                    };

                    if (subtype_tid == target) {
                        return switch (subtype.*) {
                            .terminal => null,
                            .supertype => |s| &s.subtypes,
                        };
                    }

                    if (return switch (subtype.*) {
                        .terminal => null,
                        .supertype => |s| &s.subtypes,
                    }) |subs| {
                        return find_subtypes(target, subs);
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

    const a_is_terminal = find_subtypes(a, &type_hierarchy) != null;
    const b_is_terminal = find_subtypes(b, &type_hierarchy) != null;

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
    var type_node_ptr = try self.type_nodes.addOne();
    type_node_ptr.* = type_node;
    // const result = self.records.getOrPut(expr) catch return TypeError.AlreadyRegisteredRecord;
    // result.value_ptr.* = record;
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

fn create_record_with_subst(self: *@This(), type_node: TypeNode, subst: *Substitutions) !*Record {
    // create node type
    var type_node_ptr = try self.type_nodes.addOne(self.allocator);
    type_node_ptr.* = type_node;

    // create record
    var record = try self.records.addOne(self.allocator);
    record.* = Record{
        .node = type_node_ptr.*,
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
