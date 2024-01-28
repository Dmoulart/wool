allocator: std.mem.Allocator,

ast: []*const Expr,

substs: std.ArrayListUnmanaged(Substitutions),

records: std.ArrayListUnmanaged(Record),

type_nodes: std.ArrayListUnmanaged(TypeNode),

sems: std.AutoArrayHashMapUnmanaged(*const Expr, *Record),

next_id: NodeID = 0,

ctx: Context,

contexts: std.ArrayListUnmanaged(Context),

const Substitutions = std.AutoHashMapUnmanaged(*TypeNode, TypeID);

const NodeID = usize;
const VarID = usize;

const TypeNode = MonoType;

pub const TypeID = enum {
    any,
    number,
    float,
    int,
    bool,
    string,
};

pub const MonoType = struct {
    tid: TypeID,
    var_id: ?VarID,

    pub fn is_var(self: MonoType) ?VarID {
        return if (self.var_id) |id| id else null;
    }
};

pub const FunType = struct {
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

pub fn infer_program(self: *@This()) anyerror!*std.AutoArrayHashMapUnmanaged(*const Expr, *Record) {
    for (self.ast) |expr| {
        _ = try self.infer(expr, &self.ctx);
    }

    try self.log_sems();

    return &self.sems;
}
pub fn log_sems(self: *@This()) !void {
    var types = std.ArrayList(struct { type: TypeNode, expr: Expr }).init(self.allocator);

    var iter = self.sems.iterator();
    while (iter.next()) |entry| {
        // std.debug.print("\n {any} \n", .{.{ .type = entry.value_ptr.*.*, .expr = entry.key_ptr.*.* }});
        try types.append(.{ .type = entry.value_ptr.*.*.node.*, .expr = entry.key_ptr.*.* });
    }

    try jsonPrint(types.items, "./types.json");
}

pub fn infer(self: *@This(), expr: *const Expr, ctx: *Context) !*Record {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            const record = try self.infer(const_init.initializer, ctx);
            const infered = apply_subst(record.subst, record.node);

            var type_decl = try self.create_type_node(.{
                .var_id = null,
                .tid = if (const_init.type) |decl| try type_from_str(decl.lexeme) else .any,
            });

            const s = try self.unify(type_decl, infered);

            const infered_type = apply_subst(s, infered);

            return try self.create_record_with_subst(
                TypeNode{
                    .var_id = null,
                    .tid = infered_type.tid,
                },
                s,
                expr,
            );
        },
        .Grouping => |*grouping| {
            var record = try self.infer(grouping.expr, ctx);
            _ = apply_subst(record.subst, record.node);
            return try self.create_record_with_subst(
                record.node.*,
                record.subst,
                expr,
            );
        },
        .Binary => |*binary| {
            //@todo:mem clean memory
            var args = try self.allocator.alloc(*const Expr, 2);
            args[0] = binary.left;
            args[1] = binary.right;
            defer {
                self.allocator.free(args);
            }

            var local_ctx: *Context = try self.contexts.addOne(self.allocator);
            local_ctx.* = try ctx.clone();

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

            return try self.call(
                builtins_types.get(builtin_name).?,
                args,
                local_ctx,
                expr,
            );
        },
        .Literal => |*literal| {
            const substs = try self.create_subst();

            var record = try self.create_record_with_subst(
                .{
                    .var_id = null,
                    .tid = .any,
                },
                substs,
                expr,
            );

            try substs.put(
                self.allocator,
                record.node,
                type_of(literal.value),
            );

            return record;
        },
        else => unreachable,
    };
}

fn call(self: *@This(), function: FunType, args: []*const Expr, local_ctx: *Context, expr: *const Expr) anyerror!*Record {
    if (function.args.len != args.len) {
        return TypeError.WrongArgumentsNumber;
    }
    if (std.mem.eql(u8, function.name, "!=")) {
        std.debug.print("hello", .{});
    }
    var substs = try self.create_subst();

    var before_arg: ?*TypeNode = null;

    for (args, 0..) |expr_arg, i| {
        const arg_global_ref = &function.args[i];

        const arg_local_ref = if (arg_global_ref.is_var()) |var_id| blk: {
            if (local_ctx.variables.get(var_id)) |var_type| {
                break :blk var_type;
            } else {
                var tn = try self.create_type_node(arg_global_ref.*);
                try local_ctx.variables.put(local_ctx.allocator, var_id, tn);
                break :blk tn;
            }
        } else arg_global_ref;

        var record = try self.infer(expr_arg, local_ctx);

        var arg_instance = record.node;

        _ = apply_subst(record.subst, arg_instance);

        const arg_subs = try self.unify(
            arg_local_ref,
            arg_instance,
        );

        _ = apply_subst(arg_subs, arg_local_ref);

        substs = try self.compose_subst(substs, arg_subs);

        if (arg_global_ref.is_var()) |_| {
            // exchange !
            // @todo:mem destroy old node
            record.node = arg_local_ref;
        }
        before_arg = record.node;

        // try self.log_sems();
    }
    const return_type = if (function.return_type.is_var()) |_|
        try local_ctx.get_var_instance(function.return_type)
    else
        function.return_type;

    return try self.create_record_with_subst(
        apply_subst(substs, return_type).*,
        substs,
        expr,
    );
}

pub fn apply_subst(
    subst: *Substitutions,
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

fn unify(self: *@This(), a: *TypeNode, b: *TypeNode) !*Substitutions {
    const s = try self._unify(a, b);

    if (!types_intersects(a.tid, b.tid)) {
        std.debug.print(
            "\nType Mismatch --\nExpected : {}\nFound : {}\n",
            .{ a.tid, b.tid },
        );
        return TypeError.TypeMismatch;
    }

    return s;
}

fn _unify(self: *@This(), a: *TypeNode, b: *TypeNode) !*Substitutions {
    const s1 = try self.coerce(a, b);
    const s2 = try self.substitute(a, b);

    return try self.compose_subst(s1, s2);
}

fn coerce(self: *@This(), a: *TypeNode, b: *TypeNode) !*Substitutions {
    _ = b;
    _ = a;
    // const tid_a = a.tid;
    // _ = tid_a;
    // const tid_b = b.tid;
    // _ = tid_b;

    const s = try self.create_subst();

    // if ((tid_a == .number or tid_a == .float) and (tid_b == .number or tid_b == .float)) {
    //     if ((tid_a == .float and tid_b != .float) or (tid_b == .float and tid_a != .float)) {
    //         try s.put(self.allocator, a, .float);
    //         try s.put(self.allocator, b, .float);
    //     }
    // }

    return s;
}

fn substitute(self: *@This(), a: *TypeNode, b: *TypeNode) !*Substitutions {
    const tid_a = a.tid;
    const tid_b = b.tid;

    if (is_narrower(tid_a, tid_b)) {
        const s = try self.create_subst();
        try s.put(self.allocator, a, b.tid);
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
    var tn_ptr = try self.type_nodes.addOne(self.allocator);
    tn_ptr.* = type_node;
    return tn_ptr;
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
    var type_node_ptr = try self.create_type_node(type_node);

    // create record
    var record = try self.records.addOne(self.allocator);
    record.* = Record{
        .node = type_node_ptr,
        .subst = subst,
    };

    try self.sems.put(self.allocator, expr, record);

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

    variables: std.AutoHashMapUnmanaged(VarID, *TypeNode),

    //@todo:mem deinit
    bounded_types: std.StringArrayHashMapUnmanaged(std.ArrayListUnmanaged(*TypeNode)),

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Context {
        return .{
            .variables = .{},
            .bounded_types = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.variables.deinit(self.allocator);
        self.bounded_types.deinit(self.allocator);
    }

    pub fn get_var_instance(self: *@This(), type_node: *TypeNode) !*TypeNode {
        if (type_node.is_var()) |var_id| {
            if (self.variables.get(var_id)) |type_variable| {
                return type_variable;
            }
            try self.variables.put(self.allocator, var_id, type_node);
            return type_node;
        }
        unreachable;
    }

    pub fn has_var_instance(self: *@This(), var_id: VarID) bool {
        return self.variables.get(var_id) != null;
    }

    pub fn next_id(self: *Context) u32 {
        self.next_var_id += 1;
        return self.next_var_id;
    }

    pub fn clone(self: *Context) !Context {
        return .{
            .next_var_id = self.next_var_id,
            .allocator = self.allocator,
            .bounded_types = try self.bounded_types.clone(self.allocator),
            .variables = try self.variables.clone(self.allocator),
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

var add_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .number,
    },
    .{
        .var_id = 1,
        .tid = .number,
    },
};
var add_return_type: TypeNode = .{
    .var_id = 1,
    .tid = .number,
};
var sub_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .number,
    },
    .{
        .var_id = 1,
        .tid = .number,
    },
};
var sub_return_type: TypeNode = .{
    .var_id = 1,
    .tid = .number,
};

var mul_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .number,
    },
    .{
        .var_id = 1,
        .tid = .number,
    },
};
var mul_return_type: TypeNode = .{
    .var_id = 1,
    .tid = .number,
};

var div_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .number,
    },
    .{
        .var_id = 1,
        .tid = .number,
    },
};
var div_return_type: TypeNode = .{
    .var_id = 1,
    .tid = .number,
};

var equal_equal_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .any,
    },
    .{
        .var_id = 1,
        .tid = .any,
    },
};
var equal_equal_return_type: TypeNode = .{
    .var_id = null,
    .tid = .bool,
};

var bang_equal_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .any,
    },
    .{
        .var_id = 1,
        .tid = .any,
    },
};
var bang_equal_return_type: TypeNode = .{
    .var_id = null,
    .tid = .bool,
};

var greater_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .number,
    },
    .{
        .var_id = 1,
        .tid = .number,
    },
};
var greater_return_type: TypeNode = .{
    .var_id = null,
    .tid = .bool,
};

var greater_equal_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .number,
    },
    .{
        .var_id = 1,
        .tid = .number,
    },
};
var greater_equal_return_type: TypeNode = .{
    .var_id = null,
    .tid = .bool,
};

var less_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .number,
    },
    .{
        .var_id = 1,
        .tid = .number,
    },
};
var less_return_type: TypeNode = .{
    .var_id = null,
    .tid = .bool,
};

var less_equal_args = [_]TypeNode{
    .{
        .var_id = 1,
        .tid = .number,
    },
    .{
        .var_id = 1,
        .tid = .number,
    },
};
var less_equal_return_type: TypeNode = .{
    .var_id = null,
    .tid = .bool,
};

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
