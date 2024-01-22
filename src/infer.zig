allocator: std.mem.Allocator,

ast: []*const Expr,

substs: std.ArrayListUnmanaged(Substitutions),

records: std.ArrayListUnmanaged(Record),

type_nodes: std.ArrayListUnmanaged(TypeNode),

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
    type: TypeID,
    vartype: VarType,

    pub fn get_type_id(self: TypeNode) TypeID {
        return switch (self) {
            .type => |*ty| ty.tid,
            .vartype => |*varty| varty.tid,
        };
    }
};

const Substitutions = std.AutoHashMapUnmanaged(NodeID, TypeID);

const TypeError = error{};

const Err = ErrorReporter(TypeError);

pub const Record = struct {
    type: TypeNode,
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
        try types.append(rec.type);
    }

    try jsonPrint(types.items, "./types.json");

    return &self.records;
}

pub fn infer(self: *@This(), expr: *const Expr) !*Record {
    return switch (expr.*) {
        .Literal => |*literal| {
            const substs = try self.create_subst();
            try substs.put(
                self.allocator,
                @intFromPtr(expr),
                type_of(literal.value),
            );

            return try self.create_record_with_subst(
                TypeNode{ .type = .any },
                substs,
            );
        },
        else => unreachable,

        // .Grouping => |grouping| {
        //     return try self.infer(grouping.expr, ctx);
        // },
        // .Binary => |binary| {
        //     return switch (binary.op.type) {
        //         .PLUS => {
        //             const left_record = try self.infer(binary.left, ctx);

        //             const right_record = try self.infer(binary.right, ctx);

        //             const s = try self.unify(
        //                 left_record.type,
        //                 right_record.type,
        //                 expr,
        //             );

        //             const left_type = try self.apply_subst_to_type(s, left_record.type, binary.left);
        //             const right_type = try self.apply_subst_to_type(s, right_record.type, binary.right);

        //             const s_2 = try self.unify(left_type, right_type, expr);

        //             const result_s = try self.compose_subst(s, s_2, expr);

        //             return try self.create_record_with_subst(
        //                 TypeNode{ .function = .{
        //                     .from = left_type,
        //                     .to = right_type,
        //                     .expr = expr,
        //                 } },
        //                 result_s,
        //             );
        //         },

        //         // .PLUS => {
        //         //     const left_record = try self.infer(binary.left, ctx);

        //         //     const s_left = try self.unify(
        //         //         left_record.type,
        //         //         try self.create_type_node(
        //         //             TypeNode{
        //         //                 .named = .{
        //         //                     .expr = expr,
        //         //                     .id = NUMBER_ID,
        //         //                 },
        //         //             },
        //         //         ),
        //         //         binary.left,
        //         //     );

        //         //     const ctx1 = try self.apply_subst_to_ctx(
        //         //         try self.compose_subst(
        //         //             left_record.subst,
        //         //             s_left,
        //         //             binary.left,
        //         //         ),
        //         //         ctx,
        //         //         expr,
        //         //     );

        //         //     const right_record = try self.infer(binary.right, ctx1);

        //         //     const s_right = try self.unify(
        //         //         right_record.type,
        //         //         try self.create_type_node(
        //         //             TypeNode{
        //         //                 .named = .{
        //         //                     .expr = binary.right,
        //         //                     .id = NUMBER_ID,
        //         //                 },
        //         //             },
        //         //         ),
        //         //         binary.right,
        //         //     );
        //         //     const s = try self.compose_subst(s_left, s_right, expr);

        //         //     const left_type = try self.apply_subst_to_type(s, left_record.type, binary.left);
        //         //     const right_type = try self.apply_subst_to_type(s, right_record.type, binary.right);

        //         //     const s_2 = try self.unify(left_type, right_type, expr);

        //         //     const result_s = try self.compose_subst(s, s_2, expr);

        //         //     return try self.create_record_with_subst(
        //         //         (try self.apply_subst_to_type(
        //         //             s_2,
        //         //             right_type,
        //         //             expr,
        //         //         )).*,
        //         //         result_s,
        //         //     );
        //         // },

        //         else => unreachable,
        //     };
        // },
        // .ConstInit => |const_init| {
        //     var ret = try self.infer(const_init.initializer, ctx);

        //     try ctx.add(const_init.name.lexeme, ret.type);

        //     return ret;
        // },
        // .Variable => |variable| {
        //     if (ctx.env.get(variable.name.lexeme)) |type_node| {
        //         return try self.create_record(type_node.*);
        //     } else {
        //         return TypeError.UnboundVariable;
        //     }
        // },
        // .Call => |call| {
        //     const func_record = try self.infer(call.callee, ctx);
        //     const arg_record = try self.infer(
        //         call.args[0],
        //         try self.apply_subst_to_ctx(func_record.subst, ctx, expr),
        //     );
        //     const new_var = try self.create_type_node(ctx.new_type_var(expr));
        //     const s3 = try self.compose_subst(func_record.subst, arg_record.subst, expr);
        //     const new_func_call = try self.create_type_node(TypeNode{ .function = .{
        //         .from = arg_record.type,
        //         .to = new_var,
        //         .expr = expr,
        //     } });
        //     const s4 = try self.unify(new_func_call, func_record.type, expr);
        //     const func_type_1 = try self.apply_subst_to_type(
        //         s4,
        //         func_record.type,
        //         expr,
        //     );
        //     const s5 = try self.compose_subst(s3, s4, expr);
        //     const s6 = try self.unify(
        //         try self.apply_subst_to_type(
        //             s5,
        //             func_type_1.function.from,
        //             expr,
        //         ),
        //         arg_record.type,
        //         expr,
        //     );
        //     const result_subst = try self.compose_subst(s5, s6, expr);

        //     const ret = try self.create_record_with_subst(
        //         (try self.apply_subst_to_type(
        //             result_subst,
        //             func_type_1.function.to,
        //             expr,
        //         )).*,
        //         result_subst,
        //     );

        //     return ret;
        //     // return self.apply_subst_to_type(result_subst, func_type_1.function.to)
        // },
        // .Function => |function| {
        //     const func_return_type = try self.to_type_node(
        //         try Type.from_str(function.type.lexeme),
        //         expr,
        //     );

        //     const func_ctx = try ctx.clone();

        //     const param = function.args.?[0];
        //     const param_type = try self.to_type_node(
        //         try Type.from_str(param.type.lexeme),
        //         expr,
        //     );

        //     if (function.name) |func_name| {
        //         try func_ctx.add(func_name.lexeme, func_return_type);
        //     }
        //     try func_ctx.add(param.name.lexeme, param_type);

        //     const body_record = try self.infer(function.body.?, func_ctx);

        //     if (body_record.type.get_type_id() != func_return_type.get_type_id()) {
        //         std.debug.print("\n --- BodyTypeMismatchReturnType --- \n Expected {} \n Found {} \n", .{ func_return_type.get_type_id(), body_record.type.get_type_id() });
        //         return TypeError.BodyTypeMismatchReturnType;
        //     }
        //     // const body_substs = try self.unify(body_record.type, func_return_type, function.body.?);

        //     const inferred_type = TypeNode{
        //         .function = .{
        //             .from = param_type,
        //             .to = func_return_type,
        //             .expr = expr,
        //         },
        //     };
        //     return self.create_record(
        //         inferred_type,
        //     );
        // },

    };
}

pub fn apply_subst_to_type(
    self: *@This(),
    subst: *Substitutions,
    type_node: *TypeNode,
    expr: *const Expr,
) !*TypeNode {
    return switch (type_node.*) {
        .named => type_node,
        .variable => |var_type| if (subst.get(var_type.id)) |t| t else type_node,
        .function => |function_type| try self.create_type_node(
            TypeNode{
                .function = .{
                    .from = try self.apply_subst_to_type(subst, function_type.from, expr),
                    .to = try self.apply_subst_to_type(subst, function_type.to, expr),
                    .expr = expr,
                },
            },
        ),
        .binary => |binary| try self.create_type_node(
            TypeNode{
                .binary = .{
                    .left = try self.apply_subst_to_type(subst, binary.left, expr),
                    .right = try self.apply_subst_to_type(subst, binary.right, expr),
                    .expr = expr,
                },
            },
        ),
    };
}

// apply given substitution to each type in the context's environment
// Doesn't change the input context, but returns a new one
// pub fn apply_subst_to_ctx(self: *@This(), subst: *Substitutions, ctx: *Context, expr: *const Expr) !*Context {
//     const new_ctx = try ctx.clone();
//     const env_clone = try new_ctx.env.clone();

//     for (env_clone.keys()) |key| {
//         const type_node = env_clone.get(key).?;
//         try new_ctx.env.put(key, try self.apply_subst_to_type(
//             subst,
//             type_node,
//             expr,
//         ));
//     }

//     return new_ctx;
//     // new_ctx.env.iterator()
// }

fn compose_subst(self: *@This(), s1: *Substitutions, s2: *Substitutions, expr: *const Expr) !*Substitutions {
    const result = try self.create_subst();

    var iter_s2 = s2.iterator();
    while (iter_s2.next()) |record| {
        try result.put(
            record.key_ptr.*,
            try self.apply_subst_to_type(s1, record.value_ptr.*, expr),
        );
    }

    var iter_s1 = s1.iterator();
    while (iter_s1.next()) |record| {
        try result.put(record.key_ptr.*, record.value_ptr.*);
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
        .type = type_node_ptr.*,
        .subst = subst,
    };

    return record;
}

// fn unify(self: *@This(), a: *TypeNode, b: *TypeNode, expr: *const Expr) !*Substitutions {
//     const tag_a = tag(a.*);
//     const tag_b = tag(b.*);

//     if (tag_a == .named and tag_b == .named and b.named.id == a.named.id) {
//         return try self.create_subst();
//     } else if (tag_a == .named and a.named.id == I32_ID and tag_b == .named and b.named.id == F32_ID) {
//         a.named.id = F32_ID;
//         return try self.create_subst();
//     } else if (tag_b == .named and b.named.id == I32_ID and tag_a == .named and a.named.id == F32_ID) {
//         b.named.id = F32_ID;
//         return try self.create_subst();
//     } else if (tag_a == .variable) {
//         return try self.var_bind(a.variable.id, b);
//     } else if (tag_b == .variable) {
//         return try self.var_bind(b.variable.id, a);
//     } else if (tag_a == .function and tag_b == .function) {
//         var s1 = try self.unify(a.function.from, b.function.from, expr);
//         var s2 = try self.unify(
//             try self.apply_subst_to_type(s1, a.function.to, expr),
//             try self.apply_subst_to_type(s1, b.function.to, expr),
//             expr,
//         );
//         return try self.compose_subst(s1, s2, expr);
//     } else if (tag_a == .function and tag_b == .named) {
//         var s_from = try self.unify(a.function.from, b, expr);
//         var s_to = try self.unify(a.function.to, b, expr);

//         var res_s = try self.compose_subst(s_from, s_to, expr);

//         var s2 = try self.unify(
//             try self.apply_subst_to_type(res_s, a.function.from, expr),
//             try self.apply_subst_to_type(res_s, b, expr),
//             expr,
//         );
//         return s2;
//     } else if (tag_a == .named and tag_b == .function) {
//         var s_from = try self.unify(b.function.from, a, expr);
//         var s_to = try self.unify(b.function.to, a, expr);

//         var res_s = try self.compose_subst(s_from, s_to, expr);

//         var s2 = try self.unify(
//             try self.apply_subst_to_type(res_s, b.function.from, expr),
//             try self.apply_subst_to_type(res_s, a, expr),
//             expr,
//         );
//         return s2;
//     }
//     // else if (tag_a == .binary and tag_b == .binary) {
//     //     var s1 = try self.unify(a.binary.right, b.binary.right, expr);
//     //     var s2 = try self.unify(
//     //         try self.apply_subst_to_type(s1, a.binary.left, expr),
//     //         try self.apply_subst_to_type(s1, b.binary.left, expr),
//     //         expr,
//     //     );
//     //     return try self.compose_subst(s1, s2, expr);
//     // } else if (tag_a == .binary) {
//     //     var s1 = try self.unify(a.binary.left, b.binary.right, expr);
//     //     var s2 = try self.unify(
//     //         a.binary.left,
//     //         try self.apply_subst_to_type(s1, b.binary.right, expr),
//     //         expr,
//     //     );
//     //     return try self.compose_subst(s1, s2, expr);
//     // }
//     else {
//         std.debug.print("--Type mismatch-- \n Expected {any}\n    Found {any}\n", .{
//             a.get_type_id(),
//             b.get_type_id(),
//         });
//         return TypeError.TypeMismatch;
//     }
// }

// fn var_bind(self: *@This(), tid: TypeID, type_node: *TypeNode) !*Substitutions {
//     if (tag(type_node.*) == .variable and type_node.variable.id == tid) {
//         return try self.create_subst();
//     } else if (contains(type_node, tid)) {
//         // throw `Type ${typeToString(t)} contains a reference to itself`;
//         return TypeError.CircularReference;
//     } else {
//         const subst = try self.create_subst();
//         try subst.put(tid, type_node);
//         return subst;
//     }
// }

// fn contains(t: *TypeNode, tid: TypeID) bool {
//     return switch (t.*) {
//         .named => false,
//         .variable => |*variable| variable.id == tid,
//         .function => |*function| contains(function.from, tid) or contains(function.to, tid),
//         .binary => |*binary| contains(binary.left, tid) or contains(binary.right, tid),
//     };
// }

// fn in_assignation(self: *@This()) bool {
//     return self.ctx.in(&.{ "VarInit", "ConstInit" }) != null;
// }

fn type_of(value: Expr.Literal.Value) TypeID {
    return switch (value) {
        .String => .string,
        .Number => |number| if (is_float_value(number)) .float else .number,
        .Boolean => .bool,
        else => unreachable,
    };
}

// fn get_target_type(self: *@This()) ?Type {
//     if (self.ctx.in(&.{ "VarInit", "ConstInit" })) |frame| {
//         if (frame.type) |frame_type| {
//             return frame_type;
//         }
//     }
//     return null;
// }

// fn infer_type(expr: *const Expr) !?Type {
//     return switch (expr.*) {
//         .Literal => |literal| {
//             return switch (literal.value) {
//                 .Number => |number| {
//                     const is_float = @rem(number, 1) != 0;
//                     if (is_float) {
//                         return if (number > floatMax(f32)) .f64 else .f32;
//                     } else {
//                         return if (number > maxInt(i32)) .i64 else .i32;
//                     }
//                 },
//                 else => TypeError.CannotInferType,
//             };
//         },
//         .Function => .func,
//         else => TypeError.CannotInferType,
//     };
// }
//@todo remove -> put this in types
// inline fn is_number_type(@"type": Type) bool {
//     return switch (@"type") {
//         .i32, .i64, .f32, .f64 => true,
//         else => false,
//     };
// }

inline fn is_float_value(number: f64) bool {
    return @rem(number, 1) != 0;
}

// inline fn is_integer_type(number_type: Type) bool {
//     return number_type == .i32 or number_type == .i64;
// }

// inline fn is_foat_type(number_type: Type) bool {
//     return number_type == .f32 or number_type == .f64;
// }

// fn infer_number_type(number: f64) Type {
//     return blk: {
//         if (is_float_value(number)) {
//             break :blk if (number > floatMax(f32)) .f64 else .f32;
//         } else {
//             break :blk if (number > maxInt(i32)) .i64 else .i32;
//         }
//     };
// }

// fn coerce_number_types(a: Type, b: Type) Type {
//     if (a != b) {
//         const size_of_left = a.size_of();
//         const size_of_right = b.size_of();

//         if (size_of_left == size_of_right) {
//             if (is_foat_type(a) and !is_foat_type(b)) {
//                 return a;
//             } else if (is_foat_type(b) and !is_foat_type(a)) {
//                 return b;
//             }
//         } else {
//             return if (size_of_left >= size_of_right) a else b;
//         }
//     }

//     return a;
// }

// const Context = struct {
//     next: TypeID,
//     env: Env,
//     allocator: std.mem.Allocator,

//     pub fn init(allocator: std.mem.Allocator) @This() {
//         return .{
//             .allocator = allocator,
//             .env = Env.init(allocator),
//             .next = BOOL_ID,
//         };
//     }

//     pub fn new_type_var(self: *@This(), expr: *const Expr) TypeNode {
//         return TypeNode{ .variable = .{
//             .id = self.inc(),
//             .expr = expr,
//         } };
//     }

//     pub fn inc(self: *@This()) TypeID {
//         self.next += 1;
//         return self.next;
//     }

//     pub fn clone(self: *@This()) !*@This() {
//         const new_next = self.next;
//         const new_env = try self.env.clone();
//         const new_allocator = self.allocator;

//         var new_ctx = try self.allocator.create(Context);
//         new_ctx.allocator = new_allocator;
//         new_ctx.env = new_env;
//         new_ctx.next = new_next;

//         return new_ctx;
//     }

//     pub fn add(self: *@This(), name: []const u8, type_node: *TypeNode) !void {
//         try self.env.put(name, type_node);
//     }
// };
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
