allocator: std.mem.Allocator,

ast: []*const Expr,

type_nodes: std.ArrayListUnmanaged(TypeNode),

sems: std.AutoArrayHashMapUnmanaged(*const Expr, *TypeNode),

env: Env,

const TypeBits = u64;
const ANY_TYPE: TypeBits = 1 << 0;

const NUMBER_TYPE: TypeBits = 1 << 1 | ANY_TYPE;

const INT_TYPE: TypeBits = 1 << 2 | NUMBER_TYPE;
const I32_TYPE: TypeBits = 1 << 3 | INT_TYPE | TERMINAL_TYPE;
const I64_TYPE: TypeBits = 1 << 4 | INT_TYPE | TERMINAL_TYPE;

const FLOAT_TYPE: TypeBits = 1 << 5 | NUMBER_TYPE;
const F32_TYPE: TypeBits = 1 << 6 | FLOAT_TYPE | TERMINAL_TYPE;
const F64_TYPE: TypeBits = 1 << 7 | FLOAT_TYPE | TERMINAL_TYPE;

const BOOL_TYPE: TypeBits = 1 << 8 | ANY_TYPE | TERMINAL_TYPE;
const STRING_TYPE: TypeBits = 1 << 9 | ANY_TYPE | TERMINAL_TYPE;

const VOID_TYPE: TypeBits = 1 << 10 | TERMINAL_TYPE;

const TERMINAL_TYPE: TypeBits = 1 << 11;

pub const TypeID = enum(TypeBits) {
    any = ANY_TYPE,
    number = NUMBER_TYPE,
    int = INT_TYPE,
    i32 = I32_TYPE,
    i64 = I64_TYPE,
    float = FLOAT_TYPE,
    f32 = F32_TYPE,
    f64 = F64_TYPE,
    bool = BOOL_TYPE,
    string = STRING_TYPE,
    void = VOID_TYPE,

    pub fn is_subtype_of(child: TypeID, parent: TypeID) bool {
        return (@intFromEnum(parent) & @intFromEnum(child)) == @intFromEnum(parent);
    }

    pub fn is_terminal(self: TypeID) bool {
        return @intFromEnum(self) & TERMINAL_TYPE == TERMINAL_TYPE;
    }
};

const MonoType = struct {
    tid: TypeID,
};

const VarType = struct {
    name: []const u8,
    ref: *TypeNode,
};

pub const FunType = struct {
    name: []const u8,
    args: []*TypeNode,
    return_type: *TypeNode,

    pub fn is_generic(self: *const FunType) bool {
        if (!self.return_type.get_tid().is_terminal()) {
            return true;
        }

        for (self.args) |arg| {
            if (!arg.get_tid().is_terminal()) {
                return true;
            }
        }

        return false;
    }
};

pub const TypeNode = union(enum) {
    type: MonoType,
    variable: VarType,
    function: FunType,

    pub fn set_tid(self: *TypeNode, tid: TypeID) void {
        switch (self.*) {
            .type => {
                self.type.tid = tid;
            },
            .variable => {
                self.variable.ref.set_tid(tid);
            },
            .function => {
                self.function.return_type.set_tid(tid);
            },
        }
    }

    pub fn clone(self: *TypeNode) TypeNode {
        return switch (self.*) {
            .type => |*ty| .{ .type = .{ .tid = ty.tid } },
            .variable => |*variable| .{ .variable = .{ .ref = variable.clone() } },
            .function => {
                // you should not try to clone a function directly
                unreachable;
            },
            // .function => |*function| .{ .function = .{ .name = function.name, .return_type } },
        };
    }

    pub fn get_tid(self: TypeNode) TypeID {
        return switch (self) {
            .type => |*monotype| {
                return monotype.tid;
            },
            .variable => |*variable| {
                return variable.ref.get_tid();
            },
            .function => |*func| {
                return func.return_type.get_tid();
            },
        };
    }

    pub fn as_var(self: TypeNode) ?VarType {
        return if (tag(self) == .variable) self.variable else null;
    }

    pub fn as_function(self: TypeNode) ?FunType {
        return if (tag(self) == .function) self.function else null;
    }
};

const TypeError = error{
    TypeMismatch,
    UnknownType,
    UnknownBuiltin,
    WrongArgumentsNumber,
    AllocError,
    UnknownVariable,
    AnonymousFunctionsNotImplemented,
    FunctionArgumentsCanOnlyBeIdentifiers,
    AlreadyDefinedVariable,
    NonCallableExpression,
};

const Err = ErrorReporter(TypeError);

pub fn init(allocator: std.mem.Allocator, ast: []*Expr) @This() {
    return .{
        .allocator = allocator,
        .ast = ast,
        .env = Env.init(allocator),
        .type_nodes = .{},
        .sems = .{},
    };
}

pub fn infer_program(self: *@This()) anyerror!*std.AutoArrayHashMapUnmanaged(*const Expr, *TypeNode) {
    for (self.ast) |expr| {
        _ = try self.infer(expr);
    }

    try self.write_sems_to_file();

    return &self.sems;
}

pub fn infer(self: *@This(), expr: *const Expr) !*TypeNode {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            const node = try self.infer(const_init.initializer);

            const type_decl = try self.new_type_from_token(const_init.type);

            try unify(type_decl, node);

            try self.env.define(const_init.name.lexeme, node);

            self.put_sem(expr, node);

            return node;
        },
        .VarInit => |*var_init| {
            const node = try self.infer(var_init.initializer);

            const type_decl = try self.new_type_from_token(var_init.type);

            try unify(type_decl, node);

            try self.env.define(var_init.name.lexeme, node);

            self.put_sem(expr, node);

            return node;
        },
        .Grouping => |*grouping| {
            var node = try self.infer(grouping.expr);

            self.put_sem(expr, node);

            return node;
        },
        .Binary => |*binary| {
            //@todo:mem clean memory
            var args = try self.allocator.alloc(*const Expr, 2);

            args[0] = binary.left;
            args[1] = binary.right;

            defer self.allocator.free(args);

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
            );

            self.put_sem(expr, node);

            return node;
        },
        .Literal => |*literal| {
            const node = try self.new_type_node(
                .{
                    .type = .{
                        .tid = type_of(literal.value),
                    },
                },
            );
            const variable = try self.new_type_node(
                .{
                    .variable = .{ .name = "T", .ref = node }, // @todo make name other than T does not work in binary
                },
            );

            self.put_sem(expr, variable);

            return variable;
        },
        .Variable => |*variable| {
            const node = try self.env.get(variable.name.lexeme);

            self.put_sem(expr, node);

            return node;
        },
        .If => |*if_expr| {
            const condition = try self.infer(if_expr.condition);
            const bool_condition = try self.new_type(.bool);

            try unify(condition, bool_condition);
            self.put_sem(if_expr.condition, condition);

            const then_branch = try self.infer(if_expr.then_branch);

            if (if_expr.else_branch) |else_branch| {
                const else_branch_type = try self.infer(else_branch);
                try unify(then_branch, else_branch_type);
                self.put_sem(else_branch, else_branch_type);
            }

            self.put_sem(if_expr.then_branch, then_branch);

            const node = try self.new_type_node(
                .{
                    .variable = .{
                        .name = "if",
                        .ref = then_branch,
                    },
                },
            );

            self.put_sem(expr, node);

            return node;
        },
        .Block => |*block| {
            var return_node: ?*TypeNode = null;

            self.env.begin_local_scope();
            defer self.env.end_local_scope();

            for (block.exprs) |block_expr| {
                return_node = try self.infer(block_expr);
            }

            if (return_node == null) {
                return_node = try self.new_type(.void);
            }

            self.put_sem(expr, return_node.?);

            return return_node.?;
        },
        .Import => {
            const node = try self.new_type(.any);
            self.put_sem(expr, node);
            return node;
        },
        .Call => |*call_expr| {
            const callee = try self.env.get(call_expr.callee.Variable.name.lexeme);
            if (callee.as_function()) |*func| {
                if (func.is_generic()) {
                    // var new_func = try self.clone_function_type(function);
                    // unreachable;
                }

                const node = try self.call(func.*, call_expr.args);
                self.put_sem(expr, node);
                return node;
            } else {
                return TypeError.NonCallableExpression;
            }
        },
        .Function => |*func| {
            const node = try self.function(func);

            self.put_sem(expr, node);

            return node;
        },
        else => unreachable,
    };
}

fn call(self: *@This(), func: FunType, exprs_args: []*const Expr) anyerror!*TypeNode {
    if (func.args.len != exprs_args.len) {
        return TypeError.WrongArgumentsNumber;
    }

    // @todo: just reuse one type scope. Or use another more generic object
    var type_scope: *TypeScope = try self.allocator.create(TypeScope);
    type_scope.* = TypeScope.init(self.allocator);
    try type_scope.ensureTotalCapacity(@intCast(func.args.len));
    defer type_scope.deinit();

    for (exprs_args, func.args) |expr_arg, function_arg| {
        const arg = try self.infer(expr_arg);

        // @warning: watch this crap
        arg.variable.name = function_arg.variable.name;

        const call_arg = try self.get_or_create_local_node(
            function_arg,
            arg,
            type_scope,
        );

        try unify(
            call_arg,
            arg,
        );

        // Variable binding !
        if (tag(call_arg.*) == .variable and tag(arg.*) == .variable and call_arg != arg) {
            if (std.mem.eql(u8, function_arg.variable.name, call_arg.variable.name)) {
                arg.variable.ref = call_arg;
            }
        }

        self.put_sem(
            expr_arg,
            call_arg,
        );
    }

    return try self.get_local_node(func.return_type, type_scope);
}
// @todo: why anyerror
fn function(self: *@This(), func: *const Expr.Function) anyerror!*TypeNode {
    if (func.name == null) {
        return TypeError.AnonymousFunctionsNotImplemented;
    }

    self.env.begin_local_scope();
    defer self.env.end_local_scope();

    const return_type = try self.new_var_from_token("FuncRet", func.type);

    var function_type: FunType = .{
        .name = func.name.?.lexeme,
        .args = if (func.args) |args|
            try self.allocator.alloc(*TypeNode, args.len)
        else
            &[_]*TypeNode{},
        .return_type = return_type,
    };

    if (func.args) |args| {
        for (args, 0..) |arg, i| {
            const node = try self.new_var_from_token("Arg", arg.type);
            try self.env.define(arg.expr.Variable.name.lexeme, node);

            function_type.args[i] = node;
        }
    }

    const body_type = try self.infer(func.body);

    try unify(return_type, body_type);

    return try self.new_type_node(.{ .function = function_type });
}

fn get_or_create_local_node(self: *@This(), base_node: *TypeNode, local_node: *TypeNode, scope: *TypeScope) !*TypeNode {
    if (base_node.as_var()) |variable| {
        if (scope.get(variable.name)) |registered_variable| {
            return registered_variable;
        } else {
            const new_var = switch (local_node.*) {
                .variable => local_node, // unify here ?
                else => unreachable,
                // .type => try self.create_type_node(
                //     .{
                //         .variable = .{
                //             .name = base_node.variable.name,
                //             .ref = local_node,
                //         },
                //     },
                // ),
            };
            // Maybe the local node is not of the base node type.
            // so let's clone it and unify
            const base_node_copy = try self.new_type_node(base_node.*);

            try unify(new_var, base_node_copy);

            try scope.put(new_var.variable.name, new_var);

            return new_var;
        }
    }

    // return try ctx.create_var_instance(new_var);
    return try self.new_type_node(base_node.*);
}

fn get_local_node(self: *@This(), node: *TypeNode, scope: *TypeScope) !*TypeNode {
    if (node.as_var()) |variable| {
        if (scope.get(variable.name)) |registered_variable| {
            return registered_variable;
        }
    }
    return try self.new_type_node(node.*);
}

fn unify(node_a: *TypeNode, node_b: *TypeNode) !void {
    const a = node_a.get_tid();
    const b = node_b.get_tid();

    if (b.is_subtype_of(a)) {
        node_a.set_tid(b);
    } else if (a.is_subtype_of(b)) {
        node_b.set_tid(a);
    } else {
        std.debug.print(
            "\nType Mismatch --\nExpected : {}\nFound : {}\n",
            .{ a, b },
        );
        return TypeError.TypeMismatch;
    }
}

fn new_type_node(self: *@This(), type_node: TypeNode) !*TypeNode {
    var tn_ptr = try self.type_nodes.addOne(self.allocator);
    tn_ptr.* = type_node;
    return tn_ptr;
}

fn new_type(self: *@This(), tid: TypeID) !*TypeNode {
    return try self.new_type_node(
        .{
            .type = .{ .tid = tid },
        },
    );
}

fn new_type_from_token(self: *@This(), maybe_token: ?*const Token) !*TypeNode {
    return try self.new_type_node(
        .{
            .type = .{
                .tid = if (maybe_token) |token|
                    try type_from_str(token.lexeme)
                else
                    .any,
            },
        },
    );
}

fn new_var_from_token(self: *@This(), name: []const u8, maybe_token: ?*const Token) !*TypeNode {
    return try self.new_type_node(
        .{
            .variable = .{
                .ref = try self.new_type_from_token(maybe_token),
                .name = name,
            },
        },
    );
}

fn clone_function_type(self: *@This(), func: *FunType) !*FunType {
    const return_type = self.new_type_node(func.return_type.clone());
    const args = try self.allocator.alloc(*TypeNode, func.args.len);
    for (func.args, 0..) |arg, i| {
        args[i] = self.new_type_node(arg.clone());
    }
    const function_type: FunType = .{
        .name = func.name.?.lexeme,
        .args = args,
        .return_type = return_type,
    };

    return try self.new_type_node(
        .{
            .function = function_type,
        },
    );
}

fn type_of(value: Expr.Literal.Value) TypeID {
    return switch (value) {
        .String => .string,
        .Number => |number| if (is_float_value(number)) .float else .number,
        .Boolean => .bool,
        else => unreachable,
    };
}

fn put_sem(self: *@This(), expr: *const Expr, node: *TypeNode) void {
    // @todo:err try to find a way to handle the allocations error
    self.sems.put(self.allocator, expr, node) catch unreachable;
}

inline fn is_float_value(number: f64) bool {
    // this is stupid, number should be a string and just check if it has a dot in it
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
    // use meta functions for enums ?
    if (std.mem.eql(u8, str, "i32")) {
        return .i32;
    } else if (std.mem.eql(u8, str, "i64")) {
        return .i64;
    } else if (std.mem.eql(u8, str, "f32")) {
        return .f32;
    } else if (std.mem.eql(u8, str, "f64")) {
        return .f64;
    } else if (std.mem.eql(u8, str, "void")) {
        return .void;
    } else if (std.mem.eql(u8, str, "bool")) {
        return .bool;
    } else if (std.mem.eql(u8, str, "Number")) {
        return .number;
    } else if (std.mem.eql(u8, str, "Float")) {
        return .float;
    } else if (std.mem.eql(u8, str, "Any")) {
        return .any;
    } else {
        return TypeError.UnknownType;
    }
}

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
            else => {},
        },
        *TypeNode => switch (data.*) {
            .type => |ty| {
                std.debug.print("\n [MonoType]: {} \n", .{ty});
            },
            .variable => |variable| {
                std.debug.print("\n [Variable]: {s}\n", .{variable.name});
                std.debug.print("\n TID: {} \n", .{variable.ref.type.tid});
                std.debug.print("\n $REF_PTR: {} \n", .{@intFromPtr(variable.ref)});

                std.debug.print("\n", .{});
            },
        },
        else => @compileError("Wrong type in pretty print"),
    }
    std.debug.print("\n", .{});
}

const BaseTypes = blk: {
    var map: std.EnumMap(TypeID, TypeNode) = .{};

    inline for (std.meta.fields(TypeID)) |tid| {
        map.put(
            @enumFromInt(tid.value),
            TypeNode{
                .type = .{
                    .tid = @enumFromInt(tid.value),
                },
            },
        );
    }

    break :blk map;
};

fn make_type(tid: TypeID) TypeNode {
    return BaseTypes.get(tid).?;
}

fn make_vartype(
    name: []const u8,
    ref: *TypeNode,
) TypeNode {
    return TypeNode{
        .variable = .{
            .name = name,
            .ref = ref,
        },
    };
}

var number_node = make_type(.number);
var number_var = make_vartype("T", &number_node);

var bool_node = make_type(.bool);

var any_node = make_type(.any);
var any_var = make_vartype("T", &any_node);

var add_args: [2]*TypeNode = .{
    &number_var,
    &number_var,
};
var add_return_type = make_vartype("T", &number_node);

var sub_args: [2]*TypeNode = .{
    &number_var,
    &number_var,
};
var sub_return_type = make_vartype("T", &number_node);

var mul_args: [2]*TypeNode = .{
    &number_var,
    &number_var,
};
var mul_return_type = make_vartype("T", &number_node);

var div_args: [2]*TypeNode = .{
    &number_var,
    &number_var,
};
var div_return_type = make_vartype("T", &number_node);

var equal_equal_args: [2]*TypeNode = .{
    &any_var,
    &any_var,
};
var equal_equal_return_type = make_type(.bool);

var bang_equal_args: [2]*TypeNode = .{
    &any_var,
    &any_var,
};
var bang_equal_return_type: TypeNode = make_type(.bool);

var greater_args: [2]*TypeNode = .{
    &number_var,
    &number_var,
};
var greater_return_type: TypeNode = make_type(.bool);

var greater_equal_args: [2]*TypeNode = .{
    &number_var,
    &number_var,
};
var greater_equal_return_type: TypeNode = make_type(.bool);

var less_args: [2]*TypeNode = .{
    &number_var,
    &number_var,
};
var less_return_type: TypeNode = make_type(.bool);

var less_equal_args: [2]*TypeNode = .{
    &number_var,
    &number_var,
};
var less_equal_return_type: TypeNode = make_type(.bool);

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

pub fn log_sems(self: *@This()) !void {
    std.debug.print("\n#####SEMS-STATE\n", .{});
    var iter = self.sems.iterator();
    while (iter.next()) |entry| {
        var ptr = switch (entry.value_ptr.*.*) {
            .variable => |variable| variable.ref,
            .type => entry.value_ptr.*,
        };
        pretty_print(entry.key_ptr.*);
        std.debug.print("\nREF_PTR: {}\n", .{@intFromPtr(
            ptr,
        )});
        std.debug.print("\nNODE_PTR: {}\n\n", .{@intFromPtr(
            entry.value_ptr.*,
        )});
    }
}

pub fn write_sems_to_file(self: *@This()) !void {
    var types = std.ArrayList(struct {
        type: *TypeNode,
        expr: *const Expr,
        ptr: usize,
    }).init(self.allocator);

    var iter = self.sems.iterator();
    while (iter.next()) |entry| {
        var ptr = switch (entry.value_ptr.*.*) {
            .variable => |variable| variable.ref,
            .type, .function => entry.value_ptr.*,
        };
        try types.append(
            .{
                .type = entry.value_ptr.*,
                .expr = entry.key_ptr.*,
                .ptr = @intFromPtr(
                    ptr,
                ),
            },
        );
    }

    try jsonPrint(types.items, "./types.json");
}

const Env = struct {
    allocator: std.mem.Allocator,
    local: Scope,
    global: Scope,

    current_depth: u32 = 0,

    pub fn init(allocator: std.mem.Allocator) Env {
        return .{
            .allocator = allocator,
            .global = Scope.init(allocator),
            .local = Scope.init(allocator),
        };
    }

    pub fn define(self: *Env, name: []const u8, node: *TypeNode) !void {
        if (self.in_global_scope()) {
            try self.define_global(name, node);
        } else {
            try self.define_local(name, node);
        }
    }

    pub fn define_global(self: *Env, name: []const u8, node: *TypeNode) !void {
        try self.global.define(name, node);
    }

    pub fn define_local(self: *Env, name: []const u8, node: *TypeNode) !void {
        return if (self.global.has(name))
            TypeError.AlreadyDefinedVariable
        else
            try self.local.define(name, node);
    }

    pub fn begin_local_scope(self: *Env) void {
        self.current_depth += 1;
    }

    pub fn end_local_scope(self: *Env) void {
        self.current_depth -= 1;
        if (self.in_global_scope()) {
            self.local.clear();
        }
    }

    pub fn in_global_scope(self: *Env) bool {
        return self.current_depth == 0;
    }

    pub fn get(self: *Env, name: []const u8) !*TypeNode {
        if (self.in_global_scope()) {
            return self.global.get(name);
        }

        return self.local.get(name) catch |err| switch (err) {
            TypeError.UnknownVariable => try self.global.get(name),
            else => |other_error| other_error,
        };
    }

    pub fn deinit(self: *Env) void {
        self.global.deinit();
        self.local.deinit();
    }
};

const Scope = struct {
    allocator: std.mem.Allocator,
    values: std.StringHashMapUnmanaged(*TypeNode),

    pub fn init(allocator: std.mem.Allocator) Scope {
        return .{
            .allocator = allocator,
            .values = .{},
        };
    }

    pub fn deinit(self: *Scope) void {
        self.values.deinit(self.allocator);
    }

    pub fn define(self: *Scope, name: []const u8, node: *TypeNode) !void {
        const result = try self.values.getOrPut(self.allocator, name);
        if (result.found_existing) return TypeError.AlreadyDefinedVariable;
        result.value_ptr.* = node;
    }

    pub fn clear(self: *Scope) void {
        self.values.clearAndFree(self.allocator);
    }

    pub fn get(self: *Scope, name: []const u8) !*TypeNode {
        return self.values.get(name) orelse TypeError.UnknownVariable;
    }

    pub fn has(self: *Scope, name: []const u8) bool {
        return self.values.contains(name);
    }
};

const TypeScope = std.StringHashMap(*TypeNode);

const std = @import("std");

const Expr = @import("./ast/expr.zig").Expr;
const Type = @import("./types.zig").Type;
const Token = @import("./token.zig");
const floatMax = std.math.floatMax;
const maxInt = std.math.maxInt;
const ErrorReporter = @import("./error-reporter.zig").ErrorReporter;
const tag = std.meta.activeTag;
