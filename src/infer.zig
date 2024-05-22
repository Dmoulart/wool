allocator: std.mem.Allocator,

ast: []*const Expr,

type_nodes: std.ArrayListUnmanaged(TypeNode),

sems: std.ArrayListUnmanaged(Sem),

typed_ast: std.ArrayListUnmanaged(*Sem),

env: Env,

err: Errors(InferError),

src: []const u8,

const Infer = @This();

pub const Sem = Typed(Expr);

pub fn sem_type(sem: *Sem) *TypeNode {
    return switch (sem.*) {
        inline else => |*any_sem| any_sem.type_node,
    };
}

pub fn get_sem_tid(sem: *Sem) TypeID {
    return switch (sem.*) {
        inline else => |*any_sem| any_sem.type_node.get_tid(),
    };
}

pub fn as_sems(ptr: *anyopaque) []*Sem {
    return @alignCast(@ptrCast(ptr));
}

pub fn as_sem(ptr: *anyopaque) *Sem {
    return @alignCast(@ptrCast(ptr));
}

const TypeBits = u32;

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

const FUNC_TYPE: TypeBits = 1 << 10 | ANY_TYPE | TERMINAL_TYPE;

const VOID_TYPE: TypeBits = 1 << 11 | ANY_TYPE | TERMINAL_TYPE;

const TERMINAL_TYPE: TypeBits = 1 << 12;

const EXTERN_FUNC_TYPE: TypeBits = 1 << 13 | ANY_TYPE;

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
    func = FUNC_TYPE,
    extern_func = EXTERN_FUNC_TYPE, //@temp we'll use type expressions later

    pub fn is_subtype_of(child: TypeID, parent: TypeID) bool {
        return (@intFromEnum(parent) & @intFromEnum(child)) == @intFromEnum(parent);
    }

    pub fn is_terminal(self: TypeID) bool {
        return @intFromEnum(self) & TERMINAL_TYPE == TERMINAL_TYPE;
    }

    pub fn ToZigType(self: TypeID) type {
        return switch (self) {
            .i32 => i32,
            .i64 => i64,
            .f32 => f32,
            .f64 => f64,
            else => unreachable,
        };
    }

    pub fn from_zig_type(comptime T: type) TypeID {
        return switch (T) {
            i32 => .i32,
            i64 => .i64,
            f32 => .f32,
            f64 => .f64,
            else => unreachable,
        };
    }

    pub fn TypeOf(comptime self: TypeID) TypeID {
        return switch (self) {
            .i32 => i32,
            .i64 => i64,
            .f32 => f32,
            .f64 => f64,
            else => unreachable,
        };
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
    name: ?[]const u8,
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
                unreachable;
            },
        }
    }
    // move this in Infer methods
    pub fn clone(self: *TypeNode, in: *Infer) !*TypeNode {
        return switch (self.*) {
            .type => |*ty| try in.new_type(ty.tid),
            .variable => |*variable| try in.new_type_node(
                .{
                    .variable = .{ .name = variable.name, .ref = try variable.ref.clone(in) },
                },
            ),
            .function => |*func| try in.new_type_node(
                .{
                    .function = .{
                        .name = func.name,
                        .return_type = try func.return_type.clone(in),
                        .args = blk: {
                            //@todo:mem cleanup
                            const new_args = try in.allocator.alloc(*TypeNode, func.args.len);
                            for (new_args) |new_arg| {
                                new_arg.* = (try new_arg.clone(in)).*;
                            }
                            break :blk new_args;
                        },
                    },
                },
            ),
        };
    }

    pub fn get_tid(self: *TypeNode) TypeID {
        return switch (self.*) {
            .type => |*monotype| {
                return monotype.tid;
            },
            .variable => |*variable| {
                return variable.ref.get_tid();
            },
            .function => {
                return .func;
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

pub const InferError = error{
    TypeMismatch,
    UnknownType,
    UnknownBuiltin,
    WrongArgumentsNumber,
    AllocError,
    UnknownVariable,
    UnknownFunction,
    AnonymousFunctionsNotImplemented,
    FunctionArgumentsCanOnlyBeIdentifiers,
    AlreadyDefinedVariable,
    AlreadyDefinedFunction,
    NonCallableExpression,
    GenericFunctionNotImplemented,
    CannotResolveType,
    CircularReference,
    UnknownError,
};

pub fn init(allocator: std.mem.Allocator, ast: []*Expr, src: []const u8) @This() {
    const err = Errors(InferError).init(allocator, src);

    return .{
        .allocator = allocator,
        .ast = ast,
        .env = Env.init(allocator, err),
        .type_nodes = .{},
        .sems = .{},
        .typed_ast = .{},
        .err = err,
        .src = src,
    };
}

pub fn infer_program(self: *@This()) anyerror![]*Sem {
    for (self.ast) |expr| {
        const typed_expr = try self.infer(expr);
        try self.typed_ast.append(self.allocator, typed_expr);
    }

    // try self.write_sems_to_file();

    return self.typed_ast.items;
}

pub fn infer(self: *@This(), expr: *const Expr) !*Sem {
    return switch (expr.*) {
        .ConstInit => |*const_init| {
            const typed_initializer = try self.infer(const_init.initializer);

            const const_type = if (const_init.type) |type_token|
                try self.new_type_from_token(type_token)
            else
                try self.new_type(.any);

            try unify(const_type, sem_type(typed_initializer));

            try self.env.define(const_init.name, sem_type(typed_initializer));

            return try self.create_sem(
                .{
                    .ConstInit = .{
                        .initializer = typed_initializer,
                        .type_node = try self.new_type(.void),
                        .orig_expr = expr,
                    },
                },
            );
        },
        .VarInit => |*var_init| {
            const typed_initializer = try self.infer(var_init.initializer);

            const var_type = if (var_init.type) |type_token|
                try self.new_type_from_token(type_token)
            else
                try self.new_type(.any);

            try unify(var_type, sem_type(typed_initializer));

            try self.env.define(var_init.name, sem_type(typed_initializer));

            return try self.create_sem(
                .{
                    .VarInit = .{
                        .initializer = typed_initializer,
                        .orig_expr = expr,
                        .type_node = try self.new_type(.void),
                    },
                },
            );
        },
        .Assign => |*assign| {
            const variable_type = try self.env.get(assign.name);

            const typed_assignation = try self.infer(assign.value);

            try unify(variable_type, sem_type(typed_assignation));

            return try self.create_sem(
                .{
                    .Assign = .{
                        .value = typed_assignation,
                        .orig_expr = expr,
                        .type_node = try self.new_type(.void),
                    },
                },
            );
        },
        .Grouping => |*grouping| {
            const typed_inner_expr = try self.infer(grouping.expr);
            const grouping_type = sem_type(typed_inner_expr);

            return try self.create_sem(
                .{
                    .Grouping = .{
                        .expr = typed_inner_expr,
                        .orig_expr = expr,
                        .type_node = grouping_type,
                    },
                },
            );
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
                else => InferError.UnknownBuiltin,
            };
            // dangling ptr ? smelly ?
            const builtin = &builtins_types.get(builtin_name).?;

            const call_infos = try self.call(
                builtin,
                args,
            );
            // const sem = Sem.init(
            //     expr,
            //     .{
            //         .Binary = .{
            //             .expr = node,
            //         },
            //     },
            //     node,
            // );

            // try self.create_sem(sem);
            return try self.create_sem(
                .{
                    .Binary = .{
                        .left = call_infos.args[0],
                        .right = call_infos.args[1],
                        .orig_expr = expr,
                        .type_node = call_infos.return_type,
                    },
                },
            );
        },
        .Literal => |*literal| {
            const literal_type = try self.new_type_node(
                .{
                    .variable = .{
                        .name = "T", // @todo make name other than T does not work in binary
                        .ref = try self.new_type(type_of(literal.value)),
                    },
                },
            );

            return try self.create_sem(
                .{
                    .Literal = .{
                        .orig_expr = expr,
                        .type_node = literal_type,
                    },
                },
            );
        },
        .Variable => |*variable| {
            const variable_type = try self.env.get(variable.name);

            return try self.create_sem(
                .{
                    .Variable = .{
                        .orig_expr = expr,
                        .type_node = variable_type,
                    },
                },
            );
        },
        .If => |*if_expr| {
            const typed_condition = try self.infer(if_expr.condition);

            try unify(
                sem_type(typed_condition),
                try self.new_type(.bool),
            );

            const typed_then_branch = try self.infer(if_expr.then_branch);

            const if_type = try self.new_type_node(
                .{
                    .variable = .{
                        .name = "if",
                        .ref = sem_type(typed_then_branch),
                    },
                },
            );

            var maybe_typed_else_branch: ?*Sem = null;

            if (if_expr.else_branch) |else_branch| {
                maybe_typed_else_branch = try self.infer(else_branch);
                try unify(sem_type(typed_then_branch), sem_type(maybe_typed_else_branch.?));
                //@todo pay attention to circular references !! This can cause segfaults
                try bind(sem_type(maybe_typed_else_branch.?), sem_type(typed_then_branch));
            }

            return try self.create_sem(
                .{
                    .If = .{
                        .condition = typed_condition,
                        .then_branch = typed_then_branch,
                        .else_branch = maybe_typed_else_branch,
                        .orig_expr = expr,
                        .type_node = if_type,
                    },
                },
            );
        },
        .Block => |*block| {
            var typed_inner_exprs = try self.allocator.alloc(*anyopaque, block.exprs.len);

            self.env.begin_local_scope();

            for (block.exprs, 0..) |block_expr, i| {
                typed_inner_exprs[i] = try self.infer(block_expr);
            }

            const block_return_type = if (block.exprs.len > 0)
                sem_type(@alignCast(@ptrCast(typed_inner_exprs[block.exprs.len - 1])))
            else
                try self.new_type(.void);

            try self.env.end_local_scope(false); // @todo generic block ?????

            return try self.create_sem(
                .{
                    .Block = .{
                        .exprs = typed_inner_exprs,
                        .orig_expr = expr,
                        .type_node = block_return_type,
                    },
                },
            );
        },
        .Import => {
            const args = try self.allocator.alloc(*TypeNode, 1);
            args[0] = try self.new_type(.i32);

            const node = try self.new_type_node(
                .{
                    .function = .{
                        .args = args,
                        .name = null,
                        .return_type = try self.new_type(.void),
                    },
                },
            );

            return try self.create_sem(
                .{
                    .Import = .{
                        .type_node = node,
                        .orig_expr = expr,
                    },
                },
            );
        },
        .While => |*while_expr| {
            const condition = try self.infer(while_expr.condition);
            const bool_condition = try self.new_type(.bool);

            try unify(bool_condition, sem_type(condition));

            const body = try self.infer(while_expr.body);

            return try self.create_sem(.{
                .While = .{
                    .inc = null,
                    .condition = condition,
                    .body = body,
                    .orig_expr = expr,
                    .type_node = sem_type(body),
                },
            });
        },
        .Call => |*call_expr| {
            const function_name = call_expr.callee.Variable.name;
            const callee = try self.env.get(function_name);
            if (callee.as_function()) |*func| {
                // @todo func.is_generic() and current context is concrete function
                if (func.is_generic()) {
                    return InferError.GenericFunctionNotImplemented;
                    // const func_expr = try self.env.get_function(func.name);
                    // const new_func = try self.instanciate_function(callee, func_expr, call_expr.args);
                    // const node = try self.call(&new_func.function, call_expr.args);
                    // pretty_print(new_func);
                    // return node;
                }

                const call_infos = try self.call(func, call_expr.args);

                return try self.create_sem(
                    .{
                        .Call = .{
                            .args = @alignCast(@ptrCast(call_infos.args)),
                            .callee = callee,
                            .type_node = call_infos.return_type,
                            .orig_expr = expr,
                        },
                    },
                );
            } else {
                return InferError.NonCallableExpression;
            }
        },
        .Function => {
            const function_infos = try self.function(
                expr,
                null,
                null,
            );
            // try self.env.define_function(func.name.?.lexeme, expr);

            return try self.create_sem(
                .{
                    .Function = .{
                        .type_node = function_infos.type_node,
                        .orig_expr = expr,
                        .body = function_infos.body,
                    },
                },
            );
            // return node;
        },
        .Logical => |logical| {
            const left = try self.infer(logical.left);
            const right = try self.infer(logical.right);

            try unify(sem_type(left), sem_type(right));
            //@todo pay attention to circular references !! This can cause segfaults
            try bind(sem_type(right), sem_type(left));

            const node = try self.new_type_node(
                .{
                    .variable = .{
                        .name = "logical",
                        .ref = sem_type(left),
                    },
                },
            );

            return try self.create_sem(
                .{
                    .Logical = .{
                        .type_node = node,
                        .orig_expr = expr,
                        .left = left,
                        .right = right,
                    },
                },
            );
        },
        else => unreachable,
    };
}

fn call(
    self: *@This(),
    func: *const FunType,
    exprs_args: []*const Expr,
) anyerror!struct { args: []*Sem, return_type: *TypeNode } {
    if (func.args.len != exprs_args.len) {
        return InferError.WrongArgumentsNumber;
    }

    // @todo: just reuse one type scope. Or use another more generic object
    var type_scope: *TypeScope = try self.allocator.create(TypeScope);
    type_scope.* = TypeScope.init(self.allocator);
    try type_scope.ensureTotalCapacity(@intCast(func.args.len));
    defer type_scope.deinit();

    var args_sems = try self.allocator.alloc(*Sem, exprs_args.len);

    for (exprs_args, func.args, 0..) |expr_arg, function_arg, i| {
        const arg = try self.infer(expr_arg);

        var arg_type = sem_type(arg);

        // @warning: watch this crap
        if (tag(arg_type.*) == .variable and tag(function_arg.*) == .variable) {
            arg_type.variable.name = function_arg.variable.name;
        }

        const call_arg = try self.get_or_create_local_node(
            function_arg,
            arg_type,
            type_scope,
        );

        try unify(
            call_arg,
            sem_type(arg),
        );

        // Variable binding !
        if (tag(call_arg.*) == .variable and tag(arg_type.*) == .variable and call_arg != arg_type) {
            if (std.mem.eql(u8, function_arg.variable.name, call_arg.variable.name)) {
                arg_type.variable.ref = call_arg;
                //@todo use bind
            }
        }

        args_sems[i] = arg;
    }

    return .{
        .args = args_sems,
        .return_type = try self.get_local_node(func.return_type, type_scope),
    };
}

// fn function2(self: *@This(), expr: *const Expr, maybe_args: ?[]*TypeNode, maybe_return_type: ?*TypeNode) anyerror!*TypeNode {
//     const func_expr = expr.Function;

//     // if (func_expr.name == null) {
//     //     return TypeError.AnonymousFunctionsNotImplemented;
//     // }

//     if (maybe_args) |args| {
//         if (func_expr.args != null and args.len != func_expr.args.?.len) {
//             return TypeError.WrongArgumentsNumber;
//         }
//     }

//     self.env.begin_local_scope();

//     const return_type = try self.new_var_from_token("T", func_expr.type);

//     var function_type: FunType = .{
//         // .name = func_expr.name.?.lexeme,
//         .name = null,
//         .args = if (func_expr.args) |args|
//             try self.allocator.alloc(*TypeNode, args.len)
//         else
//             &[_]*TypeNode{},
//         .return_type = return_type,
//     };

//     if (func_expr.args) |args| {
//         for (args, 0..) |arg, i| {
//             const node = try self.new_var_from_token("T", arg.type);
//             try self.env.define(arg.expr.Variable.name.lexeme, node);

//             function_type.args[i] = node;

//             if (maybe_args) |optional_args| {
//                 try unify(optional_args[i], node);
//             }
//         }
//     }

//     const body = try self.infer(func_expr.body);

//     try unify(return_type, body);

//     if (maybe_return_type) |optional_return_type| {
//         try unify(optional_return_type, return_type);
//     }

//     try self.env.end_local_scope(!function_type.is_generic());

//     // pretty_print(body);

//     return try self.new_type_node(.{ .function = function_type });
// }

// @todo: why anyerror
fn function(self: *@This(), expr: *const Expr, maybe_args: ?[]*TypeNode, maybe_return_type: ?*TypeNode) anyerror!struct { body: *Sem, type_node: *TypeNode } {
    const func_expr = expr.Function;

    // if (func_expr.name == null) {
    //     return TypeError.AnonymousFunctionsNotImplemented;
    // }

    if (maybe_args) |args| {
        if (func_expr.args != null and args.len != func_expr.args.?.len) {
            return InferError.WrongArgumentsNumber;
        }
    }

    self.env.begin_local_scope();

    const return_type = if (func_expr.type) |token_type|
        try self.new_var_type_from_token("T", token_type)
    else
        try self.new_var_type("T", try self.new_type(.any));

    var function_type: FunType = .{
        // .name = func_expr.name.?.lexeme,
        .name = null,
        .args = if (func_expr.args) |args|
            try self.allocator.alloc(*TypeNode, args.len)
        else
            &[_]*TypeNode{},
        .return_type = return_type,
    };

    if (func_expr.args) |args| {
        for (args, 0..) |arg, i| {
            const node = if (arg.type) |type_token|
                try self.new_var_type_from_token("T", type_token)
            else
                try self.new_var_type("T", try self.new_type(.any));

            try self.env.define(arg.expr.Variable.name, node);

            function_type.args[i] = node;

            if (maybe_args) |optional_args| {
                try unify(optional_args[i], node);
            }
        }
    }

    const body = try self.infer(func_expr.body);

    try unify(return_type, sem_type(body));

    if (maybe_return_type) |optional_return_type| {
        try unify(optional_return_type, return_type);
    }

    // pretty_print(return_type);
    try self.env.end_local_scope(!function_type.is_generic());

    const type_node = try self.new_type_node(.{ .function = function_type });

    return .{
        .body = body,
        .type_node = type_node,
    };
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
        return InferError.TypeMismatch;
    }
}

fn bind(from: *TypeNode, to: *TypeNode) !void {
    //@todo: not sure about this
    if (@intFromPtr(to) == @intFromPtr(from.variable.ref)) {
        return InferError.CircularReference;
    }

    from.variable.ref = to;
}

fn new_type_node(self: *@This(), type_node: TypeNode) !*TypeNode {
    const type_node_ptr = try self.type_nodes.addOne(self.allocator);
    type_node_ptr.* = type_node;
    return type_node_ptr;
}

fn new_type(self: *@This(), tid: TypeID) !*TypeNode {
    return try self.new_type_node(
        .{
            .type = .{
                .tid = tid,
            },
        },
    );
}

fn new_type_from_token(self: *@This(), token: *const Token) !*TypeNode {
    return try self.new_type_node(
        .{
            .type = .{
                .tid = try tid_from_str(token.lexeme),
            },
        },
    );
}

fn new_var_type_from_token(self: *@This(), name: []const u8, token: *const Token) !*TypeNode {
    return try self.new_type_node(
        .{
            .variable = .{
                .ref = try self.new_type_from_token(token),
                .name = name,
            },
        },
    );
}

fn new_var_type(self: *@This(), name: []const u8, type_node: *TypeNode) !*TypeNode {
    return try self.new_type_node(
        .{
            .variable = .{
                .ref = type_node,
                .name = name,
            },
        },
    );
}

fn instanciate_function(
    self: *@This(),
    func_node: *const TypeNode,
    expr: *const Expr,
    new_args: []*const Expr,
) anyerror!*TypeNode {
    const func_type = func_node.function;

    if (func_type.args.len != new_args.len) {
        return InferError.WrongArgumentsNumber;
    }

    // use the func.clone method ?
    const return_type = try func_type.return_type.clone(self);
    const args = try self.allocator.alloc(*TypeNode, func_type.args.len);

    for (func_type.args, 0..) |arg, i| {
        args[i] = try arg.clone(self);
    }

    for (new_args, 0..) |expr_arg, i| {
        const arg_node = try self.infer(expr_arg);
        // mutate cloned function args
        try unify(arg_node, args[i]);
    }

    const function_type: FunType = .{
        .name = func_type.name,
        .args = args,
        .return_type = return_type,
    };

    return try self.function(
        expr,
        function_type.args,
        function_type.return_type,
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

// fn get_bit_size(value: u64) u64 {
//     var number: u64 = value;
//     var count: u64 = 0;
//     while (number > 0) {
//         count += 1;
//         number >>= 1; // Right shift the number by 1 bit
//     }
//     return count;
// }

// pub fn size_of(value: Expr.Literal.Value) !u64 {
//     return switch (value) {
//         .String => value.String.len * 8, // @todo real string size measurement ?
//         .Number => |number| blk: {
//             var number_value = try std.fmt.parseFloat(f64, number);
//             std.debug.print("\nparsed float {}\n", .{number_value});
//             var unsigned: u64 = @bitCast(number_value);
//             std.debug.print("\n unsigned {}\n", .{unsigned});
//             break :blk get_bit_size(unsigned);
//         },
//         .Boolean => 1,
//         else => unreachable,
//     };
// }

fn create_sem(self: *@This(), sem: Sem) !*Sem {
    const sem_ptr = try self.sems.addOne(self.allocator);
    sem_ptr.* = sem;
    return sem_ptr;
}

inline fn is_float_value(number_str: []const u8) bool {
    return includes_char(number_str, '.');
}

fn includes_char(haystack: []const u8, needle: u8) bool {
    for (haystack) |char| {
        if (char == needle) {
            return true;
        }
    }
    return false;
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

pub fn tid_from_str(str: []const u8) !TypeID {
    // optimize this
    if (std.mem.indexOf(u8, str, "->")) |_| {
        return .func;
    } else if (std.mem.eql(u8, str, "i32")) {
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
        return InferError.UnknownType;
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
                std.debug.print("\n TID: {} \n", .{variable.ref.get_tid()});
                std.debug.print("\n $REF_PTR: {} \n", .{@intFromPtr(variable.ref)});

                std.debug.print("\n", .{});
            },
            .function => |func| {
                std.debug.print("\n [Function]: {s}\n", .{func.name});
                std.debug.print("\n - Return Type : \n", .{});
                pretty_print(func.return_type);

                std.debug.print("\n - Args: \n", .{});
                for (func.args) |arg| {
                    pretty_print(arg);
                }
            },
        },
        else => @compileError("Wrong type in pretty print"),
    }
    std.debug.print("\n", .{});
}

const BaseTypes = blk: {
    var map: std.EnumMap(TypeID, TypeNode) = .{};

    for (std.meta.fields(TypeID)) |tid| {
        map.put(
            @enumFromInt(tid.value),
            TypeNode{
                .type = .{
                    .tid = @enumFromInt(tid.value),
                    // .size = null,
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

pub fn write_sems_to_file2(self: *@This()) !void {
    std.debug.print("\n{any}\n", .{self.sems.items});
}

const Env = struct {
    allocator: std.mem.Allocator,
    local: Scope,
    global: Scope,
    err: Errors(InferError),

    // scopes: std.ArrayListUnmanaged(Scope),

    current_depth: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, err: Errors(InferError)) Env {
        return .{
            .allocator = allocator,
            .global = Scope.init(allocator),
            .local = Scope.init(allocator),
            // .scopes = .{},
            .err = err,
        };
    }

    pub fn define(self: *Env, token: *const Token, node: *TypeNode) InferError!void {
        if (self.in_global_scope()) {
            self.define_global(token.lexeme, node) catch |err| return self.err.fatal(token, err);
        } else {
            self.define_local(token.lexeme, node) catch |err| return self.err.fatal(token, err);
        }
    }

    pub fn define_function(self: *Env, token: *const Token, expr: *const Expr) InferError!void {
        // @todo regular scoping
        self.global.define_function(token.lexeme, expr) catch |err| {
            return switch (err) {
                inline else => |infer_err| self.err.fatal(token, infer_err),
            };
        };
    }

    pub fn define_global(self: *Env, name: []const u8, node: *TypeNode) InferError!void {
        try self.global.define(name, node);
    }

    pub fn define_local(self: *Env, name: []const u8, node: *TypeNode) InferError!void {
        return if (self.global.has(name))
            InferError.AlreadyDefinedVariable
        else
            try self.local.define(name, node);
    }

    pub fn begin_local_scope(self: *Env) void {
        self.current_depth += 1;
    }

    pub fn end_local_scope(self: *Env, must_resolve_types: bool) InferError!void {
        self.current_depth -= 1;

        if (must_resolve_types) {
            // @todo: only for non generic function
            // Could be a way to narrow non terminal values at the end of scope of concrete functions
            var iterator = self.local.values.iterator();

            while (iterator.next()) |type_node| {
                if (!type_node.value_ptr.*.get_tid().is_terminal()) {
                    std.debug.print("type node {}", .{type_node.value_ptr.*.get_tid()});
                    return InferError.CannotResolveType;
                }
            }
        }

        if (self.in_global_scope()) {
            self.local.clear(); // why ?
        }
    }

    pub fn in_global_scope(self: *Env) bool {
        return self.current_depth == 0;
    }

    pub fn get(self: *Env, token: *const Token) !*TypeNode {
        if (self.in_global_scope()) {
            return self.global.get(token.lexeme);
        }

        return self.local.get(token.lexeme) catch |err| switch (err) {
            InferError.UnknownVariable => self.global.get(token.lexeme) catch |get_variable_err|
                self.err.fatal(token, get_variable_err),
            else => |other_err| self.err.fatal(token, other_err),
        };
    }

    pub fn get_function(self: *Env, name: []const u8) !*const Expr {
        // @todo regular scoping
        return try self.global.get_function(name);
    }

    pub fn deinit(self: *Env) void {
        self.global.deinit();
        self.local.deinit();
    }
};

const Scope = struct {
    allocator: std.mem.Allocator,
    values: std.StringArrayHashMapUnmanaged(*TypeNode),
    functions: std.StringHashMapUnmanaged(*const Expr),

    pub fn init(allocator: std.mem.Allocator) Scope {
        return .{
            .allocator = allocator,
            .values = .{},
            .functions = .{},
        };
    }

    pub fn deinit(self: *Scope) void {
        self.values.deinit(self.allocator);
        self.functions.deinit(self.allocator);
    }

    pub fn define(self: *Scope, name: []const u8, node: *TypeNode) InferError!void {
        const result = self.values.getOrPut(self.allocator, name) catch {
            return InferError.UnknownError;
        };

        // if(std.mem.eql(u8, name, "a")){
        //     std.debug.print("ok", .{});
        // }

        if (result.found_existing) {
            return InferError.AlreadyDefinedVariable;
        }

        result.value_ptr.* = node;
    }

    pub fn define_function(self: *Scope, name: []const u8, expr: *const Expr) !void {
        const result = self.functions.getOrPut(self.allocator, name) catch {
            return InferError.UnknownError;
        };

        if (result.found_existing) {
            return InferError.AlreadyDefinedFunction;
        }

        result.value_ptr.* = expr;
    }

    pub fn clear(self: *Scope) void {
        self.values.clearAndFree(self.allocator);
    }

    pub fn get(self: *Scope, name: []const u8) InferError!*TypeNode {
        return self.values.get(name) orelse InferError.UnknownVariable;
    }

    pub fn get_function(self: *Scope, name: []const u8) InferError!*const Expr {
        return self.functions.get(name) orelse InferError.UnknownFunction;
    }

    pub fn has(self: *Scope, name: []const u8) bool {
        return self.values.contains(name);
    }
};

const TypeScope = std.StringHashMap(*TypeNode);

const std = @import("std");

const Expr = @import("./ast/expr.zig").Expr;
const Typed = @import("./ast/typed.zig").Typed;
const Type = @import("./types.zig").Type;
const Token = @import("./token.zig");
const floatMax = std.math.floatMax;
const maxInt = std.math.maxInt;
const Errors = @import("./error-reporter.zig").Errors;
const tag = std.meta.activeTag;
