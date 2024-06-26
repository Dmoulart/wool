allocator: std.mem.Allocator,

ast: []*const Expr,

type_nodes: std.ArrayListUnmanaged(TypeNode),

sems: std.ArrayListUnmanaged(Sem),

typed_ast: std.ArrayListUnmanaged(*Sem),

loop_scope: LoopScope,

env: *Env,

environments: std.ArrayListUnmanaged(Env),

err: Errors(InferError),

file: *const File,

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

    pub fn to_str(self: TypeID) []const u8 {
        return switch (self) {
            .any => "any",
            .number => "number",
            .int => "int",
            .i32 => "i32",
            .i64 => "i64",
            .float => "float",
            .f32 => "f32",
            .f64 => "f64",
            .bool => "bool",
            .string => "string",
            .void => "void",
            .func => "func",
            .extern_func => "extern_func",
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
    is_instance: bool,

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
    // // move this in Infer methods
    // pub fn clone(self: *TypeNode, in: *Infer) !*TypeNode {
    //     return switch (self.*) {
    //         .type => |*ty| try in.new_type(ty.tid),
    //         .variable => |*variable| try in.new_type_node(
    //             .{
    //                 .variable = .{ .name = variable.name, .ref = try variable.ref.clone(in) },
    //             },
    //         ),
    //         .function => |*func| try in.new_type_node(
    //             .{
    //                 .function = .{
    //                     .name = func.name,
    //                     .return_type = try func.return_type.clone(in),
    //                     .args = blk: {
    //                         //@todo:mem cleanup
    //                         const new_args = try in.allocator.alloc(*TypeNode, func.args.len);
    //                         for (new_args) |new_arg| {
    //                             new_arg.* = (try new_arg.clone(in)).*;
    //                         }
    //                         break :blk new_args;
    //                     },
    //                 },
    //             },
    //         ),
    //     };
    // }

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

    pub fn is_var(self: *TypeNode) bool {
        return tag(self.*) == .variable;
    }

    pub fn is_func(self: *TypeNode) bool {
        return tag(self.*) == .function;
    }

    pub fn as_function(self: TypeNode) ?FunType {
        return if (tag(self) == .function) self.function else null;
    }

    pub fn to_str(self: *TypeNode) []const u8 {
        return self.get_tid().to_str();
    }
};

pub const InferError = error{
    TypeMismatch,
    UnknownType,
    UnknownBuiltin,
    WrongNumberOfArguments,
    UnknownIdentifier,
    UnknownFunction,
    AnonymousFunctionsNotImplemented,
    AlreadyDefinedIdentifier,
    AlreadyDefinedFunction,
    NonCallableExpression,
    GenericFunctionNotImplemented,
    CannotResolveType,
    CircularReference,
    UnknownError,
    UnusedIdentifier,
    BreakOutsideBlock,
};

pub fn init(allocator: std.mem.Allocator, ast: []*Expr, file: *const File) @This() {
    const err = Errors(InferError).init(allocator, file);
    var environments: std.ArrayListUnmanaged(Env) = .{};
    const env = environments.addOne(allocator) catch unreachable;
    env.* = Env.init(allocator, file, err);
    return .{
        .allocator = allocator,
        .ast = ast,
        .env = env,
        .type_nodes = .{},
        .sems = .{},
        .typed_ast = .{},
        .err = err,
        .file = file,
        .loop_scope = .{},
        .environments = environments,
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
            // @todo hack to get function name find a better way
            if (std.meta.activeTag(const_init.initializer.*) == .Function) {
                const fun_expr: *Expr = @constCast(const_init.initializer);
                fun_expr.Function.name = const_init.name;
            }

            const typed_initializer = try self.infer(const_init.initializer);
            const initializer_type = sem_type(typed_initializer);

            const const_type = if (const_init.type) |type_tokens|
                try self.new_type_from_tokens(type_tokens)
            else
                try self.new_type(.any);

            unify(const_type, initializer_type) catch
                return self.type_mismatch_err(
                const_type,
                initializer_type,
                const_init.initializer,
            );

            self.env.define(const_init.name, initializer_type) catch
                return self.already_defined_indentifier_err(const_init.name, expr, "constant");

            if (tag(const_init.initializer.*) == .Function) {
                self.env.global.define_function(const_init.name.get_text(self.file.src), const_init.initializer) catch
                    return self.already_defined_indentifier_err(const_init.name, expr, "constant");
            }

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
            const initializer_type = sem_type(typed_initializer);

            const var_type = if (var_init.type) |type_token|
                try self.new_type_from_tokens(type_token)
            else
                try self.new_type(.any);

            unify(var_type, initializer_type) catch
                return self.type_mismatch_err(
                var_type,
                initializer_type,
                var_init.initializer,
            );

            self.env.define(var_init.name, initializer_type) catch
                return self.already_defined_indentifier_err(var_init.name, expr, "variable");

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
            const var_type = self.env.get(assign.name) catch
                return self.unknown_identifier_err(assign.name);

            const typed_assignation = try self.infer(assign.value);
            const assignation_type = sem_type(typed_assignation);

            unify(var_type, assignation_type) catch
                return self.type_mismatch_err(
                var_type,
                assignation_type,
                assign.value,
            );

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
        .Unary => |*unary| {
            // const op = try switch (unary.op.type) {
            //     .PLUS => "+",
            //     .MINUS => "-",
            //     else => unreachable,
            // };

            const typed_unary = try self.infer(unary.right);
            const unary_type = sem_type(typed_unary);

            return try self.create_sem(
                .{
                    .Unary = .{
                        .right = typed_unary,
                        .orig_expr = expr,
                        .type_node = unary_type,
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
                expr,
                builtin,
                args,
            );

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
            const variable_type = self.env.get(variable.name) catch
                return self.unknown_identifier_err(variable.name);

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
            const condition_type = sem_type(typed_condition);
            const if_condition_type = try self.new_type(.bool);

            unify(
                condition_type,
                if_condition_type,
            ) catch
                return self.type_mismatch_err(
                if_condition_type,
                condition_type,
                if_expr.condition,
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

                const then_branch_type = sem_type(typed_then_branch);
                const maybe_else_branch_type = sem_type(maybe_typed_else_branch.?);

                unify(then_branch_type, maybe_else_branch_type) catch
                    return self.type_mismatch_err(
                    then_branch_type,
                    maybe_else_branch_type,
                    if_expr.else_branch.?,
                );

                //@todo pay attention to circular references !! This can cause segfaults
                try bind(maybe_else_branch_type, then_branch_type);
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
            self.env.begin_local_scope();

            var typed_inner_exprs = try self.allocator.alloc(*anyopaque, block.exprs.len);

            for (block.exprs, 0..) |block_expr, i| {
                typed_inner_exprs[i] = try self.infer(block_expr);
            }

            const block_return_type = if (block.exprs.len > 0)
                sem_type(@alignCast(@ptrCast(typed_inner_exprs[block.exprs.len - 1])))
            else
                try self.new_type(.void);

            try self.env.end_local_scope(); // @todo generic block ?????

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
            //@todo free
            const args = try self.allocator.alloc(*TypeNode, 1);
            args[0] = try self.new_type(.any);

            const node = try self.new_type_node(
                .{
                    .function = .{
                        .args = args,
                        .name = null,
                        .return_type = try self.new_type(.any),
                        .is_instance = false,
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
            _ = self.loop_scope.begin_loop_scope();

            const condition = try self.infer(while_expr.condition);
            const condition_type = sem_type(condition);

            const bool_condition = try self.new_type(.bool);

            try unify(bool_condition, condition_type);

            const body = try self.infer(while_expr.body);
            const body_type = sem_type(body);

            _ = self.loop_scope.end_loop_scope();

            // const loop_scope = self.new
            return try self.create_sem(.{
                .While = .{
                    .inc = null,
                    .condition = condition,
                    .body = body,
                    .orig_expr = expr,
                    .type_node = body_type,
                },
            });
        },
        .Call => |*call_expr| {
            const function_name = call_expr.callee.Variable.name;
            const callee = self.env.get(function_name) catch
                return self.unknown_identifier_err(function_name);

            if (callee.as_function()) |*func| {
                // @todo func.is_generic() and current context is concrete function
                if (func.is_generic()) {
                    // return InferError.GenericFunctionNotImplemented;
                    const func_expr = try self.env.global.get_function(function_name.get_text(self.file.src));

                    self.env.begin_local_scope();

                    const new_func_infos = try self.instanciate_function(
                        &callee.function,
                        func_expr,
                        call_expr.args,
                    );

                    try self.env.end_local_scope();

                    const new_func = try self.create_sem(
                        .{
                            .Function = .{
                                .type_node = new_func_infos.type_node,
                                .orig_expr = func_expr,
                                .body = new_func_infos.body,
                            },
                        },
                    );
                    // var mutable_func_expr = @constCast(func_expr);
                    // mutable_func_expr.Function.name = "ok";

                    try self.typed_ast.append(self.allocator, new_func);

                    const call_infos = try self.call(
                        expr,
                        &new_func_infos.type_node.function,
                        call_expr.args,
                    );

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
                }

                const call_infos = try self.call(expr, func, call_expr.args);

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
                // @todo make error
                return InferError.NonCallableExpression;
            }
        },
        .Function => {
            // @todo function main type checkin', we need context, and function name, how ?
            // const func_name = expr.Function.name;
            // std.debug.print("func name {any}", .{func_name});
            self.env.begin_local_scope();
            const function_infos = try self.function(
                expr,
                null,
                null,
                null,
            );
            try self.env.end_local_scope();

            // try self.env.global.define_function(expr.Function.name.?.get_text(self.file.src), expr);

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
            const left_type = sem_type(left);

            const right = try self.infer(logical.right);
            const right_type = sem_type(right);

            unify(left_type, right_type) catch
                return self.type_mismatch_err(
                left_type,
                right_type,
                logical.right,
            );

            //@todo pay attention to circular references !! This can cause segfaults
            try bind(right_type, left_type);

            const node = try self.new_type_node(
                .{
                    .variable = .{
                        .name = "logical",
                        .ref = left_type,
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
        .Break => |brk| {
            if (!self.loop_scope.in_loop_scope()) {
                return self.break_oustide_loop_or_block_err(brk.keyword);
            }

            const maybe_brk_value = if (brk.value) |value|
                try self.infer(value)
            else
                null;

            const brk_type = if (maybe_brk_value) |brk_value|
                sem_type(brk_value)
            else
                try self.new_type(.void);

            return try self.create_sem(
                .{
                    .Break = .{
                        .type_node = brk_type,
                        .orig_expr = expr,
                        .value = maybe_brk_value,
                    },
                },
            );
        },
        .OperationAssign => |*opassign| {
            const var_type = self.env.get(opassign.name) catch
                return self.unknown_identifier_err(opassign.name);

            const typed_assignation = try self.infer(opassign.value);
            const assignation_type = sem_type(typed_assignation);

            unify(var_type, assignation_type) catch
                return self.type_mismatch_err(
                var_type,
                assignation_type,
                opassign.value,
            );

            return try self.create_sem(
                .{
                    .OperationAssign = .{
                        .value = typed_assignation,
                        .orig_expr = expr,
                        .type_node = try self.new_type(.void),
                    },
                },
            );
        },
        .Return => |*ret| {
            const maybe_return_value = if (ret.value) |value|
                try self.infer(value)
            else
                null;

            const return_type = if (maybe_return_value) |return_value|
                sem_type(return_value)
            else
                try self.new_type(.void);

            return try self.create_sem(
                .{
                    .Return = .{
                        .type_node = return_type,
                        .orig_expr = expr,
                        .value = maybe_return_value,
                    },
                },
            );
        },
        else => {
            std.debug.print("\n Unimplemented expr {any}\n", .{expr});
            unreachable;
        },
    };
}

fn call(
    self: *@This(),
    call_expr: *const Expr,
    func_type: *const FunType,
    call_args: []*const Expr,
) anyerror!struct { args: []*Sem, return_type: *TypeNode } {
    if (func_type.args.len != call_args.len) {
        const function_name = call_expr.Call.callee.Variable.name;
        return self.wrong_number_of_arguments_err(
            call_expr,
            @intCast(func_type.args.len),
            @intCast(call_args.len),
            function_name.get_text(self.file.src),
        );
    }

    // @todo: just reuse one type scope. Or use another more generic object
    var type_scope: *TypeScope = try self.allocator.create(TypeScope);
    type_scope.* = TypeScope.init(self.allocator);
    try type_scope.ensureTotalCapacity(@intCast(func_type.args.len));
    defer type_scope.deinit();

    var args_sems = try self.allocator.alloc(*Sem, call_args.len);

    for (call_args, func_type.args, 0..) |call_arg, func_arg_type, i| {
        const typed_call_arg = try self.infer(call_arg);

        var call_arg_type = sem_type(typed_call_arg);

        // @warning: watch this crap
        if (call_arg_type.is_var() and func_arg_type.is_var()) {
            call_arg_type.variable.name = func_arg_type.variable.name;
        }

        const call_arg_type_ref = self.get_or_create_type_ref(
            func_arg_type,
            call_arg_type,
            type_scope,
        ) catch |err| switch (err) {
            InferError.TypeMismatch => return self.type_mismatch_err(
                func_arg_type,
                call_arg_type,
                call_arg,
            ),
            else => unreachable,
        };

        unify(
            call_arg_type_ref,
            call_arg_type,
        ) catch return self.type_mismatch_err(
            call_arg_type_ref,
            call_arg_type,
            call_arg,
        );

        // Variable binding !
        if (call_arg_type_ref != call_arg_type and call_arg_type_ref.is_var() and call_arg_type.is_var()) {
            if (std.mem.eql(u8, func_arg_type.variable.name, call_arg_type_ref.variable.name)) {
                try bind(call_arg_type, call_arg_type_ref);
            }
        }

        args_sems[i] = typed_call_arg;
    }

    return .{
        .args = args_sems,
        .return_type = try self.get_type_ref(func_type.return_type, type_scope),
    };
}

// @todo: why anyerror
fn function(self: *@This(), expr: *const Expr, maybe_args: ?[]*TypeNode, maybe_return_type: ?*TypeNode, maybe_is_instance: ?bool) anyerror!FunctionInfos {
    const func_expr = expr.Function;
    // std.debug.print("\n declare func:{s} \n", .{func_expr.name.?.get_text(self.file.src)});
    // if (func_expr.name == null) {
    //     return TypeError.AnonymousFunctionsNotImplemented;
    // }

    if (maybe_args) |args| {
        if (func_expr.args != null and args.len != func_expr.args.?.len) {
            return InferError.WrongNumberOfArguments;
        }
    }

    // self.env.begin_local_scope();

    const func_decl_return_type = if (func_expr.type) |token_type|
        try self.new_var_type_from_token("T", token_type)
    else
        try self.new_var_type("T", try self.new_type(.any));

    var function_decl_type: FunType = .{
        // .name = func_expr.name.?.lexeme,
        .name = null,
        .args = if (func_expr.args) |args|
            try self.allocator.alloc(*TypeNode, args.len)
        else
            &[_]*TypeNode{},
        .return_type = func_decl_return_type,
        .is_instance = if (maybe_is_instance) |is_instance| is_instance else false,
    };

    if (func_expr.args) |args| {
        for (args, 0..) |arg, i| {
            const arg_type = if (arg.type) |type_token|
                try self.new_var_type_from_token("T", type_token)
            else
                try self.new_var_type("T", try self.new_type(.any));

            const arg_name = arg.expr.Variable.name.get_text(self.file.src);

            // std.debug.print("\n arg_name {s} \n", .{arg_name});

            self.env.define_local(arg_name, arg_type) catch
                return self.already_defined_indentifier_err(arg.expr.Variable.name, arg.expr, "argument");

            function_decl_type.args[i] = arg_type;

            if (maybe_args) |optional_args| {
                try unify(optional_args[i], arg_type);
            }
        }
    }

    const typed_body = try self.infer(func_expr.body);

    const body_type = sem_type(typed_body);

    subsume(
        func_decl_return_type,
        body_type,
    ) catch
        return self.type_mismatch_err(
        func_decl_return_type,
        body_type,
        func_expr.body,
    );

    if (maybe_return_type) |optional_return_type| {
        subsume(
            func_decl_return_type,
            optional_return_type,
        ) catch
            return self.type_mismatch_err(
            func_decl_return_type,
            optional_return_type,
            func_expr.body,
        );
    }

    // try self.env.end_local_scope();
    // std.debug.print("\n end func:{s} \n", .{func_expr.name.?.get_text(self.file.src)});
    return .{
        .body = typed_body,
        .type_node = try self.new_type_node(.{ .function = function_decl_type }),
    };
}

fn get_or_create_type_ref(self: *@This(), base_type: *TypeNode, instance_type: *TypeNode, scope: *TypeScope) !*TypeNode {
    if (base_type.as_var()) |variable| {
        if (scope.get(variable.name)) |registered_variable| {
            return registered_variable;
        } else {
            const type_ref = switch (instance_type.*) {
                .variable => instance_type, // unify here ?
                else => unreachable, // ?
            };
            // we don't want to modify the base_type, it would be as if we modify the types of builtin when we call it
            // so we use subsume instead of unify
            try subsume(type_ref, base_type);

            try scope.put(type_ref.variable.name, type_ref);

            return type_ref;
        }
    }

    return try self.new_type_node(base_type.*);
}

fn get_type_ref(self: *@This(), node: *TypeNode, scope: *TypeScope) !*TypeNode {
    if (node.as_var()) |variable| {
        if (scope.get(variable.name)) |registered_variable| {
            return registered_variable;
        }
    }
    return try self.new_type_node(node.*);
}

fn unify(type_a: *TypeNode, type_b: *TypeNode) InferError!void {
    const a_is_b = @intFromPtr(type_a) == @intFromPtr(type_b);

    if (a_is_b) {
        return;
    }

    if (type_a.is_func() and type_b.is_func()) {
        if (type_a.function.args.len != type_b.function.args.len) {
            return InferError.TypeMismatch;
        }

        for (type_a.function.args, type_b.function.args) |arg_a, arg_b| {
            try unify(arg_a, arg_b);
        }

        try unify(type_a.function.return_type, type_b.function.return_type);
    }

    const a = type_a.get_tid();
    const b = type_b.get_tid();

    if (a == b) {
        return;
    }

    if (b.is_subtype_of(a)) {
        type_a.set_tid(b);
    } else if (a.is_subtype_of(b)) {
        type_b.set_tid(a);
    } else {
        return InferError.TypeMismatch;
    }
}
// same that unify but only affect type_a
fn subsume(type_a: *TypeNode, type_b: *TypeNode) InferError!void {
    const a_is_b = @intFromPtr(type_a) == @intFromPtr(type_b);

    if (a_is_b) {
        return;
    }

    if (type_a.is_func() and type_b.is_func()) {
        if (type_a.function.args.len != type_b.function.args.len) {
            return InferError.TypeMismatch;
        }

        for (type_a.function.args, type_b.function.args) |arg_a, arg_b| {
            try subsume(arg_a, arg_b);
        }

        try subsume(type_a.function.return_type, type_a.function.return_type);
    }

    const a = type_a.get_tid();
    const b = type_b.get_tid();

    if (a == b) {
        return;
    }

    if (b.is_subtype_of(a)) {
        type_a.set_tid(b);
    } else if (a.is_subtype_of(b)) {
        return;
    } else {
        return InferError.TypeMismatch;
    }
}

fn bind(from: *TypeNode, to: *TypeNode) !void {
    //@todo: not sure about this
    if (@intFromPtr(to) == @intFromPtr(from.variable.ref)) {
        return;
        // return InferError.CircularReference;
    }

    // we don't need a variable to point to a terminal type. it can become the terminal type because once we have hit a terminal type
    // we can't go deeper.
    // @warning watch this..
    if (to.get_tid().is_terminal()) {
        from.* = to.*;
        return;
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
                .tid = try tid_from_str(token.get_text(self.file.src)),
            },
        },
    );
}

fn new_type_from_tokens(self: *@This(), tokens: []*const Token) !*TypeNode {
    return try self.parse_type(tokens);
}

fn new_var_type_from_token(self: *@This(), name: []const u8, token: *const Token) !*TypeNode {
    const type_str = token.get_text(self.file.src);
    return try self.new_type_node(
        .{
            .variable = .{
                .ref = try self.new_type(try tid_from_str(type_str)),
                .name = name,
            },
        },
    );
}

fn new_var_type_from_tokens(self: *@This(), name: []const u8, tokens: []*const Token) !*TypeNode {
    return try self.new_type_node(
        .{
            .variable = .{
                .ref = try self.new_type_from_tokens(tokens),
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
const FunctionInfos = struct { body: *Sem, type_node: *TypeNode };

fn instanciate_function(
    self: *Infer,
    func_type: *const FunType,
    expr: *const Expr,
    call_args: []*const Expr,
) anyerror!FunctionInfos {
    if (func_type.args.len != call_args.len) {
        return InferError.WrongNumberOfArguments;
    }

    var typed_call_args = std.ArrayList(*TypeNode).init(self.allocator);

    for (call_args, func_type.args) |call_arg, base_arg| {
        const typed_call_arg = try self.infer(call_arg);
        const call_arg_type = sem_type(typed_call_arg);
        // mutate cloned call_args args
        try subsume(call_arg_type, base_arg);

        try typed_call_args.append(call_arg_type);
    }

    // create new symbol tables, isolate new scopes
    const prev_env = self.env;
    try self.use_new_env();
    defer self.env = prev_env;

    const func_instance_type: FunType = .{
        .name = func_type.name,
        .args = try typed_call_args.toOwnedSlice(),
        .return_type = func_type.return_type,
        .is_instance = true,
    };

    return try self.function(
        expr,
        func_instance_type.args,
        func_instance_type.return_type,
        true,
    );
}

fn use_new_env(self: *Infer) !void {
    const new_env = try self.environments.addOne(self.allocator);
    new_env.* = Env.init(self.allocator, self.file, self.err);
    self.env = new_env;
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

fn create_sem(self: *Infer, sem: Sem) !*Sem {
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
pub fn parse_type(self: *Infer, tokens: []*const Token) !*TypeNode {
    if (tokens.len == 1) {
        const type_str = tokens[0].get_text(self.file.src);
        return try self.new_type(try tid_from_str(type_str));
    }

    var args = std.ArrayList(*TypeNode).init(self.allocator);

    var maybe_return_type: ?*TypeNode = null;

    var i: usize = 0;

    var in_return_type = false;

    while (i < tokens.len) : (i += 1) {
        var current_token = tokens[i];

        if (current_token.type == .MINUS_ARROW) {
            in_return_type = true;
            continue;
        }

        const tid = try tid_from_str(current_token.get_text(self.file.src));

        if (in_return_type) {
            maybe_return_type = try self.new_type(tid);
            break;
        } else {
            try args.append(try self.new_type(tid));
        }
    }

    return try self.new_type_node(.{
        .function = .{
            .name = "?",
            .args = try args.toOwnedSlice(),
            .return_type = if (maybe_return_type) |return_type| return_type else try self.new_type(.void),
            .is_instance = false,
        },
    });
}

pub fn tid_from_str(str: []const u8) !TypeID {
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

var main_function_args: []*TypeNode = [_]*TypeNode{};
var main_function_return_type = make_type(.void);

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

var i32_node = make_type(.i32);
var load_arg: [1]*TypeNode = .{&i32_node};

var load_return_type: TypeNode = make_vartype("T", &number_node);

var load_type = TypeNode{
    .function = .{
        .name = "load",
        .args = &load_arg,
        .return_type = &load_return_type,
        .is_instance = false,
    },
};

// var number_type = make_type(.number);
// var store_value_arg: TypeNode = make_vartype("T", &number_type);

// var sub_return_type = make_vartype("T", &number_node);
var store_args: [2]*TypeNode = .{ &i32_node, &number_var };

var store_return_type: TypeNode = make_type(.void);

var store_type = TypeNode{
    .function = .{
        .name = "store",
        .args = &store_args,
        .return_type = &store_return_type,
        .is_instance = false,
    },
};

const builtins_types = std.ComptimeStringMap(
    FunType,
    .{
        .{
            "+", FunType{
                .name = "+",
                .args = &add_args,
                .return_type = &add_return_type,
                .is_instance = false,
            },
        },
        .{
            "-", FunType{
                .name = "-",
                .args = &sub_args,
                .return_type = &sub_return_type,
                .is_instance = false,
            },
        },
        .{
            "*", FunType{
                .name = "*",
                .args = &mul_args,
                .return_type = &mul_return_type,
                .is_instance = false,
            },
        },
        .{
            "/", FunType{
                .name = "/",
                .args = &div_args,
                .return_type = &div_return_type,
                .is_instance = false,
            },
        },
        .{
            ">", FunType{
                .name = ">",
                .args = &greater_args,
                .return_type = &greater_return_type,
                .is_instance = false,
            },
        },
        .{
            ">=", FunType{
                .name = ">=",
                .args = &greater_equal_args,
                .return_type = &greater_equal_return_type,
                .is_instance = false,
            },
        },
        .{
            "<", FunType{
                .name = "<",
                .args = &less_args,
                .return_type = &less_return_type,
                .is_instance = false,
            },
        },
        .{
            "<=", FunType{
                .name = "<=",
                .args = &less_equal_args,
                .return_type = &less_equal_return_type,
                .is_instance = false,
            },
        },
        .{
            "==", FunType{
                .name = "==",
                .args = &equal_equal_args,
                .return_type = &equal_equal_return_type,
                .is_instance = false,
            },
        },
        .{
            "!=", FunType{
                .name = "!=",
                .args = &bang_equal_args,
                .return_type = &bang_equal_return_type,
                .is_instance = false,
            },
        },
    },
);

pub fn write_sems_to_file2(self: *@This()) !void {
    std.debug.print("\n{any}\n", .{self.sems.items});
}

const Env = struct {
    allocator: std.mem.Allocator,
    local: SymbolTable,
    global: SymbolTable,
    err: Errors(InferError),
    file: *const File,

    current_depth: u32 = 0,
    current_scope_start_index: usize = 0,

    pub fn init(allocator: std.mem.Allocator, file: *const File, err: Errors(InferError)) Env {
        var global = SymbolTable.init(allocator);
        global.define("@load", &load_type) catch unreachable;
        global.define("@store", &store_type) catch unreachable;
        return .{
            .allocator = allocator,
            .global = global,
            .local = SymbolTable.init(allocator),
            .err = err,
            .file = file,
        };
    }

    pub fn define(self: *Env, token: *const Token, node: *TypeNode) InferError!void {
        if (self.in_global_scope()) {
            return try self.define_global(token.get_text(self.file.src), node);
        } else {
            return try self.define_local(token.get_text(self.file.src), node);
        }
    }

    pub fn define_global(self: *Env, name: []const u8, node: *TypeNode) InferError!void {
        return self.global.define(name, node);
    }

    pub fn define_local(self: *Env, name: []const u8, node: *TypeNode) InferError!void {
        if (self.global.has(name)) {
            return InferError.AlreadyDefinedIdentifier;
        } else {
            return try self.local.define(name, node);
        }
    }

    pub fn get(self: *Env, token: *const Token) InferError!*TypeNode {
        if (self.in_global_scope()) {
            return self.global.get(token.get_text(self.file.src));
        }

        if (self.local.get(token.get_text(self.file.src))) |ty| {
            return ty;
        } else |_| {
            return self.global.get(token.get_text(self.file.src));
        }
    }

    pub fn begin_local_scope(self: *Env) void {
        self.current_depth += 1;
        self.current_scope_start_index = self.local.types.count();
    }

    pub fn end_local_scope(self: *Env) InferError!void {
        self.current_depth -= 1;

        if (true) {
            // @todo: only for non generic function
            // Could be a way to narrow non terminal values at the end of scope of concrete functions
            var iterator = self.local.types.iterator();

            while (iterator.next()) |type_node_entry| {
                const type_node = type_node_entry.value_ptr.*;

                if (self.local.is_unused(type_node)) {
                    // return InferError.UnusedIdentifier; // cannot do error reporting. env should contain sems ?
                }

                if (!type_node.get_tid().is_terminal()) {
                    // std.debug.print("cannot resolve type type node {}", .{type_node.get_tid()});
                    // return InferError.CannotResolveType;
                }
            }
        }

        if (self.in_global_scope()) {
            self.local.clear(); // clear all local scope types, we don't need it anymore
        } else {
            // std.debug.print("\n Before \n", .{});
            // for (self.local.types.values()) |ty| {
            //     std.debug.print("\nty {any}\n", .{ty});
            // }

            // std.debug.print("\n-- local rewind to {d}", .{self.current_scope_start_index});

            self.local.rewind_to(self.current_scope_start_index);
            self.current_scope_start_index = self.local.types.count();

            // std.debug.print("\n After \n", .{});
            // for (self.local.types.values()) |ty| {
            //     std.debug.print("\nty {any}\n", .{ty});
            // }

        }
    }

    pub fn in_global_scope(self: *Env) bool {
        return self.current_depth == 0;
    }

    pub fn deinit(self: *Env) void {
        self.global.deinit();
        self.local.deinit();
    }
};

const Scope = struct {
    parent: ?*const Scope,
    type: Scope.Type,
    symbols: SymbolTable,

    pub const Type = enum {
        block,
        function,
    };

    pub fn init(allocator: std.mem.Allocator, scope_type: Scope.Type, parent: ?*const Scope) Scope {
        return .{
            .type = scope_type,
            .parent = parent,
            .symbols = SymbolTable.init(allocator),
        };
    }
};

// const ScopesInfo = struct { stack: Stack(ScopeInfo) };

const SymbolTable = struct {
    allocator: std.mem.Allocator,
    types: std.StringArrayHashMapUnmanaged(*TypeNode),
    functions: std.StringHashMapUnmanaged(*const Expr),
    unused: std.AutoHashMapUnmanaged(*TypeNode, void),

    pub fn init(allocator: std.mem.Allocator) SymbolTable {
        return .{
            .allocator = allocator,
            .types = .{},
            .functions = .{},
            .unused = .{},
        };
    }

    pub fn deinit(self: *SymbolTable) void {
        self.types.deinit(self.allocator);
        self.functions.deinit(self.allocator);
    }

    pub fn define(self: *SymbolTable, name: []const u8, node: *TypeNode) InferError!void {
        const result = self.types.getOrPut(self.allocator, name) catch unreachable; // @todo general exception handling

        if (result.found_existing) {
            return InferError.AlreadyDefinedIdentifier;
        }

        result.value_ptr.* = node;
        self.unused.put(self.allocator, node, {}) catch unreachable; // @todo general exception handling
    }

    pub fn clear(self: *SymbolTable) void {
        self.types.clearAndFree(self.allocator);
    }

    pub fn rewind_to(self: *SymbolTable, size: usize) void {
        self.types.shrinkAndFree(self.allocator, size);
    }

    pub fn get(self: *SymbolTable, name: []const u8) InferError!*TypeNode {
        if (self.types.get(name)) |value| {
            _ = self.unused.remove(
                value,
            );

            return value;
        } else {
            return InferError.UnknownIdentifier;
        }
    }

    pub fn get_function(self: *SymbolTable, name: []const u8) InferError!*const Expr {
        return self.functions.get(name) orelse InferError.UnknownFunction;
    }

    pub fn define_function(self: *SymbolTable, name: []const u8, expr: *const Expr) !void {
        try self.functions.put(self.allocator, name, expr);
    }

    pub fn has(self: *SymbolTable, name: []const u8) bool {
        return self.types.contains(name);
    }

    pub fn is_unused(self: *SymbolTable, node: *TypeNode) bool {
        return self.unused.contains(node);
    }
};

fn already_defined_indentifier_err(self: *@This(), token: *const Token, expr: *const Expr, comptime identifier_type: []const u8) InferError {
    return self.err.fatal(InferError.AlreadyDefinedIdentifier, .{
        .column_start = token.start,
        .column_end = token.end,
        .msg = .{
            identifier_type,
            token.get_text(self.file.src),
        },
        .context = .{
            expr.get_text(self.file.src),
        },
    });
}

fn type_mismatch_err(self: *@This(), expected: *TypeNode, found: *TypeNode, found_expr: *const Expr) InferError {
    return self.err.fatal(InferError.TypeMismatch, .{
        .column_start = found_expr.get_column_start(),
        .column_end = found_expr.get_column_end(),
        .msg = .{
            expected.to_str(),
            found.to_str(),
        },
        .context = {
            // expr.get_text(self.file.src),
        },
    });
}

fn unknown_identifier_err(self: *@This(), token: *const Token) InferError {
    return self.err.fatal(InferError.UnknownIdentifier, .{
        .column_start = token.start,
        .column_end = token.end,
        .msg = .{
            token.get_text(self.file.src),
        },
        .context = {
            // expr.get_text(self.file.src),
        },
    });
}

fn unused_identifier_err(self: *@This(), token: *const Token) InferError {
    return self.err.fatal(InferError.UnusedIdentifier, .{
        .column_start = token.start,
        .column_end = token.end,
        .line = token.line,
        .msg = .{
            token.lexeme,
        },
        .context = {
            // expr.get_text(self.file.src),
        },
    });
}

fn wrong_number_of_arguments_err(
    self: *@This(),
    expr: *const Expr,
    expected: u32,
    found: u32,
    function_name: []const u8,
) InferError {
    const column_start, const column_end = expr.get_location();
    return self.err.fatal(InferError.WrongNumberOfArguments, .{
        .column_start = column_start,
        .column_end = column_end,
        .msg = .{
            function_name,
            expected,
            found,
        },
        .context = {
            // expr.get_text(self.file.src),
        },
    });
}

fn break_oustide_loop_or_block_err(
    self: *@This(),
    keyword: *const Token,
) InferError {
    return self.err.fatal(InferError.BreakOutsideBlock, .{
        .column_start = keyword.start,
        .column_end = keyword.end,
        .msg = .{},
        .context = {
            // expr.get_text(self.file.src),
        },
    });
}

const TypeScope = std.StringHashMap(*TypeNode);

const std = @import("std");

const File = @import("./file.zig");

const Expr = @import("./ast/expr.zig").Expr;
const Typed = @import("./ast/typed.zig").Typed;
const Type = @import("./types.zig").Type;
const Token = @import("./token.zig");
const LoopScope = @import("./loop-scope.zig");

const Stack = @import("./Stack.zig").Stack;
const floatMax = std.math.floatMax;
const maxInt = std.math.maxInt;
const Errors = @import("./error-reporter.zig").Errors;
const tag = std.meta.activeTag;
