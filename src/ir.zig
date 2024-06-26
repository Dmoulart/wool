allocator: std.mem.Allocator,

instructions: std.ArrayListUnmanaged(Inst),

program: std.ArrayListUnmanaged(*Inst),

globals: std.StringHashMapUnmanaged(void),
// blocks and loops identifiers
loop_scope: LoopScope,

function_locals: std.StringArrayHashMapUnmanaged(FunctionLocal),

file: *const File,

const Ir = @This();

const FunctionLocal = struct { ident: u32, tid: Infer.TypeID };

const Op = enum {
    neg_f32,
    neg_f64,
};

pub const Inst = union(enum) {
    value_bool: i32,
    value_i32: i32,
    value_i64: i64,
    value_f32: f32,
    value_f64: f64,

    local_ref: LocalRef,

    local_bool: Local,
    local_i32: Local,
    local_i64: Local,
    local_f32: Local,
    local_f64: Local,

    local_assign: LocalAssign,

    global_ref: []const u8,

    global_bool: Global(i32),
    global_i32: Global(i32),
    global_i64: Global(i64),
    global_f32: Global(f32),
    global_f64: Global(f64),

    add_i32: Binary,
    add_i64: Binary,
    add_f32: Binary,
    add_f64: Binary,

    sub_i32: Binary,
    sub_i64: Binary,
    sub_f32: Binary,
    sub_f64: Binary,

    mul_i32: Binary,
    mul_i64: Binary,
    mul_f32: Binary,
    mul_f64: Binary,

    div_i32: Binary,
    div_i64: Binary,
    div_f32: Binary,
    div_f64: Binary,

    eq_bool: Binary,
    eq_i32: Binary,
    eq_i64: Binary,
    eq_f32: Binary,
    eq_f64: Binary,

    neq_bool: Binary,
    neq_i32: Binary,
    neq_i64: Binary,
    neq_f32: Binary,
    neq_f64: Binary,

    gt_i32: Binary,
    gt_i64: Binary,
    gt_f32: Binary,
    gt_f64: Binary,

    ge_i32: Binary,
    ge_i64: Binary,
    ge_f32: Binary,
    ge_f64: Binary,

    lt_i32: Binary,
    lt_i64: Binary,
    lt_f32: Binary,
    lt_f64: Binary,

    le_i32: Binary,
    le_i64: Binary,
    le_f32: Binary,
    le_f64: Binary,

    unary: Unary,

    // not used
    select_bool: Select,
    select_i32: Select,
    select_i64: Select,
    select_f32: Select,
    select_f64: Select,

    load_i32: Load,
    load_i64: Load,
    load_f32: Load,
    load_f64: Load,

    store_i32: Store,
    store_i64: Store,
    store_f32: Store,
    store_f64: Store,

    func: Func,

    extern_func: ExternFunc,

    @"if": If,

    block: Block,

    call: Call,

    loop: Loop,

    brk: Break,

    break_if: BreakIf,

    ret: Return,

    noop: void,

    pub fn Global(comptime T: type) type {
        return struct {
            name: []const u8,
            value: T,
        };
    }

    pub const Local = struct {
        pub const Ident = u32;

        ident: Ident,
        value: *Inst,
    };

    pub const LocalRef = struct {
        identifier: Local.Ident,
        tid: Infer.TypeID,
    };

    pub const LocalAssign = struct {
        pub const Ident = u32;

        ident: Ident,
        value: *Inst,
    };

    pub const Unary = struct {
        op: Op,
        right: *Inst,
    };

    pub const Binary = struct {
        left: *Inst,
        right: *Inst,
    };

    pub const Func = struct {
        name: []const u8,
        args: []Infer.TypeID,
        ret: Infer.TypeID,
        body: *Inst,
    };

    pub const ExternFunc = struct {
        namespace: []const u8,
        member: []const u8,
        // args: []Infer.TypeID,
        // ret: Infer.TypeID,
    };

    pub const If = struct {
        then_branch: *Inst,
        else_branch: ?*Inst,
        condition: *Inst,
    };

    pub const Block = struct {
        insts: []*Inst,
        return_type: Infer.TypeID,
        id: u32,
    };

    pub const Select = struct {
        then_branch: *Inst,
        else_branch: ?*Inst,
        condition: *Inst,
    };

    pub const Load = struct {
        index: *Inst,
    };

    pub const Store = struct {
        index: *Inst,
        value: *Inst,
    };

    pub const Call = struct {
        callee: []const u8,
        args: []*Inst,
    };

    pub const Loop = struct {
        id: u32,
        body: *Inst,
    };

    pub const BreakIf = struct {
        from: u32,
        condition: *Inst,
    };

    pub const Break = struct { id: u32, value: ?*Inst };

    pub const Return = struct { value: ?*Inst };
};

const IrError = error{ NotImplemented, CannotFindLocalVariable, InvalidType, InvalidOp };

pub fn init(allocator: std.mem.Allocator, file: *const File) Ir {
    return .{
        .allocator = allocator,
        .file = file,
        .instructions = .{},
        .program = .{},
        .globals = .{},
        .function_locals = .{},
        .loop_scope = .{},
    };
}

pub fn deinit(self: *Ir) void {
    self.function_locals.deinit(self.allocator);
}

pub fn convert_program(self: *Ir, sems: []*Infer.Sem) ![]*Inst {
    for (sems) |sem| {
        const inst = try self.convert(sem);

        if (meta.activeTag(inst.*) == .noop) {
            continue;
        }

        try self.emit(inst);
    }

    return self.program.items;
}

pub fn emit(self: *Ir, inst: *Inst) !void {
    try self.program.append(self.allocator, inst);
}

pub fn convert(self: *Ir, sem: *Infer.Sem) anyerror!*Inst {
    switch (sem.*) {
        .Literal => |*lit| {
            const tid = get_sem_tid(sem);
            return try self.create_inst(try literal(tid, lit.orig_expr.Literal.value));
        },
        .Grouping => |grouping| {
            return try self.convert(as_sem(grouping.expr));
        },
        .VarInit => |*var_init| {
            const name = var_init.orig_expr.VarInit.name.get_text(self.file.src);
            const tid = get_sem_tid(as_sem(var_init.initializer));

            const local_identifier: u32 = @intCast(self.function_locals.count());

            try self.function_locals.put(self.allocator, name, .{ .ident = local_identifier, .tid = tid });
            // _ = try self.function_local_types.push(tid);

            const value = try self.convert(as_sem(var_init.initializer));

            const local_value = .{
                .ident = local_identifier,
                .value = value,
            };

            return try self.create_inst(switch (tid) {
                .i32 => .{
                    .local_i32 = local_value,
                },
                .i64 => .{
                    .local_i64 = local_value,
                },
                .f32 => .{
                    .local_f32 = local_value,
                },
                .f64 => .{
                    .local_f64 = local_value,
                },
                .bool => .{
                    .local_bool = local_value,
                },
                else => {
                    std.debug.print("\nIR Convert : Not implemented tid {}\n", .{tid});
                    return IrError.NotImplemented;
                },
            });
        },
        .ConstInit => |*const_init| {
            const name = const_init.orig_expr.ConstInit.name.get_text(self.file.src);

            try self.globals.put(self.allocator, name, {});

            return switch (get_sem_tid(as_sem(const_init.initializer))) {
                .bool => try self.create_inst(.{
                    .global_bool = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), bool, i32),
                    },
                }),
                .i32 => try self.create_inst(.{
                    .global_i32 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), i32, i32),
                    },
                }),
                .i64 => try self.create_inst(.{
                    .global_i64 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), i64, i64),
                    },
                }),
                .f32 => try self.create_inst(.{
                    .global_f32 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), f32, f32),
                    },
                }),
                .f64 => try self.create_inst(.{
                    .global_f64 = .{
                        .name = name,
                        .value = try self.eval(as_sem(const_init.initializer), f64, f64),
                    },
                }),
                .func => {
                    // const func_inst = try self.convert(as_sem(const_init.initializer));
                    // if (activeTag(func_inst.*) == .func) {
                    //     func_inst.func.name = name; //@todo hack to get the name. Find a better way.

                    // }
                    return try self.convert(as_sem(const_init.initializer));
                },
                .extern_func => {
                    const import_expr = as_sem(const_init.initializer).Import.orig_expr.Import;
                    // const import_type = as_sem(const_init.initializer).Import.type_node;

                    const namespace = import_expr.namespace.get_text(self.file.src);
                    const member = import_expr.member.get_text(self.file.src);

                    return try self.create_inst(.{
                        .extern_func = .{
                            // .args = &.{},
                            // .ret = import_type.get_tid(),
                            .namespace = namespace,
                            .member = member,
                        },
                    });
                },

                else => IrError.NotImplemented,
            };
        },
        .Function => |*function| {
            self.function_locals.shrinkRetainingCapacity(0);

            const function_type = function.type_node.function;

            // generic functions cannot be trasnlated
            if (function_type.is_generic()) {
                return try self.create_inst(.{ .noop = {} });
            }

            var args = try self.allocator.alloc(Infer.TypeID, function_type.args.len);

            for (function_type.args, 0..) |arg, i| {
                const tid = arg.get_tid();

                // @todo this is fucking awful !!
                const name = function.orig_expr.Function.args.?[i].expr.Variable.name.get_text(self.file.src);
                args[i] = tid;
                _ = try self.function_locals.put(self.allocator, name, .{
                    .ident = @intCast(self.function_locals.count()),
                    .tid = tid,
                });
            }

            // const name = if (function.orig_expr.Function.name) |name|
            //     name.get_text(self.file.src)
            // else
            //     "anonymous";

            //@todo mem
            var name = String.init(std.heap.page_allocator);
            if (function.orig_expr.Function.name) |func_name| {
                try name.concat(func_name.get_text(self.file.src));
            } else {
                return IrError.NotImplemented;
            }
            // if (function_type.is_instance) {
            for (function_type.args) |arg| {
                try name.concat(arg.to_str());
            }
            // }

            return try self.create_inst(.{
                .func = .{
                    .name = name.str(),
                    .ret = function_type.return_type.get_tid(),
                    .args = args,
                    .body = try self.convert(as_sem(function.body)),
                },
            });
        },
        .Block => |*block| {
            //@todo:mem
            const insts = try self.allocator.alloc(*Inst, block.exprs.len);

            const return_type: Infer.TypeID = if (block.exprs.len == 0)
                .void
            else
                get_sem_tid(as_sem(block.exprs[block.exprs.len - 1]));

            for (block.exprs, 0..) |expr, i| {
                insts[i] = try self.convert(as_sem(expr));
            }

            return try self.create_inst(
                .{
                    .block = .{
                        .insts = insts,
                        .return_type = return_type,
                        .id = 0,
                    },
                },
            );
        },
        .Unary => |*unar| {
            const right = try self.convert(as_sem(unar.right));

            const op = unar.orig_expr.Unary.op.type;

            // We could have picked binary.right.
            const tid = get_sem_tid(as_sem(unar.right));

            return try self.create_inst(try self.unary(tid, op, right));
        },
        .Binary => |*bin| {
            const left = try self.convert(as_sem(bin.left));
            const right = try self.convert(as_sem(bin.right));

            const op = bin.orig_expr.Binary.op.type;

            // We could have picked binary.right.
            const tid = get_sem_tid(as_sem(bin.left));

            return try self.create_inst(try binary(tid, op, left, right));
        },
        .Variable => |*variable| {
            const name = variable.orig_expr.Variable.name.get_text(self.file.src);

            if (self.globals.contains(name)) {
                return try self.create_inst(.{
                    .global_ref = name,
                });
            }

            //@todo: more efficient way of handling this
            const local_ref = self.function_locals.get(
                variable.orig_expr.Variable.name.get_text(self.file.src),
            ).?; // it should always be present

            return try self.create_inst(.{
                .local_ref = .{ .identifier = @intCast(local_ref.ident), .tid = local_ref.tid },
            });
        },
        .Assign => |*assign| {
            //@todo: more efficient way of handling this
            const local_ident = self.function_locals.get(
                assign.orig_expr.Assign.name.get_text(self.file.src),
            ).?.ident;

            return try self.create_inst(.{
                .local_assign = .{
                    .ident = @intCast(local_ident),
                    .value = try self.convert(as_sem(assign.value)),
                },
            });
        },
        .Logical => |logical| {
            const tid = get_sem_tid(sem);

            const left = try self.convert(as_sem(logical.left));
            const right = try self.convert(as_sem(logical.right));

            const op = logical.orig_expr.Logical.op.type;

            // this is not needed for i32
            const bin = try self.create_inst(try binary(
                tid,
                if (op == .AND) .BANG_EQUAL else .EQUAL_EQUAL,
                left,
                try self.create_inst(try literal(tid, if (tid == .bool) Expr.Literal.Value{
                    .Boolean = true, //@todo  this sucks
                } else Expr.Literal.Value{
                    .Number = "0",
                })),
            ));

            //@todo refacto with if
            const values = .{
                .condition = bin,
                .then_branch = right,
                .else_branch = left,
            };

            const select_inst: Inst = switch (tid) {
                .bool => .{ .select_bool = values },
                .i32 => .{ .select_i32 = values },
                .i64 => .{ .select_i64 = values },
                .f32 => .{ .select_f32 = values },
                .f64 => .{ .select_f64 = values },
                else => return IrError.InvalidType,
            };

            return try self.create_inst(select_inst);
        },
        .If => |*if_sem| {
            const condition = try self.convert(as_sem(if_sem.condition));

            const then_branch = try self.convert(as_sem(if_sem.then_branch));

            const else_branch = if (if_sem.else_branch) |else_branch|
                try self.convert(as_sem(else_branch))
            else
                null;

            return try self.create_inst(
                .{
                    .@"if" = .{
                        .then_branch = then_branch,
                        .else_branch = else_branch,
                        .condition = condition,
                    },
                },
            );
        },
        .Call => |*call| {
            // @todo dynamic calls ??
            // @todo horror museum
            const function_name = call.orig_expr.Call.callee.Variable.name.get_text(self.file.src);
            // const fun_type = sem_type(as_sem(call.callee));
            // if (fun_type.as_function()) |f| {
            //     std.debug.print("fff {any}", .{f});
            // }
            // std.debug.print("\n{any}\n", .{sem_type(as_sem(call.callee))});
            // try jsonPrint(as_sem(call.callee), "call.json");

            const args = try self.allocator.alloc(*Inst, call.args.len);

            for (call.args, 0..) |arg, i| {
                args[i] = try self.convert(as_sem(arg));
            }

            const is_macro = function_name[0] == '@';
            if (is_macro) {
                if (std.mem.eql(u8, function_name, "@load")) {
                    const tid = get_sem_tid(as_sem(call));

                    return switch (tid) {
                        .i32 => try self.create_inst(.{ .load_i32 = .{ .index = args[0] } }),
                        .i64 => try self.create_inst(.{ .load_i64 = .{ .index = args[0] } }),
                        .f32 => try self.create_inst(.{ .load_f32 = .{ .index = args[0] } }),
                        .f64 => try self.create_inst(.{ .load_f64 = .{ .index = args[0] } }),
                        else => return IrError.InvalidType,
                    };
                }

                if (std.mem.eql(u8, function_name, "@store")) {
                    const tid = get_sem_tid(as_sem(call.args[1]));

                    return switch (tid) {
                        .i32 => try self.create_inst(.{ .store_i32 = .{ .index = args[0], .value = args[1] } }),
                        .i64 => try self.create_inst(.{ .store_i64 = .{ .index = args[0], .value = args[1] } }),
                        .f32 => try self.create_inst(.{ .store_f32 = .{ .index = args[0], .value = args[1] } }),
                        .f64 => try self.create_inst(.{ .store_f64 = .{ .index = args[0], .value = args[1] } }),
                        else => return IrError.InvalidType,
                    };
                }
            }
            var name = String.init(std.heap.page_allocator);
            try name.concat(function_name);
            // if (function_type.is_instance) {
            for (call.args) |arg| {
                try name.concat(sem_type(as_sem(arg)).to_str());
            }
            return try self.create_inst(
                .{
                    .call = .{ .callee = name.str(), .args = args },
                },
            );
        },
        .Import => |*import| {
            //@todo wow this is really crazy
            const namespace = as_sem(import).Import.orig_expr.Import.namespace.get_text(self.file.src);
            const member = as_sem(import).Import.orig_expr.Import.member.get_text(self.file.src);

            return try self.create_inst(.{
                .extern_func = .{
                    .namespace = namespace,
                    .member = member,
                },
            });
        },
        .While => |*while_loop| {
            const loop_id = self.loop_scope.begin_loop_scope();
            //@mem dealloc
            const insts = try self.allocator.alloc(*Inst, 2);

            const inner = try self.convert(as_sem(while_loop.body));

            const condition = try self.convert(as_sem(while_loop.condition));

            insts[0] = try self.create_inst(.{
                .break_if = .{
                    // flip the condition
                    .condition = try self.create_inst(.{
                        .neq_bool = .{
                            .left = condition,
                            .right = try self.create_inst(.{
                                .value_bool = 1,
                            }),
                        },
                    }),
                    .from = loop_id,
                },
            });

            insts[1] = inner;

            const return_type = while_loop.type_node.get_tid();

            const block = try self.create_inst(.{
                .block = .{
                    .insts = insts,
                    .return_type = return_type,
                    .id = loop_id,
                },
            });

            _ = self.loop_scope.end_loop_scope();

            return try self.create_inst(.{
                .loop = .{
                    .body = block,
                    .id = loop_id,
                },
            });
        },
        .Break => |*brk| {
            return try self.create_inst(.{
                .brk = .{
                    .value = if (brk.value) |value| try self.convert(as_sem(value)) else null,
                    .id = self.loop_scope.current_loop_scope(),
                },
            });
        },
        .OperationAssign => |*opassign| {
            //@todo: more efficient way of handling this
            const identifier = self.function_locals.get(
                opassign.orig_expr.OperationAssign.name.get_text(self.file.src),
            ).?;

            const value = try self.convert(as_sem(opassign.value));

            const ref = try self.create_inst(.{
                .local_ref = .{
                    .identifier = @intCast(identifier.ident),
                    .tid = identifier.tid,
                },
            });

            const op: Token.Type = switch (opassign.orig_expr.OperationAssign.op.type) {
                .MINUS_EQUAL => .MINUS,
                .PLUS_EQUAL => .PLUS,
                .STAR_EQUAL => .STAR,
                .SLASH_EQUAL => .SLASH,
                else => return IrError.InvalidOp,
            };

            const tid = get_sem_tid(as_sem(opassign.value));

            const bin = try self.create_inst(try binary(tid, op, ref, value));

            return try self.create_inst(.{
                .local_assign = .{
                    .ident = @intCast(identifier.ident),
                    .value = bin,
                },
            });
        },
        .Return => |*ret| {
            return try self.create_inst(.{
                .ret = .{
                    .value = if (ret.value) |value| try self.convert(as_sem(value)) else null,
                },
            });
        },
        else => {
            std.debug.print("\nNot Implemented = {any}\n", .{sem});
            return IrError.NotImplemented;
        },
    }
}

// fn create_block(self: *Ir, insts []) Inst {
//     //@todo:mem
//     const insts = try self.allocator.alloc(*Inst, block.exprs.len);

//     const return_type: Infer.TypeID = if (block.exprs.len == 0)
//         .void
//     else
//         get_sem_tid(as_sem(block.exprs[block.exprs.len - 1]));

//     for (block.exprs, 0..) |expr, i| {
//         insts[i] = try self.convert(as_sem(expr));
//     }

//     return try self.create_inst(
//         .{
//             .block = .{ .insts = insts, .return_type = return_type },
//         },
//     );
// }

fn literal(tid: Infer.TypeID, value: Expr.Literal.Value) !Inst {
    return switch (tid) {
        .i32 => .{
            .value_i32 = try std.fmt.parseInt(i32, value.Number, 10),
        },
        .i64 => .{
            .value_i64 = try std.fmt.parseInt(i64, value.Number, 10),
        },
        .f32 => .{
            .value_f32 = try std.fmt.parseFloat(f32, value.Number),
        },
        .f64 => .{
            .value_f64 = try std.fmt.parseFloat(f64, value.Number),
        },
        .bool => .{
            .value_bool = if (value.Boolean) 1 else 0,
        },
        else => IrError.NotImplemented,
    };
}

fn unary(self: *Ir, tid: Infer.TypeID, op: Token.Type, right: *Inst) !Inst {
    return switch (op) {
        .MINUS => switch (tid) {
            .i32 => .{
                .mul_i32 = .{
                    .left = try self.create_inst(.{
                        .value_i32 = -1,
                    }),
                    .right = right,
                },
            },
            .i64 => .{
                .mul_i64 = .{
                    .left = try self.create_inst(.{
                        .value_i64 = -1,
                    }),
                    .right = right,
                },
            },
            .f32 => Inst{
                .unary = .{
                    .op = .neg_f32,
                    .right = right,
                },
            },
            .f64 => Inst{
                .unary = .{
                    .op = .neg_f64,
                    .right = right,
                },
            },
            else => return IrError.InvalidType,
        },
        else => {
            std.debug.print("\n Not implemented unary op {any}", .{op});
            return IrError.InvalidType;
        },
    };
}

fn binary(
    tid: Infer.TypeID,
    op: Token.Type,
    left: *Inst,
    right: *Inst,
) !Inst {
    const values = .{
        .left = left,
        .right = right,
    };

    const inst: Ir.Inst = switch (op) {
        .PLUS => switch (tid) {
            .i32 => .{
                .add_i32 = values,
            },
            .i64 => .{
                .add_i64 = values,
            },
            .f32 => .{
                .add_f32 = values,
            },
            .f64 => .{
                .add_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        .MINUS => switch (tid) {
            .i32 => .{
                .sub_i32 = values,
            },
            .i64 => .{
                .sub_i64 = values,
            },
            .f32 => .{
                .sub_f32 = values,
            },
            .f64 => .{
                .sub_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        .STAR => switch (tid) {
            .i32 => .{
                .mul_i32 = values,
            },
            .i64 => .{
                .mul_i64 = values,
            },
            .f32 => .{
                .mul_f32 = values,
            },
            .f64 => .{
                .mul_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        .SLASH => switch (tid) {
            .i32 => .{
                .div_i32 = values,
            },
            .i64 => .{
                .div_i64 = values,
            },
            .f32 => .{
                .div_f32 = values,
            },
            .f64 => .{
                .div_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        .EQUAL_EQUAL => switch (tid) {
            .bool => .{
                .eq_bool = values,
            },
            .i32 => .{
                .eq_i32 = values,
            },
            .i64 => .{
                .eq_i64 = values,
            },
            .f32 => .{
                .eq_f32 = values,
            },
            .f64 => .{
                .eq_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        .BANG_EQUAL => switch (tid) {
            .bool => .{
                .neq_bool = values,
            },
            .i32 => .{
                .neq_i32 = values,
            },
            .i64 => .{
                .neq_i64 = values,
            },
            .f32 => .{
                .neq_f32 = values,
            },
            .f64 => .{
                .neq_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        .GREATER => switch (tid) {
            .i32 => .{
                .gt_i32 = values,
            },
            .i64 => .{
                .gt_i64 = values,
            },
            .f32 => .{
                .gt_f32 = values,
            },
            .f64 => .{
                .gt_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        .GREATER_EQUAL => switch (tid) {
            .i32 => .{
                .ge_i32 = values,
            },
            .i64 => .{
                .ge_i64 = values,
            },
            .f32 => .{
                .ge_f32 = values,
            },
            .f64 => .{
                .ge_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        .LESS => switch (tid) {
            .i32 => .{
                .lt_i32 = values,
            },
            .i64 => .{
                .lt_i64 = values,
            },
            .f32 => .{
                .lt_f32 = values,
            },
            .f64 => .{
                .lt_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        .LESS_EQUAL => switch (tid) {
            .i32 => .{
                .le_i32 = values,
            },
            .i64 => .{
                .le_i64 = values,
            },
            .f32 => .{
                .le_f32 = values,
            },
            .f64 => .{
                .le_f64 = values,
            },
            else => return IrError.InvalidType,
        },
        else => return IrError.NotImplemented,
    };

    return inst;
}

fn find_str(item: []const u8, target: []const u8) bool {
    return std.mem.eql(u8, item, target);
}

fn create_inst(self: *Ir, inst: Inst) !*Inst {
    const inst_ptr = try self.instructions.addOne(self.allocator);
    inst_ptr.* = inst;
    return inst_ptr;
}

fn eval(self: *Ir, sem: *Infer.Sem, comptime InputType: type, comptime ReturnType: type) !ReturnType {
    return switch (sem.*) {
        .Literal => |*lit| switch (InputType) {
            bool => if (lit.orig_expr.Literal.value.Boolean) 1 else 0,
            i32, i64 => try std.fmt.parseInt(InputType, lit.orig_expr.Literal.value.Number, 10),
            f32, f64 => try std.fmt.parseFloat(InputType, lit.orig_expr.Literal.value.Number),
            else => IrError.NotImplemented,
        },
        .Binary => |*bin| blk: {
            const left = try self.eval(as_sem(bin.left), InputType, ReturnType);
            const right = try self.eval(as_sem(bin.right), InputType, ReturnType);

            break :blk switch (bin.orig_expr.Binary.op.type) {
                .PLUS => left + right,
                .MINUS => left - right,
                .STAR => left * right,
                .SLASH => @divTrunc(left, right), //@todo divison
                .GREATER => if (left > right) 1 else 0,
                .GREATER_EQUAL => if (left >= right) 1 else 0,
                .LESS => if (left < right) 1 else 0,
                .LESS_EQUAL => if (left <= right) 1 else 0,
                .EQUAL_EQUAL => if (left == right) 1 else 0,
                .BANG_EQUAL => if (left != right) 1 else 0,
                else => IrError.NotImplemented,
            };
        },
        else => IrError.NotImplemented,
    };
}

pub fn sem_type(sem: *Infer.Sem) *Infer.TypeNode {
    return switch (sem.*) {
        inline else => |*any_sem| any_sem.type_node,
    };
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

const std = @import("std");
const meta = std.meta;
const activeTag = std.meta.activeTag;
const Token = @import("./token.zig");

const File = @import("./file.zig");

const Infer = @import("./infer.zig");
const Expr = @import("./ast/expr.zig").Expr;
const as_sem = @import("./infer.zig").as_sem;
const as_sems = @import("./infer.zig").as_sems;
const get_sem_tid = @import("./infer.zig").get_sem_tid;

const LoopScope = @import("./loop-scope.zig");

const String = @import("string").String;
