//! Collects ids for all nodes in the hir tree.
//! 
//! This is used to build the [`HirRefArena`] which is used to map
//! from [`HirId`]s to [`HirRefElem`]s.
//! 
//! The way this works is it first computes the ids for all the nodes in the tree,
//! mutates the tree to store the ids, and then stores references to the various nodes
//! in the [`HirRefArena`].
//! 
//! After that, the [`HirRefArena`] can be used to immediately get the [`HirRefElem`]
//! for a given [`HirId`], which stores a reference to the actual element.
//! 
//! It is done in 2 steps because we need to mutate the tree to store the ids before
//! we can store references to the nodes in the [`HirRefArena`].
//! 
//! There's a method for each node in the hir tree, and each method has 2 "variants",
//! depending on the [`Act`] with which it is invoked.
//! 
//! The [`Act`] enum is used to distinguish between the 2 steps of the algorithm.
//! [`Act::Compute`] refers to the initial computation of the [`HirId`]s, and
//! [`Act::Push`] refers to the second step of pushing the references to the nodes
//! into the [`HirRefArena`].
//! 
//! As such, the [`Act::Compute`] stores a `&mut` reference, and the [`Act::Push`]
//! stores a `&` reference, to the hir nodes.
//! 
//! The [`_match`], [`_list`] and [`_simple_seq`] macros are used to match on the
//! [`Act`] and invoke the appropriate methods for the appropriate variants.
//! They overall just make the code a bit easier to deal with. Refer to their
//! documentation on how to work with them.
//! 
//! A small note on the macros: They all expect the expressions passed to other
//! calls to be owned values, and not references. This is because the macros
//! will build references to the expressions, depending on the [`Act`] we're currently
//! executing.
//! 
//! [`HirId`]: crate::HirId

use crate::hir::*;
use crate::hir_arena::{HirRefArena, HirRefElem};

pub struct HirCollectIdsCtxt<'hir> {
    arena: HirRefArena<'hir>,
}

impl<'hir> HirCollectIdsCtxt<'hir> {
    pub fn new(arena: HirRefArena<'hir>) -> Self {
        Self { arena }
    }

    pub fn into_arena(self) -> HirRefArena<'hir> {
        self.arena
    }
}

enum Act<'compute, 'hir, T> {
    Compute(&'compute mut T),
    Push(&'hir T),
}

pub fn initially_update_ids_mod<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    mod_node: &'compute mut ModDef,
) {
    let ModDef { items, hir_id } = mod_node;
    *hir_id = ctxt.arena.create_id();
    collect_ids_item_list(ctxt, Act::Compute(items));
}

pub fn collect_ids_mod<'hir>(ctxt: &mut HirCollectIdsCtxt<'hir>, mod_node: &'hir ModDef) {
    ctxt.arena
        .push_ref(mod_node.hir_id, HirRefElem::Mod(mod_node));
    {
        let ModDef { items, .. } = mod_node;
        collect_ids_item_list(ctxt, Act::Push(items));
    }
}

macro_rules! expand_single {
    (Compute $ctxt:expr, ($name:ident($value:expr))) => {
        $name($ctxt, Act::Compute(&mut $value));
    };

    (Push $ctxt:expr, ($name:ident($value:expr))) => {
        $name($ctxt, Act::Push(&$value));
    };

    (Compute $ctxt:expr, (?$name:ident($value:expr))) => {
        if let Some(v) = &mut $value {
            $name($ctxt, Act::Compute(v));
        }
    };
    (Push $ctxt:expr, (?$name:ident($value:expr))) => {
        if let Some(v) = &$value {
            $name($ctxt, Act::Push(v));
        }
    };
}

/// Matches on the [`Act`] and then matches again on the given expression,
/// allowing us to `match` on the various `*Kind`s in the HIR.
/// 
/// Example usage:
/// 
/// ```ignore
/// _match!((ctxt, base, base, base.kind) {
///     BaseKind::Kind1(kind1) => {(collect_ids_kind1(*kind1))},
///     BaseKind::Kind2(kind2) => {(collect_ids_kind2(*kind2))},
/// })
/// ```
/// 
/// Do note that the `*kind1` and `*kind2` *have to* be dereferenced.
/// Otherwise, the macro will build `&mut &` or `&&mut ` references,
/// which are not what we want.
macro_rules! _match {
    (($ctxt:expr, $base:expr, $t:ident, $match_expr:expr) {
        $($pat:pat => {$($single:tt),* $(,)?},)*
    }) => {
        match $base {
            Act::Compute($t) => {
                match &mut $match_expr {
                    $($pat => {
                        $(
                            expand_single!(Compute $ctxt, $single);
                        )*
                    })*
                }
            }
            Act::Push($t) => {
                match &$match_expr {
                    $($pat => {
                        $(
                            expand_single!(Push $ctxt, $single);
                        )*
                    })*
                }
            }
        }
    };

    (self $ref_kind:ident, ($ctxt:expr, $base:expr, $t:ident, $match_expr:expr) {
        $($pat:pat => {$($single:tt),*$(,)?},)*
    }) => {
        match $base {
            Act::Compute($t) => {
                $t.hir_id = $ctxt.arena.create_id();
                match &mut $match_expr {
                    $($pat => {
                        $(
                            expand_single!(Compute $ctxt, $single);
                        )*
                    })*
                }
            }
            Act::Push($t) => {
                $ctxt.arena.push_ref($t.hir_id, HirRefElem::$ref_kind($t));
                match &$match_expr {
                    $($pat => {
                        $(
                            expand_single!(Push $ctxt, $single);
                        )*
                    })*
                }
            }
        }
    };
}

/// Matches on the [`Act`] and then iterates over the given expression (which should be a `Vec<T>`),
/// calling the given function on each element.
/// 
/// Example usage:
/// 
/// ```ignore
/// let item_vec: Vec<Item> = vec![item1, item2, item3];
/// let base = Act::Compute(&mut item_vec);
/// _list!(ctxt, base, collect_ids_item);
/// ```
macro_rules! _list {
    ($ctxt:expr, $base:expr, $collect_for_item:ident) => {
        match $base {
            Act::Compute(lis) => {
                lis.iter_mut().for_each(|v| {
                    $collect_for_item($ctxt, Act::Compute(v));
                });
            }
            Act::Push(lis) => {
                lis.iter().for_each(|v| {
                    $collect_for_item($ctxt, Act::Push(v));
                });
            }
        }
    };
}

/// Matches on the [`Act`] and then calls a simple sequence of calls to the given functions.
macro_rules! _simple_seq {
    ($ctxt:expr, $base:expr, $name:ident, {$($single:tt),* $(,)?}) => {
        match $base {
            Act::Compute($name) => {
                $(
                    expand_single!(Compute $ctxt, $single);
                )*
            }
            Act::Push($name) => {
                $(
                    expand_single!(Push $ctxt, $single);
                )*
            }
        }
    };

    (self $kind:ident, $ctxt:expr, $base:expr, $name:ident, {$($single:tt),* $(,)?}) => {
        match $base {
            Act::Compute($name) => {
                $name.hir_id = $ctxt.arena.create_id();
                $(
                    expand_single!(Compute $ctxt, $single);
                )*
            }
            Act::Push($name) => {
                $ctxt.arena.push_ref($name.hir_id, HirRefElem::$kind($name));
                $(
                    expand_single!(Push $ctxt, $single);
                )*
            }
        }
    };
}

fn collect_ids_item_list<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    item_list: Act<'compute, 'hir, Vec<Item>>,
) {
    _list!(ctxt, item_list, collect_ids_item);
}

fn collect_ids_item<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    item_node: Act<'compute, 'hir, Item>,
) {
    _match!(self Item, (ctxt, item_node, item_node, item_node.kind) {
        ItemKind::FnDef(fn_def) => {(collect_ids_fn_def(*fn_def))},
        ItemKind::Stmt(stmt) => {(collect_ids_stmt(*stmt))},
    });
}

fn collect_ids_fn_def<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    fn_def_node: Act<'compute, 'hir, FnDef>,
) {
    _simple_seq!(self Fn, ctxt, fn_def_node, fn_def_node, {
        (collect_ids_fn_def_params(fn_def_node.params)),
        (collect_ids_block(fn_def_node.body)),
    });
}

fn collect_ids_fn_def_params<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    params: Act<'compute, 'hir, Vec<FnParam>>,
) {
    _list!(ctxt, params, collect_ids_fn_def_param);
}

fn collect_ids_fn_def_param<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    param_node: Act<'compute, 'hir, FnParam>,
) {
    _simple_seq!(self FnParam,
        ctxt,
        param_node,
        param_node,
        {
            (collect_ids_pat(param_node.pat)),
            (collect_ids_ty_ref(param_node.ty)),
            (?collect_ids_expr(param_node.default)),
        }
    );
}

fn collect_ids_ty_ref<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    ty_ref_node: Act<'compute, 'hir, TyRef>,
) {
    _match!(self TyRef, (ctxt, ty_ref_node, ty_ref_node, ty_ref_node.kind) {
        TyRefKind::Named(name, generics) => {
            (?collect_ids_ty_generics(*generics)),
        },
        TyRefKind::Primitive(primitive_kind) => {

        },
    });
}

fn collect_ids_ty_generics<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    generics_node: Act<'compute, 'hir, TyGenericArgs>,
) {
    _simple_seq!(ctxt, generics_node, generics_node, {
        (collect_ids_ty_generic_args_vec(generics_node.args))
    });
}

fn collect_ids_ty_generic_args_vec<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    generics: Act<'compute, 'hir, Vec<TyGenericArg>>,
) {
    _list!(ctxt, generics, collect_ids_ty_generic_arg);
}

fn collect_ids_ty_generic_arg<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    generic_arg_node: Act<'compute, 'hir, TyGenericArg>,
) {
    _match!(self TyGenericArg, (ctxt, generic_arg_node, generic_arg_node, generic_arg_node.kind) {
        TyGenericArgKind::ConstVal(expr) => {(collect_ids_expr(*expr))},
        TyGenericArgKind::Type(ty_ref) => {(collect_ids_ty_ref(*ty_ref))},
    });
}

fn collect_ids_expr<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    expr_node: Act<'compute, 'hir, Expr>,
) {
    _match!(self Expr, (ctxt, expr_node, expr_node, expr_node.kind) {
        ExprKind::Atom(_) => {},
        ExprKind::Binary {left, op: _, right} => {
            (collect_ids_expr(*left)),
            (collect_ids_expr(*right)),
        },
        ExprKind::CallExpr(call_expr) => {(collect_ids_call_expr(*call_expr))},
        ExprKind::IndexExpr(index_expr) => {(collect_ids_index_expr(*index_expr))},
        ExprKind::FieldAccess(field_access) => {(collect_ids_field_access_expr(*field_access))},
        ExprKind::MethodCall(method_call) => {(collect_ids_method_call_expr(*method_call))},
    });
}

fn collect_ids_call_expr<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    call_expr_node: Act<'compute, 'hir, CallExpr>,
) {
    _simple_seq!(ctxt, call_expr_node, call_expr_node,
        {
            (collect_ids_expr(call_expr_node.callee)),
            (collect_ids_call_expr_args(call_expr_node.args)),
        }
    );
}

fn collect_ids_index_expr<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    index_expr_node: Act<'compute, 'hir, IndexExpr>,
) {
    _simple_seq!(ctxt, index_expr_node, index_expr_node,
        {
            (collect_ids_expr(index_expr_node.base)),
            (collect_ids_expr(index_expr_node.index)),
        }
    );
}

fn collect_ids_field_access_expr<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    field_access_expr: Act<'compute, 'hir, FieldAccess>,
) {
    _simple_seq!(ctxt, field_access_expr, field_access_expr,
        {
            (collect_ids_expr(field_access_expr.base)),
        }
    );
}

fn collect_ids_method_call_expr<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    method_call_node: Act<'compute, 'hir, MethodCall>,
) {
    _simple_seq!(ctxt, method_call_node, method_call_node,
        {
            (collect_ids_expr(method_call_node.base)),
            (collect_ids_call_expr_args(method_call_node.args)),
        }
    );
}

fn collect_ids_call_expr_args<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    call_expr_args_node: Act<'compute, 'hir, CallExprArgs>,
) {
    _simple_seq!(ctxt, call_expr_args_node, call_expr_args_node,
        {
            (?collect_ids_call_expr_args_list(call_expr_args_node.args)),
            (?collect_ids_lambda_expr(call_expr_args_node.trailing_lambda)),
        }
    );
}

fn collect_ids_call_expr_args_list<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    args: Act<'compute, 'hir, Vec<Expr>>,
) {
    _list!(ctxt, args, collect_ids_expr)
}

fn collect_ids_lambda_expr<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    lambda_expr_node: Act<'compute, 'hir, LambdaExpr>,
) {
    _simple_seq!(ctxt, lambda_expr_node, lambda_expr_node,
        {
            (?collect_ids_lambda_param_list(lambda_expr_node.lambda_param_list)),
            (collect_ids_item_list(lambda_expr_node.body)),
        }
    );
}

fn collect_ids_lambda_param_list<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    lambda_param_list_node: Act<'compute, 'hir, LambdaParamList>,
) {
    _simple_seq!(ctxt, lambda_param_list_node, lambda_param_list_node, {
        (collect_ids_lambda_param_list_vec(lambda_param_list_node.params))
    });
}

fn collect_ids_lambda_param_list_vec<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    lambda_param_list: Act<'compute, 'hir, Vec<LambdaParam>>,
) {
    _list!(ctxt, lambda_param_list, collect_ids_lambda_param);
}

fn collect_ids_lambda_param<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    lambda_param_node: Act<'compute, 'hir, LambdaParam>,
) {
    _simple_seq!(ctxt, lambda_param_node, lambda_param_node, {
        (collect_ids_pat(lambda_param_node.pat)),
        (?collect_ids_ty_ref(lambda_param_node.ty)),
    });
}

fn collect_ids_pat<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    pat_node: Act<'compute, 'hir, Pat>,
) {
    _match!(self Pat, (ctxt, pat_node, pat_node, pat_node.kind) {
        PatKind::Ident(_ident) => {},
        PatKind::TupleLike(_tuple) => {},
        PatKind::Wildcard(_ident) => {},
    })
}

fn collect_ids_stmt<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    stmt_node: Act<'compute, 'hir, Stmt>,
) {
    _match!(self Stmt, (ctxt, stmt_node, stmt_node, stmt_node.kind) {
        StmtKind::LetStmt(let_stmt) => {
            (collect_ids_let_stmt(*let_stmt)),
        },
        StmtKind::ExprStmt(expr) => {
            (collect_ids_expr(*expr)),
        },
        StmtKind::ForStmt(for_stmt) => {
            (collect_ids_for_stmt(*for_stmt)),
        },
        StmtKind::WhileStmt(while_stmt) => {
            (collect_ids_while_stmt(*while_stmt)),
        },
        StmtKind::AssignmentStmt(assignment_stmt) => {
            (collect_ids_assignment_stmt(*assignment_stmt)),
        },
    });
}

fn collect_ids_let_stmt<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    let_stmt_node: Act<'compute, 'hir, LetStmt>,
) {
    _simple_seq!(ctxt, let_stmt_node, let_stmt_node,
        {
            (collect_ids_pat(let_stmt_node.pat)),
            (?collect_ids_ty_ref(let_stmt_node.ty)),
            (?collect_ids_expr(let_stmt_node.init)),
        }
    );
}

fn collect_ids_for_stmt<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    for_stmt: Act<'compute, 'hir, ForStmt>,
) {
    _simple_seq!(ctxt, for_stmt, for_stmt,
        {
            (collect_ids_pat(for_stmt.pat)),
            (collect_ids_expr(for_stmt.iter)),
            (collect_ids_block(for_stmt.body)),
        }
    );
}

fn collect_ids_while_stmt<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    while_stmt: Act<'compute, 'hir, WhileStmt>,
) {
    _simple_seq!(ctxt, while_stmt, while_stmt,
        {
            (collect_ids_expr(while_stmt.expr)),
            (collect_ids_block(while_stmt.body)),
        }
    );
}

fn collect_ids_assignment_stmt<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    assignment_stmt: Act<'compute, 'hir, AssignmentStmt>,
) {
    _simple_seq!(ctxt, assignment_stmt, assignment_stmt,
        {
            (collect_ids_expr(assignment_stmt.lhs)),
            (collect_ids_expr(assignment_stmt.rhs)),
        }
    );
}

fn collect_ids_block<'hir, 'compute>(
    ctxt: &mut HirCollectIdsCtxt<'hir>,
    block_node: Act<'compute, 'hir, Block>,
) {
    _simple_seq!(self Block, ctxt, block_node, block_node,
        {
            (collect_ids_item_list(block_node.items)),
        }
    );
}
