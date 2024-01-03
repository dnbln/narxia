use narxia_src_db::SrcFile;
use narxia_syn::syntree;
use narxia_syn::syntree::{Token, TreeNode};

use crate::hir::*;
use crate::hir_arena::HirRefArena;
use crate::hir_collect_ids::HirCollectIdsCtxt;
use crate::{HirId, HirSpan};

struct HirLowerCtxt {
    src_file: SrcFile,
}

trait HasHirSpan {
    fn span(&self) -> HirSpan;
}

impl<T> HasHirSpan for T where T: TreeNode {
    fn span(&self) -> HirSpan {
        HirSpan::of_node(self)
    }
}

impl HirLowerCtxt {
    #[cfg(hir_id_span)]
    fn dummy_hir_id<T: HasHirSpan>(&self, v: &T) -> HirId {
        HirId {
            root: self.src_file,
            id: usize::MAX,
            span: T::span(v),
        }
    }

    #[cfg(not(hir_id_span))]
    fn dummy_hir_id(&self) -> HirId {
        HirId {
            root: self.src_file,
            id: usize::MAX,
        }
    }
}

macro_rules! dummy_hir_id {
    ($ctxt:expr, $v:expr) => {
        {
            #[cfg(hir_id_span)]
            {HirLowerCtxt::dummy_hir_id($ctxt, $v)}
            #[cfg(not(hir_id_span))]
            {HirLowerCtxt::dummy_hir_id($ctxt)}
        }
    };
}

fn lower_binop(binop: &syntree::BinOp) -> BinOp {
    match binop {
        syntree::BinOp::Add(t) => BinOp::Add(HirSpan::of(t)),
        syntree::BinOp::Sub(t) => BinOp::Sub(HirSpan::of(t)),
        syntree::BinOp::Mul(t) => BinOp::Mul(HirSpan::of(t)),
        syntree::BinOp::Div(t) => BinOp::Div(HirSpan::of(t)),
        syntree::BinOp::Mod(t) => BinOp::Mod(HirSpan::of(t)),
        syntree::BinOp::Eq(t) => BinOp::Eq(HirSpan::of(t)),
        syntree::BinOp::Neq(t) => BinOp::Neq(HirSpan::of(t)),
        syntree::BinOp::Lt(t) => BinOp::Lt(HirSpan::of(t)),
        syntree::BinOp::LtEq(t) => BinOp::LtEq(HirSpan::of(t)),
        syntree::BinOp::Gt(t) => BinOp::Gt(HirSpan::of(t)),
        syntree::BinOp::GtEq(t) => BinOp::GtEq(HirSpan::of(t)),
        syntree::BinOp::And(t) => BinOp::And(HirSpan::of(t)),
        syntree::BinOp::Or(t) => BinOp::Or(HirSpan::of(t)),
        syntree::BinOp::BitAnd(t) => BinOp::BitAnd(HirSpan::of(t)),
        syntree::BinOp::BitOr(t) => BinOp::BitOr(HirSpan::of(t)),
        syntree::BinOp::Xor(t) => BinOp::Xor(HirSpan::of(t)),
    }
}

pub struct LowerCtxt {
    pub src_file: SrcFile,
    pub hir_ref_arena_start_index: HirId,
}

pub fn lower_mod_def(lower_ctxt: &mut LowerCtxt, root: syntree::Root) -> ModDef {
    let hir_lower_ctxt = HirLowerCtxt {
        src_file: lower_ctxt.src_file,
    };
    let mut mod_def = lower_mod_def_impl(&hir_lower_ctxt, root);
    let mut ref_arena = HirRefArena::new_starting_at(lower_ctxt.src_file, lower_ctxt.hir_ref_arena_start_index);
    let mut collect_ids_ctxt = HirCollectIdsCtxt::new(&mut ref_arena);
    crate::hir_collect_ids::initially_update_ids_mod(&mut collect_ids_ctxt, &mut mod_def);
    mod_def
}

fn lower_mod_def_impl(hir_lower_ctxt: &HirLowerCtxt, root: syntree::Root) -> ModDef {
    let items = root
        .get_item_list()
        .map(|item| lower_item(hir_lower_ctxt, &item))
        .collect();
    ModDef {
        items,
        hir_id: dummy_hir_id!(hir_lower_ctxt, &root),
    }
}

fn lower_item(hir_lower_ctxt: &HirLowerCtxt, item: &syntree::Item) -> Item {
    let hir_id = dummy_hir_id!(hir_lower_ctxt, item);
    if let Some(fn_def) = item.get_fn_def() {
        Item {
            kind: ItemKind::FnDef(lower_fn_def(hir_lower_ctxt, &fn_def)),
            hir_id,
        }
    } else if let Some(stmt) = item.get_stmt() {
        Item {
            kind: ItemKind::Stmt(lower_stmt(hir_lower_ctxt, &stmt)),
            hir_id,
        }
    } else {
        todo!()
    }
}

fn lower_fn_def(hir_lower_ctxt: &HirLowerCtxt, fn_def: &syntree::FnDef) -> FnDef {
    let head = fn_def.get_fn_head();
    let name = lower_ident(hir_lower_ctxt, &head.get_fn_name().unwrap().get_ident());
    let params = lower_fn_def_params(hir_lower_ctxt, &head.get_fn_param_list().unwrap());
    let ret_ty = head
        .get_fn_ret_ty()
        .as_ref()
        .map(|r| lower_fn_ret_ty(hir_lower_ctxt, r));
    let body = lower_block(hir_lower_ctxt, &fn_def.get_block().unwrap());
    FnDef {
        name,
        params,
        ret_ty,
        body,
        hir_id: dummy_hir_id!(hir_lower_ctxt, fn_def),
    }
}

fn lower_fn_ret_ty(hir_lower_ctxt: &HirLowerCtxt, ret_ty: &syntree::FnRetTy) -> FnRetTy {
    FnRetTy {
        span: HirSpan::of_node(ret_ty),
        arrow_span: HirSpan::of(&ret_ty.get_arrow()),
        ty: lower_ty_ref(hir_lower_ctxt, &ret_ty.get_ty_ref().unwrap()),
        hir_id: dummy_hir_id!(hir_lower_ctxt, ret_ty),
    }
}

fn lower_fn_def_params(
    hir_lower_ctxt: &HirLowerCtxt,
    fn_param_list: &syntree::FnParamList,
) -> Vec<FnParam> {
    fn_param_list
        .get_fn_param_list()
        .map(|it| lower_fn_def_param(hir_lower_ctxt, &it))
        .collect()
}

fn lower_fn_def_param(hir_lower_ctxt: &HirLowerCtxt, fn_param: &syntree::FnParam) -> FnParam {
    let pat = lower_pat(hir_lower_ctxt, &fn_param.get_fn_param_name().get_pat());
    let ty = lower_ty_ref(
        hir_lower_ctxt,
        &fn_param.get_fn_param_ty().unwrap().get_ty_ref(),
    );
    let default = fn_param
        .get_fn_param_default()
        .as_ref()
        .map(|it| it.get_expr_node().unwrap())
        .as_ref()
        .map(|e| lower_expr_node(hir_lower_ctxt, e));
    FnParam {
        pat,
        ty,
        default,
        param_span: HirSpan::of_node(fn_param),
        hir_id: dummy_hir_id!(hir_lower_ctxt, fn_param),
    }
}

fn lower_expr(hir_lower_ctxt: &HirLowerCtxt, expr: &syntree::Expr) -> Expr {
    let hir_id = dummy_hir_id!(hir_lower_ctxt, expr);
    match expr {
        syntree::Expr::ExprAtom(atom) => Expr {
            kind: ExprKind::Atom(lower_expr_atom(hir_lower_ctxt, atom)),
            hir_id,
        },
        syntree::Expr::ExprNode(node) => lower_expr_node(hir_lower_ctxt, node),
        syntree::Expr::BinaryOpExpr(binary_op_expr) => {
            let (left, op, right) = lower_binary_op_expr(hir_lower_ctxt, binary_op_expr);
            Expr {
                kind: ExprKind::Binary(BinaryOpExpr {
                    lhs: Box::new(left),
                    op,
                    rhs: Box::new(right),
                }),
                hir_id,
            }
        }
        syntree::Expr::CallExpr(call_expr) => Expr {
            kind: ExprKind::CallExpr(lower_call_expr(hir_lower_ctxt, call_expr)),
            hir_id,
        },
        syntree::Expr::IndexExpr(index_expr) => Expr {
            kind: ExprKind::IndexExpr(lower_index_expr(hir_lower_ctxt, index_expr)),
            hir_id,
        },
        syntree::Expr::FieldAccess(field_access) => Expr {
            kind: ExprKind::FieldAccess(lower_field_access(hir_lower_ctxt, field_access)),
            hir_id,
        },
        syntree::Expr::MethodCall(method_call) => Expr {
            kind: ExprKind::MethodCall(lower_method_call(hir_lower_ctxt, method_call)),
            hir_id,
        },
        syntree::Expr::Block(block) => {
            let block = lower_block(hir_lower_ctxt, block);
            Expr {
                kind: ExprKind::Atom(ExprAtom {
                    kind: ExprAtomKind::BlockExpr(BlockExpr { block }),
                }),
                hir_id,
            }
        }
    }
}

fn lower_call_expr(hir_lower_ctxt: &HirLowerCtxt, call_expr: &syntree::CallExpr) -> CallExpr {
    let callee = Box::new(lower_expr(hir_lower_ctxt, &call_expr.get_expr()));
    let args = lower_call_expr_args(hir_lower_ctxt, &call_expr.get_call_expr_args().unwrap());
    CallExpr { callee, args }
}

fn lower_call_expr_args(
    hir_lower_ctxt: &HirLowerCtxt,
    call_expr_args: &syntree::CallExprArgs,
) -> CallExprArgs {
    let args = call_expr_args.get_call_expr_args_list().map(|it| {
        it.get_expr_node_list()
            .map(|it| lower_expr_node(hir_lower_ctxt, &it))
            .collect()
    });
    let trailing_lambda = call_expr_args
        .get_call_expr_arg_lambda()
        .map(|it| lower_lambda_expr(hir_lower_ctxt, &it.get_lambda_expr()));
    CallExprArgs {
        args,
        trailing_lambda,
    }
}

fn lower_index_expr(hir_lower_ctxt: &HirLowerCtxt, index_expr: &syntree::IndexExpr) -> IndexExpr {
    let base = Box::new(lower_expr(hir_lower_ctxt, &index_expr.get_expr()));
    let index = Box::new(lower_expr_node(
        hir_lower_ctxt,
        &index_expr
            .get_index_expr_index()
            .unwrap()
            .get_expr_node()
            .unwrap(),
    ));
    IndexExpr { base, index }
}

fn lower_field_access(
    hir_lower_ctxt: &HirLowerCtxt,
    field_access: &syntree::FieldAccess,
) -> FieldAccess {
    let base = Box::new(lower_expr(hir_lower_ctxt, &field_access.get_expr()));
    let field = lower_ident(hir_lower_ctxt, &field_access.get_field_name().unwrap());
    FieldAccess { base, field }
}

fn lower_method_call(
    hir_lower_ctxt: &HirLowerCtxt,
    method_call: &syntree::MethodCall,
) -> MethodCall {
    let base = Box::new(lower_expr(hir_lower_ctxt, &method_call.get_expr()));
    let method = lower_ident(hir_lower_ctxt, &method_call.get_method_name().unwrap());
    let args = lower_call_expr_args(hir_lower_ctxt, &method_call.get_call_expr_args().unwrap());
    MethodCall { base, method, args }
}

fn lower_lambda_expr(
    hir_lower_ctxt: &HirLowerCtxt,
    lambda_expr: &syntree::LambdaExpr,
) -> LambdaExpr {
    let lambda_param_list = lambda_expr
        .get_lambda_param_list()
        .as_ref()
        .map(|lpl| lower_lambda_param_list(hir_lower_ctxt, lpl));
    let body = lambda_expr
        .get_item_list()
        .map(|it| lower_item(hir_lower_ctxt, &it))
        .collect();
    LambdaExpr {
        lambda_param_list,
        body,
        hir_id: dummy_hir_id!(hir_lower_ctxt, lambda_expr),
    }
}

fn lower_lambda_param_list(
    hir_lower_ctxt: &HirLowerCtxt,
    lambda_param_list: &syntree::LambdaParamList,
) -> LambdaParamList {
    let params = lambda_param_list
        .get_lambda_param_list()
        .map(|it| lower_lambda_param(hir_lower_ctxt, &it))
        .collect();
    LambdaParamList { params }
}

fn lower_lambda_param(
    hir_lower_ctxt: &HirLowerCtxt,
    lambda_param: &syntree::LambdaParam,
) -> LambdaParam {
    let pat = lower_pat(hir_lower_ctxt, &lambda_param.get_fn_param_name().get_pat());
    let ty = lambda_param
        .get_fn_param_ty()
        .map(|it| it.get_ty_ref())
        .as_ref()
        .map(|ty_ref| lower_ty_ref(hir_lower_ctxt, ty_ref));
    LambdaParam { pat, ty }
}

fn lower_binary_op_expr(
    hir_lower_ctxt: &HirLowerCtxt,
    expr: &syntree::BinaryOpExpr,
) -> (Expr, BinOp, Expr) {
    let (left, op, right) = expr.lower_assume_complete();
    let left = lower_expr(hir_lower_ctxt, &left);
    let op = lower_binop(&op);
    let right = lower_expr(hir_lower_ctxt, &right);
    (left, op, right)
}

fn lower_expr_node(hir_lower_ctxt: &HirLowerCtxt, expr_node: &syntree::ExprNode) -> Expr {
    lower_expr(hir_lower_ctxt, &expr_node.get_expr())
}

fn lower_expr_atom(hir_lower_ctxt: &HirLowerCtxt, atom: &syntree::ExprAtom) -> ExprAtom {
    if let Some(ident) = atom.get_ident() {
        ExprAtom {
            kind: ExprAtomKind::Ident(lower_ident(hir_lower_ctxt, &ident)),
        }
    } else if let Some(str_literal) = atom.get_str() {
        ExprAtom {
            kind: ExprAtomKind::Str(lower_str_literal(hir_lower_ctxt, &str_literal)),
        }
    } else if let Some(num_lit) = atom.get_num_lit() {
        ExprAtom {
            kind: ExprAtomKind::Num(lower_num_literal(hir_lower_ctxt, &num_lit)),
        }
    } else if let Some(loop_expr) = atom.get_loop_expr() {
        ExprAtom {
            kind: ExprAtomKind::LoopExpr(lower_loop_expr(hir_lower_ctxt, &loop_expr)),
        }
    } else if let Some(if_expr) = atom.get_if_expr() {
        ExprAtom {
            kind: ExprAtomKind::IfExpr(Box::new(lower_if_expr(hir_lower_ctxt, &if_expr))),
        }
    } else if let Some(return_expr) = atom.get_return_expr() {
        ExprAtom {
            kind: ExprAtomKind::ReturnExpr(Box::new(lower_return_expr(
                hir_lower_ctxt,
                &return_expr,
            ))),
        }
    } else if let Some(break_expr) = atom.get_break_expr() {
        ExprAtom {
            kind: ExprAtomKind::BreakExpr(Box::new(lower_break_expr(hir_lower_ctxt, &break_expr))),
        }
    } else if let Some(continue_expr) = atom.get_continue_expr() {
        ExprAtom {
            kind: ExprAtomKind::ContinueExpr(lower_continue_expr(hir_lower_ctxt, &continue_expr)),
        }
    } else if let Some(block_expr) = atom.get_block_expr() {
        ExprAtom {
            kind: ExprAtomKind::BlockExpr(lower_block_expr(hir_lower_ctxt, &block_expr)),
        }
    } else if let Some(tuple_like) = atom.get_tuple_like_expr() {
        ExprAtom {
            kind: ExprAtomKind::TupleLikeExpr(lower_tuple_like_expr(hir_lower_ctxt, &tuple_like)),
        }
    } else {
        todo!()
    }
}

fn lower_num_literal(hir_lower_ctxt: &HirLowerCtxt, num_lit: &syntree::NumLit) -> NumLit {
    if let Some(num_bin) = num_lit.get_num_bin() {
        NumLit::Bin(num_bin)
    } else if let Some(num_oct) = num_lit.get_num_oct() {
        NumLit::Oct(num_oct)
    } else if let Some(num_dec) = num_lit.get_num_dec() {
        NumLit::Dec(num_dec)
    } else if let Some(num_hex) = num_lit.get_num_hex() {
        NumLit::Hex(num_hex)
    } else {
        todo!()
    }
}

fn lower_loop_expr(hir_lower_ctxt: &HirLowerCtxt, loop_expr: &syntree::LoopExpr) -> LoopExpr {
    let body = lower_block(hir_lower_ctxt, &loop_expr.get_block().unwrap());
    LoopExpr { body }
}

fn lower_tuple_like_expr(hir_lower_ctxt: &HirLowerCtxt, tuple_like_expr: &syntree::TupleLikeExpr) -> TupleLikeExpr {
    let exprs = tuple_like_expr
        .get_expr_node_list()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it))
        .collect();
    TupleLikeExpr { exprs }
}

fn lower_block(hir_lower_ctxt: &HirLowerCtxt, block: &syntree::Block) -> Block {
    let items = block
        .get_item_list()
        .map(|it| lower_item(hir_lower_ctxt, &it))
        .collect();
    Block {
        items,
        hir_id: dummy_hir_id!(hir_lower_ctxt, block),
    }
}

fn lower_stmt(hir_lower_ctxt: &HirLowerCtxt, stmt: &syntree::Stmt) -> Stmt {
    let hir_id = dummy_hir_id!(hir_lower_ctxt, stmt);
    if let Some(expr) = stmt.get_expr_node() {
        Stmt {
            kind: StmtKind::ExprStmt(lower_expr_node(hir_lower_ctxt, &expr)),
            hir_id,
        }
    } else if let Some(let_stmt) = stmt.get_let_stmt() {
        Stmt {
            kind: StmtKind::LetStmt(lower_let_stmt(hir_lower_ctxt, &let_stmt)),
            hir_id,
        }
    } else if let Some(for_stmt) = stmt.get_for_stmt() {
        Stmt {
            kind: StmtKind::ForStmt(lower_for_stmt(hir_lower_ctxt, &for_stmt)),
            hir_id,
        }
    } else if let Some(while_stmt) = stmt.get_while_stmt() {
        Stmt {
            kind: StmtKind::WhileStmt(lower_while_stmt(hir_lower_ctxt, &while_stmt)),
            hir_id,
        }
    } else if let Some(assignment_stmt) = stmt.get_assignment_stmt() {
        Stmt {
            kind: StmtKind::AssignmentStmt(lower_assignment_stmt(hir_lower_ctxt, &assignment_stmt)),
            hir_id,
        }
    } else {
        todo!()
    }
}

fn lower_let_stmt(hir_lower_ctxt: &HirLowerCtxt, let_stmt: &syntree::LetStmt) -> LetStmt {
    let pat = lower_pat(hir_lower_ctxt, &let_stmt.get_pat().unwrap());
    let ty = let_stmt
        .get_ty_ref()
        .as_ref()
        .map(|ty_ref| lower_ty_ref(hir_lower_ctxt, ty_ref));
    let init = let_stmt
        .get_expr_node()
        .as_ref()
        .map(|e| lower_expr_node(hir_lower_ctxt, e));
    LetStmt { pat, ty, init }
}

fn lower_for_stmt(hir_lower_ctxt: &HirLowerCtxt, for_stmt: &syntree::ForStmt) -> ForStmt {
    let pat = lower_pat(hir_lower_ctxt, &for_stmt.get_for_pat().unwrap().get_pat());
    let iter = lower_expr_node(
        hir_lower_ctxt,
        &for_stmt.get_for_in_expr().unwrap().get_expr_node(),
    );
    let body = lower_block(hir_lower_ctxt, &for_stmt.get_block().unwrap());
    ForStmt { pat, iter, body }
}

fn lower_while_stmt(hir_lower_ctxt: &HirLowerCtxt, while_stmt: &syntree::WhileStmt) -> WhileStmt {
    let expr = lower_expr_node(
        hir_lower_ctxt,
        &while_stmt
            .get_while_condition()
            .unwrap()
            .get_expr_node()
            .unwrap(),
    );
    let body = lower_block(hir_lower_ctxt, &while_stmt.get_block().unwrap());
    WhileStmt { expr, body }
}

fn lower_assignment_stmt(
    hir_lower_ctxt: &HirLowerCtxt,
    assignment_stmt: &syntree::AssignmentStmt,
) -> AssignmentStmt {
    let lhs = lower_expr_node(
        hir_lower_ctxt,
        &assignment_stmt.get_assignment_lhs().get_expr_node(),
    );
    let op_and_rhs = assignment_stmt.get_assignment_op_and_rhs_expr().unwrap();
    let op = lower_assignment_stmt_op(hir_lower_ctxt, &op_and_rhs.get_assignment_op_node());
    let rhs = lower_expr_node(hir_lower_ctxt, &op_and_rhs.get_expr_node().unwrap());
    AssignmentStmt { lhs, op, rhs }
}

fn lower_assignment_stmt_op(
    hir_lower_ctxt: &HirLowerCtxt,
    op: &syntree::AssignmentOpNode,
) -> AssignmentOp {
    match &syntree::AssignmentOp::from_token(op.get_node().first_token().unwrap()).unwrap() {
        syntree::AssignmentOp::Eq(t) => AssignmentOp::Assign(HirSpan::of(t)),
        syntree::AssignmentOp::PlusEq(t) => AssignmentOp::AddAssign(HirSpan::of(t)),
        syntree::AssignmentOp::MinusEq(t) => AssignmentOp::SubAssign(HirSpan::of(t)),
        syntree::AssignmentOp::AsteriskEq(t) => AssignmentOp::MulAssign(HirSpan::of(t)),
        syntree::AssignmentOp::SlashEq(t) => AssignmentOp::DivAssign(HirSpan::of(t)),
        syntree::AssignmentOp::PercentEq(t) => AssignmentOp::ModAssign(HirSpan::of(t)),
        syntree::AssignmentOp::AmpEq(t) => AssignmentOp::BitAndAssign(HirSpan::of(t)),
        syntree::AssignmentOp::PipeEq(t) => AssignmentOp::BitOrAssign(HirSpan::of(t)),
        syntree::AssignmentOp::CaretEq(t) => AssignmentOp::BitXorAssign(HirSpan::of(t)),
    }
}

fn lower_str_literal(hir_lower_ctxt: &HirLowerCtxt, str_literal: &Token) -> StrLiteral {
    assert_eq!(
        str_literal.kind(),
        narxia_syn::syntax_kind::SyntaxKind::STRING
    );
    let span = HirSpan::of(str_literal);
    let text = str_literal.text().to_owned();
    StrLiteral { span, text }
}

fn lower_pat(hir_lower_ctxt: &HirLowerCtxt, pat: &syntree::Pat) -> Pat {
    if let pat_ident = pat.get_ident() {
        let pat_ident = lower_ident(hir_lower_ctxt, &pat_ident);
        let kind = match pat_ident.text.as_str() {
            "_" => PatKind::Wildcard(pat_ident),
            _ => PatKind::Ident(pat_ident),
        };
        return Pat {
            kind,
            hir_id: dummy_hir_id!(hir_lower_ctxt, pat),
        };
    } else {
        todo!()
    }
}

fn lower_ty_ref(hir_lower_ctxt: &HirLowerCtxt, ty_ref: &syntree::TyRef) -> TyRef {
    let name = lower_ident(hir_lower_ctxt, &ty_ref.get_ident());
    let kind = match name.text.as_str() {
        "i8" => TyRefKind::Primitive(PrimitiveTy::I8),
        "i16" => TyRefKind::Primitive(PrimitiveTy::I16),
        "i32" => TyRefKind::Primitive(PrimitiveTy::I32),
        "i64" => TyRefKind::Primitive(PrimitiveTy::I64),
        "i128" => TyRefKind::Primitive(PrimitiveTy::I128),
        "u8" => TyRefKind::Primitive(PrimitiveTy::U8),
        "u16" => TyRefKind::Primitive(PrimitiveTy::U16),
        "u32" => TyRefKind::Primitive(PrimitiveTy::U32),
        "u64" => TyRefKind::Primitive(PrimitiveTy::U64),
        "u128" => TyRefKind::Primitive(PrimitiveTy::U128),
        "f32" => TyRefKind::Primitive(PrimitiveTy::F32),
        "f64" => TyRefKind::Primitive(PrimitiveTy::F64),
        "char" => TyRefKind::Primitive(PrimitiveTy::Char),
        "bool" => TyRefKind::Primitive(PrimitiveTy::Bool),
        _ => TyRefKind::Named(name, None),
    };
    TyRef {
        kind,
        span: HirSpan::of_node(ty_ref),
        hir_id: dummy_hir_id!(hir_lower_ctxt, ty_ref),
    }
}

fn lower_if_expr(hir_lower_ctxt: &HirLowerCtxt, if_expr: &syntree::IfExpr) -> IfExpr {
    let cond = lower_expr_node(
        hir_lower_ctxt,
        &if_expr.get_if_condition().unwrap().get_expr_node().unwrap(),
    );
    let then = lower_expr_node(
        hir_lower_ctxt,
        &if_expr.get_if_then_clause().unwrap().get_expr_node(),
    );
    let else_ = if_expr
        .get_else_clause()
        .as_ref()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it.get_expr_node().unwrap()));
    IfExpr { cond, then, else_ }
}

fn lower_return_expr(
    hir_lower_ctxt: &HirLowerCtxt,
    return_expr: &syntree::ReturnExpr,
) -> ReturnExpr {
    let return_kw = return_expr.get_return_kw();
    let expr = return_expr
        .get_expr_node()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it));
    ReturnExpr { return_kw, expr }
}

fn lower_break_expr(hir_lower_ctxt: &HirLowerCtxt, break_expr: &syntree::BreakExpr) -> BreakExpr {
    let break_kw = break_expr.get_break_kw();
    let expr = break_expr
        .get_expr_node()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it));
    BreakExpr { break_kw, expr }
}

fn lower_continue_expr(
    hir_lower_ctxt: &HirLowerCtxt,
    continue_expr: &syntree::ContinueExpr,
) -> ContinueExpr {
    let continue_kw = continue_expr.get_continue_kw();

    ContinueExpr { continue_kw }
}

fn lower_block_expr(hir_lower_ctxt: &HirLowerCtxt, block_expr: &syntree::BlockExpr) -> BlockExpr {
    let block = lower_block(hir_lower_ctxt, &block_expr.get_block());
    BlockExpr { block }
}

fn lower_ident(hir_lower_ctxt: &HirLowerCtxt, ident: &Token) -> Ident {
    assert_eq!(ident.kind(), narxia_syn::syntax_kind::SyntaxKind::IDENT);

    let span = HirSpan::of(ident);
    let text = ident.text().to_owned();
    Ident { span, text }
}
