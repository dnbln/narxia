//! Lowering the syntree to the HIR.
//!
//! The HIR is a high-level intermediate representation of the source code.
//! It is a tree-like structure that represents the source code in a more
//! abstract way than the syntax tree.
//!
//! The HIR is used to perform type checking and other analyses on the source
//! code.
//!
//! This module contains the code that lowers the syntax tree to the HIR.

use narxia_src_db::SrcFile;
use narxia_syn::syntree;
use narxia_syn::syntree::{Token, TreeNode};

use crate::hir::*;
use crate::hir_map::{HirElem, HirMap};
use crate::{HirId, HirSpan};

struct HirLowerCtxt<'arena> {
    src_file: SrcFile,
    hir_ref_arena: &'arena mut HirMap,
}

trait HasHirSpan {
    fn span(&self) -> HirSpan;
}

impl<T> HasHirSpan for T
where
    T: TreeNode,
{
    fn span(&self) -> HirSpan {
        HirSpan::of_node(self)
    }
}

impl HasHirSpan for HirSpan {
    fn span(&self) -> HirSpan {
        *self
    }
}

impl<'arena> HirLowerCtxt<'arena> {
    fn push_ref(&mut self, elem: HirElem, span: HirSpan) -> HirId {
        self.hir_ref_arena.push_ref(elem, span)
    }
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

pub struct LowerCtxt<'ctx> {
    pub src_file: SrcFile,
    pub hir_map: &'ctx mut HirMap,
}

pub fn lower_mod_def(lower_ctxt: &mut LowerCtxt, root: syntree::Root) -> ModId {
    let mut hir_lower_ctxt = HirLowerCtxt {
        src_file: lower_ctxt.src_file,
        hir_ref_arena: lower_ctxt.hir_map,
    };
    lower_mod_def_impl(&mut hir_lower_ctxt, root)
}

fn lower_item_list(
    hir_lower_ctxt: &mut HirLowerCtxt,
    item_list: impl Iterator<Item = syntree::Item>,
) -> ItemList {
    ItemList {
        items: item_list
            .map(|it| lower_item(hir_lower_ctxt, &it))
            .collect(),
    }
}

fn lower_mod_def_impl(hir_lower_ctxt: &mut HirLowerCtxt, root: syntree::Root) -> ModId {
    let mod_def = ModDef {
        items: lower_item_list(hir_lower_ctxt, root.get_item_list()),
    };

    ModId(hir_lower_ctxt.push_ref(HirElem::Mod(mod_def), root.span()))
}

fn lower_item(hir_lower_ctxt: &mut HirLowerCtxt, item: &syntree::Item) -> ItemId {
    let hir_item = if let Some(fn_def) = item.get_fn_def() {
        Item {
            kind: ItemKind::FnDef(lower_fn_def(hir_lower_ctxt, &fn_def)),
        }
    } else if let Some(stmt) = item.get_stmt() {
        Item {
            kind: ItemKind::Stmt(lower_stmt(hir_lower_ctxt, &stmt)),
        }
    } else {
        todo!()
    };
    ItemId(hir_lower_ctxt.push_ref(HirElem::Item(hir_item), item.span()))
}

fn lower_fn_def(hir_lower_ctxt: &mut HirLowerCtxt, fn_def: &syntree::FnDef) -> FnId {
    let head = fn_def.get_fn_head();
    let name = lower_ident(hir_lower_ctxt, &head.get_fn_name().unwrap().get_ident());
    let params = lower_fn_def_params(hir_lower_ctxt, &head.get_fn_param_list().unwrap());
    let ret_ty = head
        .get_fn_ret_ty()
        .as_ref()
        .map(|r| lower_fn_ret_ty(hir_lower_ctxt, r));
    let body = lower_block(hir_lower_ctxt, &fn_def.get_block().unwrap());

    let hir_fn_def = FnDef {
        name,
        generics: None,
        params,
        ret_ty,
        body,
    };

    FnId(hir_lower_ctxt.push_ref(HirElem::Fn(hir_fn_def), fn_def.span()))
}

fn lower_fn_ret_ty(hir_lower_ctxt: &mut HirLowerCtxt, ret_ty: &syntree::FnRetTy) -> FnRetTy {
    FnRetTy {
        span: HirSpan::of_node(ret_ty),
        arrow_span: HirSpan::of(&ret_ty.get_arrow()),
        ty: lower_ty_ref(hir_lower_ctxt, &ret_ty.get_ty_ref().unwrap()),
    }
}

fn lower_fn_def_params(
    hir_lower_ctxt: &mut HirLowerCtxt,
    fn_param_list: &syntree::FnParamList,
) -> Vec<FnParam> {
    fn_param_list
        .get_fn_param_list()
        .map(|it| lower_fn_def_param(hir_lower_ctxt, &it))
        .collect()
}

fn lower_fn_def_param(hir_lower_ctxt: &mut HirLowerCtxt, fn_param: &syntree::FnParam) -> FnParam {
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
    }
}

fn lower_expr(hir_lower_ctxt: &mut HirLowerCtxt, expr: &syntree::Expr) -> ExprId {
    let hir_expr = match expr {
        syntree::Expr::ExprAtom(atom) => Expr {
            kind: ExprKind::Atom(lower_expr_atom(hir_lower_ctxt, atom)),
        },
        syntree::Expr::ExprNode(node) => return lower_expr_node(hir_lower_ctxt, node),
        syntree::Expr::BinaryOpExpr(binary_op_expr) => {
            let (left, op, right) = lower_binary_op_expr(hir_lower_ctxt, binary_op_expr);
            Expr {
                kind: ExprKind::Binary(BinaryOpExpr {
                    lhs: left,
                    op,
                    rhs: right,
                }),
            }
        }
        syntree::Expr::CallExpr(call_expr) => Expr {
            kind: ExprKind::CallExpr(lower_call_expr(hir_lower_ctxt, call_expr)),
        },
        syntree::Expr::IndexExpr(index_expr) => Expr {
            kind: ExprKind::IndexExpr(lower_index_expr(hir_lower_ctxt, index_expr)),
        },
        syntree::Expr::FieldAccess(field_access) => Expr {
            kind: ExprKind::FieldAccess(lower_field_access(hir_lower_ctxt, field_access)),
        },
        syntree::Expr::MethodCall(method_call) => Expr {
            kind: ExprKind::MethodCall(lower_method_call(hir_lower_ctxt, method_call)),
        },
        syntree::Expr::Block(block) => {
            let block = lower_block(hir_lower_ctxt, block);
            Expr {
                kind: ExprKind::Atom(ExprAtom {
                    kind: ExprAtomKind::BlockExpr(BlockExpr { block }),
                }),
            }
        }
        syntree::Expr::CustomInfixExpr(e) => {
            let base = lower_expr(hir_lower_ctxt, &e.get_expr());
            let infix = e.get_custom_infix_expr_infix().unwrap();
            let op = lower_ident(hir_lower_ctxt, &infix.get_name());
            let arg = lower_expr_node(
                hir_lower_ctxt,
                &infix
                    .get_custom_infix_expr_infix_arg()
                    .unwrap()
                    .get_expr_node(),
            );

            Expr {
                kind: ExprKind::CustomInfix(CustomInfixExpr {
                    base,
                    name: op,
                    arg,
                }),
            }
        }
    };

    ExprId(hir_lower_ctxt.push_ref(HirElem::Expr(hir_expr), expr.span()))
}

fn lower_call_expr(hir_lower_ctxt: &mut HirLowerCtxt, call_expr: &syntree::CallExpr) -> CallExpr {
    let callee = lower_expr(hir_lower_ctxt, &call_expr.get_expr());
    let args = lower_call_expr_args(hir_lower_ctxt, &call_expr.get_call_expr_args().unwrap());
    CallExpr { callee, args }
}

fn lower_call_expr_args(
    hir_lower_ctxt: &mut HirLowerCtxt,
    call_expr_args: &syntree::CallExprArgs,
) -> CallExprArgs {
    let mut args = call_expr_args
        .get_call_expr_args_list()
        .as_ref()
        .map(|it| {
            it.get_expr_node_list()
                .map(|it| lower_expr_node(hir_lower_ctxt, &it))
        })
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

    args.extend(call_expr_args.get_call_expr_arg_lambda().map(|it| {
        let expr = Expr {
            kind: ExprKind::Atom(ExprAtom {
                kind: ExprAtomKind::LambdaExpr(lower_lambda_expr(
                    hir_lower_ctxt,
                    &it.get_lambda_expr(),
                )),
            }),
        };
        ExprId(hir_lower_ctxt.push_ref(HirElem::Expr(expr), it.span()))
    }));
    CallExprArgs { args }
}

fn lower_index_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    index_expr: &syntree::IndexExpr,
) -> IndexExpr {
    let base = lower_expr(hir_lower_ctxt, &index_expr.get_expr());
    let index = lower_expr_node(
        hir_lower_ctxt,
        &index_expr
            .get_index_expr_index()
            .unwrap()
            .get_expr_node()
            .unwrap(),
    );
    IndexExpr { base, index }
}

fn lower_field_access(
    hir_lower_ctxt: &mut HirLowerCtxt,
    field_access: &syntree::FieldAccess,
) -> FieldAccess {
    let base = lower_expr(hir_lower_ctxt, &field_access.get_expr());
    let field = lower_ident(hir_lower_ctxt, &field_access.get_field_name().unwrap());
    FieldAccess { base, field }
}

fn lower_method_call(
    hir_lower_ctxt: &mut HirLowerCtxt,
    method_call: &syntree::MethodCall,
) -> MethodCall {
    let base = lower_expr(hir_lower_ctxt, &method_call.get_expr());
    let method = lower_ident(hir_lower_ctxt, &method_call.get_method_name().unwrap());
    let args = lower_call_expr_args(hir_lower_ctxt, &method_call.get_call_expr_args().unwrap());
    MethodCall { base, method, args }
}

fn lower_lambda_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    lambda_expr: &syntree::LambdaExpr,
) -> LambdaExpr {
    let lambda_param_list = lambda_expr
        .get_lambda_param_list()
        .as_ref()
        .map(|lpl| lower_lambda_param_list(hir_lower_ctxt, lpl));
    let body = lower_item_list(hir_lower_ctxt, lambda_expr.get_item_list());
    LambdaExpr {
        lambda_param_list,
        body,
    }
}

fn lower_lambda_param_list(
    hir_lower_ctxt: &mut HirLowerCtxt,
    lambda_param_list: &syntree::LambdaParamList,
) -> LambdaParamList {
    let params = lambda_param_list
        .get_lambda_param_list()
        .map(|it| lower_lambda_param(hir_lower_ctxt, &it))
        .collect();
    LambdaParamList { params }
}

fn lower_lambda_param(
    hir_lower_ctxt: &mut HirLowerCtxt,
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
    hir_lower_ctxt: &mut HirLowerCtxt,
    expr: &syntree::BinaryOpExpr,
) -> (ExprId, BinOp, ExprId) {
    let (left, op, right) = expr.lower_assume_complete();
    let left = lower_expr(hir_lower_ctxt, &left);
    let op = lower_binop(&op);
    let right = lower_expr(hir_lower_ctxt, &right);
    (left, op, right)
}

fn lower_expr_node(hir_lower_ctxt: &mut HirLowerCtxt, expr_node: &syntree::ExprNode) -> ExprId {
    lower_expr(hir_lower_ctxt, &expr_node.get_expr())
}

fn lower_expr_atom(hir_lower_ctxt: &mut HirLowerCtxt, atom: &syntree::ExprAtom) -> ExprAtom {
    if let Some(ident) = atom.get_ident() {
        ExprAtom {
            kind: ExprAtomKind::Ident(lower_ident(hir_lower_ctxt, &ident)),
        }
    } else if let Some(str_literal) = atom.get_string_literal() {
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
            kind: ExprAtomKind::IfExpr(lower_if_expr(hir_lower_ctxt, &if_expr)),
        }
    } else if let Some(return_expr) = atom.get_return_expr() {
        ExprAtom {
            kind: ExprAtomKind::ReturnExpr(lower_return_expr(hir_lower_ctxt, &return_expr)),
        }
    } else if let Some(break_expr) = atom.get_break_expr() {
        ExprAtom {
            kind: ExprAtomKind::BreakExpr(lower_break_expr(hir_lower_ctxt, &break_expr)),
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
            kind: ExprAtomKind::TupleExpr(lower_tuple_like_expr(hir_lower_ctxt, &tuple_like)),
        }
    } else {
        todo!()
    }
}

fn lower_num_literal(hir_lower_ctxt: &mut HirLowerCtxt, num_lit: &syntree::NumLit) -> NumLit {
    if let Some(num_bin) = num_lit.get_num_bin() {
        NumLit::Bin(Tk::from_token(&num_bin))
    } else if let Some(num_oct) = num_lit.get_num_oct() {
        NumLit::Oct(Tk::from_token(&num_oct))
    } else if let Some(num_dec) = num_lit.get_num_dec() {
        NumLit::Dec(Tk::from_token(&num_dec))
    } else if let Some(num_hex) = num_lit.get_num_hex() {
        NumLit::Hex(Tk::from_token(&num_hex))
    } else {
        todo!()
    }
}

fn lower_loop_expr(hir_lower_ctxt: &mut HirLowerCtxt, loop_expr: &syntree::LoopExpr) -> LoopExpr {
    let body = lower_block(hir_lower_ctxt, &loop_expr.get_block().unwrap());
    LoopExpr { body }
}

fn lower_tuple_like_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    tuple_like_expr: &syntree::TupleLikeExpr,
) -> TupleExpr {
    let exprs = tuple_like_expr
        .get_expr_node_list()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it))
        .collect();
    TupleExpr { exprs }
}

fn lower_block_extra_items(
    hir_lower_ctxt: &mut HirLowerCtxt,
    block: &syntree::Block,
    f: impl FnOnce(&mut HirLowerCtxt, &mut ItemList),
) -> BlockId {
    let mut items = lower_item_list(hir_lower_ctxt, block.get_item_list());

    f(hir_lower_ctxt, &mut items);

    let hir_block = Block { items };

    BlockId(hir_lower_ctxt.push_ref(HirElem::Block(hir_block), block.span()))
}

fn lower_block(hir_lower_ctxt: &mut HirLowerCtxt, block: &syntree::Block) -> BlockId {
    lower_block_extra_items(hir_lower_ctxt, block, |_, _| {})
}

fn lower_stmt(hir_lower_ctxt: &mut HirLowerCtxt, stmt: &syntree::Stmt) -> StmtId {
    let hir_stmt = if let Some(expr) = stmt.get_expr_node() {
        Stmt {
            kind: StmtKind::ExprStmt(lower_expr_node(hir_lower_ctxt, &expr)),
        }
    } else if let Some(let_stmt) = stmt.get_let_stmt() {
        Stmt {
            kind: StmtKind::LetStmt(lower_let_stmt(hir_lower_ctxt, &let_stmt)),
        }
    } else if let Some(for_stmt) = stmt.get_for_stmt() {
        Stmt {
            kind: StmtKind::ForStmt(lower_for_stmt(hir_lower_ctxt, &for_stmt)),
        }
    } else if let Some(while_stmt) = stmt.get_while_stmt() {
        Stmt {
            kind: StmtKind::WhileStmt(lower_while_stmt(hir_lower_ctxt, &while_stmt)),
        }
    } else if let Some(assignment_stmt) = stmt.get_assignment_stmt() {
        Stmt {
            kind: StmtKind::AssignmentStmt(lower_assignment_stmt(hir_lower_ctxt, &assignment_stmt)),
        }
    } else {
        todo!()
    };

    StmtId(hir_lower_ctxt.push_ref(HirElem::Stmt(hir_stmt), stmt.span()))
}

fn lower_let_stmt(hir_lower_ctxt: &mut HirLowerCtxt, let_stmt: &syntree::LetStmt) -> LetStmt {
    let mutability = match let_stmt.get_mut_kw() {
        Some(t) => LetMutability::Mut(HirSpan::of(&t)),
        None => LetMutability::Imm,
    };
    let pat = lower_pat(hir_lower_ctxt, &let_stmt.get_pat().unwrap());
    let ty = let_stmt
        .get_ty_ref()
        .as_ref()
        .map(|ty_ref| lower_ty_ref(hir_lower_ctxt, ty_ref));
    let init = let_stmt
        .get_expr_node()
        .as_ref()
        .map(|e| lower_expr_node(hir_lower_ctxt, e));
    LetStmt {
        mutability,
        pat,
        ty,
        init,
    }
}

fn lower_for_stmt(hir_lower_ctxt: &mut HirLowerCtxt, for_stmt: &syntree::ForStmt) -> ForStmt {
    let pat = lower_pat(hir_lower_ctxt, &for_stmt.get_for_pat().unwrap().get_pat());
    let iter = lower_expr_node(
        hir_lower_ctxt,
        &for_stmt.get_for_in_expr().unwrap().get_expr_node(),
    );
    let body = lower_block(hir_lower_ctxt, &for_stmt.get_block().unwrap());
    ForStmt { pat, iter, body }
}

fn lower_while_stmt(
    hir_lower_ctxt: &mut HirLowerCtxt,
    while_stmt: &syntree::WhileStmt,
) -> WhileStmt {
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
    hir_lower_ctxt: &mut HirLowerCtxt,
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
    hir_lower_ctxt: &mut HirLowerCtxt,
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

fn lower_str_literal(
    hir_lower_ctxt: &mut HirLowerCtxt,
    str_literal: &syntree::StringLiteral,
) -> StrLiteral {
    StrLiteral {
        span: HirSpan::of_node(str_literal),
        fragments: str_literal
            .get_string_literal_fragment_list()
            .map(|it| lower_str_literal_fragment(hir_lower_ctxt, &it))
            .collect(),
    }
}

fn lower_str_literal_fragment(
    hir_lower_ctxt: &mut HirLowerCtxt,
    fragment: &syntree::StringLiteralFragment,
) -> StrLiteralFragment {
    match fragment {
        syntree::StringLiteralFragment::StringLiteralFragTextPart(t) => StrLiteralFragment {
            kind: StrLiteralFragmentKind::Text(Tk::from_token(&t.get_string_literal_frag_text_part())),
            span: HirSpan::of_node(fragment),
        },
        syntree::StringLiteralFragment::StringLiteralFragEscapedChar(e) => {
            let t = e.get_string_literal_frag_escaped_char();
            let mut chars = t.text().chars();
            let c = chars.next().unwrap();
            assert_eq!(c, '\\');
            let c = chars.next().unwrap();

            let c = match c {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                x => x,
            };
            StrLiteralFragment {
                kind: StrLiteralFragmentKind::EscapedChar(Tk::from_token(&t), c),
                span: HirSpan::of_node(fragment),
            }
        }
        syntree::StringLiteralFragment::StringLiteralFragEscapeSequence(e) => {
            todo!()
        }
        syntree::StringLiteralFragment::StringLiteralFragDisplay(e) => StrLiteralFragment {
            kind: StrLiteralFragmentKind::Display(StrLiteralDisplayFragment {
                display_token: Tk::from_token(&e.get_display_tok()),
                span: HirSpan::of_node(fragment),
                expr: lower_displayable_to_expr(
                    hir_lower_ctxt,
                    &e.get_string_literal_frag_displayable().unwrap(),
                ),
            }),
            span: HirSpan::of_node(fragment),
        },
        syntree::StringLiteralFragment::StringLiteralFragDebug(e) => StrLiteralFragment {
            kind: StrLiteralFragmentKind::Debug(StrLiteralDebugFragment {
                debug_token: Tk::from_token(&e.get_debug_tok()),
                span: HirSpan::of_node(fragment),
                expr: lower_displayable_to_expr(
                    hir_lower_ctxt,
                    &e.get_string_literal_frag_displayable().unwrap(),
                ),
            }),
            span: HirSpan::of_node(fragment),
        },
    }
}

fn lower_displayable_to_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    displayable: &syntree::StringLiteralFragDisplayable,
) -> ExprId {
    let expr = match displayable {
        syntree::StringLiteralFragDisplayable::StringLiteralFragExpr(e) => Expr {
            kind: ExprKind::Atom(ExprAtom {
                kind: ExprAtomKind::BlockExpr(lower_block_expr(
                    hir_lower_ctxt,
                    &e.get_block_expr(),
                )),
            }),
        },
        syntree::StringLiteralFragDisplayable::StringLiteralFragIdent(e) => Expr {
            kind: ExprKind::Atom(ExprAtom {
                kind: ExprAtomKind::Ident(lower_ident(hir_lower_ctxt, &e.get_ident())),
            }),
        },
    };

    ExprId(hir_lower_ctxt.push_ref(HirElem::Expr(expr), displayable.span()))
}

fn lower_pat(hir_lower_ctxt: &mut HirLowerCtxt, pat: &syntree::Pat) -> Pat {
    if let pat_ident = pat.get_ident() {
        let pat_ident = lower_ident(hir_lower_ctxt, &pat_ident);
        let kind = match pat_ident.text.as_str() {
            "_" => PatKind::Wildcard(pat_ident),
            _ => PatKind::Ident(pat_ident),
        };
        return Pat { kind };
    } else {
        todo!()
    }
}

fn lower_ty_ref(hir_lower_ctxt: &mut HirLowerCtxt, ty_ref: &syntree::TyRef) -> TyRefId {
    let span = HirSpan::of_node(ty_ref);
    let ty_ref = if let Some(name) = ty_ref.get_ident() {
        let name = lower_ident(hir_lower_ctxt, &name);
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
            _ => TyRefKind::Named(name, TyGenericArgs { args: vec![] }), // TODO: do
        };

        TyRef { kind, span }
    } else if let Some(fn_ty) = ty_ref.get_fn_ty() {
        let fn_ty = lower_fn_ty(hir_lower_ctxt, &fn_ty);
        TyRef {
            kind: TyRefKind::Fn(fn_ty),
            span,
        }
    } else {
        todo!()
    };

    TyRefId(hir_lower_ctxt.push_ref(HirElem::TyRef(ty_ref), span))
}

fn lower_fn_ty(hir_lower_ctxt: &mut HirLowerCtxt, fn_ty: &syntree::FnTy) -> FnTy {
    let params = fn_ty.get_fn_ty_param_tys().map_or_else(Vec::new, |params| {
        lower_fn_ty_params(hir_lower_ctxt, &params)
    });
    let ret_ty = fn_ty
        .get_fn_ty_ret_ty()
        .map(|r| lower_ty_ref(hir_lower_ctxt, &r.get_ty_ref().unwrap()));
    FnTy { params, ret_ty }
}

fn lower_fn_ty_params(
    hir_lower_ctxt: &mut HirLowerCtxt,
    fn_ty_param_tys: &syntree::FnTyParamTys,
) -> Vec<TyRefId> {
    fn_ty_param_tys
        .get_ty_ref_list()
        .map(|it| lower_ty_ref(hir_lower_ctxt, &it))
        .collect()
}

fn lower_if_expr(hir_lower_ctxt: &mut HirLowerCtxt, if_expr: &syntree::IfExpr) -> IfExpr {
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
        .map(|it| lower_if_expr_else_clause(hir_lower_ctxt, &it));
    IfExpr {
        if_kw: IfKw::from_token(&if_expr.get_if_kw()),
        cond,
        then,
        else_,
    }
}

fn lower_if_expr_else_clause(
    hir_lower_ctxt: &mut HirLowerCtxt,
    if_expr_else_clause: &syntree::ElseClause,
) -> IfExprElseClause {
    IfExprElseClause {
        else_kw: ElseKw::from_token(&if_expr_else_clause.get_else_kw()),
        expr: lower_expr_node(
            hir_lower_ctxt,
            &if_expr_else_clause.get_expr_node().unwrap(),
        ),
    }
}

fn lower_return_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    return_expr: &syntree::ReturnExpr,
) -> ReturnExpr {
    let return_kw = ReturnKw::from_token(&return_expr.get_return_kw());
    let expr = return_expr
        .get_expr_node()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it));
    ReturnExpr { return_kw, expr }
}

fn lower_break_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    break_expr: &syntree::BreakExpr,
) -> BreakExpr {
    let break_kw = BreakKw::from_token(&break_expr.get_break_kw());
    let expr = break_expr
        .get_expr_node()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it));
    BreakExpr { break_kw, expr }
}

fn lower_continue_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    continue_expr: &syntree::ContinueExpr,
) -> ContinueExpr {
    let continue_kw = ContinueKw::from_token(&continue_expr.get_continue_kw());

    ContinueExpr { continue_kw }
}

fn lower_block_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    block_expr: &syntree::BlockExpr,
) -> BlockExpr {
    let block = lower_block(hir_lower_ctxt, &block_expr.get_block());
    BlockExpr { block }
}

fn lower_ident(hir_lower_ctxt: &mut HirLowerCtxt, ident: &Token) -> Ident {
    assert_eq!(ident.kind(), narxia_syn::syntax_kind::SyntaxKind::IDENT);

    let span = HirSpan::of(ident);
    let text = ident.text().to_owned();
    Ident { span, text }
}
