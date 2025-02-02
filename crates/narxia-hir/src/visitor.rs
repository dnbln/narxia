use std::marker::PhantomData;

use crate::hir::HirIdNewtype;
use crate::hir_map::HirMap;
use crate::{hir, HirId, HirSpan};

pub trait HirMapQ<'hir> {
    fn run_hir_map_query<T: 'hir, Q: FnOnce(&'hir HirMap) -> T>(&self, q: Q) -> T;
}

impl<'hir> HirMapQ<'hir> for &'hir HirMap {
    fn run_hir_map_query<T: 'hir, Q: FnOnce(&'hir HirMap) -> T>(&self, q: Q) -> T {
        q(self)
    }
}

macro_rules! const_token_visit_fns {
    ($($visit_name:ident -> $walk_name:ident -> $kw_ty:path),* $(,)?) => {
        macro_rules! const_token_visit_fns_decl_visits {
            () => {
                $(
                    fn $visit_name(&mut self, kw: &'hir $kw_ty) {
                        $walk_name(self, kw)
                    }
                )*
            };
        }

        macro_rules! const_token_visit_fns_decl_walks {
            () => {
                $(
                    fn $walk_name<'hir, V: HirVisitor<'hir> + ?Sized>(vis: &mut V, kw: &'hir $kw_ty) {
                        vis.visit_span(kw.span);
                    }
                )*
            };
        }
    };
}

const_token_visit_fns! {
    visit_if_kw -> walk_if_kw -> hir::IfKw,
    visit_else_kw -> walk_else_kw -> hir::ElseKw,
}

pub trait HirVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir HirMap)>(&mut self, q: Q);

    fn visit_mod_def(&mut self, mod_id: hir::ModId, mod_def: &'hir hir::ModDef) {
        walk_mod_def(self, mod_def)
    }

    fn visit_item(&mut self, item_id: hir::ItemId, item: &'hir hir::Item) {
        walk_item(self, item)
    }

    fn visit_ident(&mut self, ident: &'hir hir::Ident) {}

    fn visit_num_literal(&mut self, num: &'hir hir::NumLit) {}

    fn visit_item_list(&mut self, item_list: &'hir hir::ItemList) {
        walk_item_list(self, item_list)
    }

    fn visit_fn_def(&mut self, fn_id: hir::FnId, fn_def: &'hir hir::FnDef) {
        walk_fn_def(self, fn_def)
    }

    fn visit_stmt(&mut self, stmt_id: hir::StmtId, stmt: &'hir hir::Stmt) {
        walk_stmt(self, stmt_id, stmt)
    }

    fn visit_expr(&mut self, expr_id: hir::ExprId, expr: &'hir hir::Expr) {
        walk_expr(self, expr_id, expr)
    }

    fn visit_let_stmt(&mut self, stmt_id: hir::StmtId, let_stmt: &'hir hir::LetStmt) {
        walk_let_stmt(self, let_stmt)
    }

    fn visit_for_stmt(&mut self, stmt_id: hir::StmtId, for_stmt: &'hir hir::ForStmt) {
        walk_for_stmt(self, for_stmt)
    }

    fn visit_while_stmt(&mut self, stmt_id: hir::StmtId, while_stmt: &'hir hir::WhileStmt) {
        walk_while_stmt(self, while_stmt)
    }

    fn visit_assignment_stmt(
        &mut self,
        stmt_id: hir::StmtId,
        assignment_stmt: &'hir hir::AssignmentStmt,
    ) {
        walk_assignment_stmt(self, assignment_stmt)
    }

    fn visit_fn_param(&mut self, fn_param: &'hir hir::FnParam) {
        walk_fn_param(self, fn_param)
    }

    fn visit_fn_ret_ty(&mut self, fn_ret_ty: &'hir hir::FnRetTy) {
        walk_fn_ret_ty(self, fn_ret_ty)
    }

    fn visit_block(&mut self, block_id: hir::BlockId, block: &'hir hir::Block) {
        walk_block(self, block)
    }

    fn visit_block_expr(&mut self, expr_id: hir::ExprId, block_expr: &'hir hir::BlockExpr) {
        walk_block_expr(self, block_expr)
    }

    fn visit_ty_ref(&mut self, ty_ref_id: hir::TyRefId, ty_ref: &'hir hir::TyRef) {
        walk_ty_ref(self, ty_ref)
    }

    fn visit_pat(&mut self, pat: &'hir hir::Pat) {
        walk_pat(self, pat)
    }

    fn visit_ty_generic_args(&mut self, generic_args: &'hir hir::TyGenericArgs) {
        walk_ty_generic_args(self, generic_args)
    }

    fn visit_ty_generic_arg(&mut self, generic_arg: &'hir hir::TyGenericArg) {
        walk_ty_generic_arg(self, generic_arg)
    }

    fn visit_fn_ty_ref(&mut self, fn_ty: &'hir hir::FnTy) {
        walk_fn_ty_ref(self, fn_ty)
    }

    fn visit_expr_atom(&mut self, expr_id: hir::ExprId, atom: &'hir hir::ExprAtom) {
        walk_expr_atom(self, expr_id, atom)
    }

    fn visit_str_literal(&mut self, str_literal: &'hir hir::StrLiteral) {
        walk_str_literal(self, str_literal)
    }

    fn visit_str_literal_fragment(&mut self, str_literal_fragment: &'hir hir::StrLiteralFragment) {
        walk_str_literal_fragment(self, str_literal_fragment)
    }

    fn visit_str_display_fragment(
        &mut self,
        str_display_fragment: &'hir hir::StrLiteralDisplayFragment,
    ) {
        walk_str_display_fragment(self, str_display_fragment)
    }

    fn visit_str_debug_fragment(&mut self, str_debug_fragment: &'hir hir::StrLiteralDebugFragment) {
        walk_str_debug_fragment(self, str_debug_fragment)
    }

    fn visit_expr_binary_expr(
        &mut self,
        expr_id: hir::ExprId,
        binary_expr: &'hir hir::BinaryOpExpr,
    ) {
        walk_expr_binary_expr(self, binary_expr)
    }

    fn visit_expr_call_expr(&mut self, expr_id: hir::ExprId, call_expr: &'hir hir::CallExpr) {
        walk_expr_call_expr(self, call_expr)
    }

    fn visit_expr_index_expr(&mut self, expr_id: hir::ExprId, index_expr: &'hir hir::IndexExpr) {
        walk_expr_index_expr(self, index_expr)
    }

    fn visit_expr_field_access(
        &mut self,
        expr_id: hir::ExprId,
        field_access: &'hir hir::FieldAccess,
    ) {
        walk_expr_field_access(self, field_access)
    }

    fn visit_expr_method_call(&mut self, expr_id: hir::ExprId, method_call: &'hir hir::MethodCall) {
        walk_expr_method_call(self, method_call)
    }

    fn visit_custom_infix_expr(
        &mut self,
        expr_id: hir::ExprId,
        custom_infix: &'hir hir::CustomInfixExpr,
    ) {
        walk_custom_infix_expr(self, custom_infix)
    }

    fn visit_return_expr(&mut self, expr_id: hir::ExprId, ret: &'hir hir::ReturnExpr) {
        walk_return_expr(self, ret)
    }

    fn visit_break_expr(&mut self, expr_id: hir::ExprId, break_expr: &'hir hir::BreakExpr) {
        walk_break_expr(self, break_expr)
    }

    fn visit_continue_expr(
        &mut self,
        expr_id: hir::ExprId,
        continue_expr: &'hir hir::ContinueExpr,
    ) {
        walk_continue_expr(self, continue_expr)
    }

    fn visit_loop_expr(&mut self, expr_id: hir::ExprId, loop_expr: &'hir hir::LoopExpr) {
        walk_loop_expr(self, loop_expr)
    }

    fn visit_if_expr(&mut self, expr_id: hir::ExprId, if_expr: &'hir hir::IfExpr) {
        walk_if_expr(self, expr_id, if_expr)
    }

    fn visit_if_expr_else_clause(
        &mut self,
        expr_id: hir::ExprId,
        else_clause: &'hir hir::IfExprElseClause,
    ) {
        walk_if_expr_else_clause(self, else_clause)
    }

    fn visit_tuple_like_expr(
        &mut self,
        expr_id: hir::ExprId,
        tuple_like_expr: &'hir hir::TupleExpr,
    ) {
        walk_tuple_like_expr(self, tuple_like_expr)
    }

    fn visit_call_args(&mut self, call_args: &'hir hir::CallExprArgs) {
        walk_call_args(self, call_args)
    }

    fn visit_lambda_expr(&mut self, expr_id: hir::ExprId, lambda_expr: &'hir hir::LambdaExpr) {
        walk_lambda_expr(self, lambda_expr)
    }

    fn visit_lambda_param_list(&mut self, lambda_param_list: &'hir hir::LambdaParamList) {
        walk_lambda_param_list(self, lambda_param_list)
    }

    fn visit_lambda_param(&mut self, lambda_param: &'hir hir::LambdaParam) {
        walk_lambda_param(self, lambda_param)
    }

    fn visit_hir_id(&mut self, hir_id: HirId) {
        // Nothing to do.
        #[cfg(hir_id_span)]
        {
            self.visit_span(hir_id.span);
        }
    }

    #[inline(always)]
    fn end_visit_hir_id(&mut self, hir_id: HirId) {
        // Nothing to do.
    }

    #[inline(always)]
    fn visit_span(&mut self, span: HirSpan) {
        // Nothing to do.
    }

    fn visit_expr_id(&mut self, expr_id: hir::ExprId) {
        self.visit_hir_id(expr_id.hir_id());
        self.q_id_strategy(|v, hm| hm.get_expr(expr_id).accept(expr_id, v));
        self.end_visit_hir_id(expr_id.hir_id());
    }

    const_token_visit_fns_decl_visits! {}

    fn visit_item_id(&mut self, item_id: hir::ItemId) {
        self.visit_hir_id(item_id.hir_id());
        self.q_id_strategy(|v, hm| hm.get_item(item_id).accept(item_id, v));
        self.end_visit_hir_id(item_id.hir_id());
    }

    fn visit_stmt_id(&mut self, stmt_id: hir::StmtId) {
        self.visit_hir_id(stmt_id.hir_id());
        self.q_id_strategy(|v, hm| hm.get_stmt(stmt_id).accept(stmt_id, v));
        self.end_visit_hir_id(stmt_id.hir_id());
    }

    fn visit_block_id(&mut self, block_id: hir::BlockId) {
        self.visit_hir_id(block_id.hir_id());
        self.q_id_strategy(|v, hm| hm.get_block(block_id).accept(block_id, v));
        self.end_visit_hir_id(block_id.hir_id());
    }

    fn visit_fn_id(&mut self, fn_id: hir::FnId) {
        self.visit_hir_id(fn_id.hir_id());
        self.q_id_strategy(|v, hm| hm.get_fn(fn_id).accept(fn_id, v));
        self.end_visit_hir_id(fn_id.hir_id());
    }

    fn visit_mod_id(&mut self, mod_id: hir::ModId) {
        self.visit_hir_id(mod_id.hir_id());
        self.q_id_strategy(|v, hm| hm.get_mod(mod_id).accept(mod_id, v));
        self.end_visit_hir_id(mod_id.hir_id());
    }

    fn visit_ty_ref_id(&mut self, ty_ref_id: hir::TyRefId) {
        self.visit_hir_id(ty_ref_id.hir_id());
        self.q_id_strategy(|v, hm| hm.get_ty_ref(ty_ref_id).accept(ty_ref_id, v));
        self.end_visit_hir_id(ty_ref_id.hir_id());
    }
}

const_token_visit_fns_decl_walks!();

pub fn walk_mod_def<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    mod_def: &'hir hir::ModDef,
) {
    mod_def.items.accept(visitor);
}

pub fn walk_item_list<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    item_list: &'hir hir::ItemList,
) {
    for item in &item_list.items {
        item.accept(visitor);
    }
}

pub fn walk_item<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, item: &'hir hir::Item) {
    match &item.kind {
        hir::ItemKind::FnDef(fn_def) => {
            fn_def.accept(visitor);
        }
        hir::ItemKind::Stmt(stmt) => {
            stmt.accept(visitor);
        }
    }
}

pub fn walk_fn_def<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_def: &'hir hir::FnDef) {
    walk_fn_head(visitor, fn_def);
    fn_def.body.accept(visitor);
}

pub fn walk_fn_head<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_def: &'hir hir::FnDef) {
    for param in &fn_def.params {
        param.accept(visitor);
    }
    if let Some(ret_ty) = &fn_def.ret_ty {
        ret_ty.accept(visitor);
    }
}

pub fn walk_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    stmt_id: hir::StmtId,
    stmt: &'hir hir::Stmt,
) {
    match &stmt.kind {
        hir::StmtKind::ExprStmt(expr) => {
            expr.accept(visitor);
        }
        hir::StmtKind::LetStmt(let_stmt) => {
            let_stmt.accept(stmt_id, visitor);
        }
        hir::StmtKind::ForStmt(for_stmt) => {
            for_stmt.accept(stmt_id, visitor);
        }
        hir::StmtKind::WhileStmt(while_stmt) => {
            while_stmt.accept(stmt_id, visitor);
        }
        hir::StmtKind::AssignmentStmt(assignment_stmt) => {
            assignment_stmt.accept(stmt_id, visitor);
        }
    }
}

pub fn walk_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    expr_id: hir::ExprId,
    expr: &'hir hir::Expr,
) {
    match &expr.kind {
        hir::ExprKind::Atom(atom) => {
            atom.accept(expr_id, visitor);
        }
        hir::ExprKind::Binary(binary_expr) => {
            binary_expr.accept(expr_id, visitor);
        }
        hir::ExprKind::CallExpr(call_expr) => {
            call_expr.accept(expr_id, visitor);
        }
        hir::ExprKind::IndexExpr(index_expr) => {
            index_expr.accept(expr_id, visitor);
        }
        hir::ExprKind::FieldAccess(field_access) => {
            field_access.accept(expr_id, visitor);
        }
        hir::ExprKind::MethodCall(method_call) => {
            method_call.accept(expr_id, visitor);
        }
        hir::ExprKind::CustomInfix(custom_infix) => {
            custom_infix.accept(expr_id, visitor);
        }
    }
}

pub fn walk_expr_atom<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    expr_id: hir::ExprId,
    atom: &'hir hir::ExprAtom,
) {
    match &atom.kind {
        hir::ExprAtomKind::Ident(ident) => {
            ident.accept(visitor);
        }
        hir::ExprAtomKind::Num(num) => {
            num.accept(visitor);
        }
        hir::ExprAtomKind::Str(s) => {
            s.accept(visitor);
        }
        hir::ExprAtomKind::BlockExpr(block_expr) => {
            block_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::ReturnExpr(ret) => {
            ret.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::BreakExpr(break_expr) => {
            break_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::ContinueExpr(continue_expr) => {
            continue_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::LoopExpr(loop_expr) => {
            loop_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::IfExpr(if_expr) => {
            if_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::TupleExpr(tuple_like_expr) => {
            tuple_like_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::LambdaExpr(l) => {
            l.accept(expr_id, visitor);
        }
    }
}

pub fn walk_ty_ref<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, ty_ref: &'hir hir::TyRef) {
    match &ty_ref.kind {
        hir::TyRefKind::Named(name, generics) => {
            name.accept(visitor);
            generics.accept(visitor);
        }
        hir::TyRefKind::Primitive(_) => {}
        hir::TyRefKind::Fn(fn_ty) => {
            fn_ty.accept(visitor);
        }
    }
}

pub fn walk_ty_generic_args<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    generic_args: &'hir hir::TyGenericArgs,
) {
    for arg in &generic_args.args {
        arg.accept(visitor);
    }
}

pub fn walk_ty_generic_arg<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    generic_arg: &'hir hir::TyGenericArg,
) {
    match &generic_arg.kind {
        hir::TyGenericArgKind::Type(ty_ref) => {
            ty_ref.accept(visitor);
        }
        hir::TyGenericArgKind::ConstVal(const_val) => {
            const_val.accept(visitor);
        }
    }
}

pub fn walk_fn_param<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    fn_param: &'hir hir::FnParam,
) {
    fn_param.pat.accept(visitor);
    fn_param.ty.accept(visitor);
}

pub fn walk_fn_ret_ty<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    fn_ret_ty: &'hir hir::FnRetTy,
) {
    fn_ret_ty.ty.accept(visitor);
}

pub fn walk_let_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    let_stmt: &'hir hir::LetStmt,
) {
    let_stmt.pat.accept(visitor);
    if let Some(ty) = &let_stmt.ty {
        ty.accept(visitor);
    }
    if let Some(init) = let_stmt.init {
        init.accept(visitor);
    }
}

pub fn walk_for_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    for_stmt: &'hir hir::ForStmt,
) {
    for_stmt.pat.accept(visitor);
    for_stmt.iter.accept(visitor);
    for_stmt.body.accept(visitor);
}

pub fn walk_while_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    while_stmt: &'hir hir::WhileStmt,
) {
    while_stmt.expr.accept(visitor);
    while_stmt.body.accept(visitor);
}

pub fn walk_assignment_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    assignment_stmt: &'hir hir::AssignmentStmt,
) {
    assignment_stmt.lhs.accept(visitor);
    assignment_stmt.rhs.accept(visitor);
}

pub fn walk_block<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, block: &'hir hir::Block) {
    block.items.accept(visitor);
}

pub fn walk_block_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    block_expr: &'hir hir::BlockExpr,
) {
    block_expr.block.accept(visitor);
}

pub fn walk_expr_binary_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    binary_expr: &'hir hir::BinaryOpExpr,
) {
    binary_expr.lhs.accept(visitor);
    binary_expr.rhs.accept(visitor);
}

pub fn walk_expr_call_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    call_expr: &'hir hir::CallExpr,
) {
    call_expr.callee.accept(visitor);
    call_expr.args.accept(visitor);
}

pub fn walk_expr_index_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    index_expr: &'hir hir::IndexExpr,
) {
    index_expr.base.accept(visitor);
    index_expr.index.accept(visitor);
}

pub fn walk_expr_field_access<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    field_access: &'hir hir::FieldAccess,
) {
    field_access.base.accept(visitor);
}

pub fn walk_expr_method_call<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    method_call: &'hir hir::MethodCall,
) {
    method_call.base.accept(visitor);
    method_call.method.accept(visitor);
    method_call.args.accept(visitor);
}

pub fn walk_return_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    ret: &'hir hir::ReturnExpr,
) {
    if let Some(expr) = ret.expr {
        expr.accept(visitor);
    }
}

pub fn walk_break_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    break_expr: &'hir hir::BreakExpr,
) {
    if let Some(expr) = break_expr.expr {
        expr.accept(visitor);
    }
}

pub fn walk_continue_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    continue_expr: &'hir hir::ContinueExpr,
) {
}

pub fn walk_custom_infix_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    custom_infix: &'hir hir::CustomInfixExpr,
) {
    custom_infix.base.accept(visitor);
    custom_infix.name.accept(visitor);
    custom_infix.arg.accept(visitor);
}

pub fn walk_loop_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    loop_expr: &'hir hir::LoopExpr,
) {
    loop_expr.body.accept(visitor);
}

pub fn walk_if_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    expr_id: hir::ExprId,
    if_expr: &'hir hir::IfExpr,
) {
    if_expr.cond.accept(visitor);
    if_expr.then.accept(visitor);
    if let Some(else_clause) = &if_expr.else_ {
        else_clause.accept(expr_id, visitor);
    }
}

pub fn walk_if_expr_else_clause<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    else_clause: &'hir hir::IfExprElseClause,
) {
    else_clause.else_kw.accept(visitor);
    else_clause.expr.accept(visitor);
}

pub fn walk_tuple_like_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    tuple_like_expr: &'hir hir::TupleExpr,
) {
    for expr in &tuple_like_expr.exprs {
        expr.accept(visitor);
    }
}

pub fn walk_pat<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, pat: &'hir hir::Pat) {
    match &pat.kind {
        hir::PatKind::Ident(_) => {}
        hir::PatKind::Tuple(tuple_like) => {
            for pat in tuple_like {
                pat.accept(visitor);
            }
        }
        hir::PatKind::Wildcard(_) => {}
    }
}

pub fn walk_call_args<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    call_args: &'hir hir::CallExprArgs,
) {
    for arg in &call_args.args {
        arg.accept(visitor);
    }
}

pub fn walk_lambda_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    lambda_expr: &'hir hir::LambdaExpr,
) {
    if let Some(lpl) = &lambda_expr.lambda_param_list {
        lpl.accept(visitor);
    }
    lambda_expr.body.accept(visitor);
}

pub fn walk_lambda_param_list<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    lambda_param_list: &'hir hir::LambdaParamList,
) {
    for param in &lambda_param_list.params {
        param.accept(visitor);
    }
}

pub fn walk_lambda_param<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    lambda_param: &'hir hir::LambdaParam,
) {
    lambda_param.pat.accept(visitor);
    if let Some(ty) = &lambda_param.ty {
        ty.accept(visitor);
    }
}

pub fn walk_str_literal<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_literal: &'hir hir::StrLiteral,
) {
    for fragment in &str_literal.fragments {
        fragment.accept(visitor);
    }
}

pub fn walk_str_literal_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_literal_fragment: &'hir hir::StrLiteralFragment,
) {
    match &str_literal_fragment.kind {
        hir::StrLiteralFragmentKind::Text(_)
        | hir::StrLiteralFragmentKind::EscapeSequence(..)
        | hir::StrLiteralFragmentKind::EscapedChar(..) => {
            // Nothing to do.
        }
        hir::StrLiteralFragmentKind::Display(f) => {
            f.accept(visitor);
        }
        hir::StrLiteralFragmentKind::Debug(f) => {
            f.accept(visitor);
        }
    }
}

pub fn walk_str_display_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_display_fragment: &'hir hir::StrLiteralDisplayFragment,
) {
    str_display_fragment.expr.accept(visitor);
}

pub fn walk_str_debug_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_debug_fragment: &'hir hir::StrLiteralDebugFragment,
) {
    str_debug_fragment.expr.accept(visitor);
}

pub fn walk_fn_ty_ref<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_ty: &'hir hir::FnTy) {
    for ty in &fn_ty.params {
        ty.accept(visitor);
    }

    if let Some(ret_ty) = &fn_ty.ret_ty {
        ret_ty.accept(visitor);
    }
}

pub trait Visitable<'hir> {
    fn accept<V: HirVisitor<'hir> + ?Sized>(&'hir self, visitor: &mut V);
}

pub trait OwnedVisitable<'hir> {
    fn accept<V: HirVisitor<'hir> + ?Sized>(self, visitor: &mut V);
}

pub trait IdVisitable<'hir> {
    type Id;
    fn accept<V: HirVisitor<'hir> + ?Sized>(&'hir self, id: Self::Id, visitor: &mut V);
}

macro_rules! impl_visitable {
    ($($visit_name:ident ($ty:ty)),* $(,)?) => {
        $(
            impl<'hir> Visitable<'hir> for $ty {
                fn accept<V: HirVisitor<'hir>+ ?Sized>(&'hir self, visitor: &mut V) {
                    visitor.$visit_name (self);
                }
            }
        )*
    };
}

impl_visitable! {
    visit_ident(hir::Ident),
    visit_num_literal(hir::NumLit),
    visit_item_list(hir::ItemList),
    visit_fn_param(hir::FnParam),
    visit_fn_ret_ty(hir::FnRetTy),
    visit_pat(hir::Pat),
    visit_ty_generic_args(hir::TyGenericArgs),
    visit_ty_generic_arg(hir::TyGenericArg),
    visit_fn_ty_ref(hir::FnTy),
    visit_str_literal(hir::StrLiteral),
    visit_str_literal_fragment(hir::StrLiteralFragment),
    visit_str_display_fragment(hir::StrLiteralDisplayFragment),
    visit_str_debug_fragment(hir::StrLiteralDebugFragment),
    visit_call_args(hir::CallExprArgs),
    visit_lambda_param_list(hir::LambdaParamList),
    visit_lambda_param(hir::LambdaParam),
    visit_if_kw(hir::IfKw),
    visit_else_kw(hir::ElseKw),
}

macro_rules! impl_visitable_id {
    ($($visit_name:ident ($ty:ty)),* $(,)?) => {
        $(
            impl<'hir> OwnedVisitable<'hir> for $ty {
                fn accept<V: HirVisitor<'hir> + ?Sized>(self, visitor: &mut V) {
                    visitor.$visit_name (self);
                }
            }
        )*
    }
}

impl_visitable_id! {
    visit_expr_id(hir::ExprId),
    visit_item_id(hir::ItemId),
    visit_stmt_id(hir::StmtId),
    visit_block_id(hir::BlockId),
    visit_fn_id(hir::FnId),
    visit_mod_id(hir::ModId),
    visit_ty_ref_id(hir::TyRefId),

    visit_hir_id(HirId),
    visit_span(HirSpan),
}

macro_rules! impl_id_visitable {
    ($($visit_name:ident [$id:ty] ($ty:ty)),* $(,)?) => {
        $(
            impl<'hir> IdVisitable<'hir> for $ty {
                type Id = $id;
                fn accept<V: HirVisitor<'hir>+ ?Sized>(&'hir self, id: Self::Id, visitor: &mut V) {
                    visitor.$visit_name (id, self);
                }
            }
        )*
    };
}

impl_id_visitable! {
    visit_mod_def [hir::ModId] (hir::ModDef),
    visit_item [hir::ItemId] (hir::Item),
    visit_fn_def [hir::FnId] (hir::FnDef),
    visit_stmt [hir::StmtId] (hir::Stmt),
    visit_expr [hir::ExprId] (hir::Expr),
    visit_let_stmt [hir::StmtId] (hir::LetStmt),
    visit_for_stmt [hir::StmtId] (hir::ForStmt),
    visit_while_stmt [hir::StmtId] (hir::WhileStmt),
    visit_assignment_stmt [hir::StmtId] (hir::AssignmentStmt),
    visit_block [hir::BlockId] (hir::Block),
    visit_block_expr [hir::ExprId] (hir::BlockExpr),
    visit_expr_atom [hir::ExprId] (hir::ExprAtom),
    visit_expr_binary_expr [hir::ExprId] (hir::BinaryOpExpr),
    visit_expr_call_expr [hir::ExprId] (hir::CallExpr),
    visit_expr_index_expr [hir::ExprId] (hir::IndexExpr),
    visit_expr_field_access [hir::ExprId] (hir::FieldAccess),
    visit_expr_method_call [hir::ExprId] (hir::MethodCall),
    visit_custom_infix_expr [hir::ExprId] (hir::CustomInfixExpr),
    visit_return_expr [hir::ExprId] (hir::ReturnExpr),
    visit_break_expr [hir::ExprId] (hir::BreakExpr),
    visit_continue_expr [hir::ExprId] (hir::ContinueExpr),
    visit_loop_expr [hir::ExprId] (hir::LoopExpr),
    visit_if_expr [hir::ExprId] (hir::IfExpr),
    visit_if_expr_else_clause [hir::ExprId] (hir::IfExprElseClause),
    visit_tuple_like_expr [hir::ExprId] (hir::TupleExpr),
    visit_lambda_expr [hir::ExprId] (hir::LambdaExpr),
    visit_ty_ref [hir::TyRefId] (hir::TyRef),
}
