use crate::{hir, HirId};

pub trait HirVisitor<'hir> {
    fn visit_mod_def(&mut self, mod_def: &'hir hir::ModDef) {
        walk_mod_def(self, mod_def)
    }

    fn visit_item(&mut self, item: &'hir hir::Item) {
        walk_item(self, item)
    }

    fn visit_ident(&mut self, ident: &'hir hir::Ident) {
        self.visit_hir_id(ident.hir_id);
    }

    fn visit_num_literal(&mut self, num: &'hir hir::NumLit) {}

    fn visit_item_list(&mut self, item_list: &'hir hir::ItemList) {
        walk_item_list(self, item_list)
    }

    fn visit_fn_def(&mut self, fn_def: &'hir hir::FnDef) {
        walk_fn_def(self, fn_def)
    }

    fn visit_stmt(&mut self, stmt: &'hir hir::Stmt) {
        walk_stmt(self, stmt)
    }

    fn visit_expr(&mut self, expr: &'hir hir::Expr) {
        walk_expr(self, expr)
    }

    fn visit_let_stmt(&mut self, let_stmt: &'hir hir::LetStmt) {
        walk_let_stmt(self, let_stmt)
    }

    fn visit_for_stmt(&mut self, for_stmt: &'hir hir::ForStmt) {
        walk_for_stmt(self, for_stmt)
    }

    fn visit_while_stmt(&mut self, while_stmt: &'hir hir::WhileStmt) {
        walk_while_stmt(self, while_stmt)
    }

    fn visit_assignment_stmt(&mut self, assignment_stmt: &'hir hir::AssignmentStmt) {
        walk_assignment_stmt(self, assignment_stmt)
    }

    fn visit_fn_param(&mut self, fn_param: &'hir hir::FnParam) {
        walk_fn_param(self, fn_param)
    }

    fn visit_fn_ret_ty(&mut self, fn_ret_ty: &'hir hir::FnRetTy) {
        walk_fn_ret_ty(self, fn_ret_ty)
    }

    fn visit_block(&mut self, block: &'hir hir::Block) {
        walk_block(self, block)
    }

    fn visit_block_expr(&mut self, block_expr: &'hir hir::BlockExpr) {
        walk_block_expr(self, block_expr)
    }

    fn visit_ty_ref(&mut self, ty_ref: &'hir hir::TyRef) {
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

    fn visit_expr_atom(&mut self, hir_id: HirId, atom: &'hir hir::ExprAtom) {
        walk_expr_atom(self, hir_id, atom)
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

    fn visit_expr_binary_expr(&mut self, hir_id: HirId, binary_expr: &'hir hir::BinaryOpExpr) {
        walk_expr_binary_expr(self, binary_expr)
    }

    fn visit_expr_call_expr(&mut self, hir_id: HirId, call_expr: &'hir hir::CallExpr) {
        walk_expr_call_expr(self, call_expr)
    }

    fn visit_expr_index_expr(&mut self, hir_id: HirId, index_expr: &'hir hir::IndexExpr) {
        walk_expr_index_expr(self, index_expr)
    }

    fn visit_expr_field_access(&mut self, hir_id: HirId, field_access: &'hir hir::FieldAccess) {
        walk_expr_field_access(self, field_access)
    }

    fn visit_expr_method_call(&mut self, hir_id: HirId, method_call: &'hir hir::MethodCall) {
        walk_expr_method_call(self, method_call)
    }

    fn visit_return_expr(&mut self, hir_id: HirId, ret: &'hir hir::ReturnExpr) {
        walk_return_expr(self, ret)
    }

    fn visit_break_expr(&mut self, hir_id: HirId, break_expr: &'hir hir::BreakExpr) {
        walk_break_expr(self, break_expr)
    }

    fn visit_continue_expr(&mut self, hir_id: HirId, continue_expr: &'hir hir::ContinueExpr) {
        walk_continue_expr(self, continue_expr)
    }

    fn visit_loop_expr(&mut self, hir_id: HirId, loop_expr: &'hir hir::LoopExpr) {
        walk_loop_expr(self, loop_expr)
    }

    fn visit_if_expr(&mut self, hir_id: HirId, if_expr: &'hir hir::IfExpr) {
        walk_if_expr(self, if_expr)
    }

    fn visit_tuple_like_expr(&mut self, hir_id: HirId, tuple_like_expr: &'hir hir::TupleLikeExpr) {
        walk_tuple_like_expr(self, tuple_like_expr)
    }

    fn visit_call_args(&mut self, call_args: &'hir hir::CallExprArgs) {
        walk_call_args(self, call_args)
    }

    fn visit_lambda_expr(&mut self, hir_id: HirId, lambda_expr: &'hir hir::LambdaExpr) {
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
    }
}

pub fn walk_mod_def<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    mod_def: &'hir hir::ModDef,
) {
    visitor.visit_hir_id(mod_def.hir_id);
    visitor.visit_item_list(&mod_def.items);
}

pub fn walk_item_list<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    item_list: &'hir hir::ItemList,
) {
    for item in &item_list.items {
        visitor.visit_item(item);
    }
}

pub fn walk_item<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, item: &'hir hir::Item) {
    visitor.visit_hir_id(item.hir_id);
    match &item.kind {
        hir::ItemKind::FnDef(fn_def) => {
            visitor.visit_fn_def(fn_def);
        }
        hir::ItemKind::Stmt(stmt) => {
            visitor.visit_stmt(stmt);
        }
    }
}

pub fn walk_fn_def<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_def: &'hir hir::FnDef) {
    visitor.visit_hir_id(fn_def.hir_id);
    for param in &fn_def.params {
        visitor.visit_fn_param(param);
    }
    if let Some(ret_ty) = &fn_def.ret_ty {
        visitor.visit_fn_ret_ty(ret_ty);
    }
    visitor.visit_block(&fn_def.body);
}

pub fn walk_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, stmt: &'hir hir::Stmt) {
    visitor.visit_hir_id(stmt.hir_id);
    match &stmt.kind {
        hir::StmtKind::ExprStmt(expr) => {
            visitor.visit_expr(expr);
        }
        hir::StmtKind::LetStmt(let_stmt) => {
            visitor.visit_let_stmt(let_stmt);
        }
        hir::StmtKind::ForStmt(for_stmt) => {
            visitor.visit_for_stmt(for_stmt);
        }
        hir::StmtKind::WhileStmt(while_stmt) => {
            visitor.visit_while_stmt(while_stmt);
        }
        hir::StmtKind::AssignmentStmt(assignment_stmt) => {
            visitor.visit_assignment_stmt(assignment_stmt);
        }
    }
}

pub fn walk_expr<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, expr: &'hir hir::Expr) {
    visitor.visit_hir_id(expr.hir_id);
    match &expr.kind {
        hir::ExprKind::Atom(atom) => {
            visitor.visit_expr_atom(expr.hir_id, atom);
        }
        hir::ExprKind::Binary(binary_expr) => {
            visitor.visit_expr_binary_expr(expr.hir_id, binary_expr);
        }
        hir::ExprKind::CallExpr(call_expr) => {
            visitor.visit_expr_call_expr(expr.hir_id, call_expr);
        }
        hir::ExprKind::IndexExpr(index_expr) => {
            visitor.visit_expr_index_expr(expr.hir_id, index_expr);
        }
        hir::ExprKind::FieldAccess(field_access) => {
            visitor.visit_expr_field_access(expr.hir_id, field_access);
        }
        hir::ExprKind::MethodCall(method_call) => {
            visitor.visit_expr_method_call(expr.hir_id, method_call);
        }
    }
}

pub fn walk_expr_atom<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    hir_id: HirId,
    atom: &'hir hir::ExprAtom,
) {
    match &atom.kind {
        hir::ExprAtomKind::Ident(ident) => {
            visitor.visit_ident(ident);
        }
        hir::ExprAtomKind::Num(num) => {
            visitor.visit_num_literal(num);
        }
        hir::ExprAtomKind::Str(s) => {
            visitor.visit_str_literal(s);
        }
        hir::ExprAtomKind::BlockExpr(block_expr) => {
            visitor.visit_block_expr(block_expr);
        }
        hir::ExprAtomKind::ReturnExpr(ret) => {
            visitor.visit_return_expr(hir_id, ret);
        }
        hir::ExprAtomKind::BreakExpr(break_expr) => {
            visitor.visit_break_expr(hir_id, break_expr);
        }
        hir::ExprAtomKind::ContinueExpr(continue_expr) => {
            visitor.visit_continue_expr(hir_id, continue_expr);
        }
        hir::ExprAtomKind::LoopExpr(loop_expr) => {
            visitor.visit_loop_expr(hir_id, loop_expr);
        }
        hir::ExprAtomKind::IfExpr(if_expr) => {
            visitor.visit_if_expr(hir_id, if_expr);
        }
        hir::ExprAtomKind::TupleLikeExpr(tuple_like_expr) => {
            visitor.visit_tuple_like_expr(hir_id, tuple_like_expr);
        }
        hir::ExprAtomKind::LambdaExpr(l) => visitor.visit_lambda_expr(hir_id, l),
    }
}

pub fn walk_ty_ref<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, ty_ref: &'hir hir::TyRef) {
    visitor.visit_hir_id(ty_ref.hir_id);
    match &ty_ref.kind {
        hir::TyRefKind::Named(_, generics) => {
            visitor.visit_ty_generic_args(generics);
        }
        hir::TyRefKind::Primitive(_) => {}
        hir::TyRefKind::Fn(fn_ty) => visitor.visit_fn_ty_ref(fn_ty),
    }
}

pub fn walk_ty_generic_args<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    generic_args: &'hir hir::TyGenericArgs,
) {
    for arg in &generic_args.args {
        visitor.visit_ty_generic_arg(arg);
    }
}

pub fn walk_ty_generic_arg<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    generic_arg: &'hir hir::TyGenericArg,
) {
    match &generic_arg.kind {
        hir::TyGenericArgKind::Type(ty_ref) => {
            visitor.visit_ty_ref(ty_ref);
        }
        hir::TyGenericArgKind::ConstVal(const_val) => {
            visitor.visit_expr(const_val);
        }
    }
}

pub fn walk_fn_param<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    fn_param: &'hir hir::FnParam,
) {
    visitor.visit_hir_id(fn_param.hir_id);
    visitor.visit_pat(&fn_param.pat);
    visitor.visit_ty_ref(&fn_param.ty);
}

pub fn walk_fn_ret_ty<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    fn_ret_ty: &'hir hir::FnRetTy,
) {
    visitor.visit_hir_id(fn_ret_ty.hir_id);
    visitor.visit_ty_ref(&fn_ret_ty.ty);
}

pub fn walk_let_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    let_stmt: &'hir hir::LetStmt,
) {
    visitor.visit_pat(&let_stmt.pat);
    if let Some(ty) = &let_stmt.ty {
        visitor.visit_ty_ref(ty);
    }
    if let Some(init) = &let_stmt.init {
        visitor.visit_expr(init);
    }
}

pub fn walk_for_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    for_stmt: &'hir hir::ForStmt,
) {
    visitor.visit_pat(&for_stmt.pat);
    visitor.visit_expr(&for_stmt.iter);
    visitor.visit_block(&for_stmt.body);
}

pub fn walk_while_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    while_stmt: &'hir hir::WhileStmt,
) {
    visitor.visit_expr(&while_stmt.expr);
    visitor.visit_block(&while_stmt.body);
}

pub fn walk_assignment_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    assignment_stmt: &'hir hir::AssignmentStmt,
) {
    visitor.visit_expr(&assignment_stmt.lhs);
    visitor.visit_expr(&assignment_stmt.rhs);
}

pub fn walk_block<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, block: &'hir hir::Block) {
    visitor.visit_hir_id(block.hir_id);
    visitor.visit_item_list(&block.items);
}

pub fn walk_block_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    block_expr: &'hir hir::BlockExpr,
) {
    visitor.visit_block(&block_expr.block);
}

pub fn walk_expr_binary_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    binary_expr: &'hir hir::BinaryOpExpr,
) {
    visitor.visit_expr(&binary_expr.lhs);
    visitor.visit_expr(&binary_expr.rhs);
}

pub fn walk_expr_call_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    call_expr: &'hir hir::CallExpr,
) {
    visitor.visit_expr(&call_expr.callee);
    visitor.visit_call_args(&call_expr.args);
}

pub fn walk_expr_index_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    index_expr: &'hir hir::IndexExpr,
) {
    visitor.visit_expr(&index_expr.base);
    visitor.visit_expr(&index_expr.index);
}

pub fn walk_expr_field_access<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    field_access: &'hir hir::FieldAccess,
) {
    visitor.visit_expr(&field_access.base);
}

pub fn walk_expr_method_call<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    method_call: &'hir hir::MethodCall,
) {
    visitor.visit_expr(&method_call.base);
    visitor.visit_call_args(&method_call.args);
}

pub fn walk_return_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    ret: &'hir hir::ReturnExpr,
) {
    if let Some(expr) = &ret.expr {
        visitor.visit_expr(expr);
    }
}

pub fn walk_break_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    break_expr: &'hir hir::BreakExpr,
) {
    if let Some(expr) = &break_expr.expr {
        visitor.visit_expr(expr);
    }
}

pub fn walk_continue_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    continue_expr: &'hir hir::ContinueExpr,
) {
}

pub fn walk_loop_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    loop_expr: &'hir hir::LoopExpr,
) {
    visitor.visit_block(&loop_expr.body);
}

pub fn walk_if_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    if_expr: &'hir hir::IfExpr,
) {
    visitor.visit_expr(&if_expr.cond);
    visitor.visit_expr(&if_expr.then);
    if let Some(else_clause) = &if_expr.else_ {
        visitor.visit_expr(else_clause);
    }
}

pub fn walk_tuple_like_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    tuple_like_expr: &'hir hir::TupleLikeExpr,
) {
    for expr in &tuple_like_expr.exprs {
        visitor.visit_expr(expr);
    }
}

pub fn walk_pat<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, pat: &'hir hir::Pat) {
    visitor.visit_hir_id(pat.hir_id);
    match &pat.kind {
        hir::PatKind::Ident(_) => {}
        hir::PatKind::TupleLike(tuple_like) => {
            for pat in tuple_like {
                visitor.visit_pat(pat);
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
        visitor.visit_expr(arg);
    }
}

pub fn walk_lambda_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    lambda_expr: &'hir hir::LambdaExpr,
) {
    if let Some(lpl) = &lambda_expr.lambda_param_list {
        visitor.visit_lambda_param_list(lpl);
    }
    visitor.visit_item_list(&lambda_expr.body);
}

pub fn walk_lambda_param_list<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    lambda_param_list: &'hir hir::LambdaParamList,
) {
    for param in &lambda_param_list.params {
        visitor.visit_lambda_param(param);
    }
}

pub fn walk_lambda_param<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    lambda_param: &'hir hir::LambdaParam,
) {
    visitor.visit_pat(&lambda_param.pat);
    if let Some(ty) = &lambda_param.ty {
        visitor.visit_ty_ref(ty);
    }
}

pub fn walk_str_literal<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_literal: &'hir hir::StrLiteral,
) {
    for fragment in &str_literal.fragments {
        visitor.visit_str_literal_fragment(fragment);
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
            visitor.visit_str_display_fragment(f);
        }
        hir::StrLiteralFragmentKind::Debug(f) => {
            visitor.visit_str_debug_fragment(f);
        }
    }
}

pub fn walk_str_display_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_display_fragment: &'hir hir::StrLiteralDisplayFragment,
) {
    visitor.visit_expr(&str_display_fragment.expr);
}

pub fn walk_str_debug_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_debug_fragment: &'hir hir::StrLiteralDebugFragment,
) {
    visitor.visit_expr(&str_debug_fragment.expr);
}

pub fn walk_fn_ty_ref<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_ty: &'hir hir::FnTy) {
    for ty in &fn_ty.params {
        visitor.visit_ty_ref(ty);
    }

    if let Some(ret_ty) = &fn_ty.ret_ty {
        visitor.visit_ty_ref(ret_ty);
    }
}
