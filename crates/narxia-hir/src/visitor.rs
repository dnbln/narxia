use crate::{hir, HirId};

pub trait HirVisitor {
    fn visit_mod_def(&mut self, mod_def: &hir::ModDef) {
        walk_mod_def(self, mod_def)
    }

    fn visit_item(&mut self, item: &hir::Item) {
        walk_item(self, item)
    }

    fn visit_fn_def(&mut self, fn_def: &hir::FnDef) {
        walk_fn_def(self, fn_def)
    }

    fn visit_stmt(&mut self, stmt: &hir::Stmt) {
        walk_stmt(self, stmt)
    }

    fn visit_expr(&mut self, expr: &hir::Expr) {
        walk_expr(self, expr)
    }

    fn visit_let_stmt(&mut self, let_stmt: &hir::LetStmt) {
        walk_let_stmt(self, let_stmt)
    }

    fn visit_for_stmt(&mut self, for_stmt: &hir::ForStmt) {
        walk_for_stmt(self, for_stmt)
    }

    fn visit_while_stmt(&mut self, while_stmt: &hir::WhileStmt) {
        walk_while_stmt(self, while_stmt)
    }

    fn visit_assignment_stmt(&mut self, assignment_stmt: &hir::AssignmentStmt) {
        walk_assignment_stmt(self, assignment_stmt)
    }

    fn visit_fn_param(&mut self, fn_param: &hir::FnParam) {
        walk_fn_param(self, fn_param)
    }

    fn visit_fn_ret_ty(&mut self, fn_ret_ty: &hir::FnRetTy) {
        walk_fn_ret_ty(self, fn_ret_ty)
    }

    fn visit_block(&mut self, block: &hir::Block) {
        walk_block(self, block)
    }

    fn visit_ty_ref(&mut self, ty_ref: &hir::TyRef) {
        walk_ty_ref(self, ty_ref)
    }

    fn visit_pat(&mut self, pat: &hir::Pat) {
        walk_pat(self, pat)
    }

    fn visit_ty_generic_args(&mut self, generic_args: &hir::TyGenericArgs) {
        walk_ty_generic_args(self, generic_args)
    }

    fn visit_ty_generic_arg(&mut self, generic_arg: &hir::TyGenericArg) {
        walk_ty_generic_arg(self, generic_arg)
    }

    fn visit_expr_atom(&mut self, atom: &hir::ExprAtom) {
        walk_expr_atom(self, atom)
    }

    fn visit_expr_binary_expr(&mut self, binary_expr: &hir::BinaryOpExpr) {
        walk_expr_binary_expr(self, binary_expr)
    }

    fn visit_expr_call_expr(&mut self, call_expr: &hir::CallExpr) {
        walk_expr_call_expr(self, call_expr)
    }

    fn visit_expr_index_expr(&mut self, index_expr: &hir::IndexExpr) {
        walk_expr_index_expr(self, index_expr)
    }

    fn visit_expr_field_access(&mut self, field_access: &hir::FieldAccess) {
        walk_expr_field_access(self, field_access)
    }

    fn visit_expr_method_call(&mut self, method_call: &hir::MethodCall) {
        walk_expr_method_call(self, method_call)
    }

    fn visit_return_expr(&mut self, ret: &hir::ReturnExpr) {
        walk_return_expr(self, ret)
    }

    fn visit_break_expr(&mut self, break_expr: &hir::BreakExpr) {
        walk_break_expr(self, break_expr)
    }

    fn visit_continue_expr(&mut self, continue_expr: &hir::ContinueExpr) {
        walk_continue_expr(self, continue_expr)
    }

    fn visit_loop_expr(&mut self, loop_expr: &hir::LoopExpr) {
        walk_loop_expr(self, loop_expr)
    }

    fn visit_if_expr(&mut self, if_expr: &hir::IfExpr) {
        walk_if_expr(self, if_expr)
    }

    fn visit_tuple_like_expr(&mut self, tuple_like_expr: &hir::TupleLikeExpr) {
        walk_tuple_like_expr(self, tuple_like_expr)
    }

    fn visit_call_args(&mut self, call_args: &hir::CallExprArgs) {
        walk_call_args(self, call_args)
    }

    fn visit_lambda_expr(&mut self, lambda_expr: &hir::LambdaExpr) {
        walk_lambda_expr(self, lambda_expr)
    }

    fn visit_lambda_param_list(&mut self, lambda_param_list: &hir::LambdaParamList) {
        walk_lambda_param_list(self, lambda_param_list)
    }

    fn visit_lambda_param(&mut self, lambda_param: &hir::LambdaParam) {
        walk_lambda_param(self, lambda_param)
    }

    fn visit_hir_id(&mut self, hir_id: HirId) {
        // Nothing to do.
    }
}

pub fn walk_mod_def<V: HirVisitor + ?Sized>(visitor: &mut V, mod_def: &hir::ModDef) {
    visitor.visit_hir_id(mod_def.hir_id);
    for item in &mod_def.items {
        visitor.visit_item(item);
    }
}

pub fn walk_item<V: HirVisitor + ?Sized>(visitor: &mut V, item: &hir::Item) {
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

pub fn walk_fn_def<V: HirVisitor + ?Sized>(visitor: &mut V, fn_def: &hir::FnDef) {
    visitor.visit_hir_id(fn_def.hir_id);
    for param in &fn_def.params {
        visitor.visit_fn_param(param);
    }
    if let Some(ret_ty) = &fn_def.ret_ty {
        visitor.visit_fn_ret_ty(ret_ty);
    }
    visitor.visit_block(&fn_def.body);
}

pub fn walk_stmt<V: HirVisitor + ?Sized>(visitor: &mut V, stmt: &hir::Stmt) {
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

pub fn walk_expr<V: HirVisitor + ?Sized>(visitor: &mut V, expr: &hir::Expr) {
    visitor.visit_hir_id(expr.hir_id);
    match &expr.kind {
        hir::ExprKind::Atom(atom) => {
            visitor.visit_expr_atom(atom);
        }
        hir::ExprKind::Binary(binary_expr) => {
            visitor.visit_expr_binary_expr(binary_expr);
        }
        hir::ExprKind::CallExpr(call_expr) => {
            visitor.visit_expr_call_expr(call_expr);
        }
        hir::ExprKind::IndexExpr(index_expr) => {
            visitor.visit_expr_index_expr(index_expr);
        }
        hir::ExprKind::FieldAccess(field_access) => {
            visitor.visit_expr_field_access(field_access);
        }
        hir::ExprKind::MethodCall(method_call) => {
            visitor.visit_expr_method_call(method_call);
        }
    }
}

pub fn walk_expr_atom<V: HirVisitor + ?Sized>(visitor: &mut V, atom: &hir::ExprAtom) {
    match &atom.kind {
        hir::ExprAtomKind::Ident(_) => {}
        hir::ExprAtomKind::Num(_) | hir::ExprAtomKind::Str(_) => {}
        hir::ExprAtomKind::BlockExpr(block_expr) => {
            visitor.visit_block(&block_expr.block);
        }
        hir::ExprAtomKind::ReturnExpr(ret) => {
            visitor.visit_return_expr(ret);
        }
        hir::ExprAtomKind::BreakExpr(break_expr) => {
            visitor.visit_break_expr(break_expr);
        }
        hir::ExprAtomKind::ContinueExpr(continue_expr) => {
            visitor.visit_continue_expr(continue_expr);
        }
        hir::ExprAtomKind::LoopExpr(loop_expr) => {
            visitor.visit_loop_expr(loop_expr);
        }
        hir::ExprAtomKind::IfExpr(if_expr) => {
            visitor.visit_if_expr(if_expr);
        }
        hir::ExprAtomKind::TupleLikeExpr(tuple_like_expr) => {
            visitor.visit_tuple_like_expr(tuple_like_expr);
        }
    }
}

pub fn walk_ty_ref<V: HirVisitor + ?Sized>(visitor: &mut V, ty_ref: &hir::TyRef) {
    visitor.visit_hir_id(ty_ref.hir_id);
    match &ty_ref.kind {
        hir::TyRefKind::Named(_, generics) => match generics {
            Some(generics) => {
                visitor.visit_ty_generic_args(generics);
            }
            None => {}
        },
        hir::TyRefKind::Primitive(_) => {}
    }
}

pub fn walk_ty_generic_args<V: HirVisitor + ?Sized>(
    visitor: &mut V,
    generic_args: &hir::TyGenericArgs,
) {
    for arg in &generic_args.args {
        visitor.visit_ty_generic_arg(arg);
    }
}

pub fn walk_ty_generic_arg<V: HirVisitor + ?Sized>(
    visitor: &mut V,
    generic_arg: &hir::TyGenericArg,
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

pub fn walk_fn_param<V: HirVisitor + ?Sized>(visitor: &mut V, fn_param: &hir::FnParam) {
    visitor.visit_hir_id(fn_param.hir_id);
    visitor.visit_pat(&fn_param.pat);
    visitor.visit_ty_ref(&fn_param.ty);
}

pub fn walk_fn_ret_ty<V: HirVisitor + ?Sized>(visitor: &mut V, fn_ret_ty: &hir::FnRetTy) {
    visitor.visit_hir_id(fn_ret_ty.hir_id);
    visitor.visit_ty_ref(&fn_ret_ty.ty);
}

pub fn walk_let_stmt<V: HirVisitor + ?Sized>(visitor: &mut V, let_stmt: &hir::LetStmt) {
    visitor.visit_pat(&let_stmt.pat);
    if let Some(ty) = &let_stmt.ty {
        visitor.visit_ty_ref(ty);
    }
    if let Some(init) = &let_stmt.init {
        visitor.visit_expr(init);
    }
}

pub fn walk_for_stmt<V: HirVisitor + ?Sized>(visitor: &mut V, for_stmt: &hir::ForStmt) {
    visitor.visit_pat(&for_stmt.pat);
    visitor.visit_expr(&for_stmt.iter);
    visitor.visit_block(&for_stmt.body);
}

pub fn walk_while_stmt<V: HirVisitor + ?Sized>(visitor: &mut V, while_stmt: &hir::WhileStmt) {
    visitor.visit_expr(&while_stmt.expr);
    visitor.visit_block(&while_stmt.body);
}

pub fn walk_assignment_stmt<V: HirVisitor + ?Sized>(
    visitor: &mut V,
    assignment_stmt: &hir::AssignmentStmt,
) {
    visitor.visit_expr(&assignment_stmt.lhs);
    visitor.visit_expr(&assignment_stmt.rhs);
}

pub fn walk_block<V: HirVisitor + ?Sized>(visitor: &mut V, block: &hir::Block) {
    visitor.visit_hir_id(block.hir_id);
    for item in &block.items {
        visitor.visit_item(item);
    }
}

pub fn walk_expr_binary_expr<V: HirVisitor + ?Sized>(
    visitor: &mut V,
    binary_expr: &hir::BinaryOpExpr,
) {
    visitor.visit_expr(&binary_expr.lhs);
    visitor.visit_expr(&binary_expr.rhs);
}

pub fn walk_expr_call_expr<V: HirVisitor + ?Sized>(visitor: &mut V, call_expr: &hir::CallExpr) {
    visitor.visit_expr(&call_expr.callee);
    visitor.visit_call_args(&call_expr.args);
}

pub fn walk_expr_index_expr<V: HirVisitor + ?Sized>(visitor: &mut V, index_expr: &hir::IndexExpr) {
    visitor.visit_expr(&index_expr.base);
    visitor.visit_expr(&index_expr.index);
}

pub fn walk_expr_field_access<V: HirVisitor + ?Sized>(
    visitor: &mut V,
    field_access: &hir::FieldAccess,
) {
    visitor.visit_expr(&field_access.base);
}

pub fn walk_expr_method_call<V: HirVisitor + ?Sized>(
    visitor: &mut V,
    method_call: &hir::MethodCall,
) {
    visitor.visit_expr(&method_call.base);
    visitor.visit_call_args(&method_call.args);
}

pub fn walk_return_expr<V: HirVisitor + ?Sized>(visitor: &mut V, ret: &hir::ReturnExpr) {
    if let Some(expr) = &ret.expr {
        visitor.visit_expr(expr);
    }
}

pub fn walk_break_expr<V: HirVisitor + ?Sized>(visitor: &mut V, break_expr: &hir::BreakExpr) {
    if let Some(expr) = &break_expr.expr {
        visitor.visit_expr(expr);
    }
}

pub fn walk_continue_expr<V: HirVisitor + ?Sized>(
    visitor: &mut V,
    continue_expr: &hir::ContinueExpr,
) {
}

pub fn walk_loop_expr<V: HirVisitor + ?Sized>(visitor: &mut V, loop_expr: &hir::LoopExpr) {
    visitor.visit_block(&loop_expr.body);
}

pub fn walk_if_expr<V: HirVisitor + ?Sized>(visitor: &mut V, if_expr: &hir::IfExpr) {
    visitor.visit_expr(&if_expr.cond);
    visitor.visit_expr(&if_expr.then);
    if let Some(else_clause) = &if_expr.else_ {
        visitor.visit_expr(else_clause);
    }
}

pub fn walk_tuple_like_expr<V: HirVisitor + ?Sized>(
    visitor: &mut V,
    tuple_like_expr: &hir::TupleLikeExpr,
) {
    for expr in &tuple_like_expr.exprs {
        visitor.visit_expr(expr);
    }
}

pub fn walk_pat<V: HirVisitor + ?Sized>(visitor: &mut V, pat: &hir::Pat) {
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

pub fn walk_call_args<V: HirVisitor + ?Sized>(visitor: &mut V, call_args: &hir::CallExprArgs) {
    if let Some(args) = &call_args.args {
        for arg in args {
            visitor.visit_expr(arg);
        }
    }
    if let Some(trailing_lambda) = &call_args.trailing_lambda {
        visitor.visit_lambda_expr(trailing_lambda);
    }
}

pub fn walk_lambda_expr<V: HirVisitor + ?Sized>(visitor: &mut V, lambda_expr: &hir::LambdaExpr) {
    visitor.visit_hir_id(lambda_expr.hir_id);
    if let Some(lpl) = &lambda_expr.lambda_param_list {
        visitor.visit_lambda_param_list(lpl);
    }
    for item in &lambda_expr.body {
        visitor.visit_item(item);
    }
}

pub fn walk_lambda_param_list<V: HirVisitor + ?Sized>(
    visitor: &mut V,
    lambda_param_list: &hir::LambdaParamList,
) {
    for param in &lambda_param_list.params {
        visitor.visit_lambda_param(param);
    }
}

pub fn walk_lambda_param<V: HirVisitor + ?Sized>(visitor: &mut V, lambda_param: &hir::LambdaParam) {
    visitor.visit_pat(&lambda_param.pat);
    if let Some(ty) = &lambda_param.ty {
        visitor.visit_ty_ref(ty);
    }
}
