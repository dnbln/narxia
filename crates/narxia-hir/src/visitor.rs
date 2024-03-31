use crate::hir_map::HirMap;
use crate::{hir, HirId};

pub(crate) trait IdHandleStrategy<'hir, V: HirVisitor<'hir> + ?Sized>
where
    Self: Sized,
{
    fn handle_item_id(&self, visitor: &mut V, item_id: hir::ItemId);
    fn handle_block_id(&self, visitor: &mut V, block_id: hir::BlockId);
    fn handle_expr_id(&self, visitor: &mut V, expr_id: hir::ExprId);
    fn handle_stmt_id(&self, visitor: &mut V, stmt_id: hir::StmtId);
    fn handle_fn_id(&self, visitor: &mut V, fn_id: hir::FnId);
    fn handle_mod_id(&self, visitor: &mut V, mod_id: hir::ModId);
}

pub struct DoNothingIdHandleStrategy;

impl<'hir, V: HirVisitor<'hir>> IdHandleStrategy<'hir, V> for DoNothingIdHandleStrategy {
    fn handle_item_id(&self, _visitor: &mut V, _item_id: hir::ItemId) {}

    fn handle_block_id(&self, _visitor: &mut V, _block_id: hir::BlockId) {}

    fn handle_expr_id(&self, _visitor: &mut V, _expr_id: hir::ExprId) {}

    fn handle_stmt_id(&self, _visitor: &mut V, _stmt_id: hir::StmtId) {}

    fn handle_fn_id(&self, visitor: &mut V, fn_id: hir::FnId) {}

    fn handle_mod_id(&self, visitor: &mut V, mod_id: hir::ModId) {}
}

pub struct ShallowIdHandleStrategy;
impl<'hir, V: HirVisitor<'hir>> IdHandleStrategy<'hir, V> for ShallowIdHandleStrategy {
    fn handle_item_id(&self, visitor: &mut V, item_id: hir::ItemId) {
        visitor.visit_hir_id(item_id.0);
    }

    fn handle_block_id(&self, visitor: &mut V, block_id: hir::BlockId) {
        visitor.visit_hir_id(block_id.0);
    }

    fn handle_expr_id(&self, visitor: &mut V, expr_id: hir::ExprId) {
        visitor.visit_hir_id(expr_id.0);
    }

    fn handle_stmt_id(&self, visitor: &mut V, stmt_id: hir::StmtId) {
        visitor.visit_hir_id(stmt_id.0);
    }

    fn handle_fn_id(&self, visitor: &mut V, fn_id: hir::FnId) {
        visitor.visit_hir_id(fn_id.0);
    }

    fn handle_mod_id(&self, visitor: &mut V, mod_id: hir::ModId) {
        visitor.visit_hir_id(mod_id.0);
    }
}

pub struct RecursiveIdHandleStrategy<'hir>(&'hir HirMap);

impl<'hir> RecursiveIdHandleStrategy<'hir> {
    pub fn new(hir_map: &'hir HirMap) -> Self {
        Self(hir_map)
    }
}

impl<'hir, V: HirVisitor<'hir>> IdHandleStrategy<'hir, V> for RecursiveIdHandleStrategy<'hir> {
    fn handle_item_id(&self, visitor: &mut V, item_id: hir::ItemId) {
        visitor.visit_hir_id(item_id.0);
        visitor.visit_item(item_id, self.0.get_item(item_id));
    }

    fn handle_block_id(&self, visitor: &mut V, block_id: hir::BlockId) {
        visitor.visit_hir_id(block_id.0);
        visitor.visit_block(block_id, self.0.get_block(block_id));
    }

    fn handle_expr_id(&self, visitor: &mut V, expr_id: hir::ExprId) {
        visitor.visit_hir_id(expr_id.0);
        visitor.visit_expr(expr_id, self.0.get_expr(expr_id));
    }

    fn handle_stmt_id(&self, visitor: &mut V, stmt_id: hir::StmtId) {
        visitor.visit_hir_id(stmt_id.0);
        visitor.visit_stmt(stmt_id, self.0.get_stmt(stmt_id));
    }

    fn handle_fn_id(&self, visitor: &mut V, fn_id: hir::FnId) {
        visitor.visit_hir_id(fn_id.0);
        visitor.visit_fn_def(fn_id, self.0.get_fn(fn_id));
    }

    fn handle_mod_id(&self, visitor: &mut V, mod_id: hir::ModId) {
        visitor.visit_hir_id(mod_id.0);
        visitor.visit_mod_def(mod_id, self.0.get_mod(mod_id));
    }
}

pub trait HirVisitor<'hir> {
    type Strategy: IdHandleStrategy<'hir, Self>;

    fn get_strategy(&self) -> Self::Strategy;

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

    fn visit_return_expr(&mut self, ret: &'hir hir::ReturnExpr) {
        walk_return_expr(self, ret)
    }

    fn visit_break_expr(&mut self, break_expr: &'hir hir::BreakExpr) {
        walk_break_expr(self, break_expr)
    }

    fn visit_continue_expr(&mut self, continue_expr: &'hir hir::ContinueExpr) {
        walk_continue_expr(self, continue_expr)
    }

    fn visit_loop_expr(&mut self, loop_expr: &'hir hir::LoopExpr) {
        walk_loop_expr(self, loop_expr)
    }

    fn visit_if_expr(&mut self, expr_id: hir::ExprId, if_expr: &'hir hir::IfExpr) {
        walk_if_expr(self, if_expr)
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
    }

    fn visit_expr_id(&mut self, expr_id: hir::ExprId) {
        self.get_strategy().handle_expr_id(self, expr_id)
    }

    fn visit_item_id(&mut self, item_id: hir::ItemId) {
        self.get_strategy().handle_item_id(self, item_id)
    }

    fn visit_stmt_id(&mut self, stmt_id: hir::StmtId) {
        self.get_strategy().handle_stmt_id(self, stmt_id)
    }

    fn visit_block_id(&mut self, block_id: hir::BlockId) {
        self.get_strategy().handle_block_id(self, block_id)
    }

    fn visit_fn_id(&mut self, fn_id: hir::FnId) {
        self.get_strategy().handle_fn_id(self, fn_id)
    }

    fn visit_mod_id(&mut self, mod_id: hir::ModId) {
        self.get_strategy().handle_mod_id(self, mod_id)
    }
}

pub fn walk_mod_def<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    mod_def: &'hir hir::ModDef,
) {
    visitor.visit_item_list(&mod_def.items);
}

pub fn walk_item_list<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    item_list: &'hir hir::ItemList,
) {
    for item in &item_list.items {
        visitor.visit_item_id(*item);
    }
}

pub fn walk_item<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, item: &'hir hir::Item) {
    match &item.kind {
        hir::ItemKind::FnDef(fn_def) => {
            visitor.visit_fn_id(*fn_def);
        }
        hir::ItemKind::Stmt(stmt) => {
            visitor.visit_stmt_id(*stmt);
        }
    }
}

pub fn walk_fn_def<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_def: &'hir hir::FnDef) {
    walk_fn_head(visitor, fn_def);
    visitor.visit_block_id(fn_def.body);
}

pub fn walk_fn_head<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_def: &'hir hir::FnDef) {
    for param in &fn_def.params {
        visitor.visit_fn_param(param);
    }
    if let Some(ret_ty) = &fn_def.ret_ty {
        visitor.visit_fn_ret_ty(ret_ty);
    }
}

pub fn walk_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    stmt_id: hir::StmtId,
    stmt: &'hir hir::Stmt,
) {
    match &stmt.kind {
        hir::StmtKind::ExprStmt(expr) => {
            visitor.visit_expr_id(*expr);
        }
        hir::StmtKind::LetStmt(let_stmt) => {
            visitor.visit_let_stmt(stmt_id, let_stmt);
        }
        hir::StmtKind::ForStmt(for_stmt) => {
            visitor.visit_for_stmt(stmt_id, for_stmt);
        }
        hir::StmtKind::WhileStmt(while_stmt) => {
            visitor.visit_while_stmt(stmt_id, while_stmt);
        }
        hir::StmtKind::AssignmentStmt(assignment_stmt) => {
            visitor.visit_assignment_stmt(stmt_id, assignment_stmt);
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
            visitor.visit_expr_atom(expr_id, atom);
        }
        hir::ExprKind::Binary(binary_expr) => {
            visitor.visit_expr_binary_expr(expr_id, binary_expr);
        }
        hir::ExprKind::CallExpr(call_expr) => {
            visitor.visit_expr_call_expr(expr_id, call_expr);
        }
        hir::ExprKind::IndexExpr(index_expr) => {
            visitor.visit_expr_index_expr(expr_id, index_expr);
        }
        hir::ExprKind::FieldAccess(field_access) => {
            visitor.visit_expr_field_access(expr_id, field_access);
        }
        hir::ExprKind::MethodCall(method_call) => {
            visitor.visit_expr_method_call(expr_id, method_call);
        }
        hir::ExprKind::CustomInfix(custom_infix) => {
            visitor.visit_custom_infix_expr(expr_id, custom_infix);
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
            visitor.visit_if_expr(expr_id, if_expr);
        }
        hir::ExprAtomKind::TupleExpr(tuple_like_expr) => {
            visitor.visit_tuple_like_expr(expr_id, tuple_like_expr);
        }
        hir::ExprAtomKind::LambdaExpr(l) => visitor.visit_lambda_expr(expr_id, l),
    }
}

pub fn walk_ty_ref<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, ty_ref: &'hir hir::TyRef) {
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
            visitor.visit_expr_id(*const_val);
        }
    }
}

pub fn walk_fn_param<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    fn_param: &'hir hir::FnParam,
) {
    visitor.visit_pat(&fn_param.pat);
    visitor.visit_ty_ref(&fn_param.ty);
}

pub fn walk_fn_ret_ty<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    fn_ret_ty: &'hir hir::FnRetTy,
) {
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
    if let Some(init) = let_stmt.init {
        visitor.visit_expr_id(init);
    }
}

pub fn walk_for_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    for_stmt: &'hir hir::ForStmt,
) {
    visitor.visit_pat(&for_stmt.pat);
    visitor.visit_expr_id(for_stmt.iter);
    visitor.visit_block_id(for_stmt.body);
}

pub fn walk_while_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    while_stmt: &'hir hir::WhileStmt,
) {
    visitor.visit_expr_id(while_stmt.expr);
    visitor.visit_block_id(while_stmt.body);
}

pub fn walk_assignment_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    assignment_stmt: &'hir hir::AssignmentStmt,
) {
    visitor.visit_expr_id(assignment_stmt.lhs);
    visitor.visit_expr_id(assignment_stmt.rhs);
}

pub fn walk_block<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, block: &'hir hir::Block) {
    visitor.visit_item_list(&block.items);
}

pub fn walk_block_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    block_expr: &'hir hir::BlockExpr,
) {
    visitor.visit_block_id(block_expr.block);
}

pub fn walk_expr_binary_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    binary_expr: &'hir hir::BinaryOpExpr,
) {
    visitor.visit_expr_id(binary_expr.lhs);
    visitor.visit_expr_id(binary_expr.rhs);
}

pub fn walk_expr_call_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    call_expr: &'hir hir::CallExpr,
) {
    visitor.visit_expr_id(call_expr.callee);
    visitor.visit_call_args(&call_expr.args);
}

pub fn walk_expr_index_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    index_expr: &'hir hir::IndexExpr,
) {
    visitor.visit_expr_id(index_expr.base);
    visitor.visit_expr_id(index_expr.index);
}

pub fn walk_expr_field_access<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    field_access: &'hir hir::FieldAccess,
) {
    visitor.visit_expr_id(field_access.base);
}

pub fn walk_expr_method_call<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    method_call: &'hir hir::MethodCall,
) {
    visitor.visit_expr_id(method_call.base);
    visitor.visit_ident(&method_call.method);
    visitor.visit_call_args(&method_call.args);
}

pub fn walk_return_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    ret: &'hir hir::ReturnExpr,
) {
    if let Some(expr) = ret.expr {
        visitor.visit_expr_id(expr);
    }
}

pub fn walk_break_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    break_expr: &'hir hir::BreakExpr,
) {
    if let Some(expr) = break_expr.expr {
        visitor.visit_expr_id(expr);
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
    visitor.visit_expr_id(custom_infix.base);
    visitor.visit_ident(&custom_infix.name);
    visitor.visit_expr_id(custom_infix.arg);
}

pub fn walk_loop_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    loop_expr: &'hir hir::LoopExpr,
) {
    visitor.visit_block_id(loop_expr.body);
}

pub fn walk_if_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    if_expr: &'hir hir::IfExpr,
) {
    visitor.visit_expr_id(if_expr.cond);
    visitor.visit_expr_id(if_expr.then);
    if let Some(else_clause) = if_expr.else_ {
        visitor.visit_expr_id(else_clause);
    }
}

pub fn walk_tuple_like_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    tuple_like_expr: &'hir hir::TupleExpr,
) {
    for expr in &tuple_like_expr.exprs {
        visitor.visit_expr_id(*expr);
    }
}

pub fn walk_pat<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, pat: &'hir hir::Pat) {
    match &pat.kind {
        hir::PatKind::Ident(_) => {}
        hir::PatKind::Tuple(tuple_like) => {
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
        visitor.visit_expr_id(*arg);
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
    visitor.visit_expr_id(str_display_fragment.expr);
}

pub fn walk_str_debug_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_debug_fragment: &'hir hir::StrLiteralDebugFragment,
) {
    visitor.visit_expr_id(str_debug_fragment.expr);
}

pub fn walk_fn_ty_ref<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_ty: &'hir hir::FnTy) {
    for ty in &fn_ty.params {
        visitor.visit_ty_ref(ty);
    }

    if let Some(ret_ty) = &fn_ty.ret_ty {
        visitor.visit_ty_ref(ret_ty);
    }
}
