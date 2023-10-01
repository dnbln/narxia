use narxia_syn::syntree::{self, Token};
use narxia_syn::text_span::TextSpan;

pub struct HirSpan {
    span: TextSpan,
}

impl HirSpan {
    pub fn of_node<T: syntree::TreeNode>(node: &T) -> Self {
        Self {
            span: TextSpan::of_node(node.get_node()),
        }
    }

    pub fn of(token: &Token) -> Self {
        Self {
            span: TextSpan::of(token),
        }
    }
}

pub struct ModDef {
    items: Vec<Item>,
}

pub enum Item {
    FnDef(FnDef),
}

pub struct Ident {
    span: HirSpan,
    text: String,
}

pub struct FnDef {
    name: Ident,
    params: Vec<FnParam>,
}

pub struct FnParam {
    pat: Pat,
    ty: TyRef,
    default: Option<Expr>,
}

pub enum Expr {
    Atom(ExprAtom),
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    CallExpr(CallExpr),
    IndexExpr(IndexExpr),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
}

pub struct CallExpr {
    callee: Box<Expr>,
    args: CallExprArgs,
}

pub struct CallExprArgs {
    args: Vec<Expr>,
    trailing_lambda: Option<LambdaExpr>,
}

pub struct IndexExpr {
    base: Box<Expr>,
    index: Box<Expr>,
}

pub struct FieldAccess {
    base: Box<Expr>,
    field: Ident,
}

pub struct MethodCall {
    base: Box<Expr>,
    method: Ident,
    args: CallExprArgs,
}

pub struct LambdaExpr {
    lambda_param_list: Option<LambdaParamList>,
    body: Vec<Stmt>,
}

pub struct LambdaParamList {
    params: Vec<LambdaParam>,
}

pub struct LambdaParam {
    pat: Pat,
    ty: Option<TyRef>,
}

pub enum BinOp {
    Add(HirSpan),
    Sub(HirSpan),
    Mul(HirSpan),
    Div(HirSpan),
    Mod(HirSpan),
    Eq(HirSpan),
    Neq(HirSpan),
    Lt(HirSpan),
    LtEq(HirSpan),
    Gt(HirSpan),
    GtEq(HirSpan),
    And(HirSpan),
    Or(HirSpan),
    BitAnd(HirSpan),
    BitOr(HirSpan),
    Xor(HirSpan),
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

pub enum ExprAtom {
    Ident(Ident),
    Str(StrLiteral),
    LoopExpr(LoopExpr),
    IfExpr(Box<IfExpr>),
    BlockExpr(BlockExpr),
}

pub struct IfExpr {
    cond: Expr,
    then: Expr,
    else_: Option<Expr>,
}

pub struct StrLiteral {
    span: HirSpan,
    text: String,
}

pub struct LoopExpr {
    body: Block,
}

pub struct BlockExpr {
    block: Block,
}

pub struct Block {
    stmts: Vec<Stmt>,
}

pub enum Stmt {
    ExprStmt(Expr),
    LetStmt(LetStmt),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
}

pub struct LetStmt {
    pat: Pat,
    ty: Option<TyRef>,
    init: Option<Expr>,
}

pub struct ForStmt {
    pat: Pat,
    iter: Expr,
    body: Block,
}

pub struct WhileStmt {
    expr: Expr,
    body: Block,
}

pub enum Pat {
    Name(Ident),
}

pub enum TyRef {
    Name(Ident),
}

pub fn lower_mod_def(root: syntree::Root) -> ModDef {
    let items = root.get_item_list().map(lower_item).collect();
    ModDef { items }
}

fn lower_item(item: syntree::Item) -> Item {
    if let Some(fn_def) = item.get_fn_def() {
        Item::FnDef(lower_fn_def(fn_def))
    } else {
        unreachable!()
    }
}

fn lower_fn_def(fn_def: syntree::FnDef) -> FnDef {
    let head = fn_def.get_fn_head();
    let name = lower_ident(&head.get_fn_name().unwrap().get_ident());
    let params = lower_fn_def_params(&head.get_fn_param_list().unwrap());
    FnDef { name, params }
}

fn lower_fn_def_params(fn_param_list: &syntree::FnParamList) -> Vec<FnParam> {
    fn_param_list
        .get_fn_param_list()
        .map(|it| lower_fn_def_param(&it))
        .collect()
}

fn lower_fn_def_param(fn_param: &syntree::FnParam) -> FnParam {
    let pat = lower_pat(&fn_param.get_fn_param_name().get_pat());
    let ty = lower_ty_ref(&fn_param.get_fn_param_ty().unwrap().get_ty_ref());
    let default = fn_param
        .get_fn_param_default()
        .as_ref()
        .map(|it| it.get_expr_node().unwrap())
        .as_ref()
        .map(lower_expr_node);
    FnParam { pat, ty, default }
}

fn lower_expr(expr: &syntree::Expr) -> Expr {
    match expr {
        syntree::Expr::ExprAtom(atom) => Expr::Atom(lower_expr_atom(atom)),
        syntree::Expr::ExprNode(node) => lower_expr_node(node),
        syntree::Expr::BinaryOpExpr(binary_op_expr) => {
            let (left, op, right) = lower_binary_op_expr(binary_op_expr);
            Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            }
        }
        syntree::Expr::CallExpr(call_expr) => Expr::CallExpr(lower_call_expr(call_expr)),
        syntree::Expr::IndexExpr(index_expr) => Expr::IndexExpr(lower_index_expr(index_expr)),
        syntree::Expr::FieldAccess(field_access) => {
            Expr::FieldAccess(lower_field_access(field_access))
        }
        syntree::Expr::MethodCall(method_call) => Expr::MethodCall(lower_method_call(method_call)),
        syntree::Expr::Block(block) => {
            let block = lower_block(block);
            Expr::Atom(ExprAtom::BlockExpr(BlockExpr { block }))
        }
    }
}

fn lower_call_expr(call_expr: &syntree::CallExpr) -> CallExpr {
    let callee = Box::new(lower_expr(&call_expr.get_expr()));
    let args = lower_call_expr_args(&call_expr.get_call_expr_args().unwrap());
    CallExpr { callee, args }
}

fn lower_call_expr_args(call_expr_args: &syntree::CallExprArgs) -> CallExprArgs {
    let args = call_expr_args
        .get_call_expr_args_list()
        .unwrap()
        .get_expr_node_list()
        .map(|it| lower_expr_node(&it))
        .collect();
    let trailing_lambda = call_expr_args
        .get_call_expr_arg_lambda()
        .map(|it| lower_lambda_expr(&it.get_lambda_expr()));
    CallExprArgs {
        args,
        trailing_lambda,
    }
}

fn lower_index_expr(index_expr: &syntree::IndexExpr) -> IndexExpr {
    let base = Box::new(lower_expr(&index_expr.get_expr()));
    let index = Box::new(lower_expr_node(
        &index_expr
            .get_index_expr_index()
            .unwrap()
            .get_expr_node()
            .unwrap(),
    ));
    IndexExpr { base, index }
}

fn lower_field_access(field_access: &syntree::FieldAccess) -> FieldAccess {
    let base = Box::new(lower_expr(&field_access.get_expr()));
    let field = lower_ident(&field_access.get_field_name().unwrap());
    FieldAccess { base, field }
}

fn lower_method_call(method_call: &syntree::MethodCall) -> MethodCall {
    let base = Box::new(lower_expr(&method_call.get_expr()));
    let method = lower_ident(&method_call.get_method_name().unwrap());
    let args = lower_call_expr_args(&method_call.get_call_expr_args().unwrap());
    MethodCall {
        base,
        method,
        args,
    }
}

fn lower_lambda_expr(lambda_expr: &syntree::LambdaExpr) -> LambdaExpr {
    let lambda_param_list = lambda_expr
        .get_lambda_param_list()
        .as_ref()
        .map(lower_lambda_param_list);
    let body = lambda_expr
        .get_stmt_list()
        .map(|it| lower_stmt(&it))
        .collect();
    LambdaExpr {
        lambda_param_list,
        body,
    }
}

fn lower_lambda_param_list(lambda_param_list: &syntree::LambdaParamList) -> LambdaParamList {
    let params = lambda_param_list
        .get_lambda_param_list()
        .map(|it| lower_lambda_param(&it))
        .collect();
    LambdaParamList { params }
}

fn lower_lambda_param(lambda_param: &syntree::LambdaParam) -> LambdaParam {
    let pat = lower_pat(&lambda_param.get_fn_param_name().get_pat());
    let ty = lambda_param
        .get_fn_param_ty()
        .map(|it| it.get_ty_ref())
        .as_ref()
        .map(lower_ty_ref);
    LambdaParam { pat, ty }
}

fn lower_binary_op_expr(expr: &syntree::BinaryOpExpr) -> (Expr, BinOp, Expr) {
    let (left, op, right) = expr.lower_assume_complete();
    let left = lower_expr(&left);
    let op = lower_binop(&op);
    let right = lower_expr(&right);
    (left, op, right)
}

fn lower_expr_node(expr_node: &syntree::ExprNode) -> Expr {
    lower_expr(&expr_node.get_expr())
}

fn lower_expr_atom(atom: &syntree::ExprAtom) -> ExprAtom {
    if let Some(ident) = atom.get_ident() {
        ExprAtom::Ident(lower_ident(&ident))
    } else if let Some(str_literal) = atom.get_str() {
        ExprAtom::Str(lower_str_literal(&str_literal))
    } else if let Some(loop_expr) = atom.get_loop_expr() {
        ExprAtom::LoopExpr(lower_loop_expr(&loop_expr))
    } else if let Some(if_expr) = atom.get_if_expr() {
        ExprAtom::IfExpr(Box::new(lower_if_expr(&if_expr)))
    } else if let Some(block_expr) = atom.get_block_expr() {
        ExprAtom::BlockExpr(lower_block_expr(&block_expr))
    } else {
        unreachable!()
    }
}

fn lower_loop_expr(loop_expr: &syntree::LoopExpr) -> LoopExpr {
    let body = lower_block(&loop_expr.get_block().unwrap());
    LoopExpr { body }
}

fn lower_block(block: &syntree::Block) -> Block {
    let stmts = block.get_stmt_list().map(|it| lower_stmt(&it)).collect();
    Block { stmts }
}

fn lower_stmt(stmt: &syntree::Stmt) -> Stmt {
    if let Some(expr) = stmt.get_expr_node() {
        Stmt::ExprStmt(lower_expr_node(&expr))
    } else if let Some(let_stmt) = stmt.get_let_stmt() {
        Stmt::LetStmt(lower_let_stmt(&let_stmt))
    } else if let Some(for_stmt) = stmt.get_for_stmt() {
        Stmt::ForStmt(lower_for_stmt(&for_stmt))
    } else if let Some(while_stmt) = stmt.get_while_stmt() {
        Stmt::WhileStmt(lower_while_stmt(&while_stmt))
    } else {
        unreachable!()
    }
}

fn lower_let_stmt(let_stmt: &syntree::LetStmt) -> LetStmt {
    let pat = lower_pat(&let_stmt.get_pat().unwrap());
    let ty = let_stmt.get_ty_ref().as_ref().map(lower_ty_ref);
    let init = let_stmt.get_expr_node().as_ref().map(lower_expr_node);
    LetStmt { pat, ty, init }
}

fn lower_for_stmt(for_stmt: &syntree::ForStmt) -> ForStmt {
    let pat = lower_pat(&for_stmt.get_for_pat().unwrap().get_pat());
    let iter = lower_expr_node(&for_stmt.get_for_in_expr().unwrap().get_expr_node());
    let body = lower_block(&for_stmt.get_block().unwrap());
    ForStmt { pat, iter, body }
}

fn lower_while_stmt(while_stmt: &syntree::WhileStmt) -> WhileStmt {
    let expr = lower_expr_node(
        &while_stmt
            .get_while_condition()
            .unwrap()
            .get_expr_node()
            .unwrap(),
    );
    let body = lower_block(&while_stmt.get_block().unwrap());
    WhileStmt { expr, body }
}

fn lower_str_literal(str_literal: &Token) -> StrLiteral {
    assert_eq!(
        str_literal.kind(),
        narxia_syn::syntax_kind::SyntaxKind::STRING
    );
    let span = HirSpan::of(str_literal);
    let text = str_literal.text().to_owned();
    StrLiteral { span, text }
}

fn lower_pat(pat: &syntree::Pat) -> Pat {
    Pat::Name(lower_ident(&pat.get_ident()))
}

fn lower_ty_ref(ty_ref: &syntree::TyRef) -> TyRef {
    TyRef::Name(lower_ident(&ty_ref.get_ident()))
}

fn lower_if_expr(if_expr: &syntree::IfExpr) -> IfExpr {
    let cond = lower_expr_node(&if_expr.get_if_condition().unwrap().get_expr_node().unwrap());
    let then = lower_expr_node(&if_expr.get_if_then_clause().unwrap().get_expr_node());
    let else_ = if_expr
        .get_else_clause()
        .as_ref()
        .map(|it| lower_expr_node(&it.get_expr_node().unwrap()));
    IfExpr { cond, then, else_ }
}

fn lower_block_expr(block_expr: &syntree::BlockExpr) -> BlockExpr {
    let block = lower_block(&block_expr.get_block());
    BlockExpr { block }
}

fn lower_ident(ident: &Token) -> Ident {
    assert_eq!(ident.kind(), narxia_syn::syntax_kind::SyntaxKind::IDENT);

    let span = HirSpan::of(ident);
    let text = ident.text().to_owned();
    Ident { span, text }
}

