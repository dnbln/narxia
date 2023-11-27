use std::fmt;

use narxia_syn::syntree::Token;

use crate::{HirSpan, HirId};

impl fmt::Debug for HirId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} ~ {}", self.root, self.id)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ModDef {
    pub items: Vec<Item>,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ItemKind {
    FnDef(FnDef),
    Stmt(Stmt),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Ident {
    pub span: HirSpan,
    pub text: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FnDef {
    pub name: Ident,
    pub params: Vec<FnParam>,
    pub ret_ty: Option<FnRetTy>,
    pub body: Block,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FnParam {
    pub param_span: HirSpan,
    pub pat: Pat,
    pub ty: TyRef,
    pub default: Option<Expr>,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FnRetTy {
    pub span: HirSpan,
    pub arrow_span: HirSpan,
    pub ty: TyRef,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExprKind {
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

#[derive(Debug, Eq, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: CallExprArgs,
}

#[derive(Debug, Eq, PartialEq)]
pub struct CallExprArgs {
    pub args: Option<Vec<Expr>>,
    pub trailing_lambda: Option<LambdaExpr>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct IndexExpr {
    pub base: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FieldAccess {
    pub base: Box<Expr>,
    pub field: Ident,
}

#[derive(Debug, Eq, PartialEq)]
pub struct MethodCall {
    pub base: Box<Expr>,
    pub method: Ident,
    pub args: CallExprArgs,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LambdaExpr {
    pub lambda_param_list: Option<LambdaParamList>,
    pub body: Vec<Item>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LambdaParamList {
    pub params: Vec<LambdaParam>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LambdaParam {
    pub pat: Pat,
    pub ty: Option<TyRef>,
}

#[derive(Debug, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq)]
pub struct ExprAtom {
    pub kind: ExprAtomKind,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExprAtomKind {
    Ident(Ident),
    Str(StrLiteral),
    Num(NumLit),
    LoopExpr(LoopExpr),
    IfExpr(Box<IfExpr>),
    ReturnExpr(Box<ReturnExpr>),
    BreakExpr(Box<BreakExpr>),
    ContinueExpr(ContinueExpr),
    BlockExpr(BlockExpr),
}

#[derive(Debug, Eq, PartialEq)]
pub enum NumLit {
    Bin(Token),
    Oct(Token),
    Dec(Token),
    Hex(Token),
}

#[derive(Debug, Eq, PartialEq)]
pub struct IfExpr {
    pub cond: Expr,
    pub then: Expr,
    pub else_: Option<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReturnExpr {
    pub return_kw: Token,
    pub expr: Option<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BreakExpr {
    pub break_kw: Token,
    pub expr: Option<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ContinueExpr {
    pub continue_kw: Token,
}

#[derive(Debug, Eq, PartialEq)]
pub struct StrLiteral {
    pub span: HirSpan,
    pub text: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LoopExpr {
    pub body: Block,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BlockExpr {
    pub block: Block,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block {
    pub items: Vec<Item>,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub enum StmtKind {
    ExprStmt(Expr),
    LetStmt(LetStmt),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
    AssignmentStmt(AssignmentStmt),
}

#[derive(Debug, Eq, PartialEq)]
pub struct AssignmentStmt {
    pub lhs: Expr,
    pub op: AssignmentOp,
    pub rhs: Expr,
}

#[derive(Debug, Eq, PartialEq)]
pub enum AssignmentOp {
    Assign(HirSpan),
    AddAssign(HirSpan),
    SubAssign(HirSpan),
    MulAssign(HirSpan),
    DivAssign(HirSpan),
    ModAssign(HirSpan),
    BitAndAssign(HirSpan),
    BitOrAssign(HirSpan),
    BitXorAssign(HirSpan),
}

#[derive(Debug, Eq, PartialEq)]
pub struct LetStmt {
    pub pat: Pat,
    pub ty: Option<TyRef>,
    pub init: Option<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ForStmt {
    pub pat: Pat,
    pub iter: Expr,
    pub body: Block,
}

#[derive(Debug, Eq, PartialEq)]
pub struct WhileStmt {
    pub expr: Expr,
    pub body: Block,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Pat {
    pub kind: PatKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub enum PatKind {
    Ident(Ident),
    TupleLike(Vec<Pat>),
    Wildcard(Ident),
}

#[derive(Debug, Eq, PartialEq)]
pub struct TyRef {
    pub span: HirSpan,
    pub kind: TyRefKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TyRefKind {
    Named(Ident, Option<TyGenericArgs>),
    Primitive(PrimitiveTy),
}

#[derive(Debug, Eq, PartialEq)]
pub enum PrimitiveTy {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Bool,
    Char,
    Str,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TyGenericArgs {
    pub span: HirSpan,
    pub args: Vec<TyGenericArg>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TyGenericArg {
    pub kind: TyGenericArgKind,
    pub span: HirSpan,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TyGenericArgKind {
    ConstVal(Expr),
    Type(TyRef),
}