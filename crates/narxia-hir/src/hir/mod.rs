use std::fmt;

use narxia_syn::syntree::Token;

use crate::{HirId, HirSpan};
use narxia_proc::HirStructIdCheck;

mod hir_debug;

pub use hir_debug::*;

pub struct HirIdUninitialized(pub &'static str);

pub trait HirStructIdCheckTest {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized>;
}

impl HirStructIdCheckTest for HirId {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized> {
        if self.is_dummy() {
            Err(HirIdUninitialized(name))
        } else {
            Ok(())
        }
    }
}

impl<H: HirStructIdCheckTest> HirStructIdCheckTest for Box<H> {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized> {
        (&**self).check_hir_id(name)
    }
}

impl<H: HirStructIdCheckTest> HirStructIdCheckTest for Vec<H> {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized> {
        for h in self {
            h.check_hir_id(name)?;
        }
        Ok(())
    }
}

impl HirStructIdCheckTest for String {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized> {
        Ok(())
    }
}

impl HirStructIdCheckTest for char {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized> {
        Ok(())
    }
}

impl HirStructIdCheckTest for Token {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized> {
        Ok(())
    }
}

impl HirStructIdCheckTest for HirSpan {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized> {
        Ok(())
    }
}

impl<H: HirStructIdCheckTest> HirStructIdCheckTest for Option<H> {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized> {
        if let Some(h) = self {
            h.check_hir_id(name)?;
        }
        Ok(())
    }
}


#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct ItemList {
    pub items: Vec<Item>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct ModDef {
    pub items: ItemList,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct Item {
    pub kind: ItemKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum ItemKind {
    FnDef(FnDef),
    Stmt(Stmt),
}

#[derive(Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct Ident {
    pub span: HirSpan,
    pub text: String,
    pub hir_id: HirId,
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {}", self.text, self.span)
    }
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct FnDef {
    pub name: Ident,
    pub generics: Option<GenericParams>,
    pub params: Vec<FnParam>,
    pub ret_ty: Option<FnRetTy>,
    pub body: Block,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct GenericParams {
    pub span: HirSpan,
    pub params: Vec<GenericParam>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct GenericParam {
    pub span: HirSpan,
    pub kind: GenericParamKind,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum GenericParamKind {
    Type(Ident),
    Const(Ident, TyRef),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct FnParam {
    pub param_span: HirSpan,
    pub pat: Pat,
    pub ty: TyRef,
    pub default: Option<Expr>,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct FnRetTy {
    pub span: HirSpan,
    pub arrow_span: HirSpan,
    pub ty: TyRef,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct Expr {
    pub kind: ExprKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum ExprKind {
    Atom(ExprAtom),
    Binary(BinaryOpExpr),
    CallExpr(CallExpr),
    IndexExpr(IndexExpr),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct BinaryOpExpr {
    pub lhs: Box<Expr>,
    pub op: BinOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: CallExprArgs,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct CallExprArgs {
    pub args: Vec<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct IndexExpr {
    pub base: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct FieldAccess {
    pub base: Box<Expr>,
    pub field: Ident,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct MethodCall {
    pub base: Box<Expr>,
    pub method: Ident,
    pub args: CallExprArgs,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct LambdaExpr {
    pub lambda_param_list: Option<LambdaParamList>,
    pub body: ItemList,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct LambdaParamList {
    pub params: Vec<LambdaParam>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct LambdaParam {
    pub pat: Pat,
    pub ty: Option<TyRef>,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
#[derive(HirStructIdCheck)]
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
#[derive(HirStructIdCheck)]
pub struct ExprAtom {
    pub kind: ExprAtomKind,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
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
    TupleLikeExpr(TupleLikeExpr),
    LambdaExpr(LambdaExpr),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum NumLit {
    Bin(Token),
    Oct(Token),
    Dec(Token),
    Hex(Token),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct IfExpr {
    pub cond: Expr,
    pub then: Expr,
    pub else_: Option<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct ReturnExpr {
    pub return_kw: Token,
    pub expr: Option<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct BreakExpr {
    pub break_kw: Token,
    pub expr: Option<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct ContinueExpr {
    pub continue_kw: Token,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct StrLiteral {
    pub hir_id: HirId,
    pub span: HirSpan,
    pub fragments: Vec<StrLiteralFragment>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct StrLiteralFragment {
    pub kind: StrLiteralFragmentKind,
    pub span: HirSpan,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum StrLiteralFragmentKind {
    Text(Token),
    EscapedChar(Token, char),
    EscapeSequence(Token, char),
    Display(StrLiteralDisplayFragment),
    Debug(StrLiteralDebugFragment),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct StrLiteralDisplayFragment {
    pub display_token: Token,
    pub span: HirSpan,
    pub expr: Expr,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct StrLiteralDebugFragment {
    pub debug_token: Token,
    pub span: HirSpan,
    pub expr: Expr,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct LoopExpr {
    pub body: Block,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct BlockExpr {
    pub block: Block,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct Block {
    pub items: ItemList,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct TupleLikeExpr {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct Stmt {
    pub kind: StmtKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum StmtKind {
    ExprStmt(Expr),
    LetStmt(LetStmt),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
    AssignmentStmt(AssignmentStmt),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct AssignmentStmt {
    pub lhs: Expr,
    pub op: AssignmentOp,
    pub rhs: Expr,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
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
#[derive(HirStructIdCheck)]
pub struct LetStmt {
    pub mutability: LetMutability,
    pub pat: Pat,
    pub ty: Option<TyRef>,
    pub init: Option<Expr>,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum LetMutability {
    Imm,
    Mut(HirSpan),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct ForStmt {
    pub pat: Pat,
    pub iter: Expr,
    pub body: Block,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct WhileStmt {
    pub expr: Expr,
    pub body: Block,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct Pat {
    pub kind: PatKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum PatKind {
    Ident(Ident),
    TupleLike(Vec<Pat>),
    Wildcard(Ident),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct TyRef {
    pub span: HirSpan,
    pub kind: TyRefKind,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum TyRefKind {
    Named(Ident, TyGenericArgs),
    Primitive(PrimitiveTy),
    Fn(FnTy),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
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
#[derive(HirStructIdCheck)]
pub struct TyGenericArgs {
    pub args: Vec<TyGenericArg>,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct TyGenericArg {
    pub kind: TyGenericArgKind,
    pub span: HirSpan,
    pub hir_id: HirId,
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub enum TyGenericArgKind {
    ConstVal(Expr),
    Type(TyRef),
}

#[derive(Debug, Eq, PartialEq)]
#[derive(HirStructIdCheck)]
pub struct FnTy {
    pub params: Vec<TyRef>,
    pub ret_ty: Option<Box<TyRef>>,
}