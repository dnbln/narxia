use std::fmt;

use narxia_proc::HirStructIdCheck;
use narxia_syn::syntree::Token;

use crate::{HirId, HirSpan};

mod hir_debug;

pub use hir_debug::*;

macro_rules! hir_id_newtype {
    ($name:ident, $t:ty) => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq, HirStructIdCheck)]
        pub struct $name(pub HirId);
    };
}

pub struct HirIdUninitialized(pub &'static str);

pub trait HirStructIdCheckTest {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized>;
}

impl HirStructIdCheckTest for HirId {
    fn check_hir_id(&self, name: &'static str) -> Result<(), HirIdUninitialized> {
        Ok(())
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

hir_id_newtype!(ItemId, Item);

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct ItemList {
    pub items: Vec<ItemId>,
}

hir_id_newtype!(ModId, ModDef);

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct ModDef {
    pub items: ItemList,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone, Copy)]
pub enum ItemKind {
    FnDef(FnId),
    Stmt(StmtId),
}

#[derive(Eq, PartialEq, PartialOrd, Ord, HirStructIdCheck, Clone)]
pub struct Ident {
    pub span: HirSpan,
    pub text: String,
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {}", self.text, self.span)
    }
}

hir_id_newtype!(FnId, FnDef);

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct FnDef {
    pub name: Ident,
    pub generics: Option<GenericParams>,
    pub params: Vec<FnParam>,
    pub ret_ty: Option<FnRetTy>,
    pub body: BlockId,
}

hir_id_newtype!(BlockId, Block);

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct GenericParams {
    pub span: HirSpan,
    pub params: Vec<GenericParam>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct GenericParam {
    pub span: HirSpan,
    pub kind: GenericParamKind,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum GenericParamKind {
    Type(Ident),
    Const(Ident, TyRef),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct FnParam {
    pub param_span: HirSpan,
    pub pat: Pat,
    pub ty: TyRef,
    pub default: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct FnRetTy {
    pub span: HirSpan,
    pub arrow_span: HirSpan,
    pub ty: TyRef,
}

hir_id_newtype!(ExprId, Expr);

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum ExprKind {
    Atom(ExprAtom),
    Binary(BinaryOpExpr),
    CallExpr(CallExpr),
    IndexExpr(IndexExpr),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
    CustomInfix(CustomInfixExpr),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct CustomInfixExpr {
    pub base: ExprId,
    pub name: Ident,
    pub arg: ExprId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct BinaryOpExpr {
    pub lhs: ExprId,
    pub op: BinOp,
    pub rhs: ExprId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct CallExpr {
    pub callee: ExprId,
    pub args: CallExprArgs,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct CallExprArgs {
    pub args: Vec<ExprId>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct IndexExpr {
    pub base: ExprId,
    pub index: ExprId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct FieldAccess {
    pub base: ExprId,
    pub field: Ident,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct MethodCall {
    pub base: ExprId,
    pub method: Ident,
    pub args: CallExprArgs,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct LambdaExpr {
    pub lambda_param_list: Option<LambdaParamList>,
    pub body: ItemList,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct LambdaParamList {
    pub params: Vec<LambdaParam>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct LambdaParam {
    pub pat: Pat,
    pub ty: Option<TyRef>,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, HirStructIdCheck)]
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

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct ExprAtom {
    pub kind: ExprAtomKind,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum ExprAtomKind {
    Ident(Ident),
    Str(StrLiteral),
    Num(NumLit),
    LoopExpr(LoopExpr),
    IfExpr(IfExpr),
    ReturnExpr(ReturnExpr),
    BreakExpr(BreakExpr),
    ContinueExpr(ContinueExpr),
    BlockExpr(BlockExpr),
    TupleExpr(TupleExpr),
    LambdaExpr(LambdaExpr),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum NumLit {
    Bin(Token),
    Oct(Token),
    Dec(Token),
    Hex(Token),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct IfExpr {
    pub cond: ExprId,
    pub then: ExprId,
    pub else_: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct ReturnExpr {
    pub return_kw: Token,
    pub expr: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct BreakExpr {
    pub break_kw: Token,
    pub expr: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct ContinueExpr {
    pub continue_kw: Token,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct StrLiteral {
    pub span: HirSpan,
    pub fragments: Vec<StrLiteralFragment>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct StrLiteralFragment {
    pub kind: StrLiteralFragmentKind,
    pub span: HirSpan,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum StrLiteralFragmentKind {
    Text(Token),
    EscapedChar(Token, char),
    EscapeSequence(Token, char),
    Display(StrLiteralDisplayFragment),
    Debug(StrLiteralDebugFragment),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct StrLiteralDisplayFragment {
    pub display_token: Token,
    pub span: HirSpan,
    pub expr: ExprId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct StrLiteralDebugFragment {
    pub debug_token: Token,
    pub span: HirSpan,
    pub expr: ExprId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct LoopExpr {
    pub body: BlockId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct BlockExpr {
    pub block: BlockId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct Block {
    pub items: ItemList,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct TupleExpr {
    pub exprs: Vec<ExprId>,
}

hir_id_newtype!(StmtId, Stmt);

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum StmtKind {
    ExprStmt(ExprId),
    LetStmt(LetStmt),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
    AssignmentStmt(AssignmentStmt),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct AssignmentStmt {
    pub lhs: ExprId,
    pub op: AssignmentOp,
    pub rhs: ExprId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
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

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct LetStmt {
    pub mutability: LetMutability,
    pub pat: Pat,
    pub ty: Option<TyRef>,
    pub init: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum LetMutability {
    Imm,
    Mut(HirSpan),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct ForStmt {
    pub pat: Pat,
    pub iter: ExprId,
    pub body: BlockId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct WhileStmt {
    pub expr: ExprId,
    pub body: BlockId,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct Pat {
    pub kind: PatKind,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum PatKind {
    Ident(Ident),
    Tuple(Vec<Pat>),
    Wildcard(Ident),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct TyRef {
    pub span: HirSpan,
    pub kind: TyRefKind,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum TyRefKind {
    Named(Ident, TyGenericArgs),
    Primitive(PrimitiveTy),
    Fn(FnTy),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
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

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct TyGenericArgs {
    pub args: Vec<TyGenericArg>,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct TyGenericArg {
    pub kind: TyGenericArgKind,
    pub span: HirSpan,
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub enum TyGenericArgKind {
    ConstVal(ExprId),
    Type(TyRef),
}

#[derive(Debug, Eq, PartialEq, HirStructIdCheck, Clone)]
pub struct FnTy {
    pub params: Vec<TyRef>,
    pub ret_ty: Option<Box<TyRef>>,
}
