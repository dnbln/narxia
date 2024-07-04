use std::fmt;

use narxia_syn::syntree::Token;

use crate::{HirId, HirSpan};

mod hir_debug;

pub use hir_debug::*;

pub trait HirIdNewtype {
    fn hir_id(&self) -> HirId;
}

impl HirIdNewtype for HirId {
    fn hir_id(&self) -> HirId {
        *self
    }
}

macro_rules! hir_id_newtype {
    ($name:ident, $t:ty) => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        pub struct $name(pub HirId);

        impl HirIdNewtype for $name {
            fn hir_id(&self) -> HirId {
                self.0
            }
        }
    };
}

hir_id_newtype!(ItemId, Item);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ItemList {
    pub items: Vec<ItemId>,
}

hir_id_newtype!(ModId, ModDef);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ModDef {
    pub items: ItemList,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ItemKind {
    FnDef(FnId),
    Stmt(StmtId),
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnDef {
    pub name: Ident,
    pub generics: Option<GenericParams>,
    pub params: Vec<FnParam>,
    pub ret_ty: Option<FnRetTy>,
    pub body: BlockId,
}

hir_id_newtype!(BlockId, Block);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct GenericParams {
    pub span: HirSpan,
    pub params: Vec<GenericParam>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct GenericParam {
    pub span: HirSpan,
    pub kind: GenericParamKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum GenericParamKind {
    Type(Ident),
    Const(Ident, TyRefId),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnParam {
    pub param_span: HirSpan,
    pub pat: Pat,
    pub ty: TyRefId,
    pub default: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnRetTy {
    pub span: HirSpan,
    pub arrow_span: HirSpan,
    pub ty: TyRefId,
}

hir_id_newtype!(ExprId, Expr);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExprKind {
    Atom(ExprAtom),
    Binary(BinaryOpExpr),
    CallExpr(CallExpr),
    IndexExpr(IndexExpr),
    FieldAccess(FieldAccess),
    MethodCall(MethodCall),
    CustomInfix(CustomInfixExpr),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CustomInfixExpr {
    pub base: ExprId,
    pub name: Ident,
    pub arg: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BinaryOpExpr {
    pub lhs: ExprId,
    pub op: BinOp,
    pub rhs: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CallExpr {
    pub callee: ExprId,
    pub args: CallExprArgs,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CallExprArgs {
    pub args: Vec<ExprId>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IndexExpr {
    pub base: ExprId,
    pub index: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FieldAccess {
    pub base: ExprId,
    pub field: Ident,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MethodCall {
    pub base: ExprId,
    pub method: Ident,
    pub args: CallExprArgs,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LambdaExpr {
    pub lambda_param_list: Option<LambdaParamList>,
    pub body: ItemList,
}

#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord, Copy)]
pub struct LambdaExprId(pub ExprId);

impl HirIdNewtype for LambdaExprId {
    fn hir_id(&self) -> HirId {
        self.0.hir_id()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LambdaParamList {
    pub params: Vec<LambdaParam>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LambdaParam {
    pub pat: Pat,
    pub ty: Option<TyRefId>,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ExprAtom {
    pub kind: ExprAtomKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum NumLit {
    Bin(Token),
    Oct(Token),
    Dec(Token),
    Hex(Token),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum NumLitSize {
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum NumLitValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

impl NumLit {
    pub fn parse_to_size(&self, size: NumLitSize) -> NumLitValue {
        match size {
            NumLitSize::I8 => match self {
                NumLit::Bin(t) => NumLitValue::I8(i8::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I8(i8::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I8(i8::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::I8(i8::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
            NumLitSize::I16 => match self {
                NumLit::Bin(t) => NumLitValue::I16(i16::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I16(i16::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I16(i16::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::I16(i16::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
            NumLitSize::I32 => match self {
                NumLit::Bin(t) => NumLitValue::I32(i32::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I32(i32::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I32(i32::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::I32(i32::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
            NumLitSize::I64 => match self {
                NumLit::Bin(t) => NumLitValue::I64(i64::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I64(i64::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I64(i64::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::I64(i64::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
            NumLitSize::I128 => match self {
                NumLit::Bin(t) => NumLitValue::I128(i128::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I128(i128::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I128(i128::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::I128(i128::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
            NumLitSize::U8 => match self {
                NumLit::Bin(t) => NumLitValue::U8(u8::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U8(u8::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U8(u8::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::U8(u8::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
            NumLitSize::U16 => match self {
                NumLit::Bin(t) => NumLitValue::U16(u16::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U16(u16::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U16(u16::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::U16(u16::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
            NumLitSize::U32 => match self {
                NumLit::Bin(t) => NumLitValue::U32(u32::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U32(u32::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U32(u32::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::U32(u32::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
            NumLitSize::U64 => match self {
                NumLit::Bin(t) => NumLitValue::U64(u64::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U64(u64::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U64(u64::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::U64(u64::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
            NumLitSize::U128 => match self {
                NumLit::Bin(t) => NumLitValue::U128(u128::from_str_radix(&t.text()[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U128(u128::from_str_radix(&t.text()[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U128(u128::from_str_radix(&t.text(), 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::U128(u128::from_str_radix(&t.text()[2..], 16).unwrap()),
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExpr {
    pub if_kw: HirSpan,
    pub cond: ExprId,
    pub then: ExprId,
    pub else_: Option<IfExprElseClause>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExprElseClause {
    pub else_kw: HirSpan,
    pub expr: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ReturnExpr {
    pub return_kw: Token,
    pub expr: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BreakExpr {
    pub break_kw: Token,
    pub expr: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ContinueExpr {
    pub continue_kw: Token,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StrLiteral {
    pub span: HirSpan,
    pub fragments: Vec<StrLiteralFragment>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StrLiteralFragment {
    pub kind: StrLiteralFragmentKind,
    pub span: HirSpan,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StrLiteralFragmentKind {
    Text(Token),
    EscapedChar(Token, char),
    EscapeSequence(Token, char),
    Display(StrLiteralDisplayFragment),
    Debug(StrLiteralDebugFragment),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StrLiteralDisplayFragment {
    pub display_token: Token,
    pub span: HirSpan,
    pub expr: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StrLiteralDebugFragment {
    pub debug_token: Token,
    pub span: HirSpan,
    pub expr: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LoopExpr {
    pub body: BlockId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockExpr {
    pub block: BlockId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block {
    pub items: ItemList,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TupleExpr {
    pub exprs: Vec<ExprId>,
}

hir_id_newtype!(StmtId, Stmt);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StmtKind {
    ExprStmt(ExprId),
    LetStmt(LetStmt),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
    AssignmentStmt(AssignmentStmt),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AssignmentStmt {
    pub lhs: ExprId,
    pub op: AssignmentOp,
    pub rhs: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetStmt {
    pub mutability: LetMutability,
    pub pat: Pat,
    pub ty: Option<TyRefId>,
    pub init: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum LetMutability {
    Imm,
    Mut(HirSpan),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ForStmt {
    pub pat: Pat,
    pub iter: ExprId,
    pub body: BlockId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct WhileStmt {
    pub expr: ExprId,
    pub body: BlockId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Pat {
    pub kind: PatKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PatKind {
    Ident(Ident),
    Tuple(Vec<Pat>),
    Wildcard(Ident),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TyRef {
    pub span: HirSpan,
    pub kind: TyRefKind,
}

hir_id_newtype!(TyRefId, TyRef);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TyRefKind {
    Named(Ident, TyGenericArgs),
    Primitive(PrimitiveTy),
    Fn(FnTy),
}

#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TyGenericArgs {
    pub args: Vec<TyGenericArg>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TyGenericArg {
    pub kind: TyGenericArgKind,
    pub span: HirSpan,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TyGenericArgKind {
    ConstVal(ExprId),
    Type(TyRefId),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnTy {
    pub params: Vec<TyRefId>,
    pub ret_ty: Option<TyRefId>,
}
