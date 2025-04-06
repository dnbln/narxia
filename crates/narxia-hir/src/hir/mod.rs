//! The high-level intermediate representation (HIR) of the Narxia compiler.
//!
//! The HIR is a tree-like structure that represents the source code in a more structured and abstract way than the raw syntax tree.

use std::fmt;

use narxia_syn::syntax_kind::SyntaxKind;
use narxia_syn::syntree::Token;

use crate::HirId;
use crate::HirSpan;
use crate::DUMMY_SP;

mod hir_debug;

pub use hir_debug::*;

pub trait HirIdNewtype {
    fn new(hir_id: HirId) -> Self
    where
        Self: Sized;
    fn hir_id(&self) -> HirId;
}

impl HirIdNewtype for HirId {
    fn new(hir_id: HirId) -> Self
    where
        Self: Sized,
    {
        hir_id
    }
    fn hir_id(&self) -> HirId {
        *self
    }
}

macro_rules! hir_id_newtype {
    ($name:ident, $t:ty) => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        pub struct $name(pub HirId);

        impl HirIdNewtype for $name {
            fn new(hir_id: HirId) -> Self
            where
                Self: Sized,
            {
                Self(hir_id)
            }

            fn hir_id(&self) -> HirId {
                self.0
            }
        }
    };
}

pub(crate) trait ConstToken {
    const KIND: SyntaxKind;

    fn make_virtual() -> Self
    where
        Self: Sized,
    {
        Self::from_span(DUMMY_SP)
    }

    fn from_span(span: HirSpan) -> Self;
    fn from_token(token: &Token) -> Self
    where
        Self: Sized,
    {
        debug_assert_eq!(token.kind(), Self::KIND);
        debug_assert_eq!(token.text(), Self::text());
        debug_assert_eq!(HirSpan::of(token).span.len_usize(), Self::text().len());

        let span = HirSpan::of(token);
        Self::from_span(span)
    }

    fn span(&self) -> HirSpan;
    fn text() -> &'static str;
}

macro_rules! const_token {
    ($name:ident, $kind:ident, $text:literal) => {
        #[derive(Debug, Eq, PartialEq, Clone, Copy)]
        pub struct $name {
            pub span: HirSpan,
        }

        impl ConstToken for $name {
            const KIND: SyntaxKind = SyntaxKind::$kind;

            fn from_span(span: HirSpan) -> Self {
                Self { span }
            }

            fn span(&self) -> HirSpan {
                self.span
            }

            fn text() -> &'static str {
                $text
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

const_token!(ModuleKw, MODULE_KW, "module");

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ModDef {
    pub mod_kw: ModuleKw,
    pub name: Ident,
    pub body: Option<ModBody>,

    pub hir_id: ModId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ModBody {
    pub lbrace: LBrace,
    pub items: ItemList,
    pub rbrace: RBrace,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Item {
    pub attrs: AttrList,
    pub kind: ItemKind,
    pub hir_id: ItemId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AttrList {
    pub attrs: Vec<Attr>,
}

impl Default for AttrList {
    fn default() -> Self {
        Self::new()
    }
}

impl AttrList {
    pub fn new() -> Self {
        Self { attrs: Vec::new() }
    }
}

const_token!(Hash, HASH, "#");

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Attr {
    pub span: HirSpan,
    pub hash: Hash,
    pub name: Ident,
    pub meta: Option<AttrMeta>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AttrMeta {
    pub lbrack: LBracket,
    pub meta_list: Vec<AttrMetaItem>,
    pub rbrack: RBracket,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AttrMetaItem {
    pub span: HirSpan,
    pub name: Ident,
    pub kind: AttrMetaItemKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum AttrMetaItemKind {
    Eq(AttrMetaItemEq),
    Call(AttrMetaItemCall),
    NameOnly,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AttrMetaItemEq {
    pub eq: Eq,
    pub expr: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AttrMetaItemCall {
    pub lparen: LParen,
    pub meta_list: Vec<AttrMetaItem>,
    pub rparen: RParen,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ItemKind {
    FnDef(FnId),
    Stmt(StmtId),
    UseStmt(UseStmtId),

    ModDef(ModId),
}

hir_id_newtype!(UseStmtId, UseStmt);

const_token!(UseKw, USE_KW, "use");

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UseStmt {
    pub use_kw: UseKw,
    pub span: HirSpan,
    pub path: UsePath,
    pub hir_id: UseStmtId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UsePath {
    pub segments: Vec<UsePathSegmentId>,
    pub alias: Option<UseAlias>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UseAlias {
    pub as_kw: AsKw,
    pub alias: Ident,
}

hir_id_newtype!(UsePathSegmentId, UsePathSegment);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UsePathSegment {
    pub ident: Ident,
    pub hir_id: UsePathSegmentId,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Clone)]
pub struct Ident {
    pub span: HirSpan,
    pub text: String,
}

impl Ident {
    pub fn new(span: impl Into<HirSpan>, text: impl Into<String>) -> Self {
        Self {
            span: span.into(),
            text: text.into(),
        }
    }

    pub fn new_virtual(text: impl Into<String>) -> Self {
        Self::new(DUMMY_SP, text)
    }
}

pub struct SpecialIdents;

impl SpecialIdents {
    pub const ROOT_MODULE: &'static str = "___nrx_root_module";
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {}", self.text, self.span)
    }
}

hir_id_newtype!(FnId, FnDef);

const_token!(FnKw, FN_KW, "fn");

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnDef {
    pub fn_kw: FnKw,
    pub name: Ident,
    pub generics: Option<GenericParams>,
    pub params: Option<FnParamList>,
    pub ret_ty: Option<FnRetTy>,
    pub body: BlockId,

    pub hir_id: FnId,
}

hir_id_newtype!(BlockId, Block);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct GenericParams {
    pub langle: LAngle,
    pub params: Vec<GenericParam>,
    pub rangle: RAngle,
    pub span: HirSpan,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct GenericParam {
    pub span: HirSpan,
    pub kind: GenericParamKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum GenericParamKind {
    Type(GenericParamTy),
    Const(GenericParamConst),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct GenericParamTy {
    pub name: Ident,
    pub bounds: Option<GenericParamTyBounds>,
    pub default: Option<(Eq, TyRefId)>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct GenericParamConst {
    pub const_kw: ConstKw,
    pub name: Ident,
    pub colon: Colon,
    pub ty: TyRefId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct GenericParamTyBounds {
    pub colon: Colon,
    pub bounds: Vec<TyRefId>,
    pub span: HirSpan,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnParamList {
    pub lparen: LParen,
    pub params: Vec<FnParam>,
    pub rparen: RParen,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnParam {
    pub param_span: HirSpan,
    pub pat: Pat,
    pub colon: Colon,
    pub ty: TyRefId,
    pub default: Option<(Eq, ExprId)>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnRetTy {
    pub span: HirSpan,
    pub arrow: ThinArrow,
    pub ty: TyRefId,
}

hir_id_newtype!(ExprId, Expr);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub hir_id: ExprId,
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
    pub lparen: Option<LParen>,
    pub args: Vec<ExprId>,
    pub rparen: Option<RParen>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IndexExpr {
    pub base: ExprId,
    pub lbrack: LBracket,
    pub index: ExprId,
    pub rbrack: RBracket,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FieldAccess {
    pub base: ExprId,
    pub dot: Dot,
    pub field: Ident,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MethodCall {
    pub base: ExprId,
    pub dot: Dot,
    pub method: Ident,
    pub args: CallExprArgs,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LambdaExpr {
    pub lbrace: LBrace,
    pub lambda_param_list: Option<LambdaParamList>,
    pub body: ItemList,
    pub rbrace: RBrace,
}

#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord, Copy)]
pub struct LambdaExprId(pub ExprId);

impl HirIdNewtype for LambdaExprId {
    fn new(hir_id: HirId) -> Self
    where
        Self: Sized,
    {
        Self(ExprId::new(hir_id))
    }

    fn hir_id(&self) -> HirId {
        self.0.hir_id()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LambdaParamList {
    pub params: Vec<LambdaParam>,
    pub arrow: ThinArrow,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LambdaParam {
    pub pat: Pat,
    pub ty: Option<(Colon, TyRefId)>,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinOp {
    Add(Plus),
    Sub(Minus),
    Mul(Asterisk),
    Div(Slash),
    Mod(Percent),
    Eq(Eq2),
    Neq(Neq),
    Lt(LAngle),
    LtEq(LEq),
    Gt(RAngle),
    GtEq(GEq),
    And(Amp2),
    Or(Pipe2),
    BitAnd(Amp),
    BitOr(Pipe),
    Xor(Caret),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ExprAtom {
    pub kind: ExprAtomKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExprAtomKind {
    Ident(ExprAtomIdentId),
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
pub struct ExprAtomIdent {
    pub ident: Ident,
    pub hir_id: ExprAtomIdentId,
}

hir_id_newtype!(ExprAtomIdentId, ExprAtomIdent);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum NumLit {
    Bin(Tk),
    Oct(Tk),
    Dec(Tk),
    Hex(Tk),
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
    #[expect(clippy::from_str_radix_10)]
    pub fn parse_to_size(&self, size: NumLitSize) -> NumLitValue {
        match size {
            NumLitSize::I8 => match self {
                NumLit::Bin(t) => NumLitValue::I8(i8::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I8(i8::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I8(i8::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::I8(i8::from_str_radix(&t.text[2..], 16).unwrap()),
            },
            NumLitSize::I16 => match self {
                NumLit::Bin(t) => NumLitValue::I16(i16::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I16(i16::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I16(i16::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::I16(i16::from_str_radix(&t.text[2..], 16).unwrap()),
            },
            NumLitSize::I32 => match self {
                NumLit::Bin(t) => NumLitValue::I32(i32::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I32(i32::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I32(i32::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::I32(i32::from_str_radix(&t.text[2..], 16).unwrap()),
            },
            NumLitSize::I64 => match self {
                NumLit::Bin(t) => NumLitValue::I64(i64::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I64(i64::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I64(i64::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::I64(i64::from_str_radix(&t.text[2..], 16).unwrap()),
            },
            NumLitSize::I128 => match self {
                NumLit::Bin(t) => NumLitValue::I128(i128::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::I128(i128::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::I128(i128::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => {
                    NumLitValue::I128(i128::from_str_radix(&t.text[2..], 16).unwrap())
                }
            },
            NumLitSize::U8 => match self {
                NumLit::Bin(t) => NumLitValue::U8(u8::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U8(u8::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U8(u8::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::U8(u8::from_str_radix(&t.text[2..], 16).unwrap()),
            },
            NumLitSize::U16 => match self {
                NumLit::Bin(t) => NumLitValue::U16(u16::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U16(u16::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U16(u16::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::U16(u16::from_str_radix(&t.text[2..], 16).unwrap()),
            },
            NumLitSize::U32 => match self {
                NumLit::Bin(t) => NumLitValue::U32(u32::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U32(u32::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U32(u32::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::U32(u32::from_str_radix(&t.text[2..], 16).unwrap()),
            },
            NumLitSize::U64 => match self {
                NumLit::Bin(t) => NumLitValue::U64(u64::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U64(u64::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U64(u64::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => NumLitValue::U64(u64::from_str_radix(&t.text[2..], 16).unwrap()),
            },
            NumLitSize::U128 => match self {
                NumLit::Bin(t) => NumLitValue::U128(u128::from_str_radix(&t.text[2..], 2).unwrap()),
                NumLit::Oct(t) => NumLitValue::U128(u128::from_str_radix(&t.text[1..], 8).unwrap()),
                NumLit::Dec(t) => NumLitValue::U128(u128::from_str_radix(&t.text, 10).unwrap()),
                NumLit::Hex(t) => {
                    NumLitValue::U128(u128::from_str_radix(&t.text[2..], 16).unwrap())
                }
            },
        }
    }
}

const_token!(IfKw, IF_KW, "if");
const_token!(ElseKw, ELSE_KW, "else");
const_token!(ReturnKw, RETURN_KW, "return");
const_token!(BreakKw, BREAK_KW, "break");
const_token!(ContinueKw, CONTINUE_KW, "continue");
const_token!(AsKw, AS_KW, "as");
const_token!(ConstKw, CONST_KW, "const");
const_token!(ForKw, FOR_KW, "for");
const_token!(InKw, IN_KW, "in");
const_token!(WhileKw, WHILE_KW, "while");
const_token!(LoopKw, LOOP_KW, "loop");
const_token!(TrueKw, TRUE_KW, "true");
const_token!(FalseKw, FALSE_KW, "false");

const_token!(Dot, DOT, ".");
const_token!(Comma, COMMA, ",");
const_token!(Colon2, COLON2, "::");
const_token!(Colon, COLON, ":");
const_token!(ThinArrow, THIN_ARROW, "->");
const_token!(FatArrow, FAT_ARROW, "=>");
const_token!(LBrace, L_BRACE, "{");
const_token!(RBrace, R_BRACE, "}");
const_token!(LParen, L_PAREN, "(");
const_token!(RParen, R_PAREN, ")");
const_token!(LBracket, L_BRACK, "[");
const_token!(RBracket, R_BRACK, "]");
const_token!(LAngle, L_ANGLE, "<");
const_token!(RAngle, R_ANGLE, ">");

const_token!(Eq, EQ, "=");
const_token!(PlusEq, PLUS_EQ, "+=");
const_token!(MinusEq, MINUS_EQ, "-=");
const_token!(AsteriskEq, ASTERISK_EQ, "*=");
const_token!(SlashEq, SLASH_EQ, "/=");
const_token!(PercentEq, PERCENT_EQ, "%=");
const_token!(AmpEq, AMP_EQ, "&=");
const_token!(PipeEq, PIPE_EQ, "|=");
const_token!(CaretEq, CARET_EQ, "^=");
const_token!(Plus, PLUS, "+");
const_token!(Minus, MINUS, "-");
const_token!(Asterisk, ASTERISK, "*");
const_token!(Slash, SLASH, "/");
const_token!(Percent, PERCENT, "%");
const_token!(Amp, AMP, "&");
const_token!(Pipe, PIPE, "|");
const_token!(Caret, CARET, "^");
const_token!(Eq2, EQ2, "==");
const_token!(Neq, NEQ, "!=");
const_token!(LEq, LE, "<=");
const_token!(GEq, GE, ">=");
const_token!(Amp2, AMP2, "&&");
const_token!(Pipe2, PIPE2, "||");

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Tk {
    pub span: HirSpan,
    pub kind: SyntaxKind,
    pub text: String,
}

impl Tk {
    pub(crate) fn from_token(token: &Token) -> Self {
        Self {
            span: HirSpan::of(token),
            kind: token.kind(),
            text: token.text().to_string(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExpr {
    pub if_kw: IfKw,
    pub lparen: LParen,
    pub cond: ExprId,
    pub rparen: RParen,
    pub then: ExprId,
    pub else_: Option<IfExprElseClause>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExprElseClause {
    pub else_kw: ElseKw,
    pub expr: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ReturnExpr {
    pub return_kw: ReturnKw,
    pub expr: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BreakExpr {
    pub break_kw: BreakKw,
    pub expr: Option<ExprId>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ContinueExpr {
    pub continue_kw: ContinueKw,
}

const_token!(LQuote, BEGIN_STRING, "\"");
const_token!(RQuote, END_STRING, "\"");

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StrLiteral {
    pub lquote: LQuote,
    pub span: HirSpan,
    pub fragments: Vec<StrLiteralFragment>,
    pub rquote: RQuote,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StrLiteralFragment {
    pub kind: StrLiteralFragmentKind,
    pub span: HirSpan,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StrLiteralFragmentKind {
    Text(StrLiteralTextFragment),
    EscapedChar(Tk, char),
    EscapeSequence(Tk, char),
    Display(StrLiteralDisplayFragment),
    Debug(StrLiteralDebugFragment),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StrLiteralTextFragment {
    pub token: Tk,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StrLiteralDisplayFragment {
    pub display_token: Tk,
    pub span: HirSpan,
    pub expr: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StrLiteralDebugFragment {
    pub debug_token: Tk,
    pub span: HirSpan,
    pub expr: ExprId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LoopExpr {
    pub loop_kw: LoopKw,
    pub body: BlockId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BlockExpr {
    pub block: BlockId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block {
    pub lbrace: LBrace,
    pub items: ItemList,
    pub rbrace: RBrace,

    pub hir_id: BlockId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TupleExpr {
    pub lparen: LParen,
    pub exprs: Vec<ExprId>,
    pub rparen: RParen,
}

hir_id_newtype!(StmtId, Stmt);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub hir_id: StmtId,
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
    Assign(Eq),
    AddAssign(PlusEq),
    SubAssign(MinusEq),
    MulAssign(AsteriskEq),
    DivAssign(SlashEq),
    ModAssign(PercentEq),
    BitAndAssign(AmpEq),
    BitOrAssign(PipeEq),
    BitXorAssign(CaretEq),
}

const_token!(LetKw, LET_KW, "let");
const_token!(MutKw, MUT_KW, "mut");

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetStmt {
    pub let_kw: LetKw,
    pub mutability: LetMutability,
    pub pat: Pat,
    pub ty: Option<(Colon, TyRefId)>,
    pub init: Option<(Eq, ExprId)>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum LetMutability {
    Imm,
    Mut(MutKw),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ForStmt {
    pub for_kw: ForKw,
    pub lparen: LParen,
    pub pat: Pat,
    pub in_kw: InKw,
    pub iter: ExprId,
    pub rparen: RParen,
    pub body: BlockId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct WhileStmt {
    pub while_kw: WhileKw,
    pub lparen: LParen,
    pub expr: ExprId,
    pub rparen: RParen,
    pub body: BlockId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Pat {
    pub kind: PatKind,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PatKind {
    Ident(PatIdentId),
    Tuple(Vec<Pat>),
    Wildcard(PatIdentId),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PatIdent {
    pub ident: Ident,
    pub hir_id: PatIdentId,
}

hir_id_newtype!(PatIdentId, PatIdent);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TyRef {
    pub span: HirSpan,
    pub kind: TyRefKind,
    pub hir_id: TyRefId,
}

hir_id_newtype!(TyRefId, TyRef);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TyRefKind {
    Named(Ident, TyGenericArgs),
    Primitive(PrimitiveTy),
    Fn(FnTy),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PrimitiveTy {
    pub ident: Ident,
    pub kind: PrimitiveTyKind,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum PrimitiveTyKind {
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
    pub args: Vec<TyGenericArgId>,
}

hir_id_newtype!(TyGenericArgId, TyGenericArg);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TyGenericArg {
    pub kind: TyGenericArgKind,
    pub span: HirSpan,
    pub hir_id: TyGenericArgId,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TyGenericArgKind {
    ConstVal(ExprId),
    Type(TyRefId),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnTy {
    pub fn_kw: FnKw,
    pub lparen: LParen,
    pub params: Vec<TyRefId>,
    pub rparen: RParen,
    pub ret_ty: Option<(ThinArrow, TyRefId)>,
}
