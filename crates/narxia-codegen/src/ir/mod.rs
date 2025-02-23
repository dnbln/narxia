use crate::TyRef;

pub struct Mod {
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
    pub global_code: Function,
}

pub struct Global {
    pub name: String,
    pub ty: TyRef,
    pub value: Expr,
}

pub struct Expr {
    pub kind: ExprKind,
}

pub enum ExprKind {
    Atom(ExprAtom),
    Call(CallExpr),
    If(IfExpr),
    Loop(LoopExpr),
    CFExpr(CFExpr),
    Block(Block),
}

pub struct Block {
    pub instr: Vec<Instr>,
}

pub struct Instr {
    pub kind: InstrKind,
}

pub enum InstrKind {
    DeclareLocal(LocalRef, TyRef),
    AssignLocal(LocalRef, Expr),
    AssignGlobal(GlobalRef, Expr),
    Expr(Expr),
}

pub struct IfExpr {
    pub cond: Box<Expr>,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

pub struct LoopExpr {
    pub id: LoopExprId,
    pub block: Block,
}

pub struct LoopExprId {
    pub id: usize,
}

pub struct CFExpr {
    pub kind: CFExprKind,
}

pub enum CFExprKind {
    Return(Option<Box<Expr>>),
    Break(LoopExprId, Option<Box<Expr>>),
    Continue(LoopExprId),
}

pub struct CallExpr {
    pub func: GlobalRef,
    pub args: Vec<Expr>,
}

pub struct ExprAtom {
    pub kind: ExprAtomKind,
}

pub enum ExprAtomKind {
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
    F32(f32),
    F64(f64),
    Bool(bool),
    Unit,
    Global(GlobalRef),
    Local(LocalRef),
}

pub struct GlobalRef {
    pub id: usize,
}

pub struct LocalRef {
    pub id: usize,
}

pub struct Function {
    pub name: String,
    pub ty: TyRef,
    pub block: Block,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Unit,
    Never,
    Primitive(PrimitiveTy),
    Function(FunctionTy),
}

#[derive(Clone, PartialEq, Eq, Hash)]
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
    Isize,
    Usize,
    F32,
    F64,
    Bool,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionTy {
    pub args: Vec<TyRef>,
    pub ret: TyRef,
}