use std::fmt;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Instr {
    pub lhs: LocalRef,
    pub rhs: IValue,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PhiInstr {
    pub lhs: LocalRef,
    pub rhs: Vec<(BlockRef, LocalRef)>,
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} = {:?}", self.lhs, self.rhs)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub id: BlockRef,
    pub preds: Vec<BlockRef>,
    pub var_phi: Vec<PhiInstr>,
    pub phi: Vec<PhiInstr>,
    pub instrs: Vec<Instr>,
    pub end: Option<EndInstr>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct EndInstr {
    pub local_ref: LocalRef,
    pub kind: EndInstrKind,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum EndInstrKind {
    Branch(BlockRef),
    ConditionalBranch(Value, BlockRef, BlockRef),
    Return(Value),
    RetVoid,
}

#[derive(Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct PlaceRef {
    pub id: usize,
}

impl fmt::Debug for PlaceRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "p@{}", self.id)
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:", self.id)?;

        if !self.preds.is_empty() {
            write!(f, "   # preds: ")?;
            for (id, pred) in self.preds.iter().enumerate() {
                if id != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{pred:?}")?;
            }
        }

        writeln!(f)?;

        for var_phi in &self.var_phi {
            writeln!(f, "  {var_phi:?}")?;
        }
        for phi in &self.phi {
            writeln!(f, "  {phi:?}")?;
        }
        for instr in &self.instrs {
            writeln!(f, "  {instr:?}")?;
        }
        if let Some(end) = &self.end {
            writeln!(f, "  {end:?}")?;
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Local(LocalRef),
    Const(i32),
    ConstStr(String),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Local(arg0) => arg0.fmt(f),
            Self::Const(arg0) => arg0.fmt(f),
            Self::ConstStr(arg0) => {
                write!(f, "\"")?;
                for c in arg0.chars() {
                    if c == '"' {
                        write!(f, "\\\"")?;
                    } else if c == '\\' {
                        write!(f, "\\\\")?;
                    } else if c == '\n' {
                        write!(f, "\\n")?;
                    } else if c == '\r' {
                        write!(f, "\\r")?;
                    } else if c == '\t' {
                        write!(f, "\\t")?;
                    } else {
                        write!(f, "{c}")?;
                    }
                }
                write!(f, "\"")
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum IValue {
    Value(Value),
    Param(usize),
    BinaryExpr(BinaryExpr),
    Call(CallExpr),
    FunctionRef(FunctionRef),
    SConcat(Vec<LocalRef>),
    Debug(LocalRef),
    Display(LocalRef),
    DoNothing,
}

impl fmt::Debug for IValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(arg0) => arg0.fmt(f),
            Self::BinaryExpr(arg0) => arg0.fmt(f),
            Self::Call(arg0) => arg0.fmt(f),
            Self::FunctionRef(arg0) => arg0.fmt(f),
            Self::DoNothing => write!(f, "__"),
            Self::Param(p) => {
                write!(f, "param@{p}")
            }
            Self::SConcat(arg0) => {
                write!(f, "sconcat(")?;
                for (id, value) in arg0.iter().enumerate() {
                    if id != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value:?}")?;
                }
                write!(f, ")")
            }
            Self::Debug(arg0) => {
                write!(f, "debug({arg0:?})")
            }
            Self::Display(arg0) => {
                write!(f, "display({arg0:?})")
            }
        }
    }
}

impl fmt::Debug for EndInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} = {:?}", self.local_ref, self.kind)
    }
}

impl fmt::Debug for EndInstrKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Branch(arg0) => {
                write!(f, "br {arg0:?}")
            }
            Self::ConditionalBranch(arg0, arg1, arg2) => {
                write!(f, "br {arg0:?} {arg1:?} {arg2:?}")
            }
            Self::Return(arg0) => {
                write!(f, "ret {arg0:?}")
            }
            Self::RetVoid => write!(f, "ret"),
        }
    }
}

impl fmt::Debug for PhiInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} = phi(", self.lhs)?;
        for (id, (block, value)) in self.rhs.iter().enumerate() {
            if id != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{block:?} {value:?}")?;
        }
        write!(f, ")")
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Value,
    pub rhs: Value,
}

impl fmt::Debug for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?} {:?}", self.op, self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    And,
    Or,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CallExpr {
    pub function: FunctionRef,
    pub args: Vec<Value>,
}

impl fmt::Debug for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "call {:?}(", self.function)?;
        for (id, arg) in self.args.iter().enumerate() {
            if id != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg:?}")?;
        }
        write!(f, ")")
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct FunctionRef {
    pub id: usize,
}

impl fmt::Debug for FunctionRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f@{}", self.id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct LocalRef {
    pub id: usize,
}

impl fmt::Debug for LocalRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "l@{}", self.id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct BlockRef {
    pub id: usize,
}

impl fmt::Debug for BlockRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b@{}", self.id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub fn_id: FunctionRef,
    pub name: String,
    pub ty: FunctionTy,
    pub blocks: Vec<Block>,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "fn {}: {}", self.name, self.ty)?;
        for block in &self.blocks {
            writeln!(f, "{block:?}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTy {
    pub params: Vec<TyRef>,
    pub ret: TyRef,
}

impl fmt::Display for FunctionTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (id, arg) in self.params.iter().enumerate() {
            if id != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{arg:?}")?;
        }
        write!(f, ") -> {:?}", self.ret)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TyRef {
    pub id: usize,
}

impl fmt::Debug for TyRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t@{}", self.id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub functions: Vec<Function>,
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for function in &self.functions {
            function.fmt(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}
