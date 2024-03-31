use std::fmt::Debug;

use narxia_data_structures::FxHashMap;
use narxia_hir::HirId;

use crate::def_id::DefId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveTy {
    Bool,
    Char,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    Str,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TyVar {
    pub(crate) id: usize,
}

impl Debug for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_t{}", self.id)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Ty {
    Primitive(PrimitiveTy),
    Adt(TyAdt),
    TyVar(TyVar),
    Fn(FunTy),
    Never,
}

impl Debug for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Primitive(p) => write!(f, "{p:?}"),
            Ty::Adt(adt) => write!(f, "{adt:?}"),
            Ty::TyVar(ty_var) => write!(f, "{ty_var:?}"),
            Ty::Fn(fty) => write!(f, "{fty:?}"),
            Ty::Never => write!(f, "!"),
        }
    }
}

impl From<TyVar> for Ty {
    fn from(ty_var: TyVar) -> Self {
        Ty::TyVar(ty_var)
    }
}

impl From<FunTy> for Ty {
    fn from(fty: FunTy) -> Self {
        Ty::Fn(fty)
    }
}

impl Ty {
    pub const UNIT_TY: Ty = Ty::Adt(TyAdt::Unit);
}

#[derive(Clone, PartialEq, Eq)]
pub struct FunTy {
    inputs: Vec<Ty>,
    output: Box<Ty>,
}

impl Debug for FunTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, ty) in self.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{ty:?}")?;
        }
        write!(f, ")")?;

        if *self.output != Ty::UNIT_TY {
            write!(f, " -> {ty:?}", ty = self.output)?
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyAdt {
    Struct(StructTyAdt),
    Enum(EnumTyAdt),
    Tuple(TupleTyAdt),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructTyAdt {
    def_id: DefId,
    fields: FxHashMap<String, Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructTyAdtKind {
    Zst,
    Struct { fields: FxHashMap<String, Ty> },
    TupleStruct { fields: Vec<Ty> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumTyAdt {
    def_id: DefId,
    variants: FxHashMap<String, EnumVariantAdt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariantAdt {
    s: StructTyAdt,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleTyAdt {
    def_id: DefId,
    fields: Vec<Ty>,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TyClass {
    Std(StdTyClass),
    UserDefined(UserDefinedTyClass),
}

impl Debug for TyClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyClass::Std(klass) => write!(f, "{klass:?}"),
            TyClass::UserDefined(klass) => write!(f, "{klass:?}"),
        }
    }
}

impl From<StdTyClass> for TyClass {
    fn from(klass: StdTyClass) -> Self {
        TyClass::Std(klass)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinOpTyClass {
    pub(crate) rhs: Ty,
    pub(crate) output: Ty,
}

impl BinOpTyClass {
    pub fn new(rhs: &Ty, output: &Ty) -> Self {
        Self {
            rhs: rhs.clone(),
            output: output.clone(),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum StdTyClass {
    Display,
    Debug,
    Num,
    BinPlus(BinOpTyClass),
    BinMinus(BinOpTyClass),
    Mul(BinOpTyClass),
    Div(BinOpTyClass),
    Mod(BinOpTyClass),
    Eq(BinOpTyClass),
    Neq(BinOpTyClass),
    Lt(BinOpTyClass),
    LtEq(BinOpTyClass),
    Gt(BinOpTyClass),
    GtEq(BinOpTyClass),
    And(BinOpTyClass),
    Or(BinOpTyClass),
    BitAnd(BinOpTyClass),
    BitOr(BinOpTyClass),
    Xor(BinOpTyClass),
}

impl Debug for StdTyClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_bin_op {
            ($f:expr, $op:literal, $binoptyclass:expr) => {
                write!(
                    $f,
                    "_ {} {rhs:?} -> {out:?}",
                    $op,
                    rhs = $binoptyclass.rhs,
                    out = $binoptyclass.output
                )
            };
        }
        match self {
            StdTyClass::Display => write!(f, "{{display}}"),
            StdTyClass::Debug => write!(f, "{{debug}}"),
            StdTyClass::Num => write!(f, "{{num}}"),
            StdTyClass::BinPlus(op) => write_bin_op!(f, "(+)", op),
            StdTyClass::BinMinus(op) => write_bin_op!(f, "(-)", op),
            StdTyClass::Mul(op) => write_bin_op!(f, "(*)", op),
            StdTyClass::Div(op) => write_bin_op!(f, "(/)", op),
            StdTyClass::Mod(op) => write_bin_op!(f, "(%)", op),
            StdTyClass::Eq(op) => write_bin_op!(f, "(==)", op),
            StdTyClass::Neq(op) => write_bin_op!(f, "(!=)", op),
            StdTyClass::Lt(op) => write_bin_op!(f, "(<)", op),
            StdTyClass::LtEq(op) => write_bin_op!(f, "(<=)", op),
            StdTyClass::Gt(op) => write_bin_op!(f, "(>)", op),
            StdTyClass::GtEq(op) => write_bin_op!(f, "(>=)", op),
            StdTyClass::And(op) => write_bin_op!(f, "(&&)", op),
            StdTyClass::Or(op) => write_bin_op!(f, "(||)", op),
            StdTyClass::BitAnd(op) => write_bin_op!(f, "(&)", op),
            StdTyClass::BitOr(op) => write_bin_op!(f, "(|)", op),
            StdTyClass::Xor(op) => write_bin_op!(f, "(^)", op),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserDefinedTyClass {
    pub(crate) def_id: DefId,
}

pub(crate) struct FTyBuilder {
    inputs: Vec<Ty>,
    output: Ty,
}

impl FTyBuilder {
    pub fn new() -> Self {
        FTyBuilder {
            inputs: vec![],
            output: Ty::UNIT_TY,
        }
    }

    pub fn add_input(&mut self, ty: Ty) {
        self.inputs.push(ty);
    }

    pub fn set_output(&mut self, ty: Ty) {
        self.output = ty;
    }

    pub fn build(self) -> FunTy {
        FunTy {
            inputs: self.inputs,
            output: Box::new(self.output),
        }
    }
}
