use std::io;
use std::path::PathBuf;

use ir::Ty;

pub mod ir;

pub enum Out<'a> {
    File(PathBuf),
    ToWrite(&'a mut dyn io::Write),
}

pub trait CodegenBackend {
    type Error;

    fn generate_code(&mut self, tcx: &TyCtxt, ir: &ir::Mod, out: Out) -> Result<(), Self::Error>;
}

pub struct TyCtxt {
    types: Vec<Ty>,
}

impl Default for TyCtxt {
    fn default() -> Self {
        Self::new()
    }
}

impl TyCtxt {
    pub fn new() -> Self {
        Self { types: vec![] }
    }

    pub fn add_ty(&mut self, ty: Ty) -> TyRef {
        if let Some(id) = self.types.iter().position(|t| *t == ty) {
            return TyRef { id };
        }

        let id = self.types.len();
        self.types.push(ty);
        TyRef { id }
    }

    pub fn get_ty(&self, ty: TyRef) -> &Ty {
        &self.types[ty.id]
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyRef {
    id: usize,
}
