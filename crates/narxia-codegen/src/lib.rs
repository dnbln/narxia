use std::io;
use std::path::PathBuf;

use ir::Function;
use ir::FunctionRef;
use ir::Global;
use ir::GlobalRef;
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
    globals: Vec<Global>,
    functions: Vec<Function>,
}

impl Default for TyCtxt {
    fn default() -> Self {
        Self::new()
    }
}

impl TyCtxt {
    pub fn new() -> Self {
        Self {
            types: vec![],
            globals: vec![],
            functions: vec![],
        }
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

    pub fn add_global(&mut self, global: Global) -> GlobalRef {
        let id = self.globals.len();
        self.globals.push(global);
        GlobalRef { id }
    }

    pub fn get_global(&self, global: GlobalRef) -> &Global {
        &self.globals[global.id]
    }

    pub fn add_function(&mut self, function: Function) -> FunctionRef {
        let id = self.functions.len();
        self.functions.push(function);
        FunctionRef { id }
    }

    pub fn get_function(&self, function: FunctionRef) -> &Function {
        &self.functions[function.id]
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyRef {
    id: usize,
}
