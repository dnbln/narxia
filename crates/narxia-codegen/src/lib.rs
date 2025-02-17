use std::path::PathBuf;

pub mod ir;

pub enum Out {
    File(PathBuf),
}

pub trait CodegenBackend {
    type Error;

    fn generate_code(&self, ir: &ir::Mod, out: &Out) -> Result<(), Self::Error>;
}