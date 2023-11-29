use narxia_src_db::SrcFile;

use crate::hir::{
    Block, Expr, FnDef, FnParam, FnRetTy, Item, LambdaExpr, ModDef, Pat, Stmt, TyGenericArg, TyRef,
};
use crate::HirId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirRefElem<'hir> {
    Mod(&'hir ModDef),
    Item(&'hir Item),
    Fn(&'hir FnDef),
    FnParam(&'hir FnParam),
    FnRetTy(&'hir FnRetTy),
    Expr(&'hir Expr),
    LambdaExpr(&'hir LambdaExpr),
    Pat(&'hir Pat),
    Stmt(&'hir Stmt),
    Block(&'hir Block),
    TyRef(&'hir TyRef),
    TyGenericArg(&'hir TyGenericArg),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirRefArena<'hir> {
    buffer: Vec<HirRefElem<'hir>>,
    current_file: SrcFile,
    current_id: usize,
}

impl<'hir> HirRefArena<'hir> {
    pub fn new(file: SrcFile) -> Self {
        Self {
            buffer: Vec::new(),
            current_file: file,
            current_id: 0,
        }
    }

    pub fn create_id(&mut self, prev_hir_id: HirId) -> HirId {
        let id = self.current_id;
        self.current_id += 1;
        HirId {
            root: self.current_file,
            id,
            #[cfg(hir_id_span)]
            span: prev_hir_id.span,
        }
    }

    pub fn push_ref(&mut self, at: HirId, r: HirRefElem<'hir>) {
        if at.id != self.buffer.len() {
            panic!("push_ref: at.id != self.buffer.len()");
        }
        self.buffer.push(r);
    }

    pub fn get(&self, at: HirId) -> HirRefElem<'hir> {
        if at.root != self.current_file {
            panic!("get: at.root != self.current_file");
        }
        self.buffer[at.id]
    }
}
