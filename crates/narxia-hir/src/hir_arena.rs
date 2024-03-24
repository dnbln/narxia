use std::fmt;

use narxia_src_db::SrcFile;

use crate::hir::{
    AssignmentStmt, Block, Expr, FnDef, FnParam, FnRetTy, Item, ModDef, Pat, Stmt, StrLiteral, StrLiteralDebugFragment, StrLiteralDisplayFragment, TyGenericArg, TyRef
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
    Pat(&'hir Pat),
    Stmt(&'hir Stmt),
    Block(&'hir Block),
    TyRef(&'hir TyRef),
    TyGenericArg(&'hir TyGenericArg),
    AssignmentStmt(&'hir AssignmentStmt),
    StrLiteral(&'hir StrLiteral),
    StrLiteralDisplayFragment(&'hir StrLiteralDisplayFragment),
    StrLiteralDebugFragment(&'hir StrLiteralDebugFragment),
}

impl<'hir> fmt::Display for HirRefElem<'hir> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mod(m) => write!(f, "{}", m),
            Self::Item(i) => write!(f, "{}", i),
            Self::Fn(fn_def) => write!(f, "{}", fn_def),
            Self::FnParam(p) => write!(f, "{}", p),
            Self::FnRetTy(r) => write!(f, "{}", r),
            Self::Expr(e) => write!(f, "{}", e),
            Self::Pat(p) => write!(f, "{}", p),
            Self::Stmt(s) => write!(f, "{}", s),
            Self::Block(b) => write!(f, "{}", b),
            Self::TyRef(t) => write!(f, "{}", t),
            Self::TyGenericArg(t) => write!(f, "{}", t),
            Self::AssignmentStmt(a) => write!(f, "{}", a),
            Self::StrLiteral(s) => write!(f, "{}", s),
            Self::StrLiteralDisplayFragment(s) => write!(f, "{}", s),
            Self::StrLiteralDebugFragment(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirRefArena<'hir> {
    buffer: Vec<HirRefElem<'hir>>,
    current_file: SrcFile,
    current_id: usize,
    start_id: usize,
}

impl<'hir> HirRefArena<'hir> {
    pub fn new(file: SrcFile) -> Self {
        Self {
            buffer: Vec::new(),
            current_file: file,
            current_id: 0,
            start_id: 0,
        }
    }

    pub fn new_starting_at(file: SrcFile, start: HirId) -> Self {
        Self {
            buffer: Vec::new(),
            current_file: file,
            current_id: start.id,
            start_id: start.id,
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
        self.buffer[at.id]
    }
}
