use core::fmt;

use hir_arena::HirRefArena;
use narxia_src_db::SrcFile;
use narxia_syn::syntree::{self, Token};
use narxia_syn::text_span::TextSpan;

pub mod hir;
pub mod hir_arena;
mod hir_collect_ids;
pub mod lower;
pub mod visitor;
pub mod visitor_mut;

#[derive(Eq, PartialEq, Clone, Copy)]
pub struct HirSpan {
    span: TextSpan,
}

pub const DUMMY_SP: HirSpan = HirSpan {
    span: unsafe { TextSpan::new_unchecked(0, 0) },
};

impl HirSpan {
    pub fn of_node<T: syntree::TreeNode>(node: &T) -> Self {
        Self {
            span: TextSpan::of_node(node.get_node()),
        }
    }

    pub fn of(token: &Token) -> Self {
        Self {
            span: TextSpan::of(token),
        }
    }
}

impl fmt::Display for HirSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.span)
    }
}

impl fmt::Debug for HirSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.span)
    }
}

#[derive(Eq, PartialEq, Clone, Copy)]
pub struct HirId {
    root: SrcFile,
    id: usize,
    #[cfg(hir_id_span)]
    span: HirSpan,
}

impl HirId {
    pub fn new_dummy(root: SrcFile, id: usize) -> Self {
        Self {
            root,
            id,
            #[cfg(hir_id_span)]
            span: DUMMY_SP,
        }
    }

    pub fn is_dummy(self) -> bool {
        self.id == usize::MAX
    }

    pub fn src_file(self) -> SrcFile {
        self.root
    }

    #[cfg(hir_id_span)]
    pub fn span(self) -> HirSpan {
        self.span
    }
}

pub fn build_refs_to_arena<'hir, 'arena>(arena: &'arena mut HirRefArena<'hir>, mod_def: &'hir hir::ModDef) {
    let mut ctxt = hir_collect_ids::HirCollectIdsCtxt::new(arena);
    hir_collect_ids::collect_ids_mod(&mut ctxt, mod_def);
}

// TODO: nice display of HIR
