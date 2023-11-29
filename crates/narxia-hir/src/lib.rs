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

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
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

#[derive(Eq, PartialEq, Clone, Copy)]
pub struct HirId {
    root: SrcFile,
    id: usize,
    #[cfg(hir_id_span)]
    span: HirSpan,
}

impl HirId {
    pub fn is_dummy(self) -> bool {
        self.id == usize::MAX
    }

    #[cfg(hir_id_span)]
    pub fn span(self) -> HirSpan {
        self.span
    }
}

pub fn build_ref_arena(file: SrcFile, mod_def: &hir::ModDef) -> HirRefArena {
    let mut ctxt = hir_collect_ids::HirCollectIdsCtxt::new(HirRefArena::new(file));
    hir_collect_ids::collect_ids_mod(&mut ctxt, mod_def);
    ctxt.into_arena()
}

// TODO: nice display of HIR
