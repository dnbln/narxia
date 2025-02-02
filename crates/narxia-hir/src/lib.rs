#![feature(try_blocks)]
#![feature(let_chains)]

use core::fmt;

use hir_map::HirMap;
use narxia_src_db::SrcFile;
use narxia_syn::syntree::{self, Token};
use narxia_syn::text_span::TextSpan;

pub mod hir;
pub mod hir_map;
pub mod lower;
pub mod visitor;
pub mod visitor_mut;

#[derive(Eq, PartialEq, Clone, Copy, Hash, Ord, PartialOrd)]
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

#[derive(Eq, PartialEq, Clone, Copy, Hash, Ord, PartialOrd)]
pub struct HirId {
    id: usize,
    #[cfg(hir_id_span)]
    span: HirSpan,
}

impl AsRef<HirId> for HirId {
    fn as_ref(&self) -> &HirId {
        self
    }
}

impl HirId {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            #[cfg(hir_id_span)]
            span: DUMMY_SP,
        }
    }

    #[cfg(hir_id_span)]
    pub fn span(self) -> HirSpan {
        self.span
    }
}

// TODO: nice display of HIR
