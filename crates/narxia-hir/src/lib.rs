#![feature(try_blocks)]

use std::fmt;
use std::ops;

mod hir;
pub mod hir_map;
pub mod visitor;
pub mod visitor_mut;

pub use hir::*;

#[derive(Eq, PartialEq, Clone, Copy, Hash, Ord, PartialOrd)]
pub struct HirSpan {
    start: usize,
    len: usize,
}

pub const DUMMY_SP: HirSpan = HirSpan { start: 0, len: 0 };

impl HirSpan {
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end);
        Self {
            start,
            len: end - start,
        }
    }

    pub fn get_start(self) -> usize {
        self.start
    }

    pub fn get_end(self) -> usize {
        self.start + self.len
    }

    pub fn get_range(self) -> ops::Range<usize> {
        self.get_start()..self.get_end()
    }

    pub fn join(self, other: Self) -> Self {
        let start = self.start.min(other.start);
        let end = self.get_end().max(other.get_end());

        Self::new(start, end)
    }
}

impl fmt::Display for HirSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.get_start(), self.get_end())
    }
}

impl fmt::Debug for HirSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
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
    pub const ORPHAN_HIRID: Self = Self::new(usize::MAX);

    pub const fn new(id: usize) -> Self {
        Self {
            id,
            #[cfg(hir_id_span)]
            span: DUMMY_SP,
        }
    }

    #[cfg(hir_id_span)]
    pub const fn span(self) -> HirSpan {
        self.span
    }

    pub const fn is_orphan_parent(self) -> bool {
        self.id == usize::MAX
    }

    pub const fn as_usize(self) -> usize {
        self.id
    }
}

// TODO: nice display of HIR
