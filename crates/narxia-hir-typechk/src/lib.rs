use std::fmt::Debug;

use def_id::DefId;
use narxia_data_structures::FxBTreeMap;
use narxia_hir::hir::{Ident, ModDef, ModId};
use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::{self, HirVisitor};
use narxia_hir::{hir, HirId};

pub mod def_id;

pub mod ty;
pub mod tyctxt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlaceBase {
    pub hir_id: HirId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Projection {
    pub kind: ProjectionKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectionKind {
    Field(String),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Place {
    pub place_base: PlaceBase,
    pub projections: Vec<Projection>,
}
