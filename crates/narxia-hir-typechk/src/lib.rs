#![feature(let_chains)]

use std::fmt::Debug;

use narxia_hir::HirId;

pub mod def_id;

pub mod fn_collection;
pub mod scope_rules;
pub mod ty;
pub mod tyctxt;
mod tydef_collection;
mod tyinfer;

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
