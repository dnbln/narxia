//! During this step, we collect all type definitions and type aliases.
//!
//! This is the first step of type checking.

use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::HirVisitor;

use crate::def_id::DefId;
use crate::ty::Ty;
use crate::tyctxt::TyCtxt;

pub struct TyDef {
    pub def_id: DefId,
}

struct Visitor<'tcx> {
    tydefs: Vec<TyDef>,
    tcx: TyCtxt<'tcx>,
    hir_map: &'tcx HirMap,
}

impl<'tcx> HirVisitor<'tcx> for Visitor<'tcx> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'tcx HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }
}
