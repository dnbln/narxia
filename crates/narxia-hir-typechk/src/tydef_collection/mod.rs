use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::{HirVisitor, RecursiveIdHandleStrategy};

use crate::def_id::DefId;
use crate::ty::Ty;
use crate::tyctxt::TyCtxt;

/// During this step, we collect all type definitions and type aliases.
///
/// This is the first step of type checking.

pub struct TyDef {
    pub def_id: DefId,
}

struct Visitor<'tcx> {
    tydefs: Vec<TyDef>,
    tcx: TyCtxt<'tcx>,
    hir_map: &'tcx HirMap,
}

impl<'tcx> HirVisitor<'tcx> for Visitor<'tcx> {
    type Strategy = RecursiveIdHandleStrategy<'tcx>;

    fn get_strategy(&self) -> Self::Strategy {
        RecursiveIdHandleStrategy::new(self.hir_map)
    }
}
