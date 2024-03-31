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
}

impl<'tcx, 'hir: 'tcx> HirVisitor<'hir> for Visitor<'tcx> {
    type Strategy = RecursiveIdHandleStrategy<'hir>;

    fn get_strategy(&self) -> Self::Strategy {
        RecursiveIdHandleStrategy::new(tcx.hir_map())
    }
}
