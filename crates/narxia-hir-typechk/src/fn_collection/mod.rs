use core::fmt;

use narxia_hir::hir::{self, HirIdNewtype, ModId};
use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::HirVisitor;

use crate::def_id::DefId;
use crate::tyctxt::TyCtxt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDef<'hir> {
    pub hir: &'hir hir::FnDef,
    pub def_id: DefId,
}

impl fmt::Display for FnDef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <hir::FnDef as fmt::Display>::fmt(self.hir, f)
    }
}

struct Visitor<'tcx> {
    fn_defs: Vec<FnDef<'tcx>>,
    tcx: TyCtxt<'tcx>,
    hir_map: &'tcx HirMap,
}

impl<'tcx> HirVisitor<'tcx> for Visitor<'tcx> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'tcx HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    fn visit_fn_def(&mut self, fn_id: hir::FnId, fn_def: &'tcx hir::FnDef) {
        let def_id = self.tcx.add_def_id(fn_id.hir_id());
        self.fn_defs.push(FnDef {
            hir: fn_def,
            def_id,
        });
    }
}

pub fn collect_fns<'tcx>(
    tcx: TyCtxt<'tcx>,
    hir_map: &'tcx HirMap,
    module: ModId,
) -> Vec<FnDef<'tcx>> {
    let mod_def = hir_map.get_mod(module);
    let mut visitor = Visitor {
        fn_defs: Vec::new(),
        hir_map,
        tcx,
    };
    visitor.visit_mod_def(module, mod_def);
    visitor.fn_defs
}
