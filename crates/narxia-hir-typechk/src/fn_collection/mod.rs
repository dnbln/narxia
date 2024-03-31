use narxia_hir::{hir::{self, Block, BlockId, FnParam, FnRetTy, Ident}, hir_map::HirMap, visitor::{HirVisitor, RecursiveIdHandleStrategy}};

use crate::{def_id::DefId, tyctxt::TyCtxt};

pub struct FnDef<'hir> {
    pub name: &'hir Ident,
    pub params: &'hir [FnParam],
    pub ret_ty: Option<&'hir FnRetTy>,
    pub body: BlockId,
    pub def_id: DefId,
}

struct Visitor<'tcx> {
    fn_defs: Vec<FnDef<'tcx>>,
    tcx: TyCtxt<'tcx>,
    hir_map: &'tcx HirMap,
}

impl<'tcx> HirVisitor<'tcx> for Visitor<'tcx> {
    type Strategy = RecursiveIdHandleStrategy<'tcx>;

    fn get_strategy(&self) -> Self::Strategy {
        RecursiveIdHandleStrategy::new(self.hir_map)
    }

    fn visit_fn_def(&mut self, fn_id: hir::FnId, fn_def: &'tcx hir::FnDef) {
        let def_id = self.tcx.add_def_id(fn_id.0);
        self.fn_defs.push(FnDef {
            name: &fn_def.name,
            params: &fn_def.params,
            ret_ty: fn_def.ret_ty.as_ref(),
            body: fn_def.body,
            def_id,
        });
    }
}