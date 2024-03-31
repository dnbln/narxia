use std::cell::RefCell;
use std::ops::DerefMut;

use narxia_hir::hir_map::{HirElem, HirMap};
use narxia_hir::HirId;
use narxia_src_db::SrcFile;

use crate::def_id::DefId;

pub struct GlobalTyCtxt {
    def_ids: RefCell<Vec<HirId>>,
    pub hir_map: RefCell<HirMap>,
}

impl Default for GlobalTyCtxt {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalTyCtxt {
    pub fn new() -> Self {
        Self {
            def_ids: RefCell::new(Vec::new()),
            hir_map: RefCell::new(HirMap::new()),
        }
    }

    fn add_def_id(&self, target_hir: HirId) -> DefId {
        let mut rf = self.def_ids.borrow_mut();
        let idx = rf.len();
        rf.push(target_hir);
        DefId { idx }
    }

    fn lookup_def_id(&self, def_id: DefId) -> HirId {
        self.def_ids.borrow()[def_id.idx]
    }

    fn lookup_def(&self, def_id: DefId) -> HirElem {
        self.hir_map
            .borrow()
            .get(self.lookup_def_id(def_id))
            .clone()
    }

    pub fn hir_map_mut_ref(&self) -> std::cell::RefMut<HirMap> {
        self.hir_map.borrow_mut()
    }

    pub fn get_file_of(&self, hir_id: HirId) -> SrcFile {
        self.hir_map.borrow().get_file(hir_id)
    }
}

#[derive(Clone, Copy)]
pub struct TyCtxt<'tcx> {
    global_ctxt: &'tcx GlobalTyCtxt,
}

impl<'tcx> TyCtxt<'tcx> {
    pub fn new(global_ctxt: &'tcx GlobalTyCtxt) -> Self {
        Self { global_ctxt }
    }

    pub fn add_def_id(&self, target_hir: HirId) -> DefId {
        self.global_ctxt.add_def_id(target_hir)
    }

    pub fn hir_map(&self) -> std::cell::Ref<'_, HirMap> {
        self.global_ctxt.hir_map.borrow()
    }
}
