use std::ops;
use std::path::PathBuf;
use std::sync;
use std::sync::Arc;
use std::sync::RwLock;

use narxia_data_structures::FxBTreeMap;
use narxia_hir::HirId;
use narxia_hir::hir_map::FileMapEntry;
use narxia_hir::hir_map::HirElem;
use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::HirMapQ;
use narxia_src_db::SrcFile;

use crate::def_id::DefId;

struct DefMap {
    def_ids: Vec<HirId>,
}

impl DefMap {
    fn add_def_id(&mut self, target_hir: HirId) -> DefId {
        let idx = self.def_ids.len();
        self.def_ids.push(target_hir);
        DefId { idx }
    }

    fn lookup_def_id(&self, def_id: DefId) -> HirId {
        self.def_ids[def_id.idx]
    }

    fn lookup_hir_id_def(&self, hir_id: HirId) -> Option<DefId> {
        let idx = self.def_ids.iter().position(|&x| x == hir_id)?;
        Some(DefId { idx })
    }
}

pub struct GlobalTyCtxtInner {
    def_map: RwLock<DefMap>,
    pub hir_map: RwLock<HirMap>,
    name_resolution: RwLock<NameResolution>,
    src_files: RwLock<Vec<SrcFile>>,
}

pub struct NameResolution {
    name_map: FxBTreeMap<HirId, DefId>,
}

#[derive(Clone)]
pub struct GlobalTyCtxt {
    inner: Arc<GlobalTyCtxtInner>,
}

impl Default for GlobalTyCtxt {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalTyCtxt {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(GlobalTyCtxtInner {
                def_map: RwLock::new(DefMap {
                    def_ids: Vec::new(),
                }),
                hir_map: RwLock::new(HirMap::new()),
                name_resolution: RwLock::new(NameResolution {
                    name_map: FxBTreeMap::new(),
                }),
                src_files: RwLock::new(Vec::new()),
            }),
        }
    }

    pub fn make_ty_ctxt(&self) -> TyCtxt {
        TyCtxt::new(self)
    }

    fn add_def_id(&self, target_hir: HirId) -> DefId {
        let mut rf = self.inner.def_map.write().unwrap();
        rf.add_def_id(target_hir)
    }

    fn lookup_def_id(&self, def_id: DefId) -> HirId {
        self.inner.def_map.read().unwrap().lookup_def_id(def_id)
    }

    fn lookup_def(&self, def_id: DefId) -> HirElem {
        self.make_ty_ctxt()
            .hir_map()
            .get(self.lookup_def_id(def_id))
            .clone()
    }

    fn lookup_hir_id_def(&self, hir_id: HirId) -> Option<DefId> {
        let rf = self.inner.def_map.read().unwrap();
        rf.lookup_hir_id_def(hir_id)
    }

    pub fn hir_map_mut_ref(&self) -> sync::RwLockWriteGuard<HirMap> {
        self.inner.hir_map.write().unwrap()
    }

    pub fn get_file_of(&self, hir_id: HirId) -> FileMapEntry {
        self.make_ty_ctxt().hir_map().get_file(hir_id)
    }

    pub fn get_presentable_path_of_file(
        &self,
        db: &dyn narxia_src_db::SrcDb,
        file: FileMapEntry,
    ) -> PathBuf {
        let f = self.inner.src_files.read().unwrap()[file.get_id()];
        f.get_presentable_path(db)
    }

    pub fn get_file_text(&self, db: &dyn narxia_src_db::SrcDb, file: FileMapEntry) -> String {
        let f = self.inner.src_files.read().unwrap()[file.get_id()];
        f.get_text(db)
    }

    pub fn add_file_map_entry(&self, file: SrcFile) -> FileMapEntry {
        let mut rf = self.inner.src_files.write().unwrap();
        let idx = rf.len();
        rf.push(file);
        FileMapEntry::new(idx)
    }

    fn resolved_name(&self, id: HirId, def_id: DefId) {
        self.inner
            .name_resolution
            .write()
            .unwrap()
            .name_map
            .insert(id, def_id);
    }

    fn get_name_resolution(&self, id: HirId) -> DefId {
        self.inner
            .name_resolution
            .read()
            .unwrap()
            .name_map
            .get(&id)
            .copied()
            .unwrap()
    }

    fn dump_resolutions(&self) {
        use std::fmt::Write;
        let rf = self.inner.name_resolution.read().unwrap();
        let defmap = self.inner.def_map.read().unwrap();
        let mut s = String::new();
        for (id, def_id) in rf.name_map.iter() {
            writeln!(
                &mut s,
                "Resolved: {id:?} -> {def_id:?} @ {:?}",
                defmap.lookup_def_id(*def_id)
            )
            .unwrap();
        }
        narxia_log::info!("Name Resolutions:\n{s}");
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

    pub fn add_def_id(self, target_hir: HirId) -> DefId {
        self.global_ctxt.add_def_id(target_hir)
    }

    pub fn hir_map(self) -> GlobalHirMapRef<'tcx> {
        GlobalHirMapRef(self.global_ctxt.inner.hir_map.read().unwrap())
    }

    pub fn lookup_def_id(self, def_id: DefId) -> HirId {
        self.global_ctxt.lookup_def_id(def_id)
    }

    pub fn resolved_name(self, id: HirId, def_id: DefId) {
        self.global_ctxt.resolved_name(id, def_id);
    }

    pub fn get_name_resolution(self, id: HirId) -> DefId {
        self.global_ctxt.get_name_resolution(id)
    }

    pub fn lookup_hir_id_def(self, hir_id: HirId) -> Option<DefId> {
        self.global_ctxt.lookup_hir_id_def(hir_id)
    }

    pub fn dump_resolutions(self) {
        self.global_ctxt.dump_resolutions();
    }

    pub fn __get_name_resolutions(self) -> FxBTreeMap<HirId, DefId> {
        self.global_ctxt
            .inner
            .name_resolution
            .read()
            .unwrap()
            .name_map
            .clone()
    }
}

pub struct GlobalHirMapRef<'tcx>(sync::RwLockReadGuard<'tcx, HirMap>);

impl ops::Deref for GlobalHirMapRef<'_> {
    type Target = HirMap;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GlobalHirMapRef<'_> {
    pub fn get_hir_map(&self) -> &HirMap {
        &self.0
    }
}

impl<'tcx> HirMapQ<'tcx> for &'tcx GlobalHirMapRef<'tcx> {
    fn run_hir_map_query<T: 'tcx, Q: FnOnce(&'tcx HirMap) -> T>(&self, q: Q) -> T {
        q(&self.0)
    }
}
