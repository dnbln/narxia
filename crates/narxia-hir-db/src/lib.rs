#![feature(trait_upcasting)]


use narxia_hir::hir_map::HirMap;

#[salsa::db]
pub trait HirDb: salsa::Database + narxia_syn_db::SynDb {
    fn hir_map_mut_ref(&self) -> std::sync::RwLockWriteGuard<HirMap>;
}

#[salsa::tracked]
pub struct HirFile<'db> {
    #[id]
    pub file: narxia_syn_db::SynFile<'db>,
    pub mod_def: narxia_hir::hir::ModId,
}

#[salsa::tracked]
pub fn lower_file<'db>(db: &'db dyn HirDb, file: narxia_syn_db::SynFile<'db>) -> HirFile<'db> {
    let src_file = file.file(db);

    let mut hir_map = db.hir_map_mut_ref();
    let red = file.tree(db).red();

    let mod_id = narxia_hir::lower::lower_mod_def(
        &mut narxia_hir::lower::LowerCtxt {
            src_file,
            hir_map: &mut hir_map,
        },
        red.get_root(),
    );
    let hir_file = HirFile::new(db, file, mod_id);

    hir_file
}
