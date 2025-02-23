#![feature(trait_upcasting)]

use std::sync;

use narxia_hir::hir;
use narxia_hir::hir_map::HirMap;
use narxia_hir::lower;

#[salsa::db]
pub trait HirDb: salsa::Database + narxia_syn_db::SynDb {
    fn hir_map_mut_ref(&self) -> sync::RwLockWriteGuard<HirMap>;
}

#[salsa::tracked]
pub struct HirFile<'db> {
    #[id]
    pub file: narxia_syn_db::SynFile<'db>,
    pub mod_def: hir::ModId,
}

#[salsa::tracked]
pub fn lower_file<'db>(db: &'db dyn HirDb, file: narxia_syn_db::SynFile<'db>) -> HirFile<'db> {
    let src_file = file.file(db);

    let mut hir_map = db.hir_map_mut_ref();
    let red = file.tree(db).red();

    let mod_id = lower::lower_mod_def(
        &mut lower::LowerCtxt {
            src_file,
            hir_map: &mut hir_map,
        },
        red.get_root(),
    );
    let hir_file = HirFile::new(db, file, mod_id);

    hir_file
}
