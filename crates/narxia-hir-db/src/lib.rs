#![feature(trait_upcasting)]

use std::ops::DerefMut;

use narxia_hir::hir::ModDef;
use narxia_hir::hir_map::{HirElem, HirMap};
use narxia_hir::HirId;

#[salsa::jar(db = HirDb)]
pub struct Jar(HirFile, lower_file);

pub trait HirDb: salsa::DbWithJar<Jar> + narxia_syn_db::SynDb {
    fn hir_map_mut_ref(&self) -> std::cell::RefMut<HirMap>;
}

#[salsa::tracked]
pub struct HirFile {
    #[id]
    pub file: narxia_syn_db::SynFile,
    pub mod_def: narxia_hir::hir::ModId,
}

#[salsa::tracked]
pub fn lower_file(db: &dyn HirDb, file: narxia_syn_db::SynFile) -> HirFile {
    let src_file = file.file(db);

    let mut hir_map = db.hir_map_mut_ref();

    let mod_id = narxia_hir::lower::lower_mod_def(
        &mut narxia_hir::lower::LowerCtxt {
            src_file,
            hir_map: &mut hir_map,
        },
        file.tree(db).get_root(),
    );
    let hir_file = HirFile::new(db, file, mod_id);

    hir_file
}
