#![feature(trait_upcasting)]

#[salsa::jar(db = HirDb)]
pub struct Jar(
    HirFile,
    lower_file,
);

pub trait HirDb: salsa::DbWithJar<Jar> + narxia_syn_db::SynDb {}

impl<DB> HirDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + narxia_syn_db::SynDb {}

#[salsa::tracked]
pub struct HirFile {
    #[id]
    pub file: narxia_syn_db::SynFile,
    #[return_ref]
    pub mod_def: narxia_hir::ModDef,
}

#[salsa::tracked]
pub fn lower_file(db: &dyn HirDb, file: narxia_syn_db::SynFile) -> HirFile {
    let mod_def = narxia_hir::lower_mod_def(file.tree(db).get_root());
    HirFile::new(db, file, mod_def)
}