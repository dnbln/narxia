use narxia_hir::hir_arena::HirRefArena;

#[salsa::jar(db = HirDb)]
pub struct Jar(HirFile, lower_file);

pub trait HirDb: salsa::DbWithJar<Jar> + narxia_syn_db::SynDb {}

impl<DB> HirDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + narxia_syn_db::SynDb {}

#[salsa::tracked]
pub struct HirFile {
    #[id]
    pub file: narxia_syn_db::SynFile,
    #[return_ref]
    pub mod_def: narxia_hir::hir::ModDef,
}

#[salsa::tracked]
pub fn lower_file(db: &dyn HirDb, file: narxia_syn_db::SynFile) -> HirFile {
    let mod_def = narxia_hir::lower::lower_mod_def(
        &mut narxia_hir::lower::LowerCtxt {
            src_file: file.file(db),
        },
        file.tree(db).get_root(),
    );
    let hir_file = HirFile::new(db, file, mod_def);
    hir_file
}

pub fn hir_map<'hir>(db: &'hir dyn HirDb, file: HirFile) -> HirRefArena<'hir> {
    let mod_def = file.mod_def(db);
    let ref_arena = narxia_hir::build_ref_arena(file.file(db).file(db), mod_def);
    ref_arena
}
