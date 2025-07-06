use narxia_hir::hir_map::FileMapEntry;
use narxia_hir::hir_map::{self};
use narxia_src_db::SrcFile;

use crate::db::Database;

pub struct DriverCtx {
    pub db: Database,
}

impl DriverCtx {
    pub fn initialize() -> Self {
        let cx = Self {
            db: Database::default(),
        };

        narxia_log_impl::init();

        cx
    }

    pub fn initialize_in_test() -> Self {
        let cx = Self {
            db: Database::default(),
        };

        let _ = narxia_log_impl::try_init();

        cx
    }

    pub fn display_file(&self, file: SrcFile) -> crate::DisplayFile {
        crate::DisplayFile(&self.db, file)
    }

    pub fn set_current_file(&self, file: impl Into<Option<FileMapEntry>>) {
        self.db
            .get_global_ty_ctxt()
            .hir_map_mut_ref()
            .set_current_file(file.into());
    }

    pub fn lower_file<'a>(
        &'a self,
        file: FileMapEntry,
        tree: narxia_syn_db::SynFile<'a>,
    ) -> narxia_hir_db::HirFile<'a> {
        self.set_current_file(file);
        let hir_file = narxia_hir_db::lower_file(&self.db, tree);
        self.set_current_file(None);

        let hir_mod = hir_file.mod_def(&self.db);

        hir_map::hir_map_update_parents_in_mod(
            &mut self.db.get_global_ty_ctxt().hir_map_mut_ref(),
            hir_mod,
        );

        hir_file
    }

    #[track_caller]
    pub fn trace_file(&self, file: SrcFile) {
        narxia_log::t!("File contents:\n{}", self.display_file(file));
    }

    #[track_caller]
    pub fn trace_hir_file(&self, file: narxia_hir_db::HirFile) {
        self.trace_file(file.file(&self.db).file(&self.db));
    }
}
