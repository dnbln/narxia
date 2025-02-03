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

    #[track_caller]
    pub fn trace_file(&self, file: SrcFile) {
        narxia_log::t!("File contents:\n{}", self.display_file(file));
    }

    #[track_caller]
    pub fn trace_hir_file(&self, file: narxia_hir_db::HirFile) {
        self.trace_file(file.file(&self.db).file(&self.db));
    }
}
