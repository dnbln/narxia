use narxia_hir_typechk::tyctxt::{GlobalTyCtxt, TyCtxt};
use narxia_src_db::SrcFile;

use crate::db::Database;

pub struct DriverCtx {
    pub db: Database,
}

impl DriverCtx {
    pub fn initialize() -> Self {
        Self {
            db: Database::default(),
        }
    }

    pub fn init_log(&self) {
        crate::init_log();
    }

    pub fn display_file(&self, file: SrcFile) -> crate::DisplayFile {
        crate::DisplayFile(&self.db, file)
    }

    #[track_caller]
    pub fn trace_file(&self, file: SrcFile) {
        narxia_log::t!("File contents:\n{}", self.display_file(file));
    }
}
