use std::io;
use std::sync;

use narxia_hir::hir_map;
use narxia_hir_typechk::tyctxt::GlobalTyCtxt;
use narxia_src_db::FilePathInfo;
use narxia_src_db::Span;
use narxia_src_db::SrcFileDatabase;

#[salsa::db]
#[derive(Default, Clone)]
pub struct Database {
    storage: salsa::Storage<Self>,

    src_file_db: SrcFileDatabase,
    global_ty_ctxt: GlobalTyCtxt,
}

impl Database {
    pub(crate) fn lookup_hir_id_file(&self, hir_id: narxia_hir::HirId) -> narxia_src_db::SrcFile {
        self.global_ty_ctxt.get_file_of(hir_id)
    }

    pub fn get_global_ty_ctxt(&self) -> &GlobalTyCtxt {
        &self.global_ty_ctxt
    }
}

#[salsa::db]
impl narxia_src_db::SrcDb for Database {
    fn src_file_text(&self, span: narxia_src_db::Span) -> String {
        self.src_file_db.get_loaded_span(span)
    }

    fn src_file_path(&self, span: Span) -> FilePathInfo {
        self.src_file_db.get_file_path(span)
    }

    fn src_load_file(&self, path: FilePathInfo) -> io::Result<Span> {
        self.src_file_db.load_file(path)
    }

    fn src_load_file_inmemory(&self, path: FilePathInfo, text: &str) -> Span {
        self.src_file_db.load_file_from_memory(path, text)
    }
}

#[salsa::db]
impl narxia_hir_db::HirDb for Database {
    fn hir_map_mut_ref(&self) -> sync::RwLockWriteGuard<hir_map::HirMap> {
        self.global_ty_ctxt.hir_map_mut_ref()
    }
}

#[salsa::db]
impl salsa::Database for Database {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}
