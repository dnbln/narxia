use std::cell::RefCell;
use std::rc::Rc;

use narxia_hir_typechk::tyctxt::GlobalTyCtxt;
use narxia_src_db::SrcFileDatabase;

#[salsa::db(narxia_src_db::Jar, narxia_syn_db::Jar, narxia_hir_db::Jar)]
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,

    src_file_db: Rc<RefCell<SrcFileDatabase>>,
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

impl narxia_src_db::Db for Database {
    fn src_file_db<'db>(&'db self) -> std::cell::Ref<'db, SrcFileDatabase> {
        self.src_file_db.borrow()
    }

    fn src_file_db_mut<'db>(&'db self) -> std::cell::RefMut<'db, SrcFileDatabase> {
        self.src_file_db.borrow_mut()
    }

    fn src_file_text<'db>(&'db self, span: narxia_src_db::Span) -> String {
        self.src_file_db.borrow().get_loaded_span(span).to_owned()
    }
}

impl narxia_hir_db::HirDb for Database {
    fn hir_map_mut_ref(&self) -> std::cell::RefMut<narxia_hir::hir_map::HirMap> {
        self.global_ty_ctxt.hir_map_mut_ref()
    }
}

impl salsa::Database for Database {
    fn salsa_event(&self, _event: salsa::Event) {}
}
