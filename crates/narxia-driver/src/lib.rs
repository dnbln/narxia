use std::cell::RefCell;
use std::fmt::Formatter;
use std::path::PathBuf;
use std::rc::Rc;
use std::{fmt, io};

use colored::Colorize;
use narxia_hir::HirId;
use narxia_src_db::{SrcFile, SrcFileDatabase};
use narxia_syn::parse_error::ParseError;
use narxia_syn_db::SynFile;

#[salsa::db(narxia_src_db::Jar, narxia_syn_db::Jar, narxia_hir_db::Jar)]
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,

    src_file_db: Rc<RefCell<SrcFileDatabase>>,
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

impl salsa::Database for Database {
    fn salsa_event(&self, _event: salsa::Event) {}
}

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
        init_log();
    }

    pub fn display_file(&self, file: SrcFile) -> DisplayFile {
        DisplayFile(&self.db, file)
    }

    #[track_caller]
    pub fn trace_file(&self, file: SrcFile) {
        narxia_log::t!("File contents:\n{}", self.display_file(file));
    }
}

pub struct DisplayFile<'a>(&'a Database, SrcFile);

impl<'a> fmt::Display for DisplayFile<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.1.with_text(self.0, |text| {
            write!(
                f,
                "{}\n{}\n{}",
                ">>>>".bright_blue(),
                text.bright_white().bold(),
                "<<<<".bright_blue(),
            )
        })
    }
}

pub fn read_file(ctx: &DriverCtx, file: PathBuf) -> io::Result<SrcFile> {
    narxia_src_db::load_from_disk(&ctx.db, file)
}

pub fn parse_file(ctx: &DriverCtx, file: SrcFile) -> SynFile {
    parse_file_with_diagnostics(ctx, file).0
}

pub fn load_file(ctx: &DriverCtx, p: PathBuf, contents: String) -> SrcFile {
    narxia_src_db::load_from_memory(&ctx.db, p, contents)
}

pub fn parse_file_with_diagnostics(ctx: &DriverCtx, file: SrcFile) -> (SynFile, Vec<ParseError>) {
    let syn_file = narxia_syn_db::parse_file(&ctx.db, file);
    let errors = narxia_syn_db::ParsingErrors::get(&ctx.db, file);
    (syn_file, errors.unwrap_or_default())
}

pub fn parse_file_and_assert_no_errors(ctx: &DriverCtx, file: SrcFile) -> SynFile {
    narxia_syn_db::parse_file_and_assert_no_errors(&ctx.db, file)
}

pub fn parse_file_at_path_and_assert_no_errors(ctx: &DriverCtx, path: PathBuf) -> SynFile {
    let file = read_file(ctx, path).unwrap();
    parse_file_and_assert_no_errors(ctx, file)
}

pub fn lower_syn_file(ctx: &DriverCtx, file: SynFile) -> narxia_hir_db::HirFile {
    narxia_hir_db::lower_file(&ctx.db, file)
}

pub fn init_log() {
    narxia_log_impl::init();
}

pub struct HirDebugImpl<'hir, 'ctxt, H: std::fmt::Debug> {
    hir: &'hir H,
    context: &'ctxt DriverCtx,
}

impl<'hir, 'ctxt, H: std::fmt::Debug> std::fmt::Debug for HirDebugImpl<'hir, 'ctxt, H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let hir = &self.hir;
        let context = self.context;

        thread_local! {
            static DRIVER_CTXT: RefCell<*const DriverCtx> = RefCell::new(core::ptr::null());
        }
    
        DRIVER_CTXT.with(|f| {
            if !f.borrow().is_null() {
                panic!("Driver context already set");
            }

            *f.borrow_mut() = context as *const DriverCtx;
        });

        struct ContextResetGuard;

        impl Drop for ContextResetGuard {
            fn drop(&mut self) {
                DRIVER_CTXT.with(|f| {
                    *f.borrow_mut() = core::ptr::null();
                });
            }
        }

        let _guard = ContextResetGuard;
    
        fn debug_hir_id_path_callback(hir_id: HirId) -> String {
            DRIVER_CTXT.with(|f| {
                let f = f.borrow();
                let ctx = unsafe { &**f };
                let db = &ctx.db;
                format!("{}", hir_id.src_file().get_presentable_path(db).display())
            })
        }
    
        narxia_hir::hir::dbg_hir(debug_hir_id_path_callback, || {
            write!(f, "{:?}", hir)
        })
    }
}

pub trait HirDbg {
    fn hir_dbg<'hir, 'ctxt>(&'hir self, context: &'ctxt DriverCtx) -> HirDebugImpl<'hir, 'ctxt, Self> where Self: std::fmt::Debug + Sized {
        HirDebugImpl {
            hir: self,
            context,
        }
    }
}

impl<T> HirDbg for T where T: std::fmt::Debug + Sized {}
