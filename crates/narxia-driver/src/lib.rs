use std::cell::RefCell;
use std::fmt::Formatter;
use std::path::PathBuf;
use std::{fmt, io};

use narxia_hir::hir_map::HirElem;
use narxia_hir::HirId;
use narxia_src_db::{FilePathInfo, SrcFile};
use narxia_syn::parse_error::ParseError;
use narxia_syn_db::SynFile;
use owo_colors::OwoColorize;

pub mod ctxt;
pub mod db;

pub use ctxt::DriverCtx;

pub struct DisplayFile<'a>(&'a db::Database, SrcFile);

impl<'a> fmt::Display for DisplayFile<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let text = self.1.get_text(self.0);

        write!(
            f,
            "{}\n{}\n{}",
            ">>>>".bright_blue(),
            text.bright_white().bold(),
            "<<<<".bright_blue(),
        )
    }
}

pub fn read_file(ctx: &DriverCtx, file: PathBuf) -> io::Result<SrcFile> {
    narxia_src_db::load_from_disk(&ctx.db, FilePathInfo::new_from_short(file))
}

pub fn parse_file(ctx: &DriverCtx, file: SrcFile) -> SynFile {
    parse_file_with_diagnostics(ctx, file).0
}

pub fn load_file(ctx: &DriverCtx, p: PathBuf, contents: &str) -> SrcFile {
    narxia_src_db::load_from_memory(&ctx.db, FilePathInfo::new_from_short(p), contents)
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

pub fn init_panic_hook() {
    human_panic::setup_panic!(human_panic::Metadata::new(
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION")
    )
    .authors("Dinu Blanovschi <git@dnbln.dev>")
    .homepage("dnbln.dev")
    .support("- Open a support request via a GitHub issue to https://github.com/dnbln/narxia"));
}

pub struct HirDebugImpl<'hir, 'ctxt, H> {
    hir: &'hir H,
    context: &'ctxt DriverCtx,
}

fn dbg_impl_code<H>(
    dbg_impl: &HirDebugImpl<H>,
    fmt: &mut fmt::Formatter,
    f: impl FnOnce(&H, &mut fmt::Formatter) -> fmt::Result,
) -> fmt::Result {
    let hir = dbg_impl.hir;
    let context = dbg_impl.context;

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

    fn debug_hir_id_get_src_file(hir_id: HirId) -> SrcFile {
        DRIVER_CTXT.with(|f| {
            let f = f.borrow();
            let ctx: &DriverCtx = unsafe { &**f };
            ctx.db.lookup_hir_id_file(hir_id)
        })
    }

    fn debug_hir_id_path_callback(src_file: SrcFile) -> String {
        DRIVER_CTXT.with(|f| {
            let f = f.borrow();
            let ctx: &DriverCtx = unsafe { &**f };
            format!("{}", src_file.get_presentable_path(&ctx.db).display())
        })
    }

    fn debug_file_contents_callback(file: SrcFile) -> String {
        DRIVER_CTXT.with(|f| {
            let f = f.borrow();
            let ctx: &DriverCtx = unsafe { &**f };
            file.get_text(&ctx.db)
        })
    }

    fn debug_get_hir_element(hir_id: HirId) -> HirElem {
        DRIVER_CTXT.with(|f| {
            let f = f.borrow();
            let ctx: &DriverCtx = unsafe { &**f };
            ctx.db
                .get_global_ty_ctxt()
                .make_ty_ctxt()
                .hir_map()
                .get(hir_id)
                .clone()
        })
    }

    narxia_hir::hir::dbg_hir(
        debug_hir_id_get_src_file,
        debug_hir_id_path_callback,
        debug_file_contents_callback,
        debug_get_hir_element,
        || f(hir, fmt),
    )
}

impl<'hir, 'ctxt, H: std::fmt::Debug> std::fmt::Debug for HirDebugImpl<'hir, 'ctxt, H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        dbg_impl_code(self, f, |hir, f| write!(f, "{:?}", hir))
    }
}

impl<'hir, 'ctxt, H: std::fmt::Display> std::fmt::Display for HirDebugImpl<'hir, 'ctxt, H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        dbg_impl_code(self, f, |hir, f| write!(f, "{}", hir))
    }
}

pub trait HirDbg {
    fn hir_dbg<'hir, 'ctxt>(
        &'hir self,
        context: &'ctxt DriverCtx,
    ) -> HirDebugImpl<'hir, 'ctxt, Self>
    where
        Self: std::fmt::Debug + Sized,
    {
        HirDebugImpl { hir: self, context }
    }
}

impl<T> HirDbg for T where T: std::fmt::Debug + Sized {}
