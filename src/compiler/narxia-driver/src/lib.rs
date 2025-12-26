use core::ptr;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Formatter;
use std::io;
use std::path::PathBuf;

use hir::HirId;
use hir::hir_map::HirElem;
use narxia_hir as hir;
use narxia_hir::hir_map::FileMapEntry;
use narxia_src_db_impl::FilePathInfo;
use narxia_src_db::SrcFile;
use narxia_syn::parse_error::ParseError;
use narxia_syn_db::SynFile;
use owo_colors::OwoColorize;

pub mod ctxt;
pub mod db;

pub use ctxt::DriverCtx;

pub struct DisplayFile<'a>(&'a db::Database, SrcFile);

impl fmt::Display for DisplayFile<'_> {
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

pub fn parse_file(ctx: &DriverCtx, file: SrcFile) -> SynFile<'_> {
    parse_file_with_diagnostics(ctx, file).0
}

pub fn load_file(ctx: &DriverCtx, p: PathBuf, contents: &str) -> SrcFile {
    narxia_src_db::load_from_memory(&ctx.db, FilePathInfo::new_from_short(p), contents)
}

pub fn parse_file_with_diagnostics(
    ctx: &DriverCtx,
    file: SrcFile,
) -> (SynFile<'_>, Vec<ParseError>) {
    let syn_file = narxia_syn_db::parse_file(&ctx.db, file);
    let errors = narxia_syn_db::ParsingErrors::get(&ctx.db, file);
    (syn_file, errors.unwrap_or_default())
}

pub fn parse_file_and_assert_no_errors(ctx: &DriverCtx, file: SrcFile) -> SynFile<'_> {
    narxia_syn_db::parse_file_and_assert_no_errors(&ctx.db, file)
}

pub fn parse_file_at_path_and_assert_no_errors(ctx: &DriverCtx, path: PathBuf) -> SynFile<'_> {
    let file = read_file(ctx, path).unwrap();
    parse_file_and_assert_no_errors(ctx, file)
}

pub fn init_panic_hook() {
    human_panic::setup_panic!(
        human_panic::Metadata::new(env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))
            .authors("Dinu Blanovschi <git@dnbln.dev>")
            .homepage("https://nrx.dnbln.dev/")
            .support(
                "- Open a support request via a GitHub issue to https://github.com/dnbln/narxia"
            )
    );
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
        static DRIVER_CTXT: RefCell<*const DriverCtx> = const { RefCell::new(ptr::null()) };
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
                *f.borrow_mut() = ptr::null();
            });
        }
    }

    let _guard = ContextResetGuard;

    fn debug_hir_id_get_src_file(hir_id: HirId) -> FileMapEntry {
        DRIVER_CTXT.with(|f| {
            let f = f.borrow();
            #[expect(unsafe_code)]
            let ctx: &DriverCtx = unsafe { &**f };
            ctx.db.lookup_hir_id_file(hir_id)
        })
    }

    fn debug_hir_id_path_callback(src_file: FileMapEntry) -> String {
        DRIVER_CTXT.with(|f| {
            let f = f.borrow();
            #[expect(unsafe_code)]
            let ctx: &DriverCtx = unsafe { &**f };
            format!(
                "{}",
                ctx.db.get_presentable_path_of_file(src_file).display()
            )
        })
    }

    fn debug_file_contents_callback(file: FileMapEntry) -> String {
        DRIVER_CTXT.with(|f| {
            let f = f.borrow();
            #[expect(unsafe_code)]
            let ctx: &DriverCtx = unsafe { &**f };
            ctx.db.get_file_text(file)
        })
    }

    fn debug_get_hir_element(hir_id: HirId) -> HirElem {
        DRIVER_CTXT.with(|f| {
            let f = f.borrow();
            #[expect(unsafe_code)]
            let ctx: &DriverCtx = unsafe { &**f };
            ctx.db
                .get_global_ty_ctxt()
                .make_ty_ctxt()
                .hir_map()
                .get(hir_id)
                .clone()
        })
    }

    hir::dbg_hir(
        debug_hir_id_get_src_file,
        debug_hir_id_path_callback,
        debug_file_contents_callback,
        debug_get_hir_element,
        || f(hir, fmt),
    )
}

impl<H: fmt::Debug> fmt::Debug for HirDebugImpl<'_, '_, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        dbg_impl_code(self, f, |hir, f| write!(f, "{hir:?}"))
    }
}

impl<H: fmt::Display> fmt::Display for HirDebugImpl<'_, '_, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        dbg_impl_code(self, f, |hir, f| write!(f, "{hir}"))
    }
}

pub trait HirDbg {
    fn hir_dbg<'hir, 'ctxt>(
        &'hir self,
        context: &'ctxt DriverCtx,
    ) -> HirDebugImpl<'hir, 'ctxt, Self>
    where
        Self: fmt::Debug + Sized,
    {
        HirDebugImpl { hir: self, context }
    }
}

impl<T> HirDbg for T where T: fmt::Debug + Sized {}
