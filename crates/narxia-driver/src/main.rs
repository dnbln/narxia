use std::path::PathBuf;

use clap::Parser;
use miette::IntoDiagnostic;
use narxia_driver::ctxt::DriverCtx;
use narxia_driver::{db, HirDbg};
use narxia_hir::hir_map::HirMap;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
enum NarxiaDriverCommand {
    #[clap(name = "parse")]
    Parse(NarxiaDriverParseCommand),
    #[clap(name = "display-hir")]
    DisplayHir(NarxiaDriverDisplayHirCommand),
    #[clap(name = "display-hir-debug")]
    DisplayHirDebug(NarxiaDriverDisplayHirDebugCommand),
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverParseCommand {
    file: PathBuf,
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverDisplayHirCommand {
    file: PathBuf,
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverDisplayHirDebugCommand {
    file: PathBuf,
}

fn main() -> miette::Result<()> {
    let ctx = DriverCtx::initialize();
    ctx.init_log();

    let _span = narxia_log::span!(narxia_log::Level::INFO, "main").entered();

    let cmd = NarxiaDriverCommand::parse();
    match cmd {
        NarxiaDriverCommand::Parse(parse_cmd) => {
            narxia_log::i!("Parse command: {parse_cmd:?}");

            let file = parse_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
            println!("{:?}", tree.tree(&ctx.db));
        }
        NarxiaDriverCommand::DisplayHir(display_hir_cmd) => {
            narxia_log::i!("Display HIR command: {display_hir_cmd:?}");

            let file = display_hir_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
            ctx.db
                .get_global_ty_ctxt()
                .hir_map_mut_ref()
                .set_current_file(Some(file));
            let hir = narxia_hir_db::lower_file(&ctx.db, tree);
            ctx.db
                .get_global_ty_ctxt()
                .hir_map_mut_ref()
                .set_current_file(None);

            let hir_mod = hir.mod_def(&ctx.db);
            let hir_map = ctx.db.get_global_ty_ctxt().hir_map.borrow();

            println!("{}", hir_map.get_mod(hir_mod).hir_dbg(&ctx));
        }
        NarxiaDriverCommand::DisplayHirDebug(display_hir_cmd) => {
            narxia_log::i!("Display HIR debug command: {display_hir_cmd:?}");

            let file = display_hir_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
            let hir = narxia_hir_db::lower_file(&ctx.db, tree);

            println!("{:?}", hir.mod_def(&ctx.db).hir_dbg(&ctx));
        }
    }

    Ok(())
}
