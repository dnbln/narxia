use std::path::PathBuf;

use clap::Parser;
use miette::IntoDiagnostic;
use narxia_driver::{DriverCtx, HirDbg};
use narxia_hir::hir_arena::HirRefArena;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
enum NarxiaDriverCommand {
    #[clap(name = "parse")]
    Parse(NarxiaDriverParseCommand),
    #[clap(name = "display-hir")]
    DisplayHir(NarxiaDriverDisplayHirCommand),
    #[clap(name = "display-hir-debug")]
    DisplayHirDebug(NarxiaDriverDisplayHirDebugCommand),
    DisplayTyBounds(NarxiaDriverDisplayTyBoundsCommand),
    DisplayNameResolution(NarxiaDriverDisplayNameResolutionCommand),
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

#[derive(Parser, Debug)]
pub struct NarxiaDriverDisplayTyBoundsCommand {
    file: PathBuf,
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverDisplayNameResolutionCommand {
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
            let hir = narxia_hir_db::lower_file(&ctx.db, tree);

            println!("{}", hir.mod_def(&ctx.db).hir_dbg(&ctx));
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
        NarxiaDriverCommand::DisplayTyBounds(display_ty_bounds_cmd) => {
            narxia_log::i!("Display type bounds command: {display_ty_bounds_cmd:?}");

            let file = display_ty_bounds_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
            let hir = narxia_hir_db::lower_file(&ctx.db, tree);

            let mod_def = hir.mod_def(&ctx.db);

            let tbounds = narxia_hir_typechk::ty_bounds::collect_ty_bounds(mod_def);
            let mut hir_ref_arena = HirRefArena::new(file);
            narxia_hir::build_refs_to_arena(&mut hir_ref_arena, mod_def);

            for bound in &tbounds.bounds {
                println!("{:?}", bound.hir_dbg(&ctx));

                let hir_ref = hir_ref_arena.get(bound.hir_id);

                println!("Bound from:\n{}", hir_ref.hir_dbg(&ctx));
            }
        }
        NarxiaDriverCommand::DisplayNameResolution(display_name_resolution_cmd) => {
            narxia_log::i!("Display name resolution command: {display_name_resolution_cmd:?}");

            let file = display_name_resolution_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
            let hir = narxia_hir_db::lower_file(&ctx.db, tree);

            let mod_def = hir.mod_def(&ctx.db);

            let mut hir_ref_arena = HirRefArena::new(file);
            narxia_hir::build_refs_to_arena(&mut hir_ref_arena, mod_def);

            let name_ref_context = narxia_hir_typechk::do_name_resolution(&mod_def);

            for (name, place) in name_ref_context.iter() {
                println!("Name: {:?} -> {:?}", name.hir_dbg(&ctx), place.hir_dbg(&ctx));

                let hir_ref = hir_ref_arena.get(place.place_base.hir_id);

                println!("Place from:\n{}", hir_ref.hir_dbg(&ctx));
            }
        }
    }

    Ok(())
}
