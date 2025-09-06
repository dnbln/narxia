use dir_structure::traits::vfs::fs_vfs::FsVfs;
use miette::bail;
use narxia_driver::HirDbg;
use narxia_test_runner::parser_tests::lower_to_hir;
use narxia_workspace::parser_tests::ParserTestSingleFolder;

fn run_test(mut test: ParserTestSingleFolder<FsVfs>) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let hir = lower_to_hir(&mut test, &ctx)?;

    let mod_def = hir.mod_def(&ctx.db);

    let hir_map = ctx.db.get_global_ty_ctxt().make_ty_ctxt().hir_map();

    let alloc_ids = hir_map.__get_allocated_hirids();
    if !alloc_ids.is_empty() {
        for hir_id in alloc_ids {
            eprintln!("Allocated and unused hir_id: {:?}", hir_id);
            eprintln!("At: {}", hir_id.span());
            eprintln!("In:");
            eprintln!("{}", hir_map.get_mod(mod_def).hir_dbg(&ctx));
        }

        bail!("Allocated and unused hir_ids");
    }

    Ok(())
}

fn run_test_main(test: ParserTestSingleFolder<'static, FsVfs>) -> miette::Result<()> {
    run_test(test)
}

include!(concat!(env!("OUT_DIR"), "/parser_tests.rs"));
