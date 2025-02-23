use miette::bail;
use narxia_dir_structures::ParserTestSingleFolder;
use narxia_driver::HirDbg;
use narxia_test_runner::parser_tests::lower_to_hir;

fn run_test(mut test: ParserTestSingleFolder) -> miette::Result<()> {
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

narxia_test_runner::test_main_parser_tests_foreach! {
    |test| { run_test(test) }
}
