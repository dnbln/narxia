use libtest_mimic::{Arguments, Failed, Trial};
use miette::{bail, IntoDiagnostic};
use narxia_hir::hir::HirStructIdCheckTest;
use narxia_test_runner::parser_tests::ParserTestSingleFolder;

fn run_test_impl(p: ParserTestSingleFolder) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize();
    let input = p.input.perform_read().into_diagnostic()?;
    let src_file = narxia_driver::load_file(&ctx, p.input_file_path(), input.0);
    let (syn_file, errors) = narxia_driver::parse_file_with_diagnostics(&ctx, src_file);
    if !errors.is_empty() {
        bail!("Errors: {errors:?}");
    }
    let hir = narxia_hir_db::lower_file(&ctx.db, syn_file);
    let mod_def = hir.mod_def(&ctx.db);

    match mod_def.check_hir_id("<root>") {
        Ok(()) => {}
        Err(e) => {
            bail!("Dummy in {}", e.0);
        }
    }

    Ok(())
}

fn run_test(p: ParserTestSingleFolder) -> Result<(), Failed> {
    run_test_impl(p).map_err(Failed::from)
}

fn collect_trials() -> miette::Result<Vec<Trial>> {
    let mut trials = Vec::new();
    narxia_test_runner::for_each_parser_test! {
        |test| {
            let name = test.file_name().clone().into_string().unwrap();
            let folder = test.value().clone();
            let test =
                libtest_mimic::Trial::test(name, move || run_test(folder))
                    .with_ignored_flag(cfg!(not(hir_id_span)));
            trials.push(test);
        }
    }
    Ok(trials)
}

fn main() -> miette::Result<()> {
    let args = Arguments::from_args();
    let trails = collect_trials()?;

    libtest_mimic::run(&args, trails).exit();
}
