use miette::bail;
use narxia_dir_structures::ssa_tests::SsaTestSingleFolder;
use narxia_log::Level;
use narxia_test_runner::ssa_tests;

fn trial(mut test: SsaTestSingleFolder) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let _span = narxia_log::span!(Level::INFO, "ssa_tests_validate");
    let _span_guard = _span.enter();
    let ssa_mod = ssa_tests::ssa(&mut test, &ctx)?;

    let result = narxia_ssa_validator::validate(&ssa_mod);

    let Err(result) = result else {
        return Ok(());
    };

    narxia_ssa_validator::present_validation_errors(&ssa_mod, &result);

    bail!("Validation errors");
}

narxia_test_runner::test_main_ssa_tests_foreach!(|test| { trial(test) });
