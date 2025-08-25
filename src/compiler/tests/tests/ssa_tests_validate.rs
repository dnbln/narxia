use dir_structure::FsVfs;
use miette::bail;
use narxia_test_runner::ssa_tests;
use narxia_workspace::ssa_tests::SsaTestSingleFolder;

fn trial(mut test: SsaTestSingleFolder<FsVfs>) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let span = narxia_log::einfo_span!("ssa_tests_validate");
    let ssa_mod = ssa_tests::ssa(&mut test, &ctx)?;

    let result = narxia_ssa_validator::validate(&ssa_mod);

    let Err(result) = result else {
        return Ok(());
    };

    narxia_ssa_validator::present_validation_errors(&ssa_mod, &result);

    bail!("Validation errors");
}

fn run_test_main(test: SsaTestSingleFolder<'static, FsVfs>) -> miette::Result<()> {
    trial(test)
}

include!(concat!(env!("OUT_DIR"), "/ssa_tests.rs"));
