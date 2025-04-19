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

    for (error_fn_ref, errors) in result {
        let error_fn = ssa_mod
            .functions
            .iter()
            .find(|it| it.fn_id == error_fn_ref)
            .unwrap();
        narxia_log::error!("Errors in function:\n{:?}", error_fn);

        for error in errors {
            narxia_log::error!("Error:\n{:?}", error);
        }
    }

    bail!("Validation errors");
}

narxia_test_runner::test_main_ssa_tests_foreach!(|test| { trial(test) });
