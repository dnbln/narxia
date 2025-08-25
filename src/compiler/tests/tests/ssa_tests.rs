use dir_structure::FsVfs;
use miette::bail;
use miette::Context;
use miette::IntoDiagnostic;
use narxia_workspace::ssa_tests::SsaTestSingleFolder;

#[derive(Debug, Clone, Copy)]
enum TestMode {
    Overwrite,
    Compare,
}

impl TestMode {
    fn get_behavior() -> Self {
        match std::env::var("NARXIA_SSA_SNAPSHOTS_TEST_MODE").as_deref() {
            Ok("overwrite") => Self::Overwrite,
            Ok("compare") => Self::Compare,
            _ => Self::Compare,
        }
    }
}

fn trial(mut test: SsaTestSingleFolder<FsVfs>) -> miette::Result<()> {
    let test_mode = TestMode::get_behavior();
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let ssa_mod = narxia_test_runner::ssa_tests::ssa(&mut test, &ctx)?;

    let s = format!("{:?}", ssa_mod);

    match test_mode {
        TestMode::Overwrite => {
            std::fs::write(test.output_file_path(), s)
                .into_diagnostic()
                .context("Cannot write expected tree")?;
        }
        TestMode::Compare => {
            let expected = std::fs::read_to_string(test.output_file_path())
                .into_diagnostic()
                .context("Cannot read expected tree; Run with NARXIA_SSA_SNAPSHOTS_TEST_MODE=overwrite to update the expected output.")?;
            if s != expected {
                bail!(
                    "SSA does not match expected output.\nExpected:\n{}\nActual:\n{}\nRun with NARXIA_SSA_SNAPSHOTS_TEST_MODE=overwrite to update the expected output.\n",
                    expected,
                    s
                );
            }
        }
    }

    Ok(())
}

fn run_test_main(test: SsaTestSingleFolder<'static, FsVfs>) -> miette::Result<()> {
    trial(test)
}

include!(concat!(env!("OUT_DIR"), "/ssa_tests.rs"));
