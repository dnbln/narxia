use std::collections::BTreeSet;

use dir_structure::vfs::fs_vfs::FsVfs;
use miette::bail;
use miette::Context;
use miette::IntoDiagnostic;
use narxia_workspace::name_resolution_tests::NameResolutionTestSingleFolder;

#[derive(Debug, Clone, Copy)]
enum TestMode {
    Overwrite,
    Compare,
}

impl TestMode {
    fn get_behavior() -> Self {
        match std::env::var("NARXIA_NAME_RESOLUTION_SNAPSHOTS_TEST_MODE").as_deref() {
            Ok("overwrite") => Self::Overwrite,
            Ok("compare") => Self::Compare,
            _ => Self::Compare,
        }
    }
}

fn format_resolution(
    tcx: narxia_hir_typechk::tyctxt::TyCtxt<'_>,
    id: narxia_hir::HirId,
    def_id: narxia_hir_typechk::def_id::DefId,
) -> String {
    format!("{id:?} -> {:?}", tcx.lookup_def_id(def_id))
}

fn run_test(mut test: NameResolutionTestSingleFolder<FsVfs>, mode: TestMode) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let (_sema_result, names) =
        narxia_test_runner::name_resolution_tests::name_resolution(&mut test, &ctx)?;

    let tcx = ctx.db.get_global_ty_ctxt().make_ty_ctxt();

    match mode {
        TestMode::Compare => {
            let names = names
                .into_iter()
                .map(|(id, def_id)| format_resolution(tcx, id, def_id))
                .collect::<BTreeSet<_>>();
            let out_names = std::fs::read_to_string(test.output_file_path())
                .into_diagnostic()
                .context("Cannot read expected names; Run with NARXIA_NAME_RESOLUTION_SNAPSHOTS_TEST_MODE=overwrite to update the expected output.")?
                .lines().filter(|s| !s.is_empty())
                .map(str::trim)
                .map(str::to_owned)
                .collect::<BTreeSet<_>>();
            if names != out_names {
                let msg = format!("Name resolutions do not match expected output.\nExpected:\n{}\nActual:\n{}\nRun with NARXIA_NAME_RESOLUTION_SNAPSHOTS_TEST_MODE=overwrite to update the expected output.\n",
                    out_names.iter().cloned().collect::<Vec<_>>().join("\n"),
                    names.iter().cloned().collect::<Vec<_>>().join("\n"));
                println!("{msg}");
                bail!("{msg}");
            }
        }
        TestMode::Overwrite => {
            let names = names
                .into_iter()
                .map(|(id, def_id)| format_resolution(tcx, id, def_id))
                .collect::<BTreeSet<_>>();
            std::fs::write(
                test.output_file_path(),
                names.iter().cloned().collect::<Vec<_>>().join("\n"),
            )
            .into_diagnostic()
            .context("Cannot write expected names")?;
        }
    }

    Ok(())
}

fn run_test_main(test: NameResolutionTestSingleFolder<'static, FsVfs>) -> miette::Result<()> {
    run_test(test, TestMode::get_behavior())
}

include!(concat!(env!("OUT_DIR"), "/name_resolution_tests.rs"));
