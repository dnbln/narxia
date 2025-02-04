#![feature(internal_output_capture)]

pub extern crate narxia_dir_structures;

use std::path::Path;
use std::sync::{Arc, Mutex};

use libtest_mimic::Failed;

pub fn run_trial(
    nocapture: bool,
    trial: impl FnOnce() -> Result<(), Failed>,
) -> Result<(), Failed> {
    let data = Arc::new(Mutex::new(Vec::new()));

    if !nocapture {
        std::io::set_output_capture(Some(data.clone()));
    }

    let result = trial();

    if !nocapture {
        std::io::set_output_capture(None);
    }

    if !nocapture && result.is_err() {
        let data = data.lock().unwrap();
        let data = String::from_utf8_lossy(&data);
        eprintln!("{}", data);
    }

    result
}

pub mod parser_tests {
    use dir_structure::DirStructureItem;
    use miette::{bail, IntoDiagnostic};
    use narxia_dir_structures::{parser_tests_dir, ParserTestSingleFolder};
    use narxia_driver::DriverCtx;
    use narxia_hir_db::HirFile;

    pub fn lower_to_hir<'db>(
        folder: &mut ParserTestSingleFolder,
        ctx: &'db DriverCtx,
    ) -> miette::Result<HirFile<'db>> {
        let input = folder
            .input
            .perform_and_store_read()
            .into_diagnostic()?
            .clone();
        let src_file = narxia_driver::load_file(ctx, folder.input_file_path(), &input.0);
        ctx.trace_file(src_file);
        let (syn_file, errors) = narxia_driver::parse_file_with_diagnostics(ctx, src_file);
        if !errors.is_empty() {
            bail!("Errors: {errors:?}");
        }
        ctx.db
            .get_global_ty_ctxt()
            .hir_map_mut_ref()
            .set_current_file(Some(src_file));
        let hir = narxia_hir_db::lower_file(&ctx.db, syn_file);
        ctx.db
            .get_global_ty_ctxt()
            .hir_map_mut_ref()
            .set_current_file(None);

        let mod_id = hir.mod_def(&ctx.db);

        narxia_hir::hir_map::hir_map_update_parents_in_mod(
            &mut *ctx.db.get_global_ty_ctxt().hir_map_mut_ref(),
            mod_id,
        );

        Ok(hir)
    }

    dir_structure::dir_children_wrapper!(pub ParserTestsFolder ParserTestSingleFolder);

    pub fn collect_parser_tests() -> miette::Result<ParserTestsFolder> {
        ParserTestsFolder::read(&parser_tests_dir()).into_diagnostic()
    }
}

#[macro_export]
macro_rules! for_each_parser_test {
    (|$name:ident| { $($do:tt)* }) => {
        for $name in $crate::parser_tests::collect_parser_tests()? {
            $($do)*
        }
    };
}

#[macro_export]
macro_rules! parser_test_trials {
    ($collector_fn:ident, $fn_to_call:expr) => {
        fn $collector_fn() -> miette::Result<Vec<libtest_mimic::Trial>> {
            let mut trials = Vec::new();

            $crate::for_each_parser_test! {
                |test| {
                    let name = test.file_name().clone().into_string().unwrap();
                    let folder = test.value().clone();
                    trials.push(libtest_mimic::Trial::test(name, move || $fn_to_call(folder)));
                }
            }

            Ok(trials)
        }
    };
}

#[macro_export]
macro_rules! test_main_parser_tests_foreach {
    (|$name:ident| { $($do:tt)* }) => {
        fn __trial($name: $crate::narxia_dir_structures::ParserTestSingleFolder) -> Result<(), libtest_mimic::Failed> {
            {$($do)*}.map_err(libtest_mimic::Failed::from)
        }

        $crate::parser_test_trials!(__collect_trials, __trial);

        fn main() -> miette::Result<()> {
            let args = libtest_mimic::Arguments::from_args();
            let trials = __collect_trials()?;
            libtest_mimic::run(&args, trials).exit();
        }
    };
}
