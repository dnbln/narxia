#![feature(internal_output_capture)]

pub extern crate narxia_dir_structures;

pub mod parser_tests {
    use dir_structure::DirStructureItem;
    use miette::bail;
    use miette::IntoDiagnostic;
    use narxia_dir_structures::parser_tests::parser_tests_dir;
    use narxia_dir_structures::parser_tests::ParserTestSingleFolder;
    use narxia_driver::DriverCtx;
    use narxia_hir_db::HirFile;

    pub(crate) fn do_lower_to_hir(
        src_file: narxia_src_db::SrcFile,
        ctx: &DriverCtx,
    ) -> miette::Result<HirFile<'_>> {
        let file_map_entry = ctx.db.get_global_ty_ctxt().add_file_map_entry(src_file);
        ctx.trace_file(src_file);
        let (syn_file, errors) = narxia_driver::parse_file_with_diagnostics(ctx, src_file);
        if !errors.is_empty() {
            bail!("Errors: {errors:?}");
        }
        let hir = ctx.lower_file(file_map_entry, syn_file);

        Ok(hir)
    }

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
        do_lower_to_hir(src_file, ctx)
    }

    dir_structure::dir_children_wrapper!(pub ParserTestsFolder ParserTestSingleFolder);

    pub fn collect_parser_tests() -> miette::Result<ParserTestsFolder> {
        ParserTestsFolder::read(parser_tests_dir()).into_diagnostic()
    }
}

pub mod name_resolution_tests {
    use dir_structure::DirStructureItem;
    use miette::IntoDiagnostic;
    use narxia_data_structures::FxBTreeMap;
    use narxia_dir_structures::name_resolution_tests::name_resolution_tests_dir;
    use narxia_dir_structures::name_resolution_tests::NameResolutionTestSingleFolder;
    use narxia_driver::DriverCtx;
    use narxia_hir::HirId;
    use narxia_hir_typechk::def_id::DefId;
    use narxia_hir_typechk::sema;
    use narxia_hir_typechk::sema::SemanticAnalysisResult;

    use crate::parser_tests::do_lower_to_hir;

    dir_structure::dir_children_wrapper!(pub NameResolutionTestsFolder NameResolutionTestSingleFolder);

    pub fn name_resolution(
        folder: &mut NameResolutionTestSingleFolder,
        ctx: &DriverCtx,
    ) -> miette::Result<(SemanticAnalysisResult, FxBTreeMap<HirId, DefId>)> {
        let input = folder
            .input
            .perform_and_store_read()
            .into_diagnostic()?
            .clone();
        let src_file = narxia_driver::load_file(ctx, folder.input_file_path(), &input.0);
        let hir = do_lower_to_hir(src_file, ctx)?;

        let mod_id = hir.mod_def(&ctx.db);

        let tcx = ctx.db.get_global_ty_ctxt().make_ty_ctxt();
        let hir_map = tcx.hir_map();

        let analysis_results = sema::analyze_program_structure(tcx, mod_id);
        sema::resolve_work(tcx, mod_id, &analysis_results);

        Ok((
            analysis_results,
            ctx.db
                .get_global_ty_ctxt()
                .make_ty_ctxt()
                .__get_name_resolutions(),
        ))
    }

    pub fn collect_name_resolution_tests() -> miette::Result<NameResolutionTestsFolder> {
        NameResolutionTestsFolder::read(name_resolution_tests_dir()).into_diagnostic()
    }
}

pub mod ssa_tests {
    use dir_structure::DirStructureItem;
    use miette::IntoDiagnostic;
    use narxia_dir_structures::ssa_tests::ssa_tests_dir;
    use narxia_dir_structures::ssa_tests::SsaTestSingleFolder;
    use narxia_driver::DriverCtx;
    use narxia_hir_typechk::sema;
    use narxia_ssa::Module;
    use narxia_syn::narxia_log::info;

    use crate::parser_tests::do_lower_to_hir;

    dir_structure::dir_children_wrapper!(pub SsaTestsFolder SsaTestSingleFolder);

    pub fn ssa(folder: &mut SsaTestSingleFolder, ctx: &DriverCtx) -> miette::Result<Module> {
        let input = folder
            .input
            .perform_and_store_read()
            .into_diagnostic()?
            .clone();
        let src_file = narxia_driver::load_file(ctx, folder.input_file_path(), &input.0);
        let hir = do_lower_to_hir(src_file, ctx)?;

        let mod_id = hir.mod_def(&ctx.db);

        let tcx = ctx.db.get_global_ty_ctxt().make_ty_ctxt();
        let hir_map = tcx.hir_map();

        info!("Hir map updated");

        let analysis_results = sema::analyze_program_structure(tcx, mod_id);
        info!("Analysis results: {:?}", analysis_results);

        sema::resolve_work(tcx, mod_id, &analysis_results);

        Ok(narxia_ssa_lower::convert(tcx, &hir_map, mod_id))
    }

    pub fn collect_ssa_tests() -> miette::Result<SsaTestsFolder> {
        SsaTestsFolder::read(ssa_tests_dir()).into_diagnostic()
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
        fn __trial($name: $crate::narxia_dir_structures::parser_tests::ParserTestSingleFolder) -> Result<(), libtest_mimic::Failed> {
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

#[macro_export]
macro_rules! for_each_name_resolution_test {
    (|$name:ident| { $($do:tt)* }) => {
        for $name in $crate::name_resolution_tests::collect_name_resolution_tests()? {
            $($do)*
        }
    };
}

#[macro_export]
macro_rules! name_resolution_tests_trials {
    ($collector_fn:ident, $fn_to_call:expr) => {
        fn $collector_fn() -> miette::Result<Vec<libtest_mimic::Trial>> {
            let mut trials = Vec::new();

            $crate::for_each_name_resolution_test! {
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
macro_rules! test_main_name_resolution_tests_foreach {
    (|$name:ident| { $($do:tt)* }) => {
        fn __trial($name: $crate::narxia_dir_structures::name_resolution_tests::NameResolutionTestSingleFolder) -> Result<(), libtest_mimic::Failed> {
            {$($do)*}.map_err(libtest_mimic::Failed::from)
        }
        $crate::name_resolution_tests_trials!(__collect_trials, __trial);

        fn main() -> miette::Result<()> {
            let args = libtest_mimic::Arguments::from_args();
            let trials = __collect_trials()?;
            libtest_mimic::run(&args, trials).exit();
        }
    };
}

#[macro_export]
macro_rules! for_each_ssa_test {
    (|$name:ident| {$($do:tt)*}) => {
        for $name in $crate::ssa_tests::collect_ssa_tests()? {
            $($do)*
        }
    };
}

#[macro_export]
macro_rules! ssa_tests_trials {
    ($collector_fn:ident, $fn_to_call:expr) => {
        fn $collector_fn() -> miette::Result<Vec<libtest_mimic::Trial>> {
            let mut trials = Vec::new();

            $crate::for_each_ssa_test! {
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
macro_rules! test_main_ssa_tests_foreach {
    (|$name:ident| {$($do:tt)*}) => {
        fn __trial($name: $crate::narxia_dir_structures::ssa_tests::SsaTestSingleFolder) -> Result<(), libtest_mimic::Failed> {
            {$($do)*}.map_err(libtest_mimic::Failed::from)
        }

        $crate::ssa_tests_trials!(__collect_trials, __trial);

        fn main() -> miette::Result<()> {
            let args = libtest_mimic::Arguments::from_args();
            let trials = __collect_trials()?;
            libtest_mimic::run(&args, trials).exit();
        }
    };
}
