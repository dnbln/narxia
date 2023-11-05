// Tests for the parser.
//
// This file is responsible for running the parser tests.
//
// The parser tests are located in the `tests/parser-tests` directory.
// Each of them is a directory with an `input.nrx` file and an `output.txt` file.
// The `input.nrx` file contains the source code to parse, and the `output.txt` file
// contains a pretty-printed version of the resulting parse tree.
//
// The parser tests are run in two modes:
// - Overwrite: The parser will overwrite the `output.txt` file with new the pretty-printed
//   version of the parse tree.
// - Compare: The parser will compare the pretty-printed version of the parse tree with the
//   contents of the `output.txt` file. If they don't match, the test fails.

use libtest_mimic::{Arguments, Failed, Trial};
use miette::{bail, Context, IntoDiagnostic};
use narxia_syn::syntree::TreePresenterStyle;
use narxia_test_runner::parser_tests::ParserTestSingleFolder;

#[derive(Debug, Clone, Copy)]
enum TestMode {
    Overwrite,
    Compare,
}

impl TestMode {
    fn get_behavior() -> Self {
        match std::env::var("NARXIA_TEST_MODE").as_deref() {
            Ok("overwrite") => Self::Overwrite,
            Ok("compare") => Self::Compare,
            _ => Self::Compare,
        }
    }
}

fn run_test_impl(test: ParserTestSingleFolder, test_mode: TestMode) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize();
    let input = test.input.perform_read().into_diagnostic()?;
    let src_file = narxia_driver::load_file(&ctx, test.input_file_path(), input.0);
    let (syn_file, errors) = narxia_driver::parse_file_with_diagnostics(&ctx, src_file);
    if !errors.is_empty() {
        bail!("Errors: {errors:?}");
    }
    let tree = syn_file.tree(&ctx.db);

    let tree_str = tree.present_with_style(TreePresenterStyle::plain(), |p| format!("{p:?}"));
    let tree_fancy_str = format!("{:?}", tree);

    match test_mode {
        TestMode::Overwrite => {
            std::fs::write(test.output_file_path(), tree_str)
                .into_diagnostic()
                .context("Cannot write expected tree")?;
        }
        TestMode::Compare => {
            let expected = std::fs::read_to_string(test.output_file_path())
                .into_diagnostic()
                .context("Cannot read expected tree")?;
            if tree_str != expected {
                bail!(
                    "Tree does not match expected output.\nExpected:\n{}\nActual:\n{}",
                    expected,
                    tree_fancy_str
                );
            }
        }
    }

    Ok(())
}

fn run_test(test: ParserTestSingleFolder, test_mode: TestMode) -> Result<(), Failed> {
    run_test_impl(test, test_mode).map_err(Failed::from)
}

fn collect_trials() -> miette::Result<Vec<Trial>> {
    let test_mode = TestMode::get_behavior();
    let mut trials = Vec::new();

    narxia_test_runner::for_each_parser_test! {
        |test| {
            let name = test.file_name().clone().into_string().unwrap();
            let folder = test.value().clone();
            trials.push(Trial::test(name, move || run_test(folder, test_mode)));
        }
    }

    Ok(trials)
}

fn main() -> miette::Result<()> {
    let args = Arguments::from_args();
    let trails = collect_trials()?;

    libtest_mimic::run(&args, trails).exit();
}
