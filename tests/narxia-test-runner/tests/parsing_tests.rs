use std::path::PathBuf;

use libtest_mimic::{Arguments, Failed, Trial};
use miette::{bail, Context, IntoDiagnostic};
use narxia_syn::syntree::TreePresenterStyle;
use narxia_test_runner::ws_root;

fn parser_tests_dir() -> PathBuf {
    dbg!(ws_root().join("tests/parser-tests"))
}

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

const INPUT_FILE_NAME: &str = "input.nrx";
const OUTPUT_FILE_NAME: &str = "output.txt";

fn run_test_impl(path: PathBuf, test_mode: TestMode) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize();
    let syn_file =
        narxia_driver::parse_file_at_path_and_assert_no_errors(&ctx, path.join(INPUT_FILE_NAME));
    let tree = syn_file.tree(&ctx.db);

    let tree_str = tree.present_with_style(TreePresenterStyle::plain(), |p| format!("{p:?}"));
    let tree_fancy_str = format!("{:?}", tree);

    match test_mode {
        TestMode::Overwrite => {
            std::fs::write(path.join(OUTPUT_FILE_NAME), tree_str)
                .into_diagnostic()
                .context("Cannot write expected tree")?;
        }
        TestMode::Compare => {
            let expected = std::fs::read_to_string(path.join(OUTPUT_FILE_NAME))
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

fn run_test_for_path(path: PathBuf, test_mode: TestMode) -> Result<(), Failed> {
    run_test_impl(path, test_mode).map_err(Failed::from)
}

fn collect_trials() -> miette::Result<Vec<Trial>> {
    let p = parser_tests_dir();
    let test_mode = TestMode::get_behavior();
    let mut trials = Vec::new();
    for entry in p.read_dir().into_diagnostic().context("read_dir")? {
        let entry = entry.into_diagnostic().context("read_dir")?;
        let path = entry.path();
        if path.is_dir() {
            trials.push(Trial::test(
                path.file_name().unwrap().to_str().unwrap().to_owned(),
                move || run_test_for_path(path, test_mode),
            ));
        }
    }

    Ok(trials)
}

fn main() -> miette::Result<()> {
    let args = Arguments::from_args();
    let trails = collect_trials()?;

    libtest_mimic::run(&args, trails).exit();
}
