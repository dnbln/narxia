#![feature(internal_output_capture)]

use std::{path::Path, sync::{Arc, Mutex}};

use libtest_mimic::Failed;

pub fn ws_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
}

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
    use std::path::PathBuf;

    use dir_structure::{
        DeferredRead, DeferredReadOrOwn, DirStructure, DirStructureItem, FileString,
    };
    use miette::IntoDiagnostic;

    use crate::ws_root;

    pub fn parser_tests_dir() -> PathBuf {
        ws_root().join("tests/parser-tests")
    }

    pub const INPUT_FILE_NAME: &str = "input.nrx";
    pub const OUTPUT_FILE_NAME: &str = "output.txt";

    #[derive(DirStructure, Clone)]
    pub struct ParserTestSingleFolder {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredRead<FileString>,
        #[dir_structure(path = "output.txt")]
        pub output: Option<DeferredReadOrOwn<FileString>>,
        pub self_path: PathBuf,
    }

    impl ParserTestSingleFolder {
        pub fn input_file_path(&self) -> PathBuf {
            self.self_path.join(INPUT_FILE_NAME)
        }

        pub fn output_file_path(&self) -> PathBuf {
            self.self_path.join(OUTPUT_FILE_NAME)
        }
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
        fn $collector_fn() -> miette::Result<Vec<Trial>> {
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
