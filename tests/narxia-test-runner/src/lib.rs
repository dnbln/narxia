use std::path::Path;

pub fn ws_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
}

pub mod parser_tests {
    use std::path::PathBuf;

    use dir_structure::{DeferredRead, DeferredReadOrOwn, DirStructure, DirStructureItem, FileString};
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