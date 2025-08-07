pub extern crate dir_structure;

use std::path::Path;

use dir_structure::DirStructure;

pub fn ws_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
}

#[derive(DirStructure, Clone)]
pub struct Workspace {
    guides: dir_structure::DirChildren<()>,
}

pub mod parser_tests {
    use std::path::PathBuf;

    use dir_structure::DeferredReadOrOwn;
    use dir_structure::DirStructure;
    use dir_structure::FileString;

    use crate::ws_root;

    pub fn parser_tests_dir() -> PathBuf {
        ws_root().join("tests/parser-tests")
    }

    pub const INPUT_FILE_NAME: &str = "input.nrx";
    pub const OUTPUT_FILE_NAME: &str = "output.txt";

    #[derive(DirStructure, Clone)]
    pub struct ParserTestSingleFolder {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredReadOrOwn<FileString>,
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
}

pub mod name_resolution_tests {
    use std::path::PathBuf;

    use dir_structure::DeferredReadOrOwn;
    use dir_structure::DirStructure;
    use dir_structure::FileString;

    use crate::ws_root;

    pub fn name_resolution_tests_dir() -> PathBuf {
        ws_root().join("tests/name-resolution-tests")
    }

    #[derive(DirStructure, Clone)]
    pub struct NameResolutionTestSingleFolder {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredReadOrOwn<FileString>,
        #[dir_structure(path = "output.txt")]
        pub output: Option<DeferredReadOrOwn<FileString>>,
        pub self_path: PathBuf,
    }

    impl NameResolutionTestSingleFolder {
        pub fn input_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([NameResolutionTestSingleFolder @ self.self_path.clone()].input)
        }

        pub fn output_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([NameResolutionTestSingleFolder @ self.self_path.clone()].output)
        }
    }
}

pub mod ssa_tests {
    use std::path::PathBuf;

    use dir_structure::DeferredReadOrOwn;
    use dir_structure::DirStructure;
    use dir_structure::FileString;

    use crate::ws_root;

    pub fn ssa_tests_dir() -> PathBuf {
        ws_root().join("tests/ssa-tests")
    }

    #[derive(DirStructure, Clone)]
    pub struct SsaTestSingleFolder {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredReadOrOwn<FileString>,
        #[dir_structure(path = "output.nrxssa")]
        pub output: Option<DeferredReadOrOwn<FileString>>,
        pub self_path: PathBuf,
    }

    impl SsaTestSingleFolder {
        pub fn input_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([SsaTestSingleFolder @ self.self_path.clone()].input)
        }

        pub fn output_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([SsaTestSingleFolder @ self.self_path.clone()].output)
        }
    }
}
