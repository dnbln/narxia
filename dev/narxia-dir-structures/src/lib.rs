pub extern crate dir_structure;

use std::path::{Path, PathBuf};

use dir_structure::{DeferredReadOrOwn, DirStructure, FileString};

pub fn ws_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
}

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
