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

    use miette::{Context, IntoDiagnostic};

    use crate::ws_root;

    pub fn parser_tests_dir() -> PathBuf {
        ws_root().join("tests/parser-tests")
    }

    pub fn collect_parser_tests() -> miette::Result<Vec<ParserTest>> {
        let mut result = Vec::new();
        let dir = parser_tests_dir();

        for entry in dir.read_dir().into_diagnostic().context("read_dir")? {
            let entry = entry.into_diagnostic().context("read_dir")?;
            let path = entry.path();
            if path.is_dir() {
                result.push(ParserTest {
                    name: path.file_name().unwrap().to_str().unwrap().to_owned(),
                    dir_path: path,
                });
            }
        }

        Ok(result)
    }

    pub struct ParserTest {
        pub name: String,
        pub dir_path: PathBuf,
    }

    impl ParserTest {
        pub fn input_file_path(&self) -> PathBuf {
            self.dir_path.join(INPUT_FILE_NAME)
        }

        pub fn output_file_path(&self) -> PathBuf {
            self.dir_path.join(OUTPUT_FILE_NAME)
        }
    }

    pub const INPUT_FILE_NAME: &str = "input.nrx";
    pub const OUTPUT_FILE_NAME: &str = "output.txt";
}
