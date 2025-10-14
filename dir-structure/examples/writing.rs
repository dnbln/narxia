//! Example of writing a directory structure to the filesystem.

use std::error::Error;
use std::fs;

use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let dir = Dir {
        input: "This is input".to_owned(),
        output: "This is output".to_owned(),
    };

    let target_path = example_dirs::get_example_dir_path("temp_writing");
    dir.write(&target_path)?;

    assert_eq!(
        fs::read_to_string(target_path.join("input.txt"))?,
        dir.input
    );
    assert_eq!(
        fs::read_to_string(target_path.join("output.txt"))?,
        dir.output
    );

    Ok(())
}
