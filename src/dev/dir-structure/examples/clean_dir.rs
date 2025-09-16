//! An example demonstrating the use of [`CleanDir`] to ensure a directory is clean before writing.
//!
//! Requires the `derive` and `tools-clean-dir` features to be enabled.

use std::error::Error;
use std::fs;

use dir_structure::clean_dir::CleanDir;
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
    let p = example_dirs::get_example_dir_path("temp_cleaning_w");

    Dir {
        input: "some input".to_string(),
        output: "some output".to_string(),
    }
    .write(&p)?;

    println!("Wrote initial files.");

    // now let's add another file into the directory
    fs::write(p.join("extra.txt"), "extra")?;

    assert!(p.join("extra.txt").exists());
    println!("Added extra file.");

    // and now let's write again, but this time using CleanDir to ensure the directory is clean
    CleanDir(Dir {
        input: "new input".to_string(),
        output: "new output".to_string(),
    })
    .write(&p)?;

    println!("Wrote new files with cleaning.");

    assert!(!p.join("extra.txt").exists());

    Ok(())
}
