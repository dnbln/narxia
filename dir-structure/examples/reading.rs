//! A simple example of reading a directory structure from disk.

use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = example_dirs::get_example_dir_path("reading");

    let dir = Dir::read(&path)?;

    println!("input: {}", dir.input);
    println!("output: {}", dir.output);

    Ok(())
}
