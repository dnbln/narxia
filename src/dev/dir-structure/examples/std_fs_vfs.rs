//! A simple example of reading a directory structure from disk.

use std::fs;
use std::pin::Pin;

use dir_structure::prelude::*;
use dir_structure::vfs::fs_vfs::FsVfs;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // reading example
    let path = example_dirs::get_example_dir_path("reading");
    let vfs = FsVfs;

    // either
    let dir = Dir::read_from(&path, Pin::new(&vfs))?;
    // or as a shorthand
    let dir = vfs.read_typed::<Dir>(&path)?;

    println!("input: {}", dir.input);
    println!("output: {}", dir.output);

    // writing example

    let write_path = example_dirs::get_example_dir_path("temp_writing");

    // either
    dir.write_to(&write_path, Pin::new(&vfs))?;
    // or as a shorthand
    vfs.write_typed(&write_path, &dir)?;

    assert_eq!(fs::read_to_string(write_path.join("input.txt"))?, dir.input);
    assert_eq!(
        fs::read_to_string(write_path.join("output.txt"))?,
        dir.output
    );

    Ok(())
}
