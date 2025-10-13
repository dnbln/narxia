//! An example of reading a directory structure from an embedded file system using the
//! [`include_dir_vfs!`](dir_structure::include_dir_vfs) macro.
//!
//! Requires the `include_dir` feature.

use std::path::Path;
use std::pin::Pin;

use dir_structure::prelude::*;

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let vfs = dir_structure::include_dir_vfs!("$CARGO_MANIFEST_DIR/examples/example_dirs/reading");

    // either
    let dir = Dir::read_from(Path::new("."), Pin::new(&vfs))?;
    // or as a shorthand
    let dir = vfs.read_typed::<Dir>(Path::new("."))?;

    println!("input: {}", dir.input);
    println!("output: {}", dir.output);

    Ok(())
}
