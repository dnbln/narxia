//! An example demonstrating the use of [`DeferredRead`] to defer reading file contents until needed.
//!
//! Requires the `derive` and `tools-deferred-read` features to be enabled.

use std::error::Error;
use std::fs;

use dir_structure::deferred_read::DeferredRead;
use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
pub struct Dir<'vfs, Vfs: VfsCore> {
    #[dir_structure(path = "input.txt")]
    input: DeferredRead<'vfs, String, Vfs>,
    #[dir_structure(path = "output.txt")]
    output: DeferredRead<'vfs, String, Vfs>,
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let path = example_dirs::get_example_dir_path("reading_w");

    let dir = Dir::read(&path)?;

    // no file contents have been read yet

    // now we can read them on demand:
    let input = dir.input.perform_read()?;
    let output = dir.output.perform_read()?;

    println!("input: {input}");
    println!("output: {output}");

    // we can now modify the files on disk and re-read them

    fs::write(path.join("input.txt"), format!("{input} modified"))?;
    fs::write(path.join("output.txt"), format!("{output} modified"))?;

    println!("input: {}", dir.input.perform_read()?);
    println!("output: {}", dir.output.perform_read()?);

    // restore original contents (not needed, just here to keep the example dir clean)
    fs::write(path.join("input.txt"), input)?;
    fs::write(path.join("output.txt"), output)?;

    Ok(())
}
