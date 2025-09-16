//! An example demonstrating the use of [`DeferredReadOrOwn`]
//! to defer reading file contents and cache them.
//!
//! Requires the `derive` and `tools-deferred-read` features to be enabled.

use std::error::Error;

use dir_structure::deferred_read_or_own::DeferredReadOrOwn;
use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
pub struct Dir<'vfs, Vfs> {
    #[dir_structure(path = "input.txt")]
    input: DeferredReadOrOwn<'vfs, String, Vfs>,
    #[dir_structure(path = "output.txt")]
    output: DeferredReadOrOwn<'vfs, String, Vfs>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let path = example_dirs::get_example_dir_path("reading_w");

    let mut dir = Dir::read(&path)?;

    // no file contents have been read yet

    // now we can read them on demand:
    // and modify the files on disk and re-read them
    let input = dir.input.perform_and_store_read()?;
    println!("input: {input}");
    std::fs::write(path.join("input.txt"), format!("{input} modified"))?;
    println!("input: {}", dir.input.perform_and_store_read()?); // warning: still uses cached value

    let output = dir.output.perform_and_store_read()?;
    println!("output: {output}");
    std::fs::write(path.join("output.txt"), format!("{output} modified"))?;
    println!("output: {}", dir.output.perform_and_store_read()?); // warning: still uses cached value

    let mut new_dir = Dir::read(&path)?; // read a new instance to see the modified values
    println!("new input: {}", new_dir.input.perform_and_store_read()?);
    println!("new output: {}", new_dir.output.perform_and_store_read()?);

    dir.write(&path)?; // writes the cached values back to disk

    println!("Wrote old values back to disk.");

    Ok(())
}
