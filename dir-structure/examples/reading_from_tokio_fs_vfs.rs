//! Example of reading from the filesystem using [`TokioFsVfs`].
//!
//! Requires the `async` and `tokio` features to be enabled.

use std::pin::Pin;

use dir_structure::prelude::*;
use dir_structure::vfs::tokio_fs_vfs::TokioFsVfs;

mod example_dirs;

#[derive(dir_structure::DirStructureAsync)]
struct Dir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = example_dirs::get_example_dir_path("reading");
    let vfs = TokioFsVfs;
    // either
    let dir = Dir::read_from_async(path.clone(), Pin::new(&vfs)).await?;
    // or
    let dir = vfs.read_typed_async::<Dir>(path).await?;
    println!("input: {}", dir.input);
    println!("output: {}", dir.output);

    Ok(())
}
