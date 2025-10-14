//! Example of writing to the filesystem using [`TokioFsVfs`].
//!
//! Requires the `async` and `tokio` features to be enabled.

use std::error::Error;
use std::fs;
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
async fn main() -> Result<(), Box<dyn Error>> {
    let dir = Dir {
        input: "This is input".to_owned(),
        output: "This is output".to_owned(),
    };

    let vfs = TokioFsVfs;

    let target_path = example_dirs::get_example_dir_path("temp_writing");

    // either
    dir.write_to_async_ref(target_path.clone(), Pin::new(&vfs))
        .await?;
    // or
    vfs.write_typed_async_ref(target_path.clone(), &dir).await?;

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
