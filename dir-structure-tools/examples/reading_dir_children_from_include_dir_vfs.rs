//! An example demonstrating the use of `DirChildren` to handle dynamic directory contents,
//! with an embedded file system via [`include_dir_vfs!`](dir_structure::include_dir_vfs).
//!
//! Requires the `include_dir` feature.

use std::path::Path;
use std::pin::Pin;

use dir_structure::prelude::*;
use dir_structure_tools::NoFilter;
use dir_structure_tools::dir_children::DirChildren;

#[derive(dir_structure::DirStructure)]
pub struct Dir<Vfs: VfsCore> {
    #[dir_structure(path = self)]
    children: DirChildren<String, NoFilter, Vfs::Path>,
}

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    let vfs = dir_structure_include_dir_vfs::IncludeDirVfs::new(
        dir_structure_include_dir_vfs::include_dir::include_dir!(
            "$CARGO_MANIFEST_DIR/examples/example_dirs/children"
        ),
    );

    let dir = Dir::read_from(Path::new("."), Pin::new(&vfs))?;

    for child in &dir.children {
        println!("{:?}", child);
    }

    Ok(())
}
