//! An example demonstrating the use of `DirChildren` to handle dynamic directory contents,
//! with an embedded file system via [`include_dir_vfs!`](dir_structure::include_dir_vfs).
//!
//! Requires the `include_dir` feature.

use std::path::Path;
use std::pin::Pin;

use dir_structure::dir_children::DirChildren;
use dir_structure::prelude::*;

#[derive(dir_structure::DirStructure)]
pub struct Dir {
    #[dir_structure(path = self)]
    children: DirChildren<String>,
}

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    let vfs = dir_structure::include_dir_vfs!("examples/example_dirs/children");

    let dir = Dir::read_from(Path::new("."), Pin::new(&vfs))?;

    for child in &dir.children {
        println!("{:?}", child);
    }

    Ok(())
}
