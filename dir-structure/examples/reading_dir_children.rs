//! An example demonstrating the use of [`DirChildren`] to read dynamic directory contents.

use dir_structure::NoFilter;
use dir_structure::dir_children::DirChildren;
use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
pub struct Dir<Vfs: VfsCore> {
    #[dir_structure(path = self)]
    children: DirChildren<String, NoFilter, Vfs::Path>,
}

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = example_dirs::get_example_dir_path("children");

    let dir = Dir::read(&path)?;

    for child in &dir.children {
        println!("{:?}", child);
    }

    Ok(())
}
