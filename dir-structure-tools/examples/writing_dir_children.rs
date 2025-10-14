use std::fs;

use dir_structure_tools::NoFilter;
use dir_structure_tools::dir_children::DirChild;
use dir_structure_tools::dir_children::DirChildren;
use dir_structure::prelude::*;
use dir_structure::traits::vfs::VfsCore;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
pub struct Dir<'vfs, Vfs: VfsCore + 'vfs> {
    #[dir_structure(path = self)]
    children: DirChildren<String, NoFilter, Vfs::Path>,
    __marker: std::marker::PhantomData<&'vfs Vfs>,
}

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = example_dirs::get_example_dir_path("temp_children");

    let dir = Dir::<'_, _> {
        children: [
            DirChild::new("ab.txt", "Hello".to_owned()),
            DirChild::new("cd.txt", "world!".to_owned()),
        ]
        .into_iter()
        .collect(),
        __marker: std::marker::PhantomData,
    };

    println!("Writing children...");
    dir.write(&path)?;
    println!("Done writing.");

    println!("Verifying...");
    assert_eq!(fs::read_to_string(path.join("ab.txt"))?, "Hello");
    assert_eq!(fs::read_to_string(path.join("cd.txt"))?, "world!");
    println!("Verified.");

    Ok(())
}
