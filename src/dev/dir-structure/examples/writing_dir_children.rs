use std::fs;

use dir_structure::dir_children::DirChild;
use dir_structure::dir_children::DirChildren;
use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
pub struct Dir {
    #[dir_structure(path = self)]
    children: DirChildren<String>,
}

pub fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = example_dirs::get_example_dir_path("temp_children");

    let dir = Dir {
        children: DirChildren::with_children_from_iter(
            path.clone(),
            [
                DirChild::new("ab.txt", "Hello".to_owned()),
                DirChild::new("cd.txt", "world!".to_owned()),
            ],
        ),
    };

    dir.write(&path)?;

    assert_eq!(fs::read_to_string(path.join("ab.txt"))?, "Hello");
    assert_eq!(fs::read_to_string(path.join("cd.txt"))?, "world!");

    Ok(())
}
