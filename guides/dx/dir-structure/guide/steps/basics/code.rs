// !lints
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
use dir_structure::{DirStructure, DirStructureItem};

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct Dir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

// !hidden let path = "dir";
// !tooltip[/read/] DirStructureItem::read#
let dir = Dir::read(path)?;
// !tail dir_structure::Error
