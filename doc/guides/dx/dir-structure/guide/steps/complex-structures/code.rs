// !lints
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
use dir_structure::{DirStructure, traits::sync::DirStructureItem};

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct Dir {
    // !mark(1:2)
    #[dir_structure(path = "subdir")]
    subdir: SubDir,
}

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct SubDir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

// !hidden let path = "dir";
// !tooltip[/read/] DirStructureItem::read#
let dir = Dir::read(path)?;
// !tooltip[/write/] DirStructureItem::write#
dir.write(path)?;
// !tail dir_structure::error::Error
