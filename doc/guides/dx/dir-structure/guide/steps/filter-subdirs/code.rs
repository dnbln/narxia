// !req-feature derive
// !req-feature tools-dir-children
// !lints
use std::path::Path;
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
// !tooltip[/DirChildren/] DirChildren
// !tooltip[/Filter/] Filter
use dir_structure::{DirStructure, traits::sync::DirStructureItem, dir_children::{DirChildren, Filter}};

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct Dir {
    // !mark(1:2)
    #[dir_structure(path = "subdirs")]
    // !tooltip[/DirChildren/] DirChildren
    subdirs: DirChildren<SubDir, Filt>,
}

// !mark(1:11)
struct Filt;

// !tooltip[/Filter/] Filter
impl Filter for Filt {
    fn allows(path: &Path) -> bool {
        path.extension().map_or(false, |ext| ext == "d")
    }
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
