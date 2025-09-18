// !req-feature derive
// !req-feature tools-dir-children
// !lints
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
// !tooltip[/DirChildren/] DirChildren
use dir_structure::{DirStructure, traits::vfs::VfsCore, traits::sync::DirStructureItem, dir_children::DirChildren};

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct Dir<Vfs: VfsCore> {
    // !mark(1:2)
    #[dir_structure(path = "subdirs")]
    // !tooltip[/DirChildren/] DirChildren
    subdirs: DirChildren<SubDir, dir_structure::NoFilter, Vfs::Path>,
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

// !__end
// !tail dir_structure::error::Error<std::path::PathBuf>
