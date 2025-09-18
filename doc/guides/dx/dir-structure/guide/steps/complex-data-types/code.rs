// !req-feature derive
// !req-feature tools-fmt-wrapper
// !lints
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
// !tooltip[/FmtWrapper/] FmtWrapper
use dir_structure::{DirStructure, traits::sync::DirStructureItem, fmt_wrapper::FmtWrapper};

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct Dir {
    #[dir_structure(path = "input.bin")]
    binary: Vec<u8>,
    #[dir_structure(
        path = "number.txt",
        // !mark
        // !tooltip[/FmtWrapper/] FmtWrapper
        with_newtype = FmtWrapper<u64>,
    )]
    number: u64,
}

// !hidden let path = "dir";
// !tooltip[/read/] DirStructureItem::read#
let dir = Dir::read(path)?;
// !tooltip[/write/] DirStructureItem::write#
dir.write(path)?;

// !__end
// !tail dir_structure::error::Error<std::path::PathBuf>
