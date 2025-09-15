// !req-feature json
// !req-feature derive
// !lints
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
// !tooltip[/FmtWrapper/] FmtWrapper
// !tooltip[/Json/] Json
use dir_structure::{DirStructure, traits::sync::DirStructureItem, fmt_wrapper::FmtWrapper, data_formats::json::Json};
use serde::{Serialize, Deserialize};

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct Dir {
    #[dir_structure(path = "input.bin")]
    binary: Vec<u8>,
    #[dir_structure(
        path = "number.txt",
        // !tooltip[/FmtWrapper/] FmtWrapper
        with_newtype = FmtWrapper<u64>,
    )]
    number: u64,
    #[dir_structure(
        path = "f.json",
        // !mark
        // !tooltip[/Json/] Json
        with_newtype = Json<Obj>,
    )]
    f: Obj,
}

// !mark(1:5)
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
struct Obj {
    name: String,
    age: u32,
}

// !hidden let path = "dir";
// !tooltip[/read/] DirStructureItem::read#
let dir = Dir::read(path)?;
// !tooltip[/write/] DirStructureItem::write#
dir.write(path)?;
// !tail dir_structure::error::Error
