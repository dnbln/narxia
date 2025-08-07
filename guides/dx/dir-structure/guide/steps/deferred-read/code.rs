// !lints
// !tooltip[/ext_filter/] ext_filter
use dir_structure::ext_filter;
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
// !tooltip[/DirChildren/] DirChildren
// !tooltip[/DeferredRead/] DeferredRead
use dir_structure::{DirStructure, DirStructureItem, DirChildren, DeferredRead};

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct Dir {
    #[dir_structure(path = "subdirs")]
    // !tooltip[/DirChildren/] DirChildren
    subdirs: DirChildren<SubDir, Filt>,
}

// !tooltip[/ext_filter/] ext_filter
ext_filter!(Filt, "d");

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct SubDir {
    #[dir_structure(path = "input.txt")]
    // !tooltip[/DeferredRead/] DeferredRead
    // !mark
    input: DeferredRead<String>,
    #[dir_structure(path = "output.txt")]
    // !tooltip[/DeferredRead/] DeferredRead
    // !mark
    output: DeferredRead<String>,
}

// !hidden let path = "dir";
// !tooltip[/read/] DirStructureItem::read#
let dir = Dir::read(path)?;

// Read the input files lazily
// !tooltip[/iter/] DirChildren::iter-R
dir.subdirs.iter().try_for_each(|subdir| {
    // !tooltip[/perform_read/] DeferredRead::perform_read-R
    // !mark
    let input = subdir.value().input.perform_read()?;
    process_input(&*input);
// !hidden     fn process_input(input: &str) {}
    Ok(())
})?;

// !tooltip[/write/] DirStructureItem::write#
dir.write(path)?;
// !tail dir_structure::Error
