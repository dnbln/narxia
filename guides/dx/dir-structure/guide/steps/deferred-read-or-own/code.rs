// !lints
// !tooltip[/ext_filter/] ext_filter
use dir_structure::ext_filter;
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
// !tooltip[/DirChildren/] DirChildren
// !tooltip[/DeferredReadOrOwn/] DeferredReadOrOwn
use dir_structure::{DirStructure, DirStructureItem, DirChildren, DeferredReadOrOwn};

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
    // !tooltip[/DeferredReadOrOwn/] DeferredReadOrOwn
    // !mark
    input: DeferredReadOrOwn<String>,
    #[dir_structure(path = "output.txt")]
    // !tooltip[/DeferredReadOrOwn/] DeferredReadOrOwn
    // !mark
    output: DeferredReadOrOwn<String>,
}

// !hidden let path = "dir";
// !tooltip[/read/] DirStructureItem::read#
let mut dir = Dir::read(path)?;

// Read the input files lazily
// !tooltip[/iter_mut/] DirChildren::iter_mut-R
dir.subdirs.iter_mut().try_for_each(|subdir| {
    // !tooltip[/perform_and_store_read/] DeferredReadOrOwn::perform_and_store_read-R
    // !mark
    let input = subdir.value_mut().input.perform_and_store_read()?.clone();
    process_input(&*input);
// !hidden     fn process_input(input: &str) {}
    // This will return the cached value
    // !tooltip[/perform_and_store_read/] DeferredReadOrOwn::perform_and_store_read-R
    // !mark
    let input2 = subdir.value_mut().input.perform_and_store_read()?;
    assert_eq!(input, *input2);
    Ok(())
})?;

// !tooltip[/write/] DirStructureItem::write#
dir.write(path)?;
// !tail dir_structure::Error
