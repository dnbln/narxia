// !req-feature derive
// !lints
// !tooltip[/ext_filter/] ext_filter
use dir_structure::ext_filter;
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
// !tooltip[/DirChildren/] DirChildren
// !tooltip[/DeferredReadOrOwn/] DeferredReadOrOwn
use dir_structure::{DirStructure, traits::sync::DirStructureItem, dir_children::DirChildren, deferred_read_or_own::DeferredReadOrOwn};

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct Dir<'vfs, Vfs> {
    #[dir_structure(path = "subdirs")]
    // !tooltip[/DirChildren/] DirChildren
    subdirs: DirChildren<SubDir<'vfs, Vfs>, Filt>,
}

// !tooltip[/ext_filter/] ext_filter
ext_filter!(Filt, "d");

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct SubDir<'vfs, Vfs> {
    #[dir_structure(path = "input.txt")]
    // !tooltip[/DeferredReadOrOwn/] DeferredReadOrOwn
    // !mark
    input: DeferredReadOrOwn<'vfs, String, Vfs>,
    #[dir_structure(path = "output.txt")]
    // !tooltip[/DeferredReadOrOwn/] DeferredReadOrOwn
    // !mark
    output: DeferredReadOrOwn<'vfs, String, Vfs>,
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
// !tail dir_structure::error::Error
