// !req-feature derive
// !req-feature tools-dir-children
// !req-feature tools-deferred-read-or-own
// !req-feature tools-versioned
// !lints
// !tooltip[/ext_filter/] ext_filter
use dir_structure::ext_filter;
// !tooltip[/DirStructure/] DirStructure
// !tooltip[/DirStructureItem/] DirStructureItem
// !tooltip[/DirChildren/] DirChildren
// !tooltip[/DeferredReadOrOwn/] DeferredReadOrOwn
// !tooltip[/Versioned/] Versioned
use dir_structure::{DirStructure, traits::vfs::VfsCore, traits::sync::DirStructureItem, dir_children::DirChildren, deferred_read_or_own::DeferredReadOrOwn, versioned::Versioned};


// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct Dir<'vfs, Vfs: VfsCore<Path = std::path::Path>> {
    #[dir_structure(path = "subdirs")]
    // !tooltip[/DirChildren/] DirChildren
    subdirs: DirChildren<SubDir<'vfs, Vfs>, Filt, Vfs::Path>,
}

// !tooltip[/ext_filter/] ext_filter
ext_filter!(Filt, "d");

// !tooltip[/DirStructure/] DirStructure
#[derive(DirStructure)]
struct SubDir<'vfs, Vfs: VfsCore<Path = std::path::Path>> {
    #[dir_structure(path = "input.txt")]
    // !tooltip[/DeferredReadOrOwn/] DeferredReadOrOwn
    // !tooltip[/Versioned/] Versioned
    // !mark
    input: DeferredReadOrOwn<'vfs, Versioned<String, Vfs::Path>, Vfs>,
    #[dir_structure(path = "output.txt")]
    // !tooltip[/DeferredReadOrOwn/] DeferredReadOrOwn
    // !tooltip[/Versioned/] Versioned
    // !mark
    output: DeferredReadOrOwn<'vfs, Versioned<String, Vfs::Path>, Vfs>,
}

// !hidden let path = "dir";
// !tooltip[/read/] DirStructureItem::read#
let mut dir = Dir::read(path)?;

// Read the input files lazily
// !tooltip[/iter_mut/] DirChildren::iter_mut-R
dir.subdirs.iter_mut().try_for_each(|subdir| {
    // !tooltip[/perform_and_store_read/] DeferredReadOrOwn::perform_and_store_read-R
    let input = subdir.value_mut().input.perform_and_store_read()?;
    // Modify the value directly
    // !mark
    **input = "New content".to_owned();
    // Or use the edit_eq_check method
    // !mark(1:3)
    // !tooltip[/edit_eq_check/] Versioned::edit_eq_check-R
    input.edit_eq_check(|v| {
        *v = "New content".to_owned();
    });
    Ok(())
})?;

// This will write the changed values to disk
// !tooltip[/write/] DirStructureItem::write#
dir.write(path)?;

// !__end
// !tail dir_structure::error::Error<std::path::PathBuf>
