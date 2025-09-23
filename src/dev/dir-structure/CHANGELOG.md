# Unreleased

## New features

`AtomicDir<T>`: A new wrapper type that allows for atomic writes of directory structures.
When writing, it will first write the contents to a temporary directory, and then rename the
temporary directory to the target directory. If writing the contents to the temporary directory
fails, the original directory is left untouched, and the temporary directory is deleted.

## Other changes

- Changed the image async pipeline to use the new traits `ReadImageFromAsync`, `WriteImageToAsync`, and `WriteImageToAsyncRef`.
  See the documentation in the [`async_vcs`](src/traits/async_vfs.rs) module for more details.

- Added bounds for derived implementations of `ReadFrom` and `WriteTo` via the `DirStructure` derive macro.

- Fixed bugs where we were pulling in extra dependencies when certain features were enabled. For example, due to the dependency
  of `image` on `tokio/rt` for async support, enabling the `image` feature would also pull in `tokio` as a dependency, even if
  the `tokio` feature was not enabled. This has been fixed by changing the `image` feature to depend on `tokio?/rt` instead of `tokio/rt`,
  and similarly for a few other features.

# `0.2.0-rc.3`

Released: 2025-09-19

## New features

Examples! Lots of examples have been added to the [`examples/`](examples/) directory, demonstrating various features of the library.

`dir_structure::ron_pretty::RonPretty` for pretty-formatted RON files, similar to `dir_structure::json_pretty::JsonPretty`.

Add `GitVfs::new` to be able to create a `GitVfs`.

For `DirChildren`:
- Implemented `FromIterator<DirChild<T>>` for `DirChildren<T, F>`, allowing you to create a `DirChildren` from an iterator of `DirChild<T>`.
- Implemented `From<Vec<DirChild<T>>>` for `DirChildren<T, F>`, allowing you to create a `DirChildren<T, F>` from a `Vec<DirChild<T>>`.
- Implemented `Extend<DirChild<T>>` for `DirChildren<T, F>`.

For `ForceCreateDirChildren`:
- Added `ForceCreateDirChildren::with_children_from_iter`, allowing you to create a `ForceCreateDirChildren` from an iterator of `DirChild<T>`.
- Implemented `FromIterator<DirChild<T>>` for `ForceCreateDirChildren<T, F>`.
- Implemented `AsRef<DirChildren<T, F>>` for `ForceCreateDirChildren<T, F>`.
- Implemented `AsMut<DirChildren<T, F>>` for `ForceCreateDirChildren<T, F>`.
- Implemented `From<DirChildren<T, F>>` for `ForceCreateDirChildren<T, F>`.
- Implemented `From<ForceCreateDirChildren<T, F>>` for `DirChildren<T, F>`.

For `DirDescendants`:
- Implemented `Default` for `DirDescendants<T, F>`, creating an empty `DirDescendants`.
- Implemented `From<Vec<DirDescendant<T>>>` for `DirDescendants<T, F>`.
- Implemented `FromIterator<DirDescendant<T>>` for `DirDescendants<T, F>`.
- Implemented `Extend<DirDescendant<T>>` for `DirDescendants<T, F>`.


Added a `all-image-formats` feature flag, which enables support for all image formats supported by the `image` crate.

## Breaking changes

`WriteTo` now takes in a `'vfs` lifetime parameter.
It is automatically added by the `DirStructure` derive macro, so if you are using that, you don't need to do anything.
If you are implementing `WriteTo` manually, you will need to add the lifetime parameter to your impls.

Removed the `self_path` field from `DirChildren`, as it was not used anywhere, and did not make sense to be there.

`read_from`, `write_to`, `read_from_async`, `write_to_async`, and `write_to_async_ref` now take in a `&Vfs::Path` or `<Vfs::Path as PathType>::PathOwned`
instead of the old `&Path` and `PathBuf`. `Vfs::Path` is an associated type of the `Vfs` trait, which allows you to use
custom path types for your virtual file system. `Vfs::Path` must implement the `PathType` trait, which also defines the associated
type `PathOwned`, and a couple of other methods needed by the library for path manipulation.

`HasField` implementations moved from the `dir_structure::DirStructure` derive macro to the `dir_structure::HasField` derive macro,
so if you are using the `resolve_path!` / `load_path!` macros, you will need to derive `HasField` for your structs in addition to `DirStructure` / `DirStructureAsync`.

## Other changes

Relax bound for `Vfs::RFile` from `BufRead` to `Read`, as `BufRead` is only necessary
for image reading, and we can wrap the `Read` in a `BufReader` there.

Similarly, relax bound for `VfsAsync::RFile` from `AsyncBufRead` to `AsyncRead`.

*The `derive` and `tools` features* are now optional, default features.
This allows the user to opt-out of these features if they are not needed.

Every wrapper type is put behind a `tools-<wrapper>` feature flag, so you can opt-out of
the wrapper types you don't need, and only use the ones you need. The `tools` feature
enables all the wrapper types.

Opting out of all default features removes all the dependencies of the crate. The core crate
is supposed to allow you to define directory structures and read / write them, as well
as defining virtual file systems, without the derive macros or any of the wrapper types.

Uncovered a bug in the old implementations of `ReadFrom` and `ReadFromAsync` for `DirDescendants`, when converting it to the
new path types. These have been fixed.

# `v0.2.0-rc.2`

Released: 2025-09-12

## Image support

Support for image files has been added via the `image` feature flag. This allows you to read and write
various image formats such as PNG, JPEG, BMP, GIF, and TIFF using the `image` crate.

Support for individual formats can be enabled via the following feature flags:
- `image-format-png`
- `image-format-gif`
- `image-format-jpeg`
- `image-format-webp`
- `image-format-tiff`
- `image-format-tga`
- `image-format-bmp`
- `image-format-ico`
- `image-format-hdr`
- `image-format-exr`
- `image-format-pnm`
- `image-format-ff`
- `image-format-avif`
- `image-format-qoi`

## New APIs

Expanded the `DirDescendants` struct with new functions:
- `len`: Get the number of `DirDescendant` entries.
- `is_empty`: Check if there are no `DirDescendant` entries.
- `get` and `get_mut`: Get a reference to a `DirDescendant` by its index.
- `get_by_name` and `get_by_name_mut`: Get a reference to a `DirDescendant` by its name.
- `get_value_by_name` and `get_value_by_name_mut`: Get a reference or mutable reference to the value in a `DirDescendant` by its name.
- `get_by_path` and `get_by_path_mut`: Get a reference to a `DirDescendant` by its full path.
- `get_value_by_path` and `get_value_by_path_mut`: Get a reference or mutable reference to the value in a `DirDescendant` by its full path.
- `get_by_relative_path` and `get_by_relative_path_mut`: Get a reference to a `DirDescendant` by its path relative to the `DirDescendants` root.
- `get_value_by_relative_path` and `get_value_by_relative_path_mut`: Get a reference or mutable reference to the value in a `DirDescendant` by its path relative to the `DirDescendants` root.
- `map`: Create a new `DirDescendants` by applying a function to each value.
- `map_filter`: Create a new `DirDescendants` by changing the filter type.

Implemented `ReadFromAsync`, `WriteToAsync` and `WriteToAsyncRef` for `DirDescendants`.

The `DirChildren` struct has been expanded with the following methods:
- `retain`: Retain only the children that satisfy a given predicate.
- `drain`: Remove and return a range of children as an iterator.
- `extract_if`: Remove and return children that satisfy a given predicate as an iterator.

Additionally, `&DirChildren` and `&mut DirChildren` now implement `IntoIterator`, allowing you to iterate over references to the children:

```rust
use dir_structure::DirChildren;

let mut children = DirChildren::new();

for child in &children {
    // child is of type &DirChild<T>
}

for child in &mut children {
    // child is of type &mut DirChild<T>
}
```

The `DirDescendants` struct has been expanded with the following methods:
- `push`: Add a new `DirDescendant` to the end of the list.
- `retain`: Retain only the descendants that satisfy a given predicate.
- `drain`: Remove and return a range of descendants as an iterator.
- `extract_if`: Remove and return descendants that satisfy a given predicate as an iterator.

For `DirDescendant`, the following method has been added:
- `DirDescendant::as_ref` and `DirDescendant::as_mut`: Make a clone of the name and paths, return a `DirDescendant<&T>` or `DirDescendant<&mut T>` with references to the original value.

For `DirChildSingle`, the following method has been added:
- `DirChildSingle::as_mut`: Make a clone of the name and path, return a `DirChildSingle<&mut T>` with a mutable reference to the original value.

For `DirChildSingleOpt`, the following method has been added:
- `DirChildSingleOpt::as_mut`: Make a clone of the name and path, return a `DirChildSingleOpt<&mut T>` with a mutable reference to the original value.
- `DirChildSingleOpt::take_if`: Take the child if it satisfies a given predicate, replacing it with `None` in the `DirChildSingleOpt`, or returning `DirChildSingleOpt::None` otherwise.

`DirChild` and `DirDescendant` now implement `Deref` and `DerefMut` to their inner values, allowing you to use them as if they were the inner values directly.

`CleanDir` now implements `WriteToAsyncRef`, allowing you to write the directory structure asynchronously, without having to clone it (like previously with only the `WriteToAsync` impl).

## `assert_eq` support for all library types

[assert_eq] support has been added for all types in the library, allowing you to use the `assert_eq::assert_eq!` macro to compare directory structures and their components.

[assert_eq]: https://crates.io/crates/assert_eq

# `0.2.0-rc.1`

Released: 2025-09-06

## Virtual file system support

The library now supports virtual file systems via the `Vfs` and `VfsAsync` traits, or their write-supporting variants
`WriteSupportingVfs` and `WriteSupportingVfsAsync`. This allows you to use the library with in-memory file systems,
or other custom file systems.

`dir_structure::vfs::fs_vfs::FsVfs` is the default implementation for the local file system, with
`dir_structure::vfs::tokio_fs_vfs::TokioFsVfs` providing opt-in async support via Tokio (gated behind the `tokio` feature).

## `DirStructureAsync` derive macro for async support

Now you can derive `DirStructureAsync` for your structs to derive the asynchronous
interfaces `ReadFromAsync` / `WriteToAsyncRef` for your own structures.

## `VersionedHash`

A new wrapper type `VersionedHash<T, H>` has been added, which is similar to
`Versioned<T>`, but instead of tracking changes by comparing the value of `T` before and after edits,
or checking if `DerefMut::deref_mut` has been used, it tracks changes by hashing the value of `T` using the hasher `H`
(implementing `std::hash::Hasher`), and comparing the hash when the value has been first read and when it is written.

`VersionedHash<T, H>::reset` can be used, analogously to `Versioned<T>::reset`, to reset the hash to the current hash of
the inner value of type `T`, which will mark the value as clean. Similarly to `Versioned<T>::reset`, this is an unsafe API,
as it allows you to reset the version without updating the files on the file system, which might lead to data loss if used
incorrectly.

## `load_path` macro (nightly-only)

A new macro `load_path!` has been added, which allows you to load a specific part of a directory structure, without
having to load the entire structure in-memory:

```rust
#[derive(DirStructure)]
struct MyStruct {
    #[dir_structure(path = "my_field.txt")]
    my_field: String,
    #[dir_structure(path = "my_field2.d")]
    my_field2: MyStruct2,
}
#[derive(DirStructure)]
struct MyStruct2 {
    #[dir_structure(path = "my_field3.txt")]
    my_field3: String,
}
assert_eq!(
    load_path!([MyStruct @ "/path/to/dir"].my_field).unwrap(),
    std::fs::read_to_string("/path/to/dir/my_field.txt").unwrap()
);
assert_eq!(
    load_path!(["/path/to/dir" as MyStruct].my_field2.my_field3).unwrap(),
    std::fs::read_to_string("/path/to/dir/my_field2.d/my_field3.txt").unwrap(),
);

assert_eq!(
    load_path!(["/path/to/dir" as MyStruct].my_field2).unwrap(),
    MyStruct2 {
        my_field3: std::fs::read_to_string("/path/to/dir/my_field2.d/my_field3.txt").unwrap(),
    },
);
```

# `0.1.6`

Released: 2025-08-07

## Async support

The `async` feature has been added, allowing for asynchronous operations. This feature
requires an async runtime to be enabled, currently only `tokio` is supported.

## `resolve-path` feature (nightly-only)

The `resolve-path` feature has been added, which allows for resolving paths to specific
fields in a structure.

```rust
#[derive(DirStructure)]
struct MyStruct {
    #[dir_structure(path = "my_field.txt")]
    my_field: String,
    #[dir_structure(path = "my_field2.d")]
    my_field2: MyStruct2,
}
#[derive(DirStructure)]
struct MyStruct2 {
    #[dir_structure(path = "my_field3.txt")]
    my_field3: String,
}
assert_eq!(
    resolve_path!([MyStruct @ "/path/to/dir"].my_field),
    PathBuf::from("/path/to/dir/my_field.txt")
);
assert_eq!(
    resolve_path!(["/path/to/dir" as MyStruct].my_field2.my_field3),
    PathBuf::from("/path/to/dir/my_field2.d/my_field3.txt")
);
```

## `ext_filter` macro

The `ext_filter` macro has been introduced to simplify the creation of file extension filters:

```rust
use dir_structure::ext_filter;

ext_filter!(RsSourceFileFilter, "rs");
```

## `DirChildren::get_mut`

Mutable version of `DirChildren::get`, allowing you to get a mutable reference to a child by its name.

## `DirChildren::iter_mut`

A mutable iterator over the children of a `DirChildren` structure, allowing you to mutate the children while iterating.

## `DeferredReadOrOwn::perform_and_store_read` now returns a mutable reference

This method now returns a mutable reference to the read value, allowing you to modify it directly after reading, as opposed to an immutable reference, as was previously the case.

## `Versioned::reset`

Resets the versioned value to its initial version, marking it as clean.

This is an unsafe API, as it allows you to reset the version without updating the files on the file system, so a further call to `write_to` will **not** write the value to disk, even if it was previously marked as dirty. **Use with caution.**

# `0.1.5`

Released: 2025-04-16

## `Filter`

Allows for filtering of `DirChildren` entries.

## Support for toml, yaml, ron files.

Similarly to `json`, you can now also use `toml`, `yaml`, and `ron` files, by enabling the respective feature flags.

## Lots of documentation and examples

Added a lot of documentation and examples to the code.

# `0.1.4`

Released: 2024-04-15

## `Versioned`

New wrapper type: [`Versioned`](https://docs.rs/dir-structure/latest/dir_structure/struct.Versioned.html).

It will only write the value to disk if it has changed in Rust
code since it has been read. Refer to documentation for how to use.
