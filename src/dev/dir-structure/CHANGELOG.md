# 0.2.0

Released: TODO

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

# 0.1.6

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
