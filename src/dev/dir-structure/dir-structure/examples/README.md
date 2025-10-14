# `dir-structure` examples

A collection of examples demonstrating various features of the `dir-structure` crate.

## Basics

- [reading.rs](reading.rs): A simple example of reading a directory structure from disk.
- [writing.rs](writing.rs): A simple example of writing a directory structure to disk.

## `resolve_path`

- [resolve_path.rs](resolve_path.rs): An example demonstrating the use of `resolve_path` to resolve paths within a directory structure.

## `load_path`

- [load_path.rs](load_path.rs): An example demonstrating the use of `load_path` to load a specific file from a path within a directory structure, without loading the entire structure.

## `dir-structure-tools`

Also see [the examples in the `dir-structure-tools` crate](../../dir-structure-tools/examples/), which demonstrate more advanced features of the `dir-structure` crate.

## A note on the following examples

The examples above demonstrate the basic usage of the `dir-structure` crate with the default synchronous VFS (`StdFsVfs`). While
98% of use-cases can be covered with just those basic usage patterns, the crate also supports more advanced features, which are
demonstrated in the examples below.

> [!IMPORTANT]
> A warning first: In order to understand the examples below, it is recommended to read the documentation for the
> [`dir_structure::traits::vfs` module](../src/traits/vfs.rs) first.

## Virtual File Systems (VFS)

### Library-provided synchronous VFS implementations

#### `std_fs` VFS

- [std_fs_vfs.rs](std_fs_vfs.rs): An example of reading / writing a directory structure from the actual file system using the `StdFsVfs`. Note that this is the default VFS used by `DirStructureItem::read` and `DirStructureItem::write`, so all the examples listed above use this VFS implicitly. This example is just to demonstrate the explicit use of the `StdFsVfs`.

<!-- #### `include_dir` VFS

- [reading_from_include_dir_vfs.rs](reading_from_include_dir_vfs.rs): An example of reading a directory structure from an embedded file system using the `include_dir_vfs!` macro.
- [reading_dir_children_from_include_dir_vfs.rs](reading_dir_children_from_include_dir_vfs.rs): An example demonstrating the use of `DirChildren` to read dynamic directory contents from an embedded file system using the `include_dir_vfs!` macro.

Note that the `include_dir` VFS is read-only, so there are no writing examples.

#### `git` VFS

- [reading_from_git_vfs.rs](reading_from_git_vfs.rs): An example of reading a directory structure from a git repository.

Note that the `git` VFS is read-only, so there are no writing examples. -->

### Library-provided asynchronous VFS implementations

#### `tokio_fs` VFS

- [reading_from_tokio_fs_vfs.rs](reading_from_tokio_fs_vfs.rs): An example of reading a directory structure from the actual file system using `TokioFsVfs`.
- [writing_to_tokio_fs_vfs.rs](writing_to_tokio_fs_vfs.rs): An example of writing a directory structure to the actual file system using `TokioFsVfs`.

