# `dir-structure` examples

A collection of examples demonstrating various features of the `dir-structure` crate.

## Basics

- [reading.rs](reading.rs): A simple example of reading a directory structure from disk.
- [writing.rs](writing.rs): A simple example of writing a directory structure to disk.

## `CleanDir`

- [clean_dir.rs](clean_dir.rs): An example demonstrating the use of `CleanDir` to ensure a directory is clean before writing.

## `data_formats::json`, `data_formats::json_pretty`

- [json.rs](json.rs): An example demonstrating the use of `Json` to read and write JSON files.
- [json_pretty.rs](json_pretty.rs): An example demonstrating the use of `JsonPretty` to read and write pretty-printed JSON files.
- [toml.rs](toml.rs): An example demonstrating the use of `Toml` to read and write TOML files.
- [yaml.rs](yaml.rs): An example demonstrating the use of `Yaml` to read and write YAML files.
- [ron.rs](ron.rs): An example demonstrating the use of `Ron` to read and write RON files.
- [ron_pretty.rs](ron_pretty.rs): An example demonstrating the use of `RonPretty` to read and write pretty-printed RON files.

## `DeferredRead`

- [deferred_read.rs](deferred_read.rs): An example demonstrating the use of `DeferredRead` to defer reading file contents until they are needed.

## `DeferredReadOrOwn`

- [deferred_read_or_own.rs](deferred_read_or_own.rs): An example demonstrating the use of `DeferredReadOrOwn` to defer reading file contents and cache them.

## `DirChildren`

- [reading_dir_children.rs](reading_dir_children.rs): An example demonstrating the use of `DirChildren` to read dynamic directory contents.
- [writing_dir_children.rs](writing_dir_children.rs): An example demonstrating the use of `DirChildren` to write dynamic directory contents.

## Virtual File Systems (VFS)

### Library-provided synchronous VFS implementations

#### `std_fs` VFS

- [std_fs_vfs.rs](std_fs_vfs.rs): An example of reading / writing a directory structure from the actual file system using the `StdFsVfs`. Note that this is the default VFS used by `DirStructureItem::read` and `DirStructureItem::write`, so all the examples listed above use this VFS implicitly. This example is just to demonstrate the explicit use of the `StdFsVfs`.

#### `include_dir` VFS

- [reading_from_include_dir_vfs.rs](reading_from_include_dir_vfs.rs): An example of reading a directory structure from an embedded file system using the `include_dir_vfs!` macro.
- [reading_dir_children_from_include_dir_vfs.rs](reading_dir_children_from_include_dir_vfs.rs): An example demonstrating the use of `DirChildren` to read dynamic directory contents from an embedded file system using the `include_dir_vfs!` macro.

Note that the `include_dir` VFS is read-only, so there are no writing examples.

#### `git` VFS

- [reading_from_git_vfs.rs](reading_from_git_vfs.rs): An example of reading a directory structure from a git repository.

Note that the `git` VFS is read-only, so there are no writing examples.

### Library-provided asynchronous VFS implementations

#### `tokio_fs` VFS

- [reading_from_tokio_fs_vfs.rs](reading_from_tokio_fs_vfs.rs): An example of reading a directory structure from the actual file system using `TokioFsVfs`.
- [writing_to_tokio_fs_vfs.rs](writing_to_tokio_fs_vfs.rs): An example of writing a directory structure to the actual file system using `TokioFsVfs`.


## `resolve_path`

- [resolve_path.rs](resolve_path.rs): An example demonstrating the use of `resolve_path` to resolve paths within a directory structure.

## `load_path`

- [load_path.rs](load_path.rs): An example demonstrating the use of `load_path` to load a specific file from a path within a directory structure, without loading the entire structure.
