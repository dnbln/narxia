# `dir-structure-tools` examples

A collection of examples demonstrating various tools in the `dir-structure-tools` crate.

## `CleanDir`

- [clean_dir.rs](clean_dir.rs): An example demonstrating the use of `CleanDir` to ensure a directory is clean before writing.

## `data_formats::*`

- [json.rs](json.rs): An example demonstrating the use of `Json` to read and write JSON files.
- [json_pretty.rs](json_pretty.rs): An example demonstrating the use of `JsonPretty` to read and write pretty-printed JSON files.
- [toml.rs](toml.rs): An example demonstrating the use of `Toml` to read and write TOML files.
- [yaml.rs](yaml.rs): An example demonstrating the use of `Yaml` to read and write YAML files.
- [ron.rs](ron.rs): An example demonstrating the use of `Ron` to read and write RON files.
- [ron_pretty.rs](ron_pretty.rs): An example demonstrating the use of `RonPretty` to read and write pretty-printed RON files.

## A note on the following examples

The examples above demonstrate the basic usage of the `dir-structure` crate with the default synchronous VFS (`StdFsVfs`). While
98% of use-cases can be covered with just those basic usage patterns, the crate also supports more advanced features, which are
demonstrated in the examples below.

> [!IMPORTANT]
> A warning first: In order to understand the examples below, it is recommended to read the documentation for the
> [`dir_structure::traits::vfs` module](../../dir-structure/src/traits/vfs.rs) first.

## `DeferredRead`

- [deferred_read.rs](deferred_read.rs): An example demonstrating the use of `DeferredRead` to defer reading file contents until they are needed.

## `DeferredReadOrOwn`

- [deferred_read_or_own.rs](deferred_read_or_own.rs): An example demonstrating the use of `DeferredReadOrOwn` to defer reading file contents and cache them.

## `DirChildren`

- [reading_dir_children.rs](reading_dir_children.rs): An example demonstrating the use of `DirChildren` to read dynamic directory contents.
- [writing_dir_children.rs](writing_dir_children.rs): An example demonstrating the use of `DirChildren` to write dynamic directory contents.
