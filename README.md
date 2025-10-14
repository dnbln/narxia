# `dir-structure`

A library to make it easier to represent directory structures as plain Rust structures, and reducing the boiler-plate associated with
creating and manipulating them.

A quick example:

```rust ,no_run
#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use dir_structure::traits::sync::DirStructureItem;
    let path = std::path::Path::new("my_dir");
    // to read a `Dir` instance
    let dir = Dir::read(path)?;
    // to write a `Dir` instance
    dir.write(path)?;
    
    // or simply with a struct literal
    Dir {
        input: "Hello, world!".to_string(),
        output: "Hello, world!".to_string(),
    }.write(path)?;
    
    Ok(())
}
```

[This blog post][blog post] goes a bit more in-depth about the more advanced features of the library;
see also [the guides in the narxia DX documentation](https://nrx.dnbln.dev/docs/dx/dir-structure) and
[the examples](dir-structure/examples) & [dir-structure-tools examples](dir-structure-tools/examples).

Here is a quick run-down:

- Virtual file systems, so it doesn't depend on the actual file system.
- Support for async I/O (`async` + `tokio` features). Other (custom) async runtimes can be supported by implementing `VfsAsync` and optionally `WriteSupportingVfsAsync`.
- Support for reading and writing `serde` types with the `Json<T>`, `Toml<T>`, `Yaml<T>`, `Ron<T>` wrappers if the corresponding features are enabled.
- Support for `DirChildren` and `DirDescendants` to easily navigate and manipulate dynamic-layout directory structures.

Optimizations:
- Support for deferred reads via `DeferredRead` and deferred + cached reads via `DeferredReadOrOwn`.
- Support for versioning of file contents while in-memory via `Versioned` / `VersionedHash`, so if we want to later write the entire structure to disk,
  we will only write the modified parts.

## Analogy to `serde`

Conceptually, the library is similar to [serde](https://serde.rs/), but for directory structures instead of data formats.
Here are the analogies between `dir-structure` and `serde`:

| Concept              | `dir-structure`                                             | `serde`                                                     |
|----------------------|-------------------------------------------------------------|-------------------------------------------------------------|
| Macros               | `#[derive(DirStructure)]`                                   | `#[derive(Serialize, Deserialize)]`                         |
| Dematerialization    | `ReadFrom::read_from`                                       | `Deserialize::deserialize`                                  |
| Materialization      | `WriteTo::write_to`                                         | `Serialize::serialize`                                      |
| Backends             | Implementations of the VFS traits                           | Implementations of the `Serializer` / `Deserializer` traits |
| Dematerializer       | `dir_structure::traits::vfs::Vfs`                           | `serde::de::Deserializer`                                   |
| Materializer         | `dir_structure::traits::vfs::WriteSupportingVfs`            | `serde::ser::Serializer`                                    |

Both `dir-structure` and `serde` are designed to abstract away the underlying storage format (file system, JSON, XML, etc.) from the data structure itself,
allowing developers to focus on defining their data structures without worrying about how they are stored or transmitted.

A small caveat is that, because it inherently works with I/O, `dir-structure` supports asynchronous I/O natively as well, while `serde` does not have built-in support for async I/O.

## Note on async I/O

The traits powering the asynchronous I/O support in `dir-structure` are:

- `VfsAsync`
- `WriteSupportingVfsAsync`
- `ReadFromAsync`
- `WriteToAsync` and `WriteToAsyncRef`

They are mostly analogous to their synchronous counterparts.

[blog post]: https://dnbln.dev/blog/dir-structure
