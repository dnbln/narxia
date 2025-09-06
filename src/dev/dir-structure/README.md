# `dir-structure`

A library to make it easier to represent
directory structures as plain Rust structures,
and reducing the boiler-plate associated with
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

[This blog post][blog post] goes a bit more in-depth about
the more advanced features of the library; see also [the guides in the narxia DX documentation](https://nrx.dnbln.dev/docs/dx/dir-structure).

Here is a quick run-down:

- Virtual file systems, so it doesn't depend on the actual file system.
- Support for async I/O (`async` + `tokio` features). Other (custom) async runtimes can be supported by implementing `VfsAsync` and optionally `WriteSupportingVfsAsync`.
- Support for reading and writing `serde` types with the `Json<T>`, `Toml<T>`, `Yaml<T>`, `Ron<T>` wrappers if that the corresponding features are enabled.
- Support for `DirChildren` and `DirDescendants` to easily navigate and manipulate dynamic-layout directory structures.

Optimizations:
- Support for deferred reads via `DeferredRead` and deferred + cached reads via `DeferredReadOrOwn`.
- Support for versioning of file contents while in-memory via `Versioned` / `VersionedHash`, so if we want to later write the entire structure to disk,
  we will only write the modified parts.

[blog post]: https://dnbln.dev/blog/dir-structure
