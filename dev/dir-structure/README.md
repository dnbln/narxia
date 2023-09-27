# `dir-structure`

A library to make it easier to represent
directory structures as plain Rust structures,
and reducing the boiler-plate associated with
creating and manipulating them.

A quick example:

```rust
#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use dir_structure::DirStructureItem;
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
the more advanced features of the library.

[blog post]: https://blog.dnbln.dev/dir-structure/
