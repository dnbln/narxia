//! An example demonstrating the use of [`resolve_path`] to resolve a specific path within a directory structure.
//!
//! Requires the `resolve-path` feature to be enabled.

use dir_structure::traits::resolve::resolve_path;

mod example_dirs;

#[derive(dir_structure::DirStructure, dir_structure::HasField)]
pub struct ComplexRoot {
    a: A,
    b: B,
    c: C,
}

#[derive(dir_structure::DirStructure, dir_structure::HasField)]
pub struct A {
    #[dir_structure(path = "a1.txt")]
    a1: String,
    #[dir_structure(path = "a2.txt")]
    a2: String,
}

#[derive(dir_structure::DirStructure, dir_structure::HasField)]
pub struct B {
    #[dir_structure(path = "b1.txt")]
    b1: String,
    #[dir_structure(path = "b2.txt")]
    b2: String,
}

#[derive(dir_structure::DirStructure, dir_structure::HasField)]
pub struct C {
    #[dir_structure(path = "c1.txt")]
    c1: String,
    #[dir_structure(path = "c2.txt")]
    c2: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = example_dirs::get_example_dir_path("complex");
    // resolve_path! macro can be used to get the path to a specific field in a directory structure, from a given root path.
    let a1 = resolve_path!([path.clone() as ComplexRoot].a.a1);
    let b2 = resolve_path!([path.clone() as ComplexRoot].b.b2);
    // also accepts [Type @ path] syntax
    let c1 = resolve_path!([ComplexRoot @ path.clone()].c.c1);

    println!("a1: {}", a1.display());
    println!("b2: {}", b2.display());
    println!("c1: {}", c1.display());

    assert_eq!(a1, path.join("a").join("a1.txt"));
    assert_eq!(b2, path.join("b").join("b2.txt"));
    assert_eq!(c1, path.join("c").join("c1.txt"));

    Ok(())
}
