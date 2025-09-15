//! Example of using the [`load_path!`] macro to load specific fields from a directory structure,
//! without loading the entire structure.
//!
//! Requires the `resolve-path` feature to be enabled.

use dir_structure::traits::resolve::load_path;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
pub struct ComplexRoot {
    a: A,
    b: B,
    c: C,
}

#[derive(dir_structure::DirStructure)]
pub struct A {
    #[dir_structure(path = "a1.txt")]
    a1: String,
    #[dir_structure(path = "a2.txt")]
    a2: String,
}

#[derive(dir_structure::DirStructure)]
pub struct B {
    #[dir_structure(path = "b1.txt")]
    b1: String,
    #[dir_structure(path = "b2.txt")]
    b2: String,
}

#[derive(dir_structure::DirStructure)]
pub struct C {
    #[dir_structure(path = "c1.txt")]
    c1: String,
    #[dir_structure(path = "c2.txt")]
    c2: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = example_dirs::get_example_dir_path("complex");
    // resolve_path! macro can be used to load the value for a specific field in a directory structure, from a given root path.
    let a1 = load_path!([path.clone() as ComplexRoot].a.a1)?;
    let b2 = load_path!([path.clone() as ComplexRoot].b.b2)?;
    let c1 = load_path!([path.clone() as ComplexRoot].c.c1)?;

    println!("a1: {}", a1);
    println!("b2: {}", b2);
    println!("c1: {}", c1);

    Ok(())
}
