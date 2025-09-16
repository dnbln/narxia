//! An example demonstrating the use of [`Toml`](dir_structure::data_formats::toml::Toml) to read and write TOML files.
//!
//! Requires the `derive` and `toml` features to be enabled.

use std::error::Error;

use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "data.toml", with_newtype = dir_structure::data_formats::toml::Toml<toml::Value>)]
    data: toml::Value,
}

fn main() -> Result<(), Box<dyn Error>> {
    let path = example_dirs::get_example_dir_path("toml_rw");

    let dir = Dir::read(&path)?;

    println!("Read TOML data: {:?}", dir.data);

    let new_data = toml::Value::Table(toml::toml! {
        new_key = "new_value"
        number = 42
        array = [1, 2, 3]
    });

    let new_dir = Dir { data: new_data };

    new_dir.write(&path)?;

    println!(
        "Wrote new TOML data to {}",
        path.join("data.toml").display()
    );

    Ok(())
}
