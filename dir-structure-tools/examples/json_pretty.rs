//! An example demonstrating the use of [`JsonPretty`](dir_structure::data_formats::json_pretty::JsonPretty) to read and write JSON files.
//!
//! Requires the `derive` and `json` features to be enabled.

use std::error::Error;

use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "data.json", with_newtype = dir_structure_tools::data_formats::json_pretty::JsonPretty<serde_json::Value>)]
    data: serde_json::Value,
}

fn main() -> Result<(), Box<dyn Error>> {
    let path = example_dirs::get_example_dir_path("json_pretty_rw");

    let dir = Dir::read(&path)?;

    println!("Read JSON data: {:?}", dir.data);

    let new_data = serde_json::json!({
        "new_key": "new_value",
        "number": 42,
        "array": [1, 2, 3]
    });

    let new_dir = Dir { data: new_data };

    new_dir.write(&path)?;

    println!(
        "Wrote new JSON data to {}",
        path.join("data.json").display()
    );

    Ok(())
}
