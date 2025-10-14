//! An example demonstrating the use of [`RonPretty`](dir_structure::data_formats::ron_pretty::RonPretty) to read and write RON files.
//!
//! Requires the `derive` and `ron` features to be enabled.

use std::error::Error;

use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "data.ron", with_newtype = dir_structure_tools::data_formats::ron_pretty::RonPretty<ron::Value>)]
    data: ron::Value,
}

fn main() -> Result<(), Box<dyn Error>> {
    let path = example_dirs::get_example_dir_path("ron_pretty_rw");

    let dir = Dir::read(&path)?;

    println!("Read RON data: {:?}", dir.data);

    let new_data = ron::Value::Map(
        vec![
            (
                ron::Value::String("new_key".to_string()),
                ron::Value::String("new_value".to_string()),
            ),
            (
                ron::Value::String("number".to_string()),
                ron::Value::Number(42.into()),
            ),
            (
                ron::Value::String("array".to_string()),
                ron::Value::Seq(vec![
                    ron::Value::Number(1.into()),
                    ron::Value::Number(2.into()),
                    ron::Value::Number(3.into()),
                ]),
            ),
        ]
        .into_iter()
        .collect(),
    );

    let new_dir = Dir { data: new_data };

    new_dir.write(&path)?;

    println!("Wrote new RON data to {}", path.join("data.ron").display());

    Ok(())
}
