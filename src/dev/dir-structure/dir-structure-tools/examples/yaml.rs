//! An example demonstrating the use of [`Yaml`](dir_structure::data_formats::yaml::Yaml) to read and write YAML files.
//!
//! Requires the `derive` and `yaml` features to be enabled.

use std::error::Error;

use dir_structure::prelude::*;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "data.yaml", with_newtype = dir_structure_tools::data_formats::yaml::Yaml<serde_yaml::Value>)]
    data: serde_yaml::Value,
}

fn main() -> Result<(), Box<dyn Error>> {
    let path = example_dirs::get_example_dir_path("yaml_rw");

    let dir = Dir::read(&path)?;

    println!("Read YAML data: {:?}", dir.data);

    let new_data = serde_yaml::Value::Mapping(serde_yaml::Mapping::from_iter([
        (
            serde_yaml::Value::String("new_key".to_string()),
            serde_yaml::Value::String("new_value".to_string()),
        ),
        (
            serde_yaml::Value::String("number".to_string()),
            serde_yaml::Value::Number(42.into()),
        ),
        (
            serde_yaml::Value::String("array".to_string()),
            serde_yaml::Value::Sequence(vec![
                serde_yaml::Value::Number(1.into()),
                serde_yaml::Value::Number(2.into()),
                serde_yaml::Value::Number(3.into()),
            ]),
        ),
    ]));

    let new_dir = Dir { data: new_data };

    new_dir.write(&path)?;

    println!(
        "Wrote new YAML data to {}",
        path.join("data.yaml").display()
    );

    Ok(())
}
