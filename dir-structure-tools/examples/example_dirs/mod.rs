use std::path::PathBuf;

pub fn get_example_dir_path(name: &str) -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let mut path = PathBuf::from(&manifest_dir);
    path.push("examples");
    path.push("example_dirs");
    path.push(name);
    path
}
