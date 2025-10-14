//! Example of reading from a git repository using [`GitVfs`].
//!
//! This example assumes you have a git repository set up with files `input.txt` and `output.txt`.
//! You can modify the `setup_repo` function to create such a repository for testing purposes.
//!
//! The [`GitVfs`] needs a reference to the [`git2::Repository`] and a [`git2::Tree`] to read from.
//!
//! Requires the `git` feature to be enabled.

use std::error::Error;
use std::fs;
use std::path::Path;
use std::pin::Pin;

use dir_structure::prelude::*;
use dir_structure::vfs::git_vfs::GitVfs;
use git2::Index;

mod example_dirs;

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "input.txt")]
    input: String,
    #[dir_structure(path = "output.txt")]
    output: String,
}

fn setup_repo() -> Result<git2::Repository, Box<dyn Error>> {
    let dir = example_dirs::get_example_dir_path("temp_git_repo");
    if dir.exists() {
        fs::remove_dir_all(&dir)?;
    }
    fs::create_dir_all(&dir)?;
    let repo = git2::Repository::init(&dir)?;
    fs::write(dir.join("input.txt"), "This is input")?;
    fs::write(dir.join("output.txt"), "This is output")?;
    let mut index = repo.index()?;
    index.add_path(std::path::Path::new("input.txt"))?;
    index.add_path(std::path::Path::new("output.txt"))?;
    Index::write(&mut index)?;
    let tree_id = index.write_tree()?;
    {
        let tree = repo.find_tree(tree_id)?;
        let sig = repo.signature()?;
        repo.commit(Some("HEAD"), &sig, &sig, "Initial commit", &tree, &[])?;
    }
    Ok(repo)
}

fn main() -> Result<(), Box<dyn Error>> {
    let repo = setup_repo()?;
    let vfs = GitVfs::new(&repo, repo.head()?.peel_to_tree()?);

    let dir = Dir::read_from(Path::new(""), Pin::new(&vfs))?;

    println!("input: {}", dir.input);
    println!("output: {}", dir.output);

    Ok(())
}
