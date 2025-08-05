use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
enum Cli {
    /// Run the doc extraction tool
    DocExtract {
        /// Path to the git repository of the tutorial
        path: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli {
        Cli::DocExtract { path } => {
            let repo = git2::Repository::open(path).expect("Failed to open repository");
            let docs = git_journey::collect(&repo).expect("Failed to collect documentation");

            let out = git_journey::render(&docs);
            println!("{out}");
        }
    }
}
