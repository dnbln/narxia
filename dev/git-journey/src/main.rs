use std::fmt::Write as _;
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

#[derive(Debug)]
enum Extract {
    Begin {
        content: String,
    },
    Step {
        pre: Option<String>,
        post: Option<String>,
        file: String,
    },
    End {
        content: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli {
        Cli::DocExtract { path } => {
            // println!("Extracting documentation from {:?}", path);
            let repo = git2::Repository::open(path).expect("Failed to open repository");
            let mut commit = repo
                .head()
                .expect("Failed to get HEAD")
                .peel_to_commit()
                .expect("Failed to peel HEAD to commit");

            let mut docs = Vec::new();

            let commit_message = commit.message().unwrap_or("No commit message");
            docs.push((
                commit.id(),
                Extract::End {
                    content: commit_message.to_owned(),
                },
            ));
            if commit.parent_count() == 0 {
                panic!("No parent commit found for HEAD");
            }
            commit = commit.parent(0).expect("Failed to get parent commit");

            loop {
                let tree = commit.tree().expect("Failed to get tree from commit");
                let source = tree.get_name("source").expect("Failed to get source file");
                let obj = source
                    .to_object(&repo)
                    .expect("Failed to get source object");
                let blob = obj.into_blob().expect("Expected blob object");
                let content = String::from_utf8(blob.content().to_vec())
                    .expect("Invalid UTF-8 in blob content");
                let message = commit.message().unwrap_or("No commit message");

                if message.starts_with("git-journey-begin:\n") {
                    let content = message["git-journey-begin:\n".len()..].to_string();
                    docs.push((commit.id(), Extract::Begin { content }));
                    break;
                }

                let message_lowercase = message.to_lowercase();
                let pre = message_lowercase.find("git-journey-pre:\n").map(|pos| {
                    let end = message_lowercase[pos + "git-journey-pre:\n".len()..]
                        .find("git-journey-post:")
                        .unwrap_or(message_lowercase.len() - pos - "git-journey-pre:\n".len());
                    message
                        [pos + "git-journey-pre:\n".len()..pos + "git-journey-pre:\n".len() + end]
                        .to_string()
                });
                let post = message_lowercase.find("git-journey-post:\n").map(|pos| {
                    let end = message_lowercase[pos + "git-journey-post:\n".len()..]
                        .find('\n')
                        .unwrap_or(message_lowercase.len() - pos - "git-journey-post:\n".len());
                    message
                        [pos + "git-journey-post:\n".len()..pos + "git-journey-post:\n".len() + end]
                        .to_string()
                });
                docs.push((
                    commit.id(),
                    Extract::Step {
                        pre,
                        post,
                        file: content,
                    },
                ));

                if commit.parent_count() == 0 {
                    break;
                }
                commit = commit.parent(0).expect("Failed to get parent commit");
            }

            let mut out = String::new();
            for (commit, doc) in docs.iter().rev() {
                // println!("Commit {}:\n{:?}", commit, doc);
                match doc {
                    Extract::Begin { content } => {
                        writeln!(&mut out, "{content}").unwrap();
                    }
                    Extract::Step { pre, post, file } => {
                        if let Some(pre) = pre {
                            writeln!(&mut out, "{pre}").unwrap();
                        }
                        writeln!(&mut out, "{file}").unwrap();
                        if let Some(post) = post {
                            writeln!(&mut out, "{post}").unwrap();
                        }
                    }
                    Extract::End { content } => {
                        writeln!(&mut out, "{content}").unwrap();
                    }
                }
            }

            println!("{}", out);
        }
    }
}
