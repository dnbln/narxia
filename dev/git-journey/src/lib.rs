use std::fmt::Write as _;

pub extern crate git2;

use git2::Repository;

#[derive(Debug)]
pub enum Extract {
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

pub fn collect(repo: &Repository) -> Result<Vec<(git2::Oid, Extract)>, git2::Error> {
    let mut commit = repo.head()?.peel_to_commit()?;

    let mut docs = Vec::new();

    let commit_message = commit
        .message()
        .ok_or(git2::Error::from_str("Failed to get commit message"))?;
    docs.push((
        commit.id(),
        Extract::End {
            content: commit_message.to_owned(),
        },
    ));
    if commit.parent_count() == 0 {
        return Err(git2::Error::from_str(
            "No parent commit found, cannot collect documentation",
        ));
    }
    commit = commit.parent(0)?;

    loop {
        let tree = commit.tree()?;
        let source = tree
            .get_name("source")
            .ok_or_else(|| git2::Error::from_str("Failed to get source file"))?;
        let obj = source.to_object(repo)?;
        let blob = obj
            .into_blob()
            .map_err(|_| git2::Error::from_str("Expected blob object"))?;
        let content = String::from_utf8(blob.content().to_vec())
            .map_err(|_| git2::Error::from_str("Failed to convert blob content to UTF-8"))?;
        let message = commit
            .message()
            .ok_or_else(|| git2::Error::from_str("Failed to get commit message"))?;

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
            message[pos + "git-journey-pre:\n".len()..pos + "git-journey-pre:\n".len() + end]
                .to_string()
        });
        let post = message_lowercase.find("git-journey-post:\n").map(|pos| {
            let end = message_lowercase[pos + "git-journey-post:\n".len()..]
                .find("git-journey-end:")
                .unwrap_or(message_lowercase.len() - pos - "git-journey-post:\n".len());
            message[pos + "git-journey-post:\n".len()..pos + "git-journey-post:\n".len() + end]
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
        commit = commit.parent(0)?;
    }

    Ok(docs)
}

pub fn render(docs: &[(git2::Oid, Extract)]) -> String {
    let mut output = String::new();
    for (_commit, doc) in docs.iter().rev() {
        match doc {
            Extract::Begin { content } => {
                writeln!(&mut output, "{content}").unwrap();
            }
            Extract::Step { pre, post, file } => {
                if let Some(pre) = pre {
                    writeln!(&mut output, "{pre}").unwrap();
                }
                writeln!(&mut output, "{file}").unwrap();
                if let Some(post) = post {
                    writeln!(&mut output, "{post}").unwrap();
                }
            }
            Extract::End { content } => {
                writeln!(&mut output, "{content}").unwrap();
            }
        }
    }
    output
}
