//! A virtual filesystem implementation that reads from a git repository.

use std::io;
use std::io::BufRead;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::error::Error;
use crate::error::Result;
use crate::error::VfsResult;
use crate::traits::vfs;
use crate::traits::vfs::PathType;
use crate::traits::vfs::VfsCore;

/// A virtual filesystem that reads from a git repository.
pub struct GitVfs<'r> {
    repo: &'r git2::Repository,
    tree: git2::Tree<'r>,
}

impl<'r> GitVfs<'r> {
    /// Create a new `GitVfs` from a git repository and a tree.
    pub fn new(repo: &'r git2::Repository, tree: git2::Tree<'r>) -> Self {
        Self { repo, tree }
    }
}

impl<'r> VfsCore for GitVfs<'r> {
    type Path = Path;
}

impl<'r> vfs::Vfs<'r> for GitVfs<'r> {
    type DirWalk<'a>
        = GitDirWalk
    where
        'r: 'a,
        Self: 'a;

    type RFile = GitRFile<'r>;

    fn open_read(self: Pin<&Self>, path: &Path) -> VfsResult<Self::RFile, Self> {
        let entry = self
            .tree
            .get_path(path)
            .map_err(|e| Error::Parse(path.to_path_buf(), Box::new(e)))?;
        let blob = entry
            .to_object(self.repo)
            .map_err(|e| Error::Parse(path.to_path_buf(), Box::new(e)))?
            .into_blob()
            .map_err(|e| {
                Error::Parse(
                    path.to_path_buf(),
                    format!("Object is not blob: {:?}", e.kind().unwrap()).into(),
                )
            })?;
        Ok(GitRFile { blob, offset: 0 })
    }

    fn read(self: Pin<&Self>, path: &Path) -> VfsResult<Vec<u8>, Self> {
        let entry = self
            .tree
            .get_path(path)
            .map_err(|e| Error::Parse(path.to_path_buf(), Box::new(e)))?;
        let blob = entry
            .to_object(self.repo)
            .map_err(|e| Error::Parse(path.to_path_buf(), Box::new(e)))?
            .into_blob()
            .map_err(|e| {
                Error::Parse(
                    path.to_path_buf(),
                    format!("Object is not blob: {:?}", e.kind().unwrap()).into(),
                )
            })?;
        Ok(blob.content().to_vec())
    }

    fn exists(self: Pin<&Self>, path: &Path) -> VfsResult<bool, Self> {
        Ok(self.tree.get_path(path).is_ok())
    }

    fn is_dir(self: Pin<&Self>, path: &Self::Path) -> VfsResult<bool, Self> {
        match self.tree.get_path(path) {
            Ok(entry) => Ok(entry.kind() == Some(git2::ObjectType::Tree)),
            Err(_) => Ok(false),
        }
    }

    fn walk_dir<'a>(self: Pin<&'a Self>, path: &Path) -> VfsResult<Self::DirWalk<'a>, Self>
    where
        'r: 'a,
    {
        let mut collector = Vec::new();
        self.tree
            .walk(git2::TreeWalkMode::PreOrder, |root, entry| {
                // We don't actually care about the callback, we just want the iterator.
                let root_path = PathBuf::from(root);
                if !path.starts_with(&root_path) {
                    return git2::TreeWalkResult::Skip;
                }
                let entry_path = match entry.name() {
                    Some(name) => {
                        let mut ep = PathBuf::from(root);
                        ep.push(name);
                        ep
                    }
                    None => {
                        // not valid UTF-8 in file name, skip
                        return git2::TreeWalkResult::Skip;
                    }
                };
                if root_path == path {
                    let name = match entry_path.file_name() {
                        Some(f) => f.to_os_string(),
                        None => {
                            // no last component, skip
                            return git2::TreeWalkResult::Skip;
                        }
                    };
                    let kind = match entry.kind() {
                        Some(git2::ObjectType::Blob) => vfs::DirEntryKind::File,
                        Some(git2::ObjectType::Tree) => vfs::DirEntryKind::Directory,
                        Some(_ot) => {
                            // unsupported git object type, skip
                            return git2::TreeWalkResult::Skip;
                        }
                        None => {
                            // unknown git object type, skip
                            return git2::TreeWalkResult::Skip;
                        }
                    };
                    collector.push(vfs::DirEntryInfo {
                        name,
                        kind,
                        path: entry_path,
                    });
                    git2::TreeWalkResult::Ok
                } else if path.starts_with(&entry_path) {
                    git2::TreeWalkResult::Ok
                } else {
                    git2::TreeWalkResult::Skip
                }
            })
            .map_err(|e| Error::Parse(path.to_path_buf(), Box::new(e)))?;
        let mut iter = collector.into_iter();
        Ok(GitDirWalk {
            next: Box::new(move || iter.next()),
        })
    }
}

/// A directory walker that reads from a git tree.
pub struct GitDirWalk {
    next: Box<dyn FnMut() -> Option<vfs::DirEntryInfo<Path>>>,
}

impl<'r> vfs::DirWalker<'r> for GitDirWalk {
    type P = Path;

    fn next(
        &mut self,
    ) -> Option<Result<vfs::DirEntryInfo<Self::P>, <Self::P as PathType>::OwnedPath>> {
        Some(Ok((self.next)()?))
    }
}

/// A read-only file that reads from a git blob.
pub struct GitRFile<'r> {
    blob: git2::Blob<'r>,
    offset: usize,
}

impl BufRead for GitRFile<'_> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        let data = self.blob.content();
        Ok(&data[self.offset..])
    }

    fn consume(&mut self, amt: usize) {
        self.offset += amt;
    }
}

impl Read for GitRFile<'_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let data = self.blob.content();
        let remaining = &data[self.offset..];
        let to_read = buf.len().min(remaining.len());
        buf[..to_read].copy_from_slice(&remaining[..to_read]);
        self.offset += to_read;
        Ok(to_read)
    }
}

#[cfg(test)]
mod tests {
    use std::io::Read;
    use std::path::Path;
    use std::pin::Pin;

    use crate::prelude::Vfs;
    use crate::traits::vfs;
    use crate::traits::vfs::DirWalker;
    use crate::vfs::git_vfs::GitVfs;

    fn open_repo() -> git2::Repository {
        git2::Repository::open_from_env().expect("Failed to open git repository")
    }

    #[test]
    fn test_read() {
        let repo = open_repo();
        let tree = repo
            .head()
            .expect("Failed to get HEAD")
            .peel_to_tree()
            .expect("Failed to get tree");
        let vfs = GitVfs { repo: &repo, tree };
        let vfs = Pin::new(&vfs);

        let content = vfs
            .read_string(Path::new("dir-structure/README.md"))
            .expect("Failed to read README.md");
        assert_eq!(content, include_str!("../../README.md"));
    }

    #[test]
    fn test_exists() {
        let repo = open_repo();
        let tree = repo
            .head()
            .expect("Failed to get HEAD")
            .peel_to_tree()
            .expect("Failed to get tree");
        let vfs = GitVfs { repo: &repo, tree };
        let vfs = Pin::new(&vfs);

        assert!(
            vfs.exists(Path::new("dir-structure/README.md"))
                .expect("Failed to check existence")
        );
        assert!(
            !vfs.exists(Path::new("NON_EXISTENT_FILE"))
                .expect("Failed to check existence")
        );
    }

    #[test]
    fn test_open_read() {
        let repo = open_repo();
        let tree = repo
            .head()
            .expect("Failed to get HEAD")
            .peel_to_tree()
            .expect("Failed to get tree");
        let vfs = GitVfs { repo: &repo, tree };
        let vfs = Pin::new(&vfs);

        let mut file = vfs
            .open_read(Path::new("dir-structure/README.md"))
            .expect("Failed to open README.md");
        let mut content = String::new();
        file.read_to_string(&mut content)
            .expect("Failed to read README.md");
        assert_eq!(content, include_str!("../../README.md"));
    }

    #[test]
    fn test_walk_dir() {
        let repo = open_repo();
        let tree = repo
            .head()
            .expect("Failed to get HEAD")
            .peel_to_tree()
            .expect("Failed to get tree");
        let vfs = GitVfs { repo: &repo, tree };
        let vfs = Pin::new(&vfs);
        let mut walker = vfs.walk_dir(Path::new("src")).expect("Failed to walk dir");
        let mut entries = Vec::new();
        while let Some(entry) = walker.next() {
            entries.push(entry.expect("error while walking dir"));
        }

        entries.sort_by_key(|e| e.name.clone());

        assert_eq!(
            entries,
            vec![
                vfs::DirEntryInfo {
                    name: "compiler".into(),
                    kind: vfs::DirEntryKind::Directory,
                    path: Path::new("src/compiler").into(),
                },
                vfs::DirEntryInfo {
                    name: "dev".into(),
                    kind: vfs::DirEntryKind::Directory,
                    path: Path::new("src/dev").into(),
                },
                vfs::DirEntryInfo {
                    name: "lib".into(),
                    kind: vfs::DirEntryKind::Directory,
                    path: Path::new("src/lib").into(),
                },
            ]
        );

        let mut walker = vfs
            .walk_dir(Path::new("src/dev/narxia-workspace"))
            .expect("Failed to walk dir");
        let mut entries = Vec::new();
        while let Some(entry) = walker.next() {
            entries.push(entry.expect("error while walking dir"));
        }

        entries.sort_by_key(|e| e.name.clone());

        assert_eq!(
            entries,
            vec![
                vfs::DirEntryInfo {
                    name: "Cargo.toml".into(),
                    kind: vfs::DirEntryKind::File,
                    path: Path::new("src/dev/narxia-workspace/Cargo.toml").into(),
                },
                vfs::DirEntryInfo {
                    name: "src".into(),
                    kind: vfs::DirEntryKind::Directory,
                    path: Path::new("src/dev/narxia-workspace/src").into(),
                },
            ]
        );
    }
}
