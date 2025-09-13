//! A virtual filesystem implementation that reads from a git repository.

use std::io;
use std::io::BufRead;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::error::Error;
use crate::error::Result;
use crate::traits::vfs;

/// A virtual filesystem that reads from a git repository.
pub struct GitVfs<'r> {
    repo: &'r git2::Repository,
    tree: git2::Tree<'r>,
}

impl<'r> vfs::Vfs<'r> for GitVfs<'r> {
    type DirWalk<'a>
        = GitDirWalk
    where
        'r: 'a,
        Self: 'a;

    type RFile = GitRFile<'r>;

    fn open_read(self: Pin<&Self>, path: &Path) -> Result<Self::RFile> {
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

    fn read(self: Pin<&Self>, path: &Path) -> Result<Vec<u8>> {
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

    fn exists(self: Pin<&Self>, path: &Path) -> Result<bool> {
        Ok(self.tree.get_path(path).is_ok())
    }

    fn walk_dir<'a>(self: Pin<&'a Self>, path: &Path) -> Result<Self::DirWalk<'a>>
    where
        'r: 'a,
    {
        let mut collector = Vec::new();
        self.tree
            .walk(git2::TreeWalkMode::PreOrder, |root, entry| {
                // We don't actually care about the callback, we just want the iterator.
                let root_path = PathBuf::from(root);
                if !path.starts_with(&root_path) || !root_path.starts_with(path) {
                    return git2::TreeWalkResult::Skip;
                }
                if root_path == path {
                    let entry_path = match entry.name() {
                        Some(name) => PathBuf::from(name),
                        None => {
                            // not valid UTF-8 in file name, skip
                            return git2::TreeWalkResult::Skip;
                        }
                    };
                    let name = entry_path
                        .file_name()
                        .expect("file name is not valid")
                        .to_os_string();
                    let kind = match entry.kind() {
                        Some(git2::ObjectType::Blob) => vfs::DirEntryKind::File,
                        Some(git2::ObjectType::Tree) => vfs::DirEntryKind::Directory,
                        Some(ot) => {
                            // unsupported git object type, skip
                            eprintln!(
                                "Unsupported git object type for entry {:?}: {:?}",
                                entry_path, ot
                            );
                            return git2::TreeWalkResult::Skip;
                        }
                        None => {
                            // unknown git object type, skip
                            eprintln!("Unknown git object type for entry {:?}", entry_path);
                            return git2::TreeWalkResult::Skip;
                        }
                    };
                    collector.push(vfs::DirEntryInfo {
                        name,
                        kind,
                        path: entry_path,
                    });
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
    next: Box<dyn FnMut() -> Option<vfs::DirEntryInfo>>,
}

impl<'r> vfs::DirWalker<'r> for GitDirWalk {
    fn next(&mut self) -> Option<Result<vfs::DirEntryInfo>> {
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
