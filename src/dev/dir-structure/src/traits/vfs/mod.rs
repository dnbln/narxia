use std::ffi::OsString;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::Result;

pub trait Vfs: Clone {
    type DirWalk: DirWalker;

    fn read(self: Pin<&Self>, path: &Path) -> Result<Vec<u8>>;
    fn read_string(self: Pin<&Self>, path: &Path) -> Result<String>;
    fn write(self: Pin<&Self>, path: &Path, data: &[u8]) -> Result<()>;
    fn exists(self: Pin<&Self>, path: &Path) -> Result<bool>;
    fn remove_dir_all(self: Pin<&Self>, path: &Path) -> Result<()>;
    fn create_dir(self: Pin<&Self>, path: &Path) -> Result<()>;
    fn create_dir_all(self: Pin<&Self>, path: &Path) -> Result<()>;
    fn create_parent_dir(self: Pin<&Self>, path: &Path) -> Result<()> {
        if let Some(parent) = path.parent() {
            if !self.exists(parent)? {
                self.create_dir_all(parent)?;
            }
        }
        Ok(())
    }
    fn walk_dir(self: Pin<&Self>, path: &Path) -> Result<Self::DirWalk>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DirEntryKind {
    File,
    Directory,
}

impl DirEntryKind {
    pub fn is_file(self) -> bool {
        matches!(self, DirEntryKind::File)
    }

    pub fn is_dir(self) -> bool {
        matches!(self, DirEntryKind::Directory)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirEntryInfo {
    pub name: OsString,
    pub path: PathBuf,
    pub kind: DirEntryKind,
}

pub trait DirWalker {
    fn next(&mut self) -> Option<Result<DirEntryInfo>>;
}

mod fs_vfs;
pub use fs_vfs::FsVfs;
