//! Virtual file system traits.

use std::ffi::OsString;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::error::Error;
use crate::error::Result;

/// A virtual file system. Writing operations are provided by the [`WriteSupportingVfs` trait](self::WriteSupportingVfs).
pub trait Vfs {
    /// The type of the directory walker returned by the [`walk_dir` method](Vfs::walk_dir).
    type DirWalk: DirWalker;

    /// Reads the contents of a file, at the specified path.
    fn read(self: Pin<&Self>, path: &Path) -> Result<Vec<u8>>;
    /// Reads the contents of a file, at the specified path, and returns it as a string.
    fn read_string(self: Pin<&Self>, path: &Path) -> Result<String> {
        self.read(path).and_then(|bytes| {
            String::from_utf8(bytes).map_err(|e| Error::Parse(path.to_path_buf(), Box::new(e)))
        })
    }
    /// Checks if a file exists at the specified path.
    fn exists(self: Pin<&Self>, path: &Path) -> Result<bool>;
    /// Walks a directory at the specified path, returning a stream of directory entries.
    fn walk_dir(self: Pin<&Self>, path: &Path) -> Result<Self::DirWalk>;
}

/// A virtual file system that supports writing operations.
pub trait WriteSupportingVfs: Vfs {
    /// Writes the data to a file, to the specified path.
    fn write(self: Pin<&Self>, path: &Path, data: &[u8]) -> Result<()>;
    /// Removes a directory and all its contents.
    fn remove_dir_all(self: Pin<&Self>, path: &Path) -> Result<()>;
    /// Creates a new directory at the specified path.
    fn create_dir(self: Pin<&Self>, path: &Path) -> Result<()>;
    /// Creates a new directory and all its parent directories at the specified path.
    fn create_dir_all(self: Pin<&Self>, path: &Path) -> Result<()>;
    /// Creates the parent directory for the specified path, if it does not exist.
    fn create_parent_dir(self: Pin<&Self>, path: &Path) -> Result<()> {
        if let Some(parent) = path.parent()
            && !self.exists(parent)?
        {
            self.create_dir_all(parent)?;
        }
        Ok(())
    }
}

/// The type of a directory entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DirEntryKind {
    /// A regular file.
    File,
    /// A directory.
    Directory,
}

impl DirEntryKind {
    /// Returns true if the entry is a file.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::traits::vfs::DirEntryKind;
    ///
    /// let entry = DirEntryKind::File;
    /// assert!(entry.is_file());
    ///
    /// let entry = DirEntryKind::Directory;
    /// assert!(!entry.is_file());
    /// ```
    pub fn is_file(self) -> bool {
        matches!(self, DirEntryKind::File)
    }

    /// Returns true if the entry is a directory.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::traits::vfs::DirEntryKind;
    ///
    /// let entry = DirEntryKind::File;
    /// assert!(!entry.is_dir());
    ///
    /// let entry = DirEntryKind::Directory;
    /// assert!(entry.is_dir());
    /// ```
    pub fn is_dir(self) -> bool {
        matches!(self, DirEntryKind::Directory)
    }
}

/// Information about a directory entry.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirEntryInfo {
    /// The name of the entry.
    pub name: OsString,
    /// The path of the entry.
    pub path: PathBuf,
    /// The kind of the entry.
    pub kind: DirEntryKind,
}

/// A trait for walking a directory.
///
/// Behaves similarly to an [`Iterator`] over Result<[`DirEntryInfo`]>.
pub trait DirWalker {
    /// Returns the next directory entry.
    fn next(&mut self) -> Option<Result<DirEntryInfo>>;
}
