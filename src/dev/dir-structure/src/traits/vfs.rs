//! Virtual file system traits.

use std::ffi::OsString;
use std::io::BufRead;
use std::io::Seek;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::error::Error;
use crate::error::Result;
use crate::error::WrapIoError as _;
use crate::prelude::*;

/// A virtual file system. Writing operations are provided by the [`WriteSupportingVfs` trait](self::WriteSupportingVfs).
pub trait Vfs {
    /// The type of the directory walker returned by the [`walk_dir` method](Vfs::walk_dir).
    type DirWalk: DirWalker;
    /// The type of the file returned by the [`open_read` method](Vfs::open_read).
    type RFile: BufRead;

    /// Opens a file for reading, at the specified path.
    fn open_read(self: Pin<&Self>, path: &Path) -> Result<Self::RFile>;

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

/// Extension trait for [`Vfs`] that provides additional convenience methods.
pub trait VfsExt: Vfs {
    /// Reads a file / directory at the specified path, and parses it into the specified type using its
    /// [`ReadFrom`] implementation.
    ///
    /// This method takes `self` as a pinned reference, to ensure that the `Vfs` implementation
    /// is not moved while the read operation is in progress.
    fn read_typed_pinned<'a, T: ReadFrom<'a, Self>>(
        self: Pin<&'a Self>,
        path: impl AsRef<Path>,
    ) -> Result<T> {
        T::read_from(path.as_ref(), self)
    }

    /// Reads a file / directory at the specified path, and parses it into the specified type using its
    /// [`ReadFrom`] implementation.
    ///
    /// This method takes `self` as a regular reference, and pins it internally.
    fn read_typed<'a, T: ReadFrom<'a, Self>>(&'a self, path: impl AsRef<Path>) -> Result<T>
    where
        Self: Unpin,
    {
        Pin::new(self).read_typed_pinned(path)
    }
}

// Blanket impl.
impl<V: Vfs + ?Sized> VfsExt for V {}

/// Marks that the [`RFile`](Vfs::RFile) type of this [`Vfs`] also implements [`Seek`](std::io::Seek),
/// allowing it to be used in contexts that require seeking, such as image decoding.
///
/// This trait is automatically implemented for any [`Vfs`] whose [`RFile`](Vfs::RFile) implements [`Seek`](std::io::Seek).
pub trait VfsWithSeekRead: Vfs
where
    Self::RFile: Seek,
{
}

impl<T: Vfs> VfsWithSeekRead for T where T::RFile: Seek {}

/// A virtual file system that supports writing operations.
pub trait WriteSupportingVfs: Vfs {
    /// The type of the file returned by the [`open_write` method](WriteSupportingVfs::open_write).
    type WFile: Write;

    /// Opens a file for writing, at the specified path.
    fn open_write(self: Pin<&Self>, path: &Path) -> Result<Self::WFile>;

    /// Writes the data to a file, to the specified path.
    fn write(self: Pin<&Self>, path: &Path, data: &[u8]) -> Result<()> {
        self.open_write(path)?
            .write_all(data)
            .wrap_io_error_with(path)
    }
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

/// Extension trait for [`WriteSupportingVfs`] that provides additional convenience methods.
pub trait WriteSupportingVfsExt: WriteSupportingVfs {
    /// Writes a file / directory at the specified path, using the specified data type's
    /// [`WriteTo`] implementation.
    ///
    /// This method takes `self` as a pinned reference, to ensure that the `Vfs` implementation
    /// is not moved while the write operation is in progress.
    fn write_typed_pinned<'a, T: WriteTo<Self>>(
        self: Pin<&'a Self>,
        path: impl AsRef<Path>,
        value: &T,
    ) -> Result<()> {
        value.write_to(path.as_ref(), self)
    }

    /// Writes a file / directory at the specified path, using the specified data type's
    /// [`WriteTo`] implementation.
    ///
    /// This method takes `self` as a regular reference, and pins it internally.
    fn write_typed<'a, T: WriteTo<Self>>(&'a self, path: impl AsRef<Path>, value: &T) -> Result<()>
    where
        Self: Unpin,
    {
        Pin::new(self).write_typed_pinned(path, value)
    }
}

// Blanket impl.
impl<Vfs: WriteSupportingVfs + ?Sized> WriteSupportingVfsExt for Vfs {}

/// Marks that the [`WFile`](WriteSupportingVfs::WFile) type of this [`WriteSupportingVfs`] also implements [`Seek`](std::io::Seek),
/// allowing it to be used in contexts that require seeking.
///
/// This trait is automatically implemented for any [`WriteSupportingVfs`] whose [`WFile`](WriteSupportingVfs::WFile) implements [`Seek`](std::io::Seek).
pub trait VfsWithSeekWrite: WriteSupportingVfs
where
    Self::WFile: Seek,
{
}

impl<T: WriteSupportingVfs> VfsWithSeekWrite for T where T::WFile: Seek {}

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
