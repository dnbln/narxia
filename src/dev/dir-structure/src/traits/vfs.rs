//! Virtual file system traits.

use std::error::Error as StdError;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::io::Read;
use std::io::Seek;
use std::io::Write;
use std::path;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
use std::result::Result as StdResult;

use crate::error::Error;
use crate::error::Result;
use crate::error::WrapIoError as _;
use crate::prelude::*;

/// Core trait for a virtual file system, providing the associated path type.
pub trait VfsCore {
    /// The path type used to represent paths in this virtual file system.
    type Path: PathType + ?Sized;
}

/// A virtual file system. Writing operations are provided by the [`WriteSupportingVfs` trait](self::WriteSupportingVfs).
pub trait Vfs<'vfs>: VfsCore + 'vfs {
    /// The type of the directory walker returned by the [`walk_dir` method](Vfs::walk_dir).
    type DirWalk<'a>: DirWalker<'a, P = Self::Path>
    where
        'vfs: 'a,
        Self: 'a;
    /// The type of the file returned by the [`open_read` method](Vfs::open_read).
    type RFile: Read + 'vfs;

    /// Opens a file for reading, at the specified path.
    fn open_read(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<Self::RFile, <Self::Path as PathType>::OwnedPath>;

    /// Reads the contents of a file, at the specified path.
    fn read(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<Vec<u8>, <Self::Path as PathType>::OwnedPath>;
    /// Reads the contents of a file, at the specified path, and returns it as a string.
    fn read_string(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<String, <Self::Path as PathType>::OwnedPath> {
        self.read(path).and_then(|bytes| {
            String::from_utf8(bytes).map_err(|e| Error::Parse(path.owned(), Box::new(e)))
        })
    }
    /// Checks if a file exists at the specified path.
    fn exists(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<bool, <Self::Path as PathType>::OwnedPath>;

    /// Checks if a directory exists at the specified path.
    fn is_dir(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<bool, <Self::Path as PathType>::OwnedPath>;

    /// Walks a directory at the specified path, returning a stream of directory entries.
    fn walk_dir<'b>(
        self: Pin<&'b Self>,
        path: &Self::Path,
    ) -> Result<Self::DirWalk<'b>, <Self::Path as PathType>::OwnedPath>
    where
        'vfs: 'b;
}

/// Extension trait for [`Vfs`] that provides additional convenience methods.
pub trait VfsExt<'vfs>: Vfs<'vfs> {
    /// Reads a file / directory at the specified path, and parses it into the specified type using its
    /// [`ReadFrom`] implementation.
    ///
    /// This method takes `self` as a pinned reference, to ensure that the `Vfs` implementation
    /// is not moved while the read operation is in progress.
    fn read_typed_pinned<T: ReadFrom<'vfs, Self>>(
        self: Pin<&'vfs Self>,
        path: impl AsRef<Self::Path>,
    ) -> Result<T, <Self::Path as PathType>::OwnedPath> {
        T::read_from(path.as_ref(), self)
    }

    /// Reads a file / directory at the specified path, and parses it into the specified type using its
    /// [`ReadFrom`] implementation.
    ///
    /// This method takes `self` as a regular reference, and pins it internally.
    fn read_typed<T: ReadFrom<'vfs, Self>>(
        &'vfs self,
        path: impl AsRef<Self::Path>,
    ) -> Result<T, <Self::Path as PathType>::OwnedPath>
    where
        Self: Unpin,
    {
        Pin::new(self).read_typed_pinned(path)
    }
}

// Blanket impl.
impl<'vfs, V: Vfs<'vfs> + ?Sized> VfsExt<'vfs> for V {}

/// Marks that the [`RFile`](Vfs::RFile) type of this [`Vfs`] also implements [`Seek`](std::io::Seek),
/// allowing it to be used in contexts that require seeking, such as image decoding.
///
/// This trait is automatically implemented for any [`Vfs`] whose [`RFile`](Vfs::RFile) implements [`Seek`](std::io::Seek).
pub trait VfsWithSeekRead<'vfs>: Vfs<'vfs>
where
    Self::RFile: Seek,
{
}

impl<'vfs, T: Vfs<'vfs>> VfsWithSeekRead<'vfs> for T where T::RFile: Seek {}

/// A virtual file system that supports writing operations.
pub trait WriteSupportingVfs<'vfs>: Vfs<'vfs> {
    /// The type of the file returned by the [`open_write` method](WriteSupportingVfs::open_write).
    type WFile: Write + 'vfs;

    /// Opens a file for writing, at the specified path.
    fn open_write(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<Self::WFile, <Self::Path as PathType>::OwnedPath>;

    /// Writes the data to a file, to the specified path.
    fn write(
        self: Pin<&Self>,
        path: &Self::Path,
        data: &[u8],
    ) -> Result<(), <Self::Path as PathType>::OwnedPath> {
        self.open_write(path)?
            .write_all(data)
            .wrap_io_error_with(path)
    }
    /// Removes a directory and all its contents.
    fn remove_dir_all(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath>;
    /// Creates a new directory at the specified path.
    fn create_dir(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath>;
    /// Creates a new directory and all its parent directories at the specified path.
    fn create_dir_all(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath>;
    /// Creates the parent directory for the specified path, if it does not exist.
    fn create_parent_dir(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath>;
}

/// Extension trait for [`WriteSupportingVfs`] that provides additional convenience methods.
pub trait WriteSupportingVfsExt<'vfs>: WriteSupportingVfs<'vfs> {
    /// Writes a file / directory at the specified path, using the specified data type's
    /// [`WriteTo`] implementation.
    ///
    /// This method takes `self` as a pinned reference, to ensure that the `Vfs` implementation
    /// is not moved while the write operation is in progress.
    fn write_typed_pinned<T: WriteTo<'vfs, Self>>(
        self: Pin<&'vfs Self>,
        path: impl AsRef<Self::Path>,
        value: &T,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath> {
        value.write_to(path.as_ref(), self)
    }

    /// Writes a file / directory at the specified path, using the specified data type's
    /// [`WriteTo`] implementation.
    ///
    /// This method takes `self` as a regular reference, and pins it internally.
    fn write_typed<T: WriteTo<'vfs, Self>>(
        &'vfs self,
        path: impl AsRef<Self::Path>,
        value: &T,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath>
    where
        Self: Unpin,
    {
        Pin::new(self).write_typed_pinned(path, value)
    }
}

// Blanket impl.
impl<'vfs, Vfs: WriteSupportingVfs<'vfs> + ?Sized> WriteSupportingVfsExt<'vfs> for Vfs {}

/// Marks that the [`WFile`](WriteSupportingVfs::WFile) type of this [`WriteSupportingVfs`] also implements [`Seek`](std::io::Seek),
/// allowing it to be used in contexts that require seeking.
///
/// This trait is automatically implemented for any [`WriteSupportingVfs`] whose [`WFile`](WriteSupportingVfs::WFile) implements [`Seek`](std::io::Seek).
pub trait VfsWithSeekWrite<'vfs>: WriteSupportingVfs<'vfs>
where
    Self::WFile: Seek,
{
}

impl<'vfs, T: WriteSupportingVfs<'vfs>> VfsWithSeekWrite<'vfs> for T where T::WFile: Seek {}

/// A trait representing a path in a virtual file system.
pub trait PathType: PartialEq + Send + Sync {
    /// The owned version of this path type.
    type OwnedPath: OwnedPathType<RefType = Self>;
    /// The type of a path segment (a component of a path).
    type PathSegmentRef: ToOwned<Owned = Self::PathSegmentOwned> + PartialEq + ?Sized;
    /// The owned version of a path segment.
    type PathSegmentOwned: Send + Sync + Clone + Eq + AsRef<Self::PathSegmentRef>;

    /// Returns the parent path, if it exists.
    fn parent(&self) -> Option<&Self>;
    /// Joins this path with another path fragment, returning a new owned path.
    fn join(&self, new_fragment: impl AsRef<Self>) -> Self::OwnedPath;
    /// Joins this path with a path segment, returning a new owned path.
    fn join_segment(&self, new_fragment: impl AsRef<Self::PathSegmentRef>) -> Self::OwnedPath;
    /// Joins this path with a string slice as a path segment, returning a new owned path.
    fn join_segment_str(&self, new_fragment: &str) -> Self::OwnedPath;

    /// The error type returned when stripping a prefix fails.
    type StripPrefixError: StdError + Send + Sync + 'static;

    /// Strips the given base path from this path, returning the relative path if successful.
    fn strip_prefix(&self, base: &Self) -> StdResult<&Self, Self::StripPrefixError>;
    /// Converts this path to its owned version.
    fn owned(&self) -> Self::OwnedPath;
}

impl PathType for Path {
    type OwnedPath = PathBuf;
    type PathSegmentRef = OsStr;
    type PathSegmentOwned = OsString;

    fn parent(&self) -> Option<&Self> {
        self.parent()
    }

    fn join(&self, new_fragment: impl AsRef<Self>) -> Self::OwnedPath {
        Self::join(self, new_fragment.as_ref())
    }

    fn join_segment(&self, new_fragment: impl AsRef<Self::PathSegmentRef>) -> Self::OwnedPath {
        Self::join(self, new_fragment.as_ref())
    }

    fn join_segment_str(&self, new_fragment: &str) -> Self::OwnedPath {
        Self::join(self, new_fragment)
    }

    type StripPrefixError = path::StripPrefixError;

    fn strip_prefix(&self, base: &Self) -> StdResult<&Self, Self::StripPrefixError> {
        self.strip_prefix(base)
    }

    fn owned(&self) -> Self::OwnedPath {
        self.to_path_buf()
    }
}

/// A trait representing an owned path in a virtual file system.
pub trait OwnedPathType: Clone + AsRef<Self::RefType> + PartialEq + Send + Sync {
    /// The reference type corresponding to this owned path type.
    type RefType: PathType<OwnedPath = Self> + ?Sized;

    /// Returns the parent path, if it exists.
    fn parent(&self) -> Option<&Self::RefType>;

    /// Inserts a new path fragment at the front of this path.
    fn insert_in_front(&mut self, new_fragment: &<Self::RefType as PathType>::PathSegmentRef);

    /// Pushes a new path segment at the end of this path.
    fn push_segment_str(&mut self, new_fragment: &str);
}

impl OwnedPathType for PathBuf {
    type RefType = Path;

    fn parent(&self) -> Option<&Self::RefType> {
        self.as_path().parent()
    }

    fn insert_in_front(&mut self, new_fragment: &<Self::RefType as PathType>::PathSegmentRef) {
        let mut new_path = PathBuf::from(new_fragment);
        new_path.push(&self);
        *self = new_path;
    }

    fn push_segment_str(&mut self, new_fragment: &str) {
        self.push(new_fragment);
    }
}

/// The type of a directory entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
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
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct DirEntryInfo<P: PathType + ?Sized> {
    /// The name of the entry.
    pub name: P::PathSegmentOwned,
    /// The path of the entry.
    pub path: P::OwnedPath,
    /// The kind of the entry.
    pub kind: DirEntryKind,
}

/// A trait for walking a directory.
///
/// Behaves similarly to an [`Iterator`] over Result<[`DirEntryInfo`]>.
pub trait DirWalker<'vfs>: 'vfs {
    /// The path type used by this directory walker.
    ///
    /// This is the same as the path type used by the [`Vfs`] that created this walker.
    type P: PathType + ?Sized;
    /// Returns the next directory entry.
    fn next(&mut self) -> Option<Result<DirEntryInfo<Self::P>, <Self::P as PathType>::OwnedPath>>;
}
