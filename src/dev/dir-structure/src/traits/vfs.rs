//! Virtual file system traits.
//!
//! This module defines traits for virtual file systems (VFS) that can be implemented
//! for different backends, such as the local file system, in-memory file systems, or
//! even remote (e.g., cloud storage) file systems.
//!
//! Ultimately, all uses of the library will end up in VFS calls, on either [`Vfs`] or
//! [`WriteSupportingVfs`], depending on whether they are read-only or read-write operations.
//!
//! The trait hierarchy is as follows:
//!
//! [`WriteSupportingVfs`] < [`Vfs`] < [`VfsCore`]
//!
//! [`VfsCore`] is a type whose only purpose is to define the associated [`Path`][VfsCore::Path] type,
//! which is used throughout the other traits. It is a super-trait of [`Vfs`].
//!
//! [`Vfs`] is the main trait for read-only virtual file systems. It provides methods for reading
//! files and directories. It is a super-trait of [`WriteSupportingVfs`].
//!
//! [`WriteSupportingVfs`] is a trait for virtual file systems that support writing operations.
//! It extends [`Vfs`] with methods for writing files and creating/removing directories.
//!
//! # Why `Pin<&Self>`?
//!
//! The reason why the methods of the VFS traits take `self` as `Pin<&Self>` instead of
//! `&self` is to allow for implementations that may need to keep the VFS instance
//! pinned in memory. This is particularly important for
//! [`DeferredRead`][crate::deferred_read::DeferredRead], which will hold a reference
//! to the VFS instance for a potentially long time, and must ensure that the VFS
//! instance does not move in memory during that time. Using `Pin<&Self>` allows
//! such implementations to be safe and sound. This is more for the [`ReadFrom`] /
//! [`WriteTo`] implementations with [`DeferredRead`][crate::deferred_read::DeferredRead]
//! than for the VFS implementations, but using it here as well ensures that all
//! [`ReadFrom`] / [`WriteTo`] implementations  have a consistent interface with the
//! [`Vfs`] / [`WriteSupportingVfs`] traits.
//!
//! The Virtual File Systems have an associated path type, defined by the [`PathType`] trait.
//! See its documentation for more details.
//!
//! The reason why the path type is an associated type in the [`VfsCore`] trait, instead of being
//! an associated type in the [`Vfs`] trait directly, is to allow a clear common interface between
//! the syncrhonous and asynchronous VFS traits. The asynchronous VFS trait,
//! [`VfsAsync`](crate::traits::async_vfs::VfsAsync), also has
//! as a super-trait [`VfsCore`], and thus shares the same associated path type, but does not depend on
//! the [`Vfs`] trait directly.
//!
//! This change allows us to specify that our types only work with a specific path type, without
//! forcing them to be tied to a specific VFS implementation. As an example, we can have a
//! type that works with any VFS that uses [`Path`] as its path type, without being tied to a specific
//! VFS implementation / kind of implementation:
//!
//! ```rust,ignore
//! use std::path::Path;
//! use std::pin::Pin;
//!
//! use dir_structure::prelude::*;
//!
//! #[derive(DirStructure)]
//! struct PathOnlyDir<Vfs: VfsCore<Path = Path>> {
//!     #[dir_structure(path = "file.txt")]
//!     file: String,
//!     __phantom: std::marker::PhantomData<Vfs>,
//! }
//!
//! // sync
//! PathOnlyDir::read_from(Path::new("some/dir"), Pin::new(&SomeVfsImplementationThatUsesPath));
//! // async works as well
//! PathOnlyDir::read_from_async(PathBuf::from("some/dir"), Pin::new(&SomeAsyncVfsImplementationThatUsesPath)).await;
//! ```
//!
//! # For implementers of Virtual File Systems
//!
//! This section covers a bit about what you need to know to implement your own VFS.
//! Ultimately, you will need to implement the [`Vfs`] trait for your type. If your
//! Vfs accepts write operations as well, it should additionally implement
//! [`WriteSupportingVfs`]. Before you can implement [`Vfs`], you will need to implement
//! its super-trait, [`VfsCore`], which only requires you to define the associated
//! path type. The path type must implement the [`PathType`] trait, which is also
//! defined in this module. This [`PathType`] trait is implemented for [`Path`], so
//! if your VFS uses [`Path`] as its path type, you can just use that.
//!
//! If you want to implement your own path type, you will need to implement the
//! [`PathType`] trait for it. The [`PathType`] trait is designed to be as general
//! as possible, to allow for a wide variety of path types. However, it does have
//! some requirements, such as being able to join paths and to get the parent path.
//! See the documentation for the [`PathType`] trait for more details.
//!
//! ## Tool-specific extensions of the VFS traits
//!
//! Some tools (wrappers) in the library provide their own extensions of the VFS traits, with
//! extra functionality that is required by the tool. For example, the [`AtomicDir`](crate::atomic_dir::AtomicDir)
//! tool requires the ability to create temporary directories, and thus provides its own traits
//! required for working with temporary directories, but they need to be implemented by the VFS
//! type.
//!
//! Here we list all such extensions, which implementers of VFS traits may want to be aware of,
//! if they want to support these tools.
//!
//! ### [`AtomicDir<T>`](crate::atomic_dir::AtomicDir)
//!
//! For the [`AtomicDir`](crate::atomic_dir::AtomicDir) tool to work, the VFS type itself must implement
//! the [`VfsSupportsTemporaryDirectories`](crate::atomic_dir::VfsSupportsTemporaryDirectories) trait.
//! See its documentation for more details.

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
use crate::error::VfsResult;
use crate::error::WrapIoError as _;
use crate::prelude::*;

/// Core trait for a virtual file system, providing the associated path type.
pub trait VfsCore {
    /// The path type used to represent paths in this virtual file system.
    type Path: PathType + ?Sized;
}

/// A virtual file system. Writing operations are provided by the [`WriteSupportingVfs` trait](self::WriteSupportingVfs).
///
/// See the [module-level documentation](self) for more details.
pub trait Vfs<'vfs>: VfsCore + 'vfs {
    /// The type of the directory walker returned by the [`walk_dir` method](Vfs::walk_dir).
    ///
    /// See [`DirWalker`] for more details.
    type DirWalk<'a>: DirWalker<'a, P = Self::Path>
    where
        'vfs: 'a,
        Self: 'a;

    /// The type of the file returned by the [`open_read` method](Vfs::open_read). This allows us to
    /// read a file in chunks, which might be required for some certain file formats.
    ///
    /// In addtion, [`Vfs`] types whose [`RFile`](Self::RFile) implements [`Seek`] can be used in
    /// contexts that require seeking, such as image decoding. The [`VfsWithSeekRead`] trait encodes this property.
    ///
    /// See the [`VfsWithSeekRead`] trait for more details. Note that it is automatically implemented for all [`Vfs`]
    /// types with a seekable [`RFile`](Self::RFile).
    type RFile: Read + 'vfs;

    /// Opens a file for reading, at the specified path.
    fn open_read(self: Pin<&Self>, path: &Self::Path) -> VfsResult<Self::RFile, Self>;

    /// Reads the contents of a file, at the specified path. This is a core method that
    /// all other read methods are built upon, except for the ones that read via
    /// [`open_read`][Vfs::open_read] and [`RFile`][Vfs::RFile].
    fn read(self: Pin<&Self>, path: &Self::Path) -> VfsResult<Vec<u8>, Self>;
    /// Reads the contents of a file, at the specified path, and returns it as a string.
    ///
    /// This is a convenience method that reads the file as bytes using the [`read`][Vfs::read] method,
    /// and then converts the bytes to a string. If the bytes are not valid UTF-8, it returns a
    /// [`Error::Parse`] error.
    ///
    /// Overriding this method can be useful for VFS implementations that can read files with guarantees
    /// about their encoding, or where the underlying storage provides a more efficient way to read strings
    /// directly, but is not required.
    fn read_string(self: Pin<&Self>, path: &Self::Path) -> VfsResult<String, Self> {
        self.read(path).and_then(|bytes| {
            String::from_utf8(bytes).map_err(|e| Error::Parse(path.owned(), Box::new(e)))
        })
    }

    /// Checks if a file exists at the specified path.
    fn exists(self: Pin<&Self>, path: &Self::Path) -> VfsResult<bool, Self>;

    /// Checks if a directory exists at the specified path.
    fn is_dir(self: Pin<&Self>, path: &Self::Path) -> VfsResult<bool, Self>;

    /// Walks a directory at the specified path, returning a stream of directory entries.
    ///
    /// [`DirWalker`] represents a stream of directory entries, and behaves similarly to an [`Iterator`] over
    /// `Result<DirEntryInfo>`.
    ///
    /// This method returns a [`DirWalker`] that can be used to iterate over the entries in the directory.
    ///
    /// See the [`DirWalker`] documentation for more details.
    fn walk_dir<'b>(self: Pin<&'b Self>, path: &Self::Path) -> VfsResult<Self::DirWalk<'b>, Self>
    where
        'vfs: 'b;
}

/// Extension trait for [`Vfs`] that provides additional convenience methods.
///
/// This trait is automatically implemented for all types that implement [`Vfs`].
pub trait VfsExt<'vfs>: Vfs<'vfs> {
    /// Reads a file / directory at the specified path, and parses it into the specified type using its
    /// [`ReadFrom`] implementation.
    ///
    /// This method takes `self` as a pinned reference, to ensure that the `Vfs` implementation
    /// is not moved while the read operation is in progress, or if the type `T` being read stores
    /// a reference to the `Vfs` instance (e.g., via [`DeferredRead`][crate::deferred_read::DeferredRead]).
    fn read_typed_pinned<T: ReadFrom<'vfs, Self>>(
        self: Pin<&'vfs Self>,
        path: impl AsRef<Self::Path>,
    ) -> VfsResult<T, Self> {
        T::read_from(path.as_ref(), self)
    }

    /// Reads a file / directory at the specified path, and parses it into the specified type using its
    /// [`ReadFrom`] implementation.
    ///
    /// This method takes `self` as a regular reference, and pins it internally, calling
    /// [`read_typed_pinned`][VfsExt::read_typed_pinned] on the pinned reference.
    fn read_typed<T: ReadFrom<'vfs, Self>>(
        &'vfs self,
        path: impl AsRef<Self::Path>,
    ) -> VfsResult<T, Self>
    where
        Self: Unpin,
    {
        Pin::new(self).read_typed_pinned(path)
    }
}

// Blanket impl.
impl<'vfs, V: Vfs<'vfs> + ?Sized> VfsExt<'vfs> for V {}

/// Marks that the [`RFile`](Vfs::RFile) type of this [`Vfs`] also implements [`Seek`],
/// allowing it to be used in contexts that require seeking, such as image decoding.
///
/// This trait is automatically implemented for any [`Vfs`] whose [`RFile`](Vfs::RFile) implements [`Seek`].
pub trait VfsWithSeekRead<'vfs>: Vfs<'vfs>
where
    Self::RFile: Seek,
{
}

// Blanket impl.
impl<'vfs, T: Vfs<'vfs>> VfsWithSeekRead<'vfs> for T where T::RFile: Seek {}

/// A virtual file system that supports writing operations.
///
/// This trait extends the [`Vfs`] trait with methods for writing files and creating/removing directories.
///
/// See the [module-level documentation](self) for more details.
pub trait WriteSupportingVfs<'vfs>: Vfs<'vfs> {
    /// The type of the file returned by the [`open_write` method](WriteSupportingVfs::open_write).
    ///
    /// This allows us to write a file in chunks, which might be required for some certain file formats.
    ///
    /// If this type additionally implements [`Seek`], then, similarly to [`VfsWithSeekRead`],
    /// the [`VfsWithSeekWrite`] trait will be automatically implemented for this [`WriteSupportingVfs`].
    ///
    /// See [`VfsWithSeekWrite`] for more.
    type WFile: Write + 'vfs;

    /// Opens a file for writing, at the specified path.
    ///
    /// This method will create the file if it does not exist, or truncate it if it does.
    /// Note that this does not create parent directories, that should happen separately in the calling code,
    /// which can be done using the [`create_parent_dir`][WriteSupportingVfs::create_parent_dir] method.
    fn open_write(self: Pin<&Self>, path: &Self::Path) -> VfsResult<Self::WFile, Self>;

    /// Writes the data to a file, to the specified path.
    ///
    /// Similarly to [`open_write`][WriteSupportingVfs::open_write], this method will not create parent directories.
    /// The parent directories should be created separately in the calling code, which can be done using
    /// the [`create_parent_dir`][WriteSupportingVfs::create_parent_dir] method.
    fn write(self: Pin<&Self>, path: &Self::Path, data: &[u8]) -> VfsResult<(), Self> {
        self.open_write(path)?
            .write_all(data)
            .wrap_io_error_with(path)
    }

    /// Removes a directory and all its contents.
    ///
    /// This method should remove the directory at the specified path, along with all its contents.
    fn remove_dir_all(self: Pin<&Self>, path: &Self::Path) -> VfsResult<(), Self>;

    /// Creates a new directory at the specified path.
    ///
    /// This method should create a new, empty directory at the specified path.
    fn create_dir(self: Pin<&Self>, path: &Self::Path) -> VfsResult<(), Self>;

    /// Creates a new directory and all its parent directories at the specified path.
    ///
    /// This is a convenience method that can be used to ensure that a directory exists, along with all its parent directories.
    fn create_dir_all(self: Pin<&Self>, path: &Self::Path) -> VfsResult<(), Self>;

    /// Creates the parent directory for the specified path, if it does not exist.
    ///
    /// This is a convenience method that can be used before writing a file, to ensure that the parent directory
    /// exists. If the parent directory already exists, this method does nothing. If the path has no parent, this method does nothing.
    fn create_parent_dir(self: Pin<&Self>, path: &Self::Path) -> VfsResult<(), Self> {
        if let Some(parent) = path.parent()
            && !self.exists(parent)?
        {
            self.create_dir_all(parent)?;
        }
        Ok(())
    }
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
    ) -> VfsResult<(), Self> {
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
    ) -> VfsResult<(), Self>
    where
        Self: Unpin,
    {
        Pin::new(self).write_typed_pinned(path, value)
    }
}

// Blanket impl.
impl<'vfs, Vfs: WriteSupportingVfs<'vfs> + ?Sized> WriteSupportingVfsExt<'vfs> for Vfs {}

/// Marks that the [`WFile`](WriteSupportingVfs::WFile) type of this [`WriteSupportingVfs`] also implements [`Seek`],
/// allowing it to be used in contexts that require seeking.
///
/// This trait is automatically implemented for any [`WriteSupportingVfs`] whose [`WFile`](WriteSupportingVfs::WFile) implements [`Seek`].
pub trait VfsWithSeekWrite<'vfs>: WriteSupportingVfs<'vfs>
where
    Self::WFile: Seek,
{
}

// Blanket impl.
impl<'vfs, T: WriteSupportingVfs<'vfs>> VfsWithSeekWrite<'vfs> for T where T::WFile: Seek {}

/// A trait representing a path in a virtual file system.
///
/// This can be implemented for custom path types, allowing you to create [`Vfs`] implementations that
/// work with path types other than [`Path`] and [`PathBuf`]. [`PathType`] is implemented for the reference type,
/// which is "linked" to the owned type via the associated type [`OwnedPath`][PathType::OwnedPath].
///
/// Once you have a value of that owned type (e.g., `MyPathBuf`), you can get a reference to the path type
/// (e.g., `&MyPath`) via the [`AsRef`] trait, which is a super-trait of [`OwnedPathType`].
///
/// This trait is implemented for [`Path`], so if your VFS uses [`Path`] as its path type, you can just use that.
///
/// All the examples in the documentation for [`PathType`] and [`OwnedPathType`] use [`Path`] and [`PathBuf`]. You
/// may treat them as expected behaviours for your own implementations of these traits, and derive your tests from them.
pub trait PathType: PartialEq + Send + Sync {
    /// The owned version of this path type.
    ///
    /// This owned type must implement the [`OwnedPathType`] trait, which has as a super-trait of [`AsRef`],
    /// allowing us to get a reference to this [`PathType`] type from the [`OwnedPathType`] type.
    type OwnedPath: OwnedPathType<RefType = Self>;

    /// The type of a path segment (a component of a path).
    ///
    /// This represents a single segment of a path, such as a file or directory name.
    ///
    /// In the case of [`Path`], this is [`OsStr`].
    type PathSegmentRef: ToOwned<Owned = Self::PathSegmentOwned> + PartialEq + ?Sized;

    /// The owned version of a path segment.
    ///
    /// This represents a single segment of a path, such as a file or directory name.
    ///
    /// This is the owned version of [`PathType::PathSegmentRef`].
    type PathSegmentOwned: Send + Sync + Clone + Eq + AsRef<Self::PathSegmentRef>;

    /// Returns the parent path, if it exists.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::traits::vfs::PathType;
    ///
    /// let path = Path::new("some/dir/file.txt");
    /// let parent = PathType::parent(path);
    /// assert_eq!(parent, Some(Path::new("some/dir")));
    ///
    /// let path = Path::new("file.txt");
    /// let parent = PathType::parent(path);
    /// assert_eq!(parent, Some(Path::new("")));
    ///
    /// let empty = Path::new("");
    /// let parent = PathType::parent(empty);
    /// assert_eq!(parent, None);
    /// ```
    fn parent(&self) -> Option<&Self>;

    /// Joins this path with another path fragment, returning a new owned path.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::traits::vfs::PathType;
    ///
    /// let path = Path::new("some/dir");
    /// let new_path = PathType::join(path, Path::new("file.txt"));
    /// assert_eq!(new_path, PathBuf::from("some/dir/file.txt"));
    /// ```
    fn join(&self, new_fragment: impl AsRef<Self>) -> Self::OwnedPath;

    /// Joins this path with a path segment, returning a new owned path.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use std::ffi::OsStr;
    /// use dir_structure::traits::vfs::PathType;
    ///
    /// let path = Path::new("some/dir");
    /// let new_path = PathType::join_segment(path, OsStr::new("file.txt"));
    /// assert_eq!(new_path, PathBuf::from("some/dir/file.txt"));
    /// ```
    fn join_segment(&self, new_fragment: impl AsRef<Self::PathSegmentRef>) -> Self::OwnedPath;

    /// Joins this path with a string slice as a path segment, returning a new owned path.
    ///
    /// This is the method used to derive the paths of subfields in a directory structure,
    /// when using the derive macro.
    ///
    /// # Example
    ///
    /// ```
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::traits::vfs::PathType;
    ///
    /// let path = Path::new("some/dir");
    /// let new_path = PathType::join_segment_str(path, "file.txt");
    /// assert_eq!(new_path, PathBuf::from("some/dir/file.txt"));
    /// ```
    fn join_segment_str(&self, new_fragment: &str) -> Self::OwnedPath;

    /// The error type returned when stripping a prefix fails.
    type StripPrefixError: StdError + Send + Sync + 'static;

    /// Strips the given base path from this path, returning the relative path if successful.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::traits::vfs::PathType;
    ///
    /// let path = Path::new("some/dir/file.txt");
    /// let base = Path::new("some");
    /// let relative = PathType::strip_prefix(path, base).unwrap();
    /// assert_eq!(relative, Path::new("dir/file.txt"));
    ///
    /// let base = Path::new("other");
    /// let relative = PathType::strip_prefix(path, base);
    /// assert!(relative.is_err());
    /// ```
    fn strip_prefix(&self, base: &Self) -> StdResult<&Self, Self::StripPrefixError>;

    /// Converts this path to its owned version.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::traits::vfs::PathType;
    ///
    /// let path = Path::new("some/dir/file.txt");
    /// let owned = PathType::owned(path);
    /// assert_eq!(owned, PathBuf::from("some/dir/file.txt"));
    /// ```
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
///
/// This can be implemented for custom owned path types, allowing you to create [`Vfs`] implementations that
/// work with owned path types other than [`PathBuf`]. [`OwnedPathType`] is implemented for the owned type,
/// which is "linked" to the reference type via the associated type [`RefType`][OwnedPathType::RefType].
///
/// Once you have a value of that owned type (e.g., `MyPathBuf`), you can get a reference to the path type
/// (e.g., `&MyPath`) via the [`AsRef`] trait, which is a super-trait of [`OwnedPathType`].
///
/// A couple of methods are provided to manipulate the path in place, such as inserting a new fragment
/// at the front of the path, or pushing a new segment at the end of the path.
pub trait OwnedPathType: Clone + AsRef<Self::RefType> + PartialEq + Send + Sync {
    /// The reference type corresponding to this owned path type.
    ///
    /// This reference type must implement the [`PathType`] trait, and its associated
    /// [`OwnedPath`][PathType::OwnedPath] must be `Self`.
    type RefType: PathType<OwnedPath = Self> + ?Sized;

    /// Returns the parent path, if it exists.
    ///
    /// Convenience method that calls the [`parent` method](PathType::parent) on the reference type.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::traits::vfs::OwnedPathType;
    ///
    /// let path = PathBuf::from("some/dir/file.txt");
    /// let parent = OwnedPathType::parent(&path);
    /// assert_eq!(parent, Some(Path::new("some/dir")));
    ///
    /// let path = PathBuf::from("file.txt");
    /// let parent = OwnedPathType::parent(&path);
    /// assert_eq!(parent, Some(Path::new("")));
    ///
    /// let empty = PathBuf::from("");
    /// let parent = OwnedPathType::parent(&empty);
    /// assert_eq!(parent, None);
    /// ```
    fn parent(&self) -> Option<&Self::RefType> {
        self.as_ref().parent()
    }

    /// Inserts a new path fragment at the front of this path.
    ///
    /// This is needed when reading a [`DirDescendants`][crate::dir_descendants::DirDescendants],
    /// to prepend the base path to the relative paths of the entries found recursively.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use std::ffi::OsStr;
    /// use dir_structure::traits::vfs::OwnedPathType;
    ///
    /// let mut path = PathBuf::from("dir/file.txt");
    /// path.insert_in_front(OsStr::new("some"));
    /// assert_eq!(path, PathBuf::from("some/dir/file.txt"));
    ///
    /// let mut path = PathBuf::from("file.txt");
    /// path.insert_in_front(OsStr::new("some"));
    /// assert_eq!(path, PathBuf::from("some/file.txt"));
    ///
    /// let mut path = PathBuf::from("");
    /// path.insert_in_front(OsStr::new("some"));
    /// assert_eq!(path, PathBuf::from("some"));
    /// ```
    fn insert_in_front(&mut self, new_fragment: &<Self::RefType as PathType>::PathSegmentRef);

    /// Pushes a new path segment at the end of this path.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::traits::vfs::OwnedPathType;
    ///
    /// let mut path = PathBuf::from("some/dir");
    /// path.push_segment_str("file.txt");
    /// assert_eq!(path, PathBuf::from("some/dir/file.txt"));
    ///
    /// let mut path = PathBuf::from("some/dir/");
    /// path.push_segment_str("file.txt");
    /// assert_eq!(path, PathBuf::from("some/dir/file.txt"));
    /// ```
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
/// Behaves similarly to an [`Iterator`] over Result<[`DirEntryInfo`], <Self::P as PathType>::OwnedPath>.
pub trait DirWalker<'vfs>: 'vfs {
    /// The path type used by this directory walker.
    ///
    /// This is the same as the path type used by the [`Vfs`] that created this walker.
    type P: PathType + ?Sized;
    /// Returns the next directory entry.
    fn next(&mut self) -> Option<Result<DirEntryInfo<Self::P>, <Self::P as PathType>::OwnedPath>>;
}
