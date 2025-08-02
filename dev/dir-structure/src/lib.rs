//! A library for reading and writing directory structures.
//!
//! This library provides a macro for defining directory structures, and a
//! trait for reading and writing those structures to / from disk.
//!
//! # Example
//!
//! ## Writing a structure to disk
//! ```
//! use std::path::Path;
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     use dir_structure::DirStructureItem;
//!     #[derive(dir_structure::DirStructure)]
//!     struct Dir {
//!         #[dir_structure(path = "f1.txt")]
//!         f1: String,
//!         #[dir_structure(path = "subdir/f2.txt")]
//!         f2: String,
//!         // default path is just a file name from the field's name.
//!         f3: String,
//!         // also works with nested structures
//!         #[dir_structure(path = "subdir2")]
//!         subdir: Subdir,
//!     }
//!     #[derive(dir_structure::DirStructure)]
//!     struct Subdir {
//!         #[dir_structure(path = "f4.txt")]
//!         f4: String,
//!     }
//!
//!     let d = Path::new("dir");
//!     Dir {
//!         f1: "f1".to_owned(),
//!         f2: "f2".to_owned(),
//!         f3: "f3".to_owned(),
//!         subdir: Subdir {
//!             f4: "f4".to_owned(),
//!         },
//!     }.write(&d)?;
//!     assert_eq!(std::fs::read_to_string(d.join("f1.txt"))?, "f1");
//!     assert_eq!(std::fs::read_to_string(d.join("subdir/f2.txt"))?, "f2");
//!     assert_eq!(std::fs::read_to_string(d.join("f3"))?, "f3");
//!     assert_eq!(std::fs::read_to_string(d.join("subdir2/f4.txt"))?, "f4");
//!
//!     # std::fs::remove_dir_all(&d)?;
//!
//!     Ok(())
//! }
//! ```
//!
//! ## Reading a structure from disk
//!
//! ```
//! use std::path::Path;
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     use dir_structure::DirStructureItem;
//!     #[derive(dir_structure::DirStructure)]
//!     struct Dir {
//!         #[dir_structure(path = "f1.txt")]
//!         f1: String,
//!         #[dir_structure(path = "subdir/f2.txt")]
//!         f2: String,
//!         // default path is just a file name from the field's name.
//!         f3: String,
//!         // also works with nested structures
//!         #[dir_structure(path = "subdir2")]
//!         subdir: Subdir,
//!     }
//!     #[derive(dir_structure::DirStructure)]
//!     struct Subdir {
//!         #[dir_structure(path = "f4.txt")]
//!         f4: String,
//!     }
//!     let d = Path::new("dir");
//!     std::fs::create_dir_all(&d)?;
//!     std::fs::create_dir_all(d.join("subdir"))?;
//!     std::fs::create_dir_all(d.join("subdir2"))?;
//!     std::fs::write(d.join("f1.txt"), "f1")?;
//!     std::fs::write(d.join("subdir/f2.txt"), "f2")?;
//!     std::fs::write(d.join("f3"), "f3")?;
//!     std::fs::write(d.join("subdir2/f4.txt"), "f4")?;
//!     let dir = Dir::read(&d)?;
//!     assert_eq!(dir.f1, "f1");
//!     assert_eq!(dir.f2, "f2");
//!     assert_eq!(dir.f3, "f3");
//!     assert_eq!(dir.subdir.f4, "f4");
//!
//!     # std::fs::remove_dir_all(&d)?;
//!
//!     Ok(())
//! }
//! ```

#![cfg_attr(docsrs, feature(doc_cfg))]

use std::ffi::OsStr;
use std::ffi::OsString;
use std::fmt::Display;
use std::fs::File;
use std::marker;
use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;

/// The error type for this library.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// An IO error.
    #[error("IO error at {0:?}: {1}")]
    Io(PathBuf, #[source] std::io::Error),
    /// Parse error.
    #[error("Parse error at {0:?}: {1}")]
    Parse(PathBuf, #[source] Box<dyn std::error::Error + Send + Sync>),
    /// Serde error.
    #[error("Serde error at {0:?}: {1}")]
    Serde(PathBuf, #[source] Box<dyn std::error::Error + Send + Sync>),
}

trait WrapIoError: Sized {
    type Output;

    fn wrap_io_error(self, get_path: impl FnOnce() -> PathBuf) -> Result<Self::Output>;

    fn wrap_io_error_with(self, path: &Path) -> Result<Self::Output> {
        self.wrap_io_error(|| path.to_path_buf())
    }
}

impl<T> WrapIoError for std::io::Result<T> {
    type Output = T;

    fn wrap_io_error(self, get_path: impl FnOnce() -> PathBuf) -> Result<Self::Output> {
        self.map_err(|e| Error::Io(get_path(), e))
    }
}

pub type Result<T> = std::result::Result<T, Error>;

/// The main trait. This is implemented for
/// all directory structures by the derive macro.
///
/// This trait doesn't have any methods, just a supertype:
/// [`DirStructureItem`].
pub trait DirStructure: DirStructureItem {}

/// Helper trait, implemented for all types that have a [`ReadFrom`]
/// and [`WriteTo`] implementation.
pub trait DirStructureItem: ReadFrom + WriteTo {
    /// Uses the [`ReadFrom`] implementation to read the structure from
    /// disk, from the specified path.
    fn read(path: impl AsRef<Path>) -> Result<Self>
    where
        Self: Sized,
    {
        Self::read_from(path.as_ref())
    }

    /// Uses the [`WriteTo`] implementation to write the structure
    /// to disk at the specified path.
    fn write(&self, path: impl AsRef<Path>) -> Result<()> {
        self.write_to(path.as_ref())
    }
}

// Blanket impl.
impl<T> DirStructureItem for T where T: ReadFrom + WriteTo {}

/// Trait for types / structures that can be
/// read from disk, either from a file or a directory.
pub trait ReadFrom {
    /// Reads the structure from the specified path, which
    /// can be either a file or a directory.
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized;
}

/// Trait for types / structures that can be
/// written to disk. All types in the library that
/// write to files first check that the parent
/// directories exist, so implementations of
/// this that create the whole directory are
/// not necessary (unless used empty children
/// directories, in which case no directories will
/// really be created).
pub trait WriteTo {
    /// Writes the structure to the specified path.
    fn write_to(&self, path: &Path) -> Result<()>;
}

/// Trait to use when using the `with_newtype` attribute.
///
/// This is used to convert a reference to a normal type
/// (like `String`, `Vec<u8>` etc. into a type that is a
/// reference to them, like `&str`, `&[u8]` etc.), so that
/// the `WriteTo` implementation can be written only for the
/// reference types, and all the other [`WriteTo`] impls will
/// only cast what they have to write to those reference types
/// (via the function below), and then call the [`WriteTo::write_to`]
/// method on that reference.
pub trait FromRefForWriter<'a> {
    /// The inner type to cast.
    type Inner: ?Sized;
    /// The reference type to cast to.
    type Wr: WriteTo + 'a;

    /// Casts the reference to the inner type to a [`WriteTo`]
    /// reference type.
    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr;
}

/// Trait to use when using the `with_newtype` attribute.
///
/// This is used to convert a newtype to its inner type.
/// We are using this because we cannot make blanket impls with
/// [`From`] due to the orphan rules.
pub trait NewtypeToInner {
    /// The inner type.
    type Inner;

    /// Converts the newtype to its inner type.
    fn into_inner(self) -> Self::Inner;
}

/// A directory structure where we don't know the names of the folders at compile-time,
/// and as such we cannot use the derive macro.
///
/// Instead we know that all the entries in the directory are folders,
/// and that they all have the same structure inside (defined by the `T` type parameter),
/// or they are all files (which can be read with [`DirChildren`]<[`String`]> for example).
///
/// In either case, [`ReadFrom::read_from`] must be able to read all the entries in
/// the directory.
///
/// The [`WriteTo`] implementation will directly write the children to the directory it
/// is passed, with no regards to the path stored in `self_path`.
#[derive(Debug, PartialEq, Eq)]
pub struct DirChildren<T, F: Filter = NoFilter>
where
    T: DirStructureItem,
{
    /// The path to the root directory.
    ///
    /// This path doesn't influence writing in any way, it is only to
    /// point out the directory after it has been read and parsed.
    pub self_path: PathBuf,
    /// The children of the root directory.
    pub children: Vec<DirChild<T>>,

    filter: marker::PhantomData<F>,
}

impl<T, F> Clone for DirChildren<T, F>
where
    T: DirStructureItem + Clone,
    F: Filter,
{
    fn clone(&self) -> Self {
        Self {
            self_path: self.self_path.clone(),
            children: self.children.clone(),
            filter: marker::PhantomData,
        }
    }
}

/// A filter for the children of a [`DirChildren`] structure.
///
/// This is used to filter out children that we don't want to
/// read into the structure. For example, if we have a directory
/// with a lot of files, we can use this to only read the
/// files we want, for example, that have just a certain extension.
///
/// # Examples
///
/// For example, for a [`Filter`] that only allows `.txt` files:
///
/// ```rust
/// use std::path::Path;
/// use std::path::PathBuf;
///
/// use dir_structure::{DirStructure, DirStructureItem, DirChildren, Filter};
///
/// pub struct TextFileFilter;
///
/// impl Filter for TextFileFilter {
///     fn make_filter() -> Self {
///        Self
///     }
///
///     fn allows(&self, path: &Path) -> bool {
///         path.extension()
///             .and_then(|s| s.to_str())
///             .map_or(false, |s| s == "txt")
///     }
/// }
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let path = PathBuf::from("dir");
///     #[derive(DirStructure)]
///     struct Dir {
///        #[dir_structure(path = self)]
///        text_files: DirChildren<String, TextFileFilter>,
///     }
///
///     # std::fs::create_dir_all(&path)?;
///
///     std::fs::write(path.join("file1.txt"), "file1")?;
///     std::fs::write(path.join("file2.txt"), "file2")?;
///     std::fs::write(path.join("file3.bin"), "aaa")?;
///
///     let dir = Dir::read(&path)?;
///     assert_eq!(dir.text_files.len(), 2);
///     assert_eq!(dir.text_files.get_value_by_name("file1.txt"), Some(&String::from("file1")));
///     assert_eq!(dir.text_files.get_value_by_name("file2.txt"), Some(&String::from("file2")));
///     assert_eq!(dir.text_files.get_value_by_name("file3.bin"), None);
///
///     # std::fs::remove_dir_all(&path)?;
///
///     Ok(())
/// }
/// ```
pub trait Filter {
    /// Creates an instance of this filter.
    fn make_filter() -> Self;
    /// Checks if the path is allowed by this filter.
    fn allows(&self, path: &Path) -> bool;
}

/// A [`Filter`] that allows all paths.
///
/// ```rust
/// # use std::path::Path;
/// # use dir_structure::{Filter, NoFilter};
/// #
/// let filter = NoFilter::make_filter();
/// assert!(filter.allows(Path::new("foo.txt")));
/// assert!(filter.allows(Path::new("foo/bar.txt")));
/// assert!(filter.allows(Path::new("foo/bar/baz.txt")));
/// assert!(filter.allows(Path::new("foo/bar/baz")));
/// assert!(filter.allows(Path::new("foo/bar/baz/")));
/// assert!(filter.allows(Path::new("foo/bar/baz/.")));
/// assert!(filter.allows(Path::new("foo/bar/baz/..")));
/// assert!(filter.allows(Path::new("foo/bar/baz/../..")));
/// assert!(filter.allows(Path::new("foo/bar/baz/../../..")));
/// assert!(filter.allows(Path::new("foo/bar/baz/../../../..")));
/// assert!(filter.allows(Path::new("foo/bar/baz/../../../../..")));
/// assert!(filter.allows(Path::new("foo/bar/baz/../../../../../..")));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NoFilter;

impl Filter for NoFilter {
    fn make_filter() -> Self {
        Self
    }

    fn allows(&self, _path: &Path) -> bool {
        true
    }
}

#[macro_export]
macro_rules! ext_filter {
    ($vis:vis $name:ident, $Ext:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        $vis struct $name;

        impl $crate::Filter for $name {
            fn make_filter() -> Self {
                Self
            }

            fn allows(&self, path: &::std::path::Path) -> bool {
                path.extension()
                    .map_or(false, |s| s == $Ext)
            }
        }
    };
}

impl<T> Default for DirChildren<T>
where
    T: DirStructureItem,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T, F> DirChildren<T, F>
where
    T: DirStructureItem,
    F: Filter,
{
    /// Creates an empty [`DirChildren`], with no children.
    pub fn new() -> Self {
        Self {
            self_path: PathBuf::new(),
            children: Vec::new(),
            filter: marker::PhantomData,
        }
    }

    /// Creates a [`DirChildren`] with the given path and children.
    pub fn with_children_from_iter(
        self_path: impl Into<PathBuf>,
        children: impl IntoIterator<Item = DirChild<T>>,
    ) -> Self {
        Self {
            self_path: self_path.into(),
            children: children.into_iter().collect(),
            filter: marker::PhantomData,
        }
    }

    /// Maps the children of this [`DirChildren`] to a new type.
    ///
    /// This is useful for converting the children to a different type,
    /// for example, if you want to convert the children to a different
    /// type of [`DirStructureItem`].
    ///
    /// This is a convenience method that allows you to use the
    /// `map` method on the children of this [`DirChildren`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::Path;
    /// use std::path::PathBuf;
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild, ReadFrom, WriteTo};
    ///
    /// #[derive(Debug, PartialEq, Eq)]
    /// struct NewType(String);
    ///
    /// impl ReadFrom for NewType {
    ///     fn read_from(path: &Path) -> dir_structure::Result<Self> {
    ///         String::read_from(path).map(Self)
    ///     }
    /// }
    ///
    /// impl WriteTo for NewType {
    ///     fn write_to(&self, path: &Path) -> dir_structure::Result<()> {
    ///         self.0.write_to(path)
    ///     }
    /// }
    ///
    /// let d = PathBuf::from("dir");
    /// let dir = DirChildren::<_, dir_structure::NoFilter>::with_children_from_iter(
    ///     d.clone(),
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///         DirChild::new("file3.txt", "file3".to_owned()),
    ///     ],
    /// );
    /// let dir = dir.map(|child| child.map_value(NewType));
    /// assert_eq!(
    ///     dir,
    ///     DirChildren::with_children_from_iter(
    ///         d.clone(),
    ///         vec![
    ///             DirChild::new("file1.txt", NewType("file1".to_owned())),
    ///             DirChild::new("file2.txt", NewType("file2".to_owned())),
    ///             DirChild::new("file3.txt", NewType("file3".to_owned())),
    ///         ],
    ///     )
    /// );
    /// ```
    pub fn map<U, MapF>(self, f: MapF) -> DirChildren<U, F>
    where
        MapF: FnMut(DirChild<T>) -> DirChild<U>,
        U: DirStructureItem,
    {
        let children = self.children.into_iter().map(f).collect();
        DirChildren {
            self_path: self.self_path,
            children,
            filter: marker::PhantomData,
        }
    }

    /// Maps the filter type. The children remain unchanged.
    ///
    /// This is useful if you are trying to pass a DirChildren<T, F1> to
    /// a function requiring a DirChildren<T, F2>, where F1 and F2 are two
    /// distinct types implementing [`Filter`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::Path;
    /// use dir_structure::{Filter, DirChildren};
    ///
    /// struct NewFilter;
    ///
    /// impl Filter for NewFilter {
    ///     fn make_filter() -> Self {
    ///         Self
    ///     }
    ///
    ///     fn allows(&self, _path: &Path) -> bool {
    ///         true
    ///     }
    /// }
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// let d2: DirChildren<String, NewFilter> = d.map_filter::<NewFilter>();
    /// ```
    pub fn map_filter<NewF: Filter>(self) -> DirChildren<T, NewF>
    where
        NewF: Filter,
    {
        DirChildren {
            self_path: self.self_path,
            children: self.children,
            filter: marker::PhantomData,
        }
    }

    /// Returns the number of children.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.len(), 0);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     PathBuf::new(),
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// assert_eq!(d.len(), 2);
    /// ```
    pub fn len(&self) -> usize {
        self.children.len()
    }

    /// Gets the child at the specified index.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get(0), None);
    /// assert_eq!(d.get(1), None);
    /// assert_eq!(d.get(100), None);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     PathBuf::new(),
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// assert_eq!(d.get(0), Some(&DirChild::new("file1.txt", "file1".to_owned())));
    /// assert_eq!(d.get(1), Some(&DirChild::new("file2.txt", "file2".to_owned())));
    /// assert_eq!(d.get(2), None);
    /// assert_eq!(d.get(100), None);
    /// ```
    pub fn get(&self, index: usize) -> Option<&DirChild<T>> {
        self.children.get(index)
    }

    /// Gets a mutable reference to the child at the specified index.
    /// 
    /// # Examples
    /// 
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild};
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get_mut(0), None);
    /// assert_eq!(d.get_mut(1), None);
    /// assert_eq!(d.get_mut(100), None);
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     PathBuf::new(),
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// assert_eq!(d.get_mut(0), Some(&mut DirChild::new("file1.txt", "file1".to_owned())));
    /// assert_eq!(d.get_mut(1), Some(&mut DirChild::new("file2.txt", "file2".to_owned())));
    /// assert_eq!(d.get_mut(2), None);
    /// assert_eq!(d.get_mut(100), None);
    /// ```
    pub fn get_mut(&mut self, index: usize) -> Option<&mut DirChild<T>> {
        self.children.get_mut(index)
    }

    /// Gets the child with the specified "file" name (last segment of path).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get_name(""), None);
    /// assert_eq!(d.get_name("any_name"), None);
    /// assert_eq!(d.get_name("aaaa"), None);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     PathBuf::new(),
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// assert_eq!(d.get_name("file1.txt"), Some(&DirChild::new("file1.txt", "file1".to_owned())));
    /// assert_eq!(d.get_name("file2.txt"), Some(&DirChild::new("file2.txt", "file2".to_owned())));
    /// assert_eq!(d.get_name("any_name"), None);
    /// assert_eq!(d.get_name("aaaa"), None);
    /// ```
    pub fn get_name(&self, name: impl AsRef<OsStr>) -> Option<&DirChild<T>> {
        self.children
            .iter()
            .find(|child| child.file_name == name.as_ref())
    }

    /// Gets the value of the child with the specified "file" name (last segment of path).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get_value_by_name(""), None);
    /// assert_eq!(d.get_value_by_name("any_name"), None);
    /// assert_eq!(d.get_value_by_name("aaaa"), None);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     PathBuf::new(),
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// assert_eq!(d.get_value_by_name("file1.txt"), Some(&"file1".to_owned()));
    /// assert_eq!(d.get_value_by_name("file2.txt"), Some(&"file2".to_owned()));
    /// assert_eq!(d.get_value_by_name("any_name"), None);
    /// assert_eq!(d.get_value_by_name("aaaa"), None);
    /// ```
    pub fn get_value_by_name(&self, name: impl AsRef<OsStr>) -> Option<&T> {
        self.get_name(name).map(|child| &child.value)
    }

    /// Returns an iterator over the children.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// let mut i = d.iter();
    /// assert_eq!(i.next(), None);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     PathBuf::new(),
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// let mut i = d.iter();
    /// assert_eq!(i.next(), Some(&DirChild::new("file1.txt", "file1".to_owned())));
    /// assert_eq!(i.next(), Some(&DirChild::new("file2.txt", "file2".to_owned())));
    /// assert_eq!(i.next(), None);
    /// ```
    pub fn iter(&self) -> DirChildrenIter<'_, T> {
        DirChildrenIter(self.children.iter())
    }

    /// Returns a mutable iterator over the children.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild};
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     PathBuf::new(),
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// let mut i = d.iter_mut();
    /// assert_eq!(i.next(), Some(&mut DirChild::new("file1.txt", "file1".to_owned())));
    /// assert_eq!(i.next(), Some(&mut DirChild::new("file2.txt", "file2".to_owned())));
    /// assert_eq!(i.next(), None);
    /// ```
    ///
    /// Modifying the children is also possible:
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild};
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     PathBuf::new(),
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// d.iter_mut().for_each(|child| *child.value_mut() = "modified".to_owned());
    /// let mut i = d.iter();
    /// assert_eq!(i.next(), Some(&DirChild::new("file1.txt", "modified".to_owned())));
    /// assert_eq!(i.next(), Some(&DirChild::new("file2.txt", "modified".to_owned())));
    /// assert_eq!(i.next(), None);
    /// ```
    pub fn iter_mut(&mut self) -> DirChildrenIterMut<'_, T> {
        DirChildrenIterMut(self.children.iter_mut())
    }
}

impl<T, F> ReadFrom for DirChildren<T, F>
where
    T: DirStructureItem,
    F: Filter,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        let filter = F::make_filter();

        let mut children = Vec::new();
        for child in path.read_dir().wrap_io_error_with(path)? {
            let child = child.wrap_io_error_with(path)?;
            let child_path = child.path();

            if !filter.allows(&child_path) {
                continue;
            }

            let value = T::read_from(&child_path)?;
            let file_name = child.file_name();
            children.push(DirChild { file_name, value });
        }

        Ok(DirChildren {
            self_path: path.to_path_buf(),
            children,
            filter: marker::PhantomData,
        })
    }
}

impl<T, F> WriteTo for DirChildren<T, F>
where
    T: DirStructureItem,
    F: Filter,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        for child in &self.children {
            let child_path = path.join(&child.file_name);
            child.value.write_to(&child_path)?;
        }

        Ok(())
    }
}

/// A single child of a [`DirChildren`] structure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirChild<T>
where
    T: DirStructureItem,
{
    /// The file name of the child.
    file_name: OsString,
    /// The parsed value of the child.
    value: T,
}

impl<T> DirChild<T>
where
    T: DirStructureItem,
{
    /// Creates a new [`DirChild`] with the specified file name and value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::DirChild;
    ///
    /// let d = DirChild::new("file.txt", "file".to_owned());
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// assert_eq!(d.value(), &"file".to_owned());
    /// ```
    pub fn new(file_name: impl Into<OsString>, value: T) -> Self {
        Self {
            file_name: file_name.into(),
            value,
        }
    }

    /// Gets the file name of the child (or the name of the directory; the last segment in the path).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::DirChild;
    ///
    /// let d = DirChild::new("file.txt", "file".to_owned());
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// ```
    pub fn file_name(&self) -> &OsString {
        &self.file_name
    }

    /// Gets the file name of the child (or the name of the directory; the last segment in the path).
    ///
    /// Mutable reference version of [`Self::file_name`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::DirChild;
    ///
    /// let mut d = DirChild::new("file.txt", "file".to_owned());
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// *d.file_name_mut() = OsString::from("new_file.txt");
    /// assert_eq!(d.file_name(), &OsString::from("new_file.txt"));
    /// ```
    pub fn file_name_mut(&mut self) -> &mut OsString {
        &mut self.file_name
    }

    /// Gets the value of the child.
    ///
    /// This is the parsed value of the file / directory.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::DirChild;
    ///
    /// let d = DirChild::new("file.txt", "file".to_owned());
    /// assert_eq!(d.value(), &"file".to_owned());
    /// ```
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Gets the value of the child.
    ///
    /// This is the parsed value of the file / directory.
    ///
    /// Mutable reference version of [`Self::value`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::DirChild;
    ///
    /// let mut d = DirChild::new("file.txt", "file".to_owned());
    /// assert_eq!(d.value(), &"file".to_owned());
    /// *d.value_mut() = "new_file".to_owned();
    /// assert_eq!(d.value(), &"new_file".to_owned());
    /// ```
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }

    /// Maps the file name of this [`DirChild`] to a new value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::DirChild;
    ///
    /// let d = DirChild::new("file.txt", "file".to_owned());
    /// assert_eq!(d.map_file_name(|s| s.to_str().unwrap().to_uppercase()), DirChild::new("FILE.TXT", "file".to_owned()));
    /// ```
    pub fn map_file_name<F, O>(self, f: F) -> Self
    where
        F: FnOnce(OsString) -> O,
        O: Into<OsString>,
    {
        let file_name = f(self.file_name).into();
        DirChild {
            file_name,
            value: self.value,
        }
    }

    /// Maps the value of this [`DirChild`] to a new type.
    ///
    /// This is useful for converting the value to a different type,
    /// for example, if you want to convert the value to a different
    /// type of [`DirStructureItem`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::DirChild;
    /// use dir_structure::FileString;
    ///
    /// let d = DirChild::new("file.txt", "file".to_owned());
    /// assert_eq!(d.map_value(|v| FileString(v)), DirChild::new("file.txt", FileString("file".to_owned())));
    /// ```
    pub fn map_value<U, F>(self, f: F) -> DirChild<U>
    where
        F: FnOnce(T) -> U,
        U: DirStructureItem,
    {
        let value = f(self.value);
        DirChild {
            file_name: self.file_name,
            value,
        }
    }
}

impl<T> IntoIterator for DirChildren<T>
where
    T: DirStructureItem,
{
    type Item = DirChild<T>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

/// A [`DirChildren`] iterator. It iterates over the children of a
/// [`DirChildren`] structure.
///
/// See [`DirChildren::iter`] for more information.
pub struct DirChildrenIter<'a, T: DirStructureItem>(std::slice::Iter<'a, DirChild<T>>);

impl<'a, T> Iterator for DirChildrenIter<'a, T>
where
    T: DirStructureItem,
{
    type Item = &'a DirChild<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T> ExactSizeIterator for DirChildrenIter<'_, T>
where
    T: DirStructureItem,
{
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T> DoubleEndedIterator for DirChildrenIter<'_, T>
where
    T: DirStructureItem,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

/// A mutable iterator over the children of a [`DirChildren`] structure.
/// This allows you to mutate the children of the
/// [`DirChildren`] structure while iterating over them.
///
/// See [`DirChildren::iter_mut`] for more information.
pub struct DirChildrenIterMut<'a, T: DirStructureItem>(std::slice::IterMut<'a, DirChild<T>>);

impl<'a, T> Iterator for DirChildrenIterMut<'a, T>
where
    T: DirStructureItem,
{
    type Item = &'a mut DirChild<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T> ExactSizeIterator for DirChildrenIterMut<'_, T>
where
    T: DirStructureItem,
{
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T> DoubleEndedIterator for DirChildrenIterMut<'_, T>
where
    T: DirStructureItem,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

/// A simple macro that generates a [`DirChildren`] newtype, together with
/// a few impls to make it easy to use.
#[macro_export]
macro_rules! dir_children_wrapper {
    ($vis:vis $name:ident $ty:ty) => {
        $vis struct $name(pub $crate::DirChildren<$ty>);

        impl $crate::ReadFrom for $name {
            fn read_from(path: &::std::path::Path) -> $crate::Result<Self>
            where
                Self: Sized,
            {
                Ok(Self(<$crate::DirChildren<$ty>>::read_from(path)?))
            }
        }

        impl $crate::WriteTo for $name {
            fn write_to(&self, path: &::std::path::Path) -> $crate::Result<()> {
                self.0.write_to(path)
            }
        }

        impl std::ops::Deref for $name {
            type Target = $crate::DirChildren<$ty>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl std::ops::DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl std::iter::IntoIterator for $name {
            type Item = $crate::DirChild<$ty>;
            type IntoIter = std::vec::IntoIter<Self::Item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.into_iter()
            }
        }
    };
}

pub use dir_structure_macros::DirStructure;

macro_rules! data_format_impl {
    (
        $(#[$mod_attr:meta])*
        $mod_name:ident,
        $(#[$main_ty_attrs:meta])*
        $main_ty:ident,

        $from_str_impl:expr,
        $from_str_error:ty,

        $(#[$to_str_ty_attrs:meta])*
        $to_str_ty:ident,
        $to_str_impl:expr,
        $to_writer_impl:expr,
        $to_str_error:ty,

        $(#[$writer_ty_attrs:meta])*
        $writer_ty:ident,

        $extension:literal,
        $text:literal $(,)?
    ) => {
        $(#[$mod_attr])*
        pub mod $mod_name {
            #![doc = concat!(r##"
With the `"##, stringify!($mod_name), r##"` feature, this module provides the [`"##, stringify!($main_ty), r##"`] type,

This allows us to read and parse `"##, stringify!($mod_name), r##"` files to some `serde::Deserialize` type,
and write them back to disk.

# Examples

## Reading a "##, stringify!($mod_name), r##" file

```
use std::path::Path;

use dir_structure::DirStructureItem;
use dir_structure::"##, stringify!($mod_name), "::", stringify!($main_ty), r##";

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "f"##, $extension, r##"", with_newtype = "##, stringify!($main_ty), r##"<Obj>)]
    f: Obj,
}

#[derive(Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
struct Obj {
    name: String,
    age: u32,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let d = Path::new("dir");
    std::fs::create_dir_all(&d)?;
    std::fs::write(d.join("f"##, $extension, r##""), "##, $text, r##")?;
    let dir = Dir::read(&d)?;
    assert_eq!(dir.f, Obj { name: "John".to_owned(), age: 30 });
    # std::fs::remove_dir_all(&d)?;
    Ok(())
}
```

## Writing a "##, stringify!($mod_name), r##" file

```
use std::path::Path;

use dir_structure::DirStructureItem;
use dir_structure::"##, stringify!($mod_name), "::", stringify!($main_ty), r##";

#[derive(dir_structure::DirStructure)]
struct Dir {
    #[dir_structure(path = "f"##, $extension, r##"", with_newtype = "##, stringify!($main_ty), r##"<Obj>)]
    f: Obj,
}

#[derive(Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
struct Obj {
    name: String,
    age: u32,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let d = Path::new("dir");
    let dir = Dir {
        f: Obj {
            name: "John".to_owned(),
            age: 30,
        },
    };
    dir.write(&d)?;
    assert_eq!(std::fs::read_to_string(d.join("f"##, $extension, r##""))?,
        "##, $text, r##"
    );
    # std::fs::remove_dir_all(&d)?;
    Ok(())
}
```
"##)]

            use std::fmt;
            use std::fmt::Formatter;
            use std::path::Path;
            use std::str::FromStr;

            use crate::FromRefForWriter;
            use crate::NewtypeToInner;
            use crate::ReadFrom;
            use crate::WriteTo;

            $(#[$main_ty_attrs])*
            #[derive(Debug, Copy, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Hash)]
            #[serde(transparent)]
            pub struct $main_ty<T>(#[serde(bound = "")] pub T)
            where
                T: 'static + serde::Serialize + for<'d> serde::Deserialize<'d>;

            impl<T> FromStr for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Err = $from_str_error;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    $from_str_impl(s).map(Self)
                }
            }

            $(#[$to_str_ty_attrs])*
            struct $to_str_ty<'a, T>(&'a T)
            where
                T: serde::Serialize + 'a;

            impl<'a, T> $to_str_ty<'a, T>
            where
                T: serde::Serialize + 'a
            {
                fn to_str(&self) -> Result<String, $to_str_error> {
                    $to_str_impl(&self.0)
                }

                fn to_writer<W>(&self, writer: &mut W) -> Result<(), ToWriterError>
                where
                    W: std::io::Write,
                {
                    $to_writer_impl(&self.0, writer)
                }
            }

            enum ToWriterError {
                #[allow(unused)]
                Io(std::io::Error),
                Serde($to_str_error),
            }

            impl<'a, T> fmt::Display for $to_str_ty<'a, T>
            where
                T: serde::Serialize + 'a,
            {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    let s = self.to_str().map_err(|_| fmt::Error)?;
                    write!(f, "{}", s)
                }
            }

            impl<T> fmt::Display for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    $to_str_ty(&self.0).fmt(f)
                }
            }

            impl<T> ReadFrom for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                fn read_from(path: &Path) -> crate::Result<Self> {
                    let contents = crate::FileString::read_from(path)?.0;
                    let v = contents
                        .parse::<$main_ty<T>>()
                        .map_err(|e| crate::Error::Parse(path.to_path_buf(), e.into()))?;
                    Ok(v)
                }
            }

            impl<T> WriteTo for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                fn write_to(&self, path: &Path) -> crate::Result<()> {
                    Self::from_ref_for_writer(&self.0).write_to(path)
                }
            }

            impl<T> NewtypeToInner for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Inner = T;

                fn into_inner(self) -> Self::Inner {
                    self.0
                }
            }

            impl<'a, T> FromRefForWriter<'a> for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Inner = T;
                type Wr = $writer_ty<'a, T>;

                fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
                    $writer_ty(value)
                }
            }

            $(#[$writer_ty_attrs])*
            pub struct $writer_ty<'a, T>(&'a T)
            where
                T: serde::Serialize + 'a;

            impl<'a, T> WriteTo for $writer_ty<'a, T>
            where
                T: serde::Serialize + 'a,
            {
                fn write_to(&self, path: &Path) -> crate::Result<()> {
                    let mut f = crate::sfw::StreamingFileWriter::new(path)?;
                    $to_str_ty(self.0).to_writer(&mut f)
                        .map_err(|e| match e {
                            ToWriterError::Io(e) => crate::Error::Io(path.to_path_buf(), e),
                            ToWriterError::Serde(e) => crate::Error::Serde(path.to_path_buf(), e.into()),
                        })?;

                    Ok(())
                }
            }
        }
    };
}

data_format_impl!(
    #[cfg(feature = "json")]
    #[cfg_attr(docsrs, doc(cfg(feature = "json")))]
    json,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to json when we read / write a
    /// directory structure.
    Json,
    |s| serde_json::from_str(s),
    serde_json::Error,
    JsonToStr,
    |v| serde_json::to_string(&v),
    |v, w| serde_json::to_writer(w, v).map_err(ToWriterError::Serde),
    serde_json::Error,
    /// [`FromRefForWriter`] implementation for [`Json`].
    JsonRefWr,
    ".json", r##"r#"{"name":"John","age":30}"#"##,
);

data_format_impl!(
    #[cfg(feature = "toml")]
    #[cfg_attr(docsrs, doc(cfg(feature = "toml")))]
    toml,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to toml when we read / write a
    /// directory structure.
    Toml,
    |s| toml::de::from_str(s),
    toml::de::Error,
    TomlToStr,
    |v| toml::ser::to_string(&v),
    |v, w: &mut dyn std::io::Write| {
        let s = toml::ser::to_string(&v).map_err(ToWriterError::Serde)?;
        w.write_all(s.as_bytes()).map_err(ToWriterError::Io)?;
        Ok(())
    },
    toml::ser::Error,
    /// [`FromRefForWriter`] implementation for [`Toml`].
    TomlRefWr,
    ".toml", r##"r#"
name = "John"
age = 30
"#.trim_start()"##,
);

data_format_impl!(
    #[cfg(feature = "yaml")]
    #[cfg_attr(docsrs, doc(cfg(feature = "yaml")))]
    yaml,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to yaml when we read / write a
    /// directory structure.
    Yaml,
    |s| serde_yaml::from_str(s),
    serde_yaml::Error,
    YamlToStr,
    |v| serde_yaml::to_string(&v),
    |v, w| serde_yaml::to_writer(w, v).map_err(ToWriterError::Serde),
    serde_yaml::Error,
    /// [`FromRefForWriter`] implementation for [`Yaml`].
    YamlRefWr,
    ".yaml", r##"r#"
name: John
age: 30
"#.trim_start()"##,
);

data_format_impl!(
    #[cfg(feature = "ron")]
    #[cfg_attr(docsrs, doc(cfg(feature = "ron")))]
    ron,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to ron when we read / write a
    /// directory structure.
    Ron,
    |s| ron::de::from_str(s),
    ron::error::SpannedError,
    RonToStr,
    |v| ron::ser::to_string(&v),
    |v, w| ron::ser::to_writer(w, v).map_err(ToWriterError::Serde),
    ron::error::Error,
    /// [`FromRefForWriter`] implementation for [`Ron`].
    RonRefWr,
    ".ron", r##"r#"(name:"John",age:30)"#"##,
);

/// A wrapper around a type which will use the [`Display`] and [`FromStr`] implementations
/// for serialization / deserialization.
///
/// For example: u8, i8, i16, u16, all integer types... bool etc.
///
/// # Examples
///
/// ```rust
/// use std::path::Path;
/// use dir_structure::DirStructureItem;
///
/// use dir_structure::FmtWrapper;
///
/// #[derive(dir_structure::DirStructure, PartialEq, Debug)]
/// struct Dir {
///    #[dir_structure(path = "f.txt", with_newtype = FmtWrapper<u8>)]
///    f: u8,
///    #[dir_structure(path = "b.txt", with_newtype = FmtWrapper<bool>)]
///    b: bool,
/// }
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let d = Path::new("dir");
///     std::fs::create_dir_all(&d)?;
///     std::fs::write(d.join("f.txt"), "42")?;
///     std::fs::write(d.join("b.txt"), "true")?;
///     let mut dir = Dir::read(&d)?;
///     assert_eq!(dir.f, 42);
///     assert_eq!(dir.b, true);
///     dir.f = 100;
///     dir.b = false;
///     dir.write(&d)?;
///     assert_eq!(std::fs::read_to_string(d.join("f.txt"))?, "100");
///     assert_eq!(std::fs::read_to_string(d.join("b.txt"))?, "false");
///     # std::fs::remove_dir_all(&d)?;
///     Ok(())
/// }
/// ```
pub struct FmtWrapper<T>(pub T);

impl<T> NewtypeToInner for FmtWrapper<T> {
    type Inner = T;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

impl<T> ReadFrom for FmtWrapper<T>
where
    T: FromStr,
    T::Err: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        let contents = FileString::read_from(path)?.0;
        match contents.parse::<T>() {
            Ok(v) => Ok(Self(v)),
            Err(e) => Err(Error::Parse(path.to_path_buf(), e.into())),
        }
    }
}

impl<T> WriteTo for FmtWrapper<T>
where
    T: Display,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

impl<'a, T> FromRefForWriter<'a> for FmtWrapper<T>
where
    T: Display + 'a,
{
    type Inner = T;
    type Wr = FmtWrapperRefWr<'a, T>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FmtWrapperRefWr(value)
    }
}

/// A [`WriteTo`] wrapper around a reference to a type which will use the [`Display`]
/// implementation to write the value.
pub struct FmtWrapperRefWr<'a, T: ?Sized>(pub &'a T);

impl<T> WriteTo for FmtWrapperRefWr<'_, T>
where
    T: Display + ?Sized,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        use std::io::Write;
        utils::create_parent_dir(path)?;
        let mut f = File::create(path).wrap_io_error_with(path)?;
        write!(f, "{}", self.0).wrap_io_error_with(path)?;
        Ok(())
    }
}

/// A newtype around a `Vec<u8>`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileBytes(pub Vec<u8>);

impl FileBytes {
    /// Creates a new [`FileBytes`] from the specified `Vec<u8>`.
    pub fn new(v: impl Into<Vec<u8>>) -> Self {
        Self(v.into())
    }
}

impl ReadFrom for FileBytes {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        std::fs::read(path).wrap_io_error_with(path).map(Self)
    }
}

impl WriteTo for FileBytes {
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

impl From<FileBytes> for Vec<u8> {
    fn from(value: FileBytes) -> Self {
        value.0
    }
}

impl From<Vec<u8>> for FileBytes {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl NewtypeToInner for FileBytes {
    type Inner = Vec<u8>;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

impl<'a> FromRefForWriter<'a> for FileBytes {
    type Inner = [u8];
    type Wr = FileBytesRefWr<'a>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileBytesRefWr(value)
    }
}

/// The [`WriteTo`] wrapper around a reference to a `[u8]`.
pub struct FileBytesRefWr<'a>(&'a [u8]);

impl WriteTo for FileBytesRefWr<'_> {
    fn write_to(&self, path: &Path) -> Result<()> {
        utils::create_parent_dir(path)?;
        std::fs::write(path, self.0).wrap_io_error_with(path)?;
        Ok(())
    }
}

/// A newtype around a [`String`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileString(pub String);

impl FileString {
    /// Creates a new [`FileString`] from the specified [`String`].
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }
}

impl From<FileString> for String {
    fn from(value: FileString) -> Self {
        value.0
    }
}

impl From<String> for FileString {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl NewtypeToInner for FileString {
    type Inner = String;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

impl ReadFrom for FileString {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        std::fs::read_to_string(path)
            .wrap_io_error_with(path)
            .map(Self)
    }
}

impl WriteTo for FileString {
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

impl<'a> FromRefForWriter<'a> for FileString {
    type Inner = str;
    type Wr = FileStrWr<'a>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileStrWr(value)
    }
}

/// The [`WriteTo`] wrapper around a reference to a [`str`].
pub struct FileStrWr<'a>(&'a str);

impl WriteTo for FileStrWr<'_> {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytes::from_ref_for_writer(self.0.as_bytes()).write_to(path)
    }
}

impl<T> ReadFrom for Option<T>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        if path.exists() {
            T::read_from(path).map(Some)
        } else {
            Ok(None)
        }
    }
}

impl<T> WriteTo for Option<T>
where
    T: WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        if let Some(v) = self {
            v.write_to(path)
        } else {
            Ok(())
        }
    }
}

/// A wrapper that defers the reading of a file until it is actually needed.
///
/// The only thing you can do with a [`DeferredRead`] is to call [`DeferredRead::perform_read`],
/// which will read the file and return the value.
///
/// See the [`DeferredRead::perform_read`] method for more details.
#[derive(Debug, Clone, Hash)]
pub struct DeferredRead<T>(pub PathBuf, marker::PhantomData<T>)
where
    T: ReadFrom;

impl<T> ReadFrom for DeferredRead<T>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(path.to_path_buf(), marker::PhantomData))
    }
}

impl<T> DeferredRead<T>
where
    T: ReadFrom,
{
    /// Performs the read and returns the value.
    ///
    /// If the value changed on disk since the [`DeferredRead`] was created, then the
    /// new value will be read from disk and returned.
    ///
    /// For a cached version see [`DeferredReadOrOwn`].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::Path;
    /// use dir_structure::DirStructureItem;
    /// use dir_structure::DeferredRead;
    ///
    /// #[derive(dir_structure::DirStructure)]
    /// struct Dir {
    ///     #[dir_structure(path = "f.txt")]
    ///     f: DeferredRead<String>,
    /// }
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let d = Path::new("dir");
    ///
    ///     std::fs::create_dir_all(&d)?;
    ///     std::fs::write(d.join("f.txt"), "Hello, world!")?;
    ///
    ///     let dir = Dir::read(&d)?;
    ///     assert_eq!(dir.f.perform_read()?, "Hello, world!");
    ///
    ///     std::fs::write(d.join("f.txt"), "Goodbye, world!")?;
    ///     assert_eq!(dir.f.perform_read()?, "Goodbye, world!");
    ///
    ///     # std::fs::remove_dir_all(&d)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn perform_read(&self) -> Result<T> {
        T::read_from(&self.0)
    }
}

impl<T> WriteTo for DeferredRead<T>
where
    T: ReadFrom + WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        if path == self.0 {
            // Optimization: We were asked to write to the same path
            // we are supposed to read from. We can just ignore it, since
            // the file / directory should already be in the given state.

            // If `T` has trivial `ReadFrom` / `WriteTo` implementations,
            // this should not be a problem, but if it is, a custom `DeferredRead`
            // implementation should be written for it.
            return Ok(());
        }

        let r = self.perform_read()?;
        r.write_to(path)
    }
}

/// A wrapper that defers the reading of a file until it is actually needed,
/// but can also store the value.
///
/// It allows us to read the value from disk, and then store it in memory,
/// and if we ever need it again, we can just return the stored value.
///
/// This type exposes 2 functions: [`DeferredReadOrOwn::get`] and
/// [`DeferredReadOrOwn::perform_and_store_read`].
///
/// The table below summarizes the differences between the two functions:
///
/// | State             | [`DeferredReadOrOwn::get`]               | [`DeferredReadOrOwn::perform_and_store_read`] |
/// |-------------------|------------------------------------------|-----------------------------------------------|
/// | New, not cached   | Reads the value, does not cache          | Reads the value, and caches it                |
/// | Cached            | Returns the cached value                 | Returns the cached value                      |
///
/// As such, [`DeferredReadOrOwn::get`] has the signature of `fn(&self) -> Result<T>` and
/// [`DeferredReadOrOwn::perform_and_store_read`] has the signature of `fn(&mut self) -> Result<&T>`.
///
/// If you never call [`DeferredReadOrOwn::perform_and_store_read`], and only ever call [`DeferredReadOrOwn::get`],
/// that would effectively be the same as using a [`DeferredRead`], and that should be preferred instead.
#[derive(Debug, Clone, Hash)]
pub enum DeferredReadOrOwn<T>
where
    T: ReadFrom,
{
    Own(T),
    Deferred(DeferredRead<T>),
}

impl<T> DeferredReadOrOwn<T>
where
    T: ReadFrom,
{
    /// Gets the value. If it is not already read, it will read it, but without saving it.
    ///
    /// This is useful if you want to read the value, but you don't want to store it.
    ///
    /// Though never calling [`DeferredReadOrOwn::perform_and_store_read`] and only calling
    /// [`DeferredReadOrOwn::get`] is equivalent to using a [`DeferredRead`], and that should be preferred.
    ///
    /// See [`DeferredReadOrOwn`] for more details.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::Path;
    /// use dir_structure::DirStructureItem;
    /// use dir_structure::DeferredRead;
    /// use dir_structure::DeferredReadOrOwn;
    /// use dir_structure::ReadFrom;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let d = Path::new("dir");
    ///     std::fs::create_dir_all(&d)?;
    ///     let deferred = DeferredReadOrOwn::<String>::Deferred(
    ///         DeferredRead::read_from(&d.join("f.txt")).unwrap()
    ///     );
    ///     assert!(deferred.get().is_err());
    ///     std::fs::write(d.join("f.txt"), "Hello, world!")?;
    ///     assert_eq!(deferred.get()?, "Hello, world!");
    ///     std::fs::write(d.join("f.txt"), "Goodbye, world!")?;
    ///     assert_eq!(deferred.get()?, "Goodbye, world!");
    ///     # std::fs::remove_dir_all(&d)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn get(&self) -> Result<T>
    where
        T: Clone,
    {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own.clone()),
            DeferredReadOrOwn::Deferred(d) => Ok(d.perform_read()?),
        }
    }

    /// Performs the read and stores the value. If the value is already read, it will
    /// just return a reference to it.
    ///
    /// See [`DeferredReadOrOwn`] for more details.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::Path;
    /// use dir_structure::DirStructureItem;
    /// use dir_structure::DeferredRead;
    /// use dir_structure::DeferredReadOrOwn;
    /// use dir_structure::ReadFrom;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let d = Path::new("dir");
    ///     std::fs::create_dir_all(&d)?;
    ///     let mut deferred = DeferredReadOrOwn::<String>::Deferred(
    ///         DeferredRead::read_from(&d.join("f.txt")).unwrap()
    ///     );
    ///     assert!(deferred.perform_and_store_read().is_err());
    ///     std::fs::write(d.join("f.txt"), "Hello, world!")?;
    ///     assert_eq!(deferred.perform_and_store_read()?, "Hello, world!");
    ///     std::fs::write(d.join("f.txt"), "Goodbye, world!")?;
    ///     assert_eq!(deferred.perform_and_store_read()?, "Hello, world!");
    ///     # std::fs::remove_dir_all(&d)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn perform_and_store_read(&mut self) -> Result<&mut T> {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own),
            DeferredReadOrOwn::Deferred(d) => {
                let value = d.perform_read()?;
                *self = DeferredReadOrOwn::Own(value);
                let DeferredReadOrOwn::Own(own) = self else {
                    unreachable!()
                };
                Ok(own)
            }
        }
    }
}

impl<T> ReadFrom for DeferredReadOrOwn<T>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        ReadFrom::read_from(path).map(Self::Deferred)
    }
}

impl<T> WriteTo for DeferredReadOrOwn<T>
where
    T: ReadFrom + WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        match self {
            DeferredReadOrOwn::Own(own) => own.write_to(path),
            DeferredReadOrOwn::Deferred(d) => d.write_to(path),
        }
    }
}

/// A newtype that will clean the directory it is written to, before writing
/// the value.
///
/// This is useful when we want to write a directory structure, but we want
/// to make sure that the directory is clean before writing it, so that there
/// are no old files / directories left in it.
///
/// ```rust
/// use std::path::Path;
///
/// use dir_structure::DirStructureItem;
/// use dir_structure::CleanDir;
///
/// #[derive(dir_structure::DirStructure)]
/// struct Dir {
///    #[dir_structure(path = "f.txt")]
///    f: String,
/// }
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let d = Path::new("dir");
///     std::fs::create_dir_all(&d)?;
///     std::fs::write(d.join("f.txt"), "Hello, world!")?;
///     std::fs::write(d.join("f2.txt"), "Hello, world! (2)")?;
///     let dir = Dir::read(&d)?;
///     assert_eq!(dir.f, "Hello, world!");
///     assert_eq!(std::fs::read_to_string(d.join("f2.txt"))?, "Hello, world! (2)");
///     CleanDir(dir).write(&d)?;
///     assert_eq!(std::fs::read_to_string(d.join("f.txt"))?, "Hello, world!");
///     assert!(!d.join("f2.txt").exists());
///     # std::fs::remove_dir_all(&d)?;
///     Ok(())
/// }
/// ```
pub struct CleanDir<T: DirStructureItem>(pub T);

impl<T> ReadFrom for CleanDir<T>
where
    T: DirStructureItem,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(T::read_from(path)?))
    }
}

impl<T> WriteTo for CleanDir<T>
where
    T: DirStructureItem,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

impl<'a, T> FromRefForWriter<'a> for CleanDir<T>
where
    T: DirStructureItem + 'a,
{
    type Inner = T;
    type Wr = CleanDirRefWr<'a, T>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        CleanDirRefWr(value)
    }
}

impl<T> NewtypeToInner for CleanDir<T>
where
    T: DirStructureItem,
{
    type Inner = T;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

/// [`WriteTo`] impl for [`CleanDir`]
pub struct CleanDirRefWr<'a, T: ?Sized + DirStructureItem>(&'a T);

impl<T> WriteTo for CleanDirRefWr<'_, T>
where
    T: ?Sized + DirStructureItem,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        if path.exists() {
            std::fs::remove_dir_all(path).wrap_io_error_with(path)?;
        } else {
            utils::create_parent_dir(path)?;
        }
        self.0.write_to(path)
    }
}

/// A versioned value. This is a wrapper around a value that will keep track of
/// how many times it has been changed. This is useful to not write the value
/// to disk if it hasn't changed.
///
/// You can get a reference to the value via its [`Deref`] implementation, and
/// you can get a mutable reference to the value via its [`DerefMut`] implementation.
///
/// The version is incremented every time [`DerefMut::deref_mut`] is called.
///
/// Alternatively, for [`Eq`] types, you can use the [`Versioned::edit_eq_check`]
/// method to edit the value, and it will increment the version if the value has changed.
///
/// # Example
///
/// ```
/// use dir_structure::VersionedString;
///
/// let mut v = VersionedString::new("value".to_owned(), "path");
/// assert!(v.is_clean());
/// assert!(!v.is_dirty());
///
/// *v = "new value".to_owned();
/// assert!(v.is_dirty());
/// ```
#[derive(Debug, Clone, Hash)]
pub struct Versioned<T: DirStructureItem> {
    value: T,
    version: usize,
    path: PathBuf,
}

impl<T: DirStructureItem> Versioned<T> {
    const DEFAULT_VERSION: usize = 0;

    /// Creates a new [`Versioned`] with the specified value.
    ///
    /// The version is set to the default value.
    pub fn new(value: T, path: impl Into<PathBuf>) -> Self {
        Self {
            value,
            version: Self::DEFAULT_VERSION,
            path: path.into(),
        }
    }

    /// Creates a new [`Versioned`] with the specified value, and in a dirty state.
    ///
    /// # Example
    ///
    /// ```
    /// use dir_structure::VersionedString;
    ///
    /// let v = VersionedString::new_dirty("value".to_owned(), "path");
    /// assert!(v.is_dirty());
    /// ```
    pub fn new_dirty(value: T, path: impl Into<PathBuf>) -> Self {
        Self {
            value,
            version: Self::DEFAULT_VERSION + 1,
            path: path.into(),
        }
    }

    /// Checks if the value has been changed.
    pub fn is_dirty(&self) -> bool {
        !self.is_clean()
    }

    /// Checks if the value has not been changed.
    pub fn is_clean(&self) -> bool {
        self.version == Self::DEFAULT_VERSION
    }

    /// Edits the value using the provided closure, and increments the version
    /// if the value has changed.
    ///
    /// # Example
    ///
    /// ```
    /// use dir_structure::VersionedString;
    ///
    /// let mut v = VersionedString::new("value".to_owned(), "path");
    ///
    /// v.edit_eq_check(|s| *s = "value".to_owned());
    /// assert!(v.is_clean());
    /// v.edit_eq_check(|s| *s = "new value".to_owned());
    /// assert!(v.is_dirty());
    /// ```
    pub fn edit_eq_check(&mut self, f: impl FnOnce(&mut T))
    where
        T: Eq + Clone,
    {
        let copy = self.value.clone();

        f(&mut self.value);

        if copy != self.value {
            self.version += 1;
        }
    }

    /// Resets the version to the default value, making the value clean.
    /// This is useful if you want to mark the value as not changed,
    /// without actually changing it.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it allows you to reset the version to 0,
    /// which means that the value will be considered clean, and any unsaved changes
    /// will be lost. Trying to save a clean value (e.g. after calling this function) will *not* write it to disk!
    ///
    /// Use with caution!
    /// 
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{DirStructureItem, VersionedString};
    /// std::fs::write("path", "value").unwrap();
    ///
    /// let mut v = VersionedString::new("value".to_owned(), "path");
    /// assert!(v.is_clean());
    /// v.edit_eq_check(|s| *s = "new value".to_owned());
    /// assert!(v.is_dirty());
    /// unsafe { v.reset(); }
    /// assert!(v.is_clean());
    ///
    /// // if you try to write it now, it won't write anything,
    /// v.write("path").unwrap();
    /// 
    /// assert_eq!(std::fs::read_to_string("path").unwrap(), "value");
    /// # std::fs::remove_file("path").unwrap();
    /// ```
    pub unsafe fn reset(&mut self) {
        // This is unsafe because it allows us to reset the version to 0,
        // which means that the value will be considered clean.
        // Use with caution!
        self.version = Self::DEFAULT_VERSION;
    }
}

impl<T: DirStructureItem> ReadFrom for Versioned<T> {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        T::read_from(path).map(|it| Self::new(it, path))
    }
}

impl<T: DirStructureItem> WriteTo for Versioned<T> {
    fn write_to(&self, path: &Path) -> Result<()> {
        if self.path == path && self.is_clean() {
            return Ok(());
        }

        self.value.write_to(path)
    }
}

impl<T: DirStructureItem> Deref for Versioned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: DirStructureItem> DerefMut for Versioned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // We will assume that the value has changed, if `deref_mut` was called.
        // So we increment the version.
        self.version += 1;

        &mut self.value
    }
}

pub type VersionedString = Versioned<String>;
pub type VersionedBytes = Versioned<Vec<u8>>;

// Impls for std types.

impl ReadFrom for String {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        FileString::read_from(path).map(|v| v.0)
    }
}

impl WriteTo for String {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileString::from_ref_for_writer(self).write_to(path)
    }
}

impl ReadFrom for Vec<u8> {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        FileBytes::read_from(path).map(|v| v.0)
    }
}

impl WriteTo for Vec<u8> {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytes::from_ref_for_writer(self).write_to(path)
    }
}

impl WriteTo for str {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileStrWr(self).write_to(path)
    }
}

impl WriteTo for &str {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileStrWr(self).write_to(path)
    }
}

impl WriteTo for [u8] {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytesRefWr(self).write_to(path)
    }
}

impl WriteTo for &[u8] {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytesRefWr(self).write_to(path)
    }
}

mod utils {
    use crate::WrapIoError;

    pub fn create_parent_dir(path: &std::path::Path) -> crate::Result<()> {
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                std::fs::create_dir_all(parent).wrap_io_error_with(parent)?;
            }
        }
        Ok(())
    }
}

#[cfg(any(feature = "json", feature = "toml", feature = "yaml", feature = "ron"))]
mod sfw {
    struct StreamingFileWriter {
        f: File,
    }

    impl StreamingFileWriter {
        fn new(path: &Path) -> Result<Self> {
            utils::create_parent_dir(path)?;
            let f = File::create(path).wrap_io_error_with(path)?;
            Ok(Self { f })
        }
    }

    impl std::io::Write for StreamingFileWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.f.write(buf)
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.f.flush()
        }
    }

    impl std::fmt::Write for StreamingFileWriter {
        fn write_str(&mut self, s: &str) -> std::fmt::Result {
            use std::io::Write;

            self.f
                .write_all(s.as_bytes())
                .map_err(|_| std::fmt::Error)?;
            Ok(())
        }
    }
}
