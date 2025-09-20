//! A structure representing the children of a directory.
//!
//! See [`DirChildren`] for more details.
//!
//! Additionally, [`ForceCreateDirChildren`] is a variant that forces the creation of the directory
//! structure, even without any children.

use std::fmt;
use std::hash;
use std::marker;
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::RangeBounds;
use std::path::Path;
use std::pin::Pin;
use std::slice;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;
use std::vec;

#[cfg(feature = "async")]
use futures::Stream;
#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::NoFilter;
use crate::error::Error;
use crate::error::VfsResult;
use crate::prelude::*;
#[cfg(feature = "async")]
use crate::traits::asy::ReadFromAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::WriteSupportingVfsAsync;
#[cfg(feature = "resolve-path")]
use crate::traits::resolve::DynamicHasField;
use crate::traits::sync::DirStructureItem;
use crate::traits::vfs;
use crate::traits::vfs::DirEntryInfo;
use crate::traits::vfs::DirWalker as _;
#[cfg(feature = "resolve-path")]
use crate::traits::vfs::OwnedPathType;
use crate::traits::vfs::PathType;
#[cfg(feature = "async")]
use crate::traits::vfs::VfsCore;

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
#[derive(PartialEq, Eq)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct DirChildren<T, F: Filter<P> = NoFilter, P: PathType + ?Sized = Path> {
    /// The children of the root directory.
    pub children: Vec<DirChild<T, P>>,

    #[cfg_attr(feature = "assert_eq", assert_eq(ignore))]
    filter: marker::PhantomData<(F, P)>,
}

impl<T, P: PathType + ?Sized, F: Filter<P>> Clone for DirChildren<T, F, P>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            children: self.children.clone(),
            filter: marker::PhantomData,
        }
    }
}

impl<T, P: PathType + ?Sized, F: Filter<P>> fmt::Debug for DirChildren<T, F, P>
where
    T: fmt::Debug,
    P::PathSegmentOwned: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DirChildren")
            .field("children", &self.children)
            .finish()
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
#[cfg_attr(feature = "derive", doc = "```rust")]
#[cfg_attr(not(feature = "derive"), doc = "```rust,compile_fail")]
/// use std::path::Path;
/// use std::path::PathBuf;
///
/// use dir_structure::{DirStructure, traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, Filter}};
/// use dir_structure::prelude::*;
///
/// pub struct TextFileFilter;
///
/// impl Filter<Path> for TextFileFilter {
///     fn allows(path: &Path) -> bool {
///         path.extension()
///             .and_then(|s| s.to_str())
///             .map_or(false, |s| s == "txt")
///     }
/// }
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let path = PathBuf::from("dir");
///     #[derive(DirStructure)]
///     struct Dir<Vfs: VfsCore<Path = Path>> {
///        #[dir_structure(path = self)]
///        text_files: DirChildren<String, TextFileFilter, Vfs::Path>,
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
pub trait Filter<P: PathType + ?Sized> {
    /// Checks if the path is allowed by this filter.
    fn allows(path: &P) -> bool;
}

impl<P: PathType + ?Sized> Filter<P> for NoFilter {
    fn allows(_path: &P) -> bool {
        true
    }
}

/// Creates a [`Filter`] type that only allows files / folders with a specific [extension](Path::extension).
///
/// # Examples
///
/// ```rust
/// use std::path::Path;
/// use dir_structure::dir_children::Filter;
///
/// dir_structure::ext_filter!(RustFile, "rs");
///
/// assert!(RustFile::allows(Path::new("main.rs")));
/// assert!(RustFile::allows(Path::new("src/a/b/mod.rs")));
/// assert!(!RustFile::allows(Path::new("main.txt")));
/// ```
#[macro_export]
macro_rules! ext_filter {
    ($vis:vis $name:ident, $Ext:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        $vis struct $name;

        impl $crate::dir_children::Filter<::std::path::Path> for $name {
            fn allows(path: &::std::path::Path) -> bool {
                path.extension()
                    .map_or(false, |s| s == $Ext)
            }
        }
    };
}

/// Creates a [`Filter`] type that only allows files / folders with a specific [stem](Path::file_stem).
///
/// # Examples
///
/// ```rust
/// use std::path::Path;
/// use dir_structure::dir_children::Filter;
///
/// dir_structure::stem_filter!(MainFile, "main");
///
/// assert!(MainFile::allows(Path::new("main.rs")));
/// assert!(MainFile::allows(Path::new("src/a/b/main.rs")));
/// assert!(MainFile::allows(Path::new("main.txt")));
/// assert!(!MainFile::allows(Path::new("main.a.rs")));
/// ```
#[macro_export]
macro_rules! stem_filter {
    ($vis:vis $name:ident, $base_name:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        $vis struct $name;

        impl $crate::dir_children::Filter<::std::path::Path> for $name {
            fn allows(path: &::std::path::Path) -> bool {
                path.file_stem()
                    .and_then(|s| s.to_str())
                    .map_or(false, |s| s == $base_name)
            }
        }
    };
}

/// Creates a [`Filter`] type that only allows files / folders with a specific [prefix](Path::file_prefix).
///
/// # Examples
///
/// ```rust
/// use std::path::Path;
/// use dir_structure::dir_children::Filter;
///
/// dir_structure::file_prefix_filter!(LogFile, "log");
///
/// assert!(LogFile::allows(Path::new("log.txt")));
/// assert!(LogFile::allows(Path::new("log")));
/// assert!(LogFile::allows(Path::new("log.log")));
/// assert!(LogFile::allows(Path::new("src/a/b/log.txt")));
/// assert!(!LogFile::allows(Path::new("src/a/b/file.txt")));
/// ```
#[macro_export]
macro_rules! file_prefix_filter {
    ($vis:vis $name:ident, $file_prefix:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        $vis struct $name;

        impl $crate::dir_children::Filter<::std::path::Path> for $name {
            fn allows(path: &::std::path::Path) -> bool {
                path.file_prefix()
                    .and_then(|s| s.to_str())
                    .map_or(false, |s| s == $file_prefix)
            }
        }
    };
}

impl<T, P: PathType + ?Sized, F: Filter<P>> Default for DirChildren<T, F, P> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, F: Filter<P>, P: PathType + ?Sized> DirChildren<T, F, P> {
    /// Creates an empty [`DirChildren`], with no children.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::{dir_children::DirChildren, NoFilter};
    ///
    /// let d = DirChildren::<String, NoFilter>::new();
    /// assert!(d.is_empty());
    /// ```
    pub fn new() -> Self {
        Self {
            children: Vec::new(),
            filter: marker::PhantomData,
        }
    }

    /// Creates a [`DirChildren`] with the given path and children.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_children::{DirChildren, DirChild}, NoFilter};
    ///
    /// let d = DirChildren::<String, NoFilter>::with_children_from_iter(
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// assert!(!d.is_empty());
    /// ```
    pub fn with_children_from_iter(children: impl IntoIterator<Item = DirChild<T, P>>) -> Self {
        Self {
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
    /// use std::pin::Pin;
    /// use dir_structure::{dir_children::{DirChildren, DirChild}, prelude::*};
    ///
    /// #[derive(Debug, PartialEq, Eq)]
    /// struct NewType(String);
    ///
    /// let dir = DirChildren::<_, dir_structure::NoFilter>::with_children_from_iter(
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///         DirChild::new("file3.txt", "file3".to_owned()),
    ///     ],
    /// );
    /// let dir = dir.map::<_, _, dir_structure::NoFilter, Path>(|child| child.map_value(NewType));
    /// assert_eq!(
    ///     dir,
    ///     DirChildren::with_children_from_iter(
    ///         vec![
    ///             DirChild::new("file1.txt", NewType("file1".to_owned())),
    ///             DirChild::new("file2.txt", NewType("file2".to_owned())),
    ///             DirChild::new("file3.txt", NewType("file3".to_owned())),
    ///         ],
    ///     )
    /// );
    /// ```
    pub fn map<U, MapF, F2, P2>(self, f: MapF) -> DirChildren<U, F2, P2>
    where
        MapF: FnMut(DirChild<T, P>) -> DirChild<U, P2>,
        F2: Filter<P2>,
        P2: PathType + ?Sized,
    {
        let children = self.children.into_iter().map(f).collect();
        DirChildren {
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
    /// use dir_structure::traits::vfs::PathType;
    /// use dir_structure::dir_children::{Filter, DirChildren};
    ///
    /// struct NewFilter;
    ///
    /// impl<P: PathType + ?Sized> Filter<P> for NewFilter {
    ///     fn allows(_path: &P) -> bool {
    ///         true
    ///     }
    /// }
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// let d2: DirChildren<String, NewFilter> = d.map_filter::<NewFilter>();
    /// ```
    pub fn map_filter<NewF>(self) -> DirChildren<T, NewF, P>
    where
        NewF: Filter<P>,
    {
        DirChildren {
            children: self.children,
            filter: marker::PhantomData,
        }
    }

    /// Returns the number of children.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.len(), 0);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
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

    /// Returns true if there are no children.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert!(d.is_empty());
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// assert!(!d.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    /// Gets the child at the specified index.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get(0), None);
    /// assert_eq!(d.get(1), None);
    /// assert_eq!(d.get(100), None);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
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
    pub fn get(&self, index: usize) -> Option<&DirChild<T, P>> {
        self.children.get(index)
    }

    /// Gets a mutable reference to the child at the specified index.
    /// This is a mutable version of [`get`][Self::get].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get_mut(0), None);
    /// assert_eq!(d.get_mut(1), None);
    /// assert_eq!(d.get_mut(100), None);
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
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
    pub fn get_mut(&mut self, index: usize) -> Option<&mut DirChild<T, P>> {
        self.children.get_mut(index)
    }

    /// Gets the child with the specified "file" name (last segment of path).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get_name(""), None);
    /// assert_eq!(d.get_name("any_name"), None);
    /// assert_eq!(d.get_name("aaaa"), None);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
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
    pub fn get_name(&self, name: impl AsRef<P::PathSegmentRef>) -> Option<&DirChild<T, P>>
    where
        P::PathSegmentRef: PartialEq,
    {
        self.children
            .iter()
            .find(|child| child.file_name.as_ref() == name.as_ref())
    }

    /// Gets the child with the specified "file" name (last segment of path).
    /// This is a mutable version of [`get_name`][Self::get_name].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get_name_mut(""), None);
    /// assert_eq!(d.get_name_mut("any_name"), None);
    /// assert_eq!(d.get_name_mut("aaaa"), None);
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// assert_eq!(d.get_name_mut("file1.txt"), Some(&mut DirChild::new("file1.txt", "file1".to_owned())));
    /// assert_eq!(d.get_name_mut("file2.txt"), Some(&mut DirChild::new("file2.txt", "file2".to_owned())));
    /// assert_eq!(d.get_name_mut("any_name"), None);
    /// assert_eq!(d.get_name_mut("aaaa"), None);
    /// ```
    pub fn get_name_mut(
        &mut self,
        name: impl AsRef<P::PathSegmentRef>,
    ) -> Option<&mut DirChild<T, P>>
    where
        P::PathSegmentRef: PartialEq,
    {
        self.children
            .iter_mut()
            .find(|child| child.file_name.as_ref() == name.as_ref())
    }

    /// Gets the value of the child with the specified "file" name (last segment of path).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get_value_by_name(""), None);
    /// assert_eq!(d.get_value_by_name("any_name"), None);
    /// assert_eq!(d.get_value_by_name("aaaa"), None);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
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
    pub fn get_value_by_name(&self, name: impl AsRef<P::PathSegmentRef>) -> Option<&T>
    where
        P::PathSegmentRef: PartialEq,
    {
        self.get_name(name).map(|child| &child.value)
    }

    /// Gets the value of the child with the specified "file" name (last segment of path).
    /// This is a mutable version of [`get_value_by_name`][Self::get_value_by_name].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// assert_eq!(d.get_value_by_name_mut(""), None);
    /// assert_eq!(d.get_value_by_name_mut("any_name"), None);
    /// assert_eq!(d.get_value_by_name_mut("aaaa"), None);
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// assert_eq!(d.get_value_by_name_mut("file1.txt"), Some(&mut "file1".to_owned()));
    /// assert_eq!(d.get_value_by_name_mut("file2.txt"), Some(&mut "file2".to_owned()));
    /// assert_eq!(d.get_value_by_name_mut("any_name"), None);
    /// assert_eq!(d.get_value_by_name_mut("aaaa"), None);
    /// ```
    pub fn get_value_by_name_mut(&mut self, name: impl AsRef<P::PathSegmentRef>) -> Option<&mut T>
    where
        P::PathSegmentRef: PartialEq,
    {
        self.get_name_mut(name).map(|child| &mut child.value)
    }

    /// Returns an iterator over the children.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::new();
    /// let mut i = d.iter();
    /// assert_eq!(i.next(), None);
    ///
    /// let d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
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
    pub fn iter(&self) -> DirChildrenIter<'_, T, P> {
        DirChildrenIter(self.children.iter())
    }

    /// Returns a mutable iterator over the children.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
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
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    ///
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
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
    pub fn iter_mut(&mut self) -> DirChildrenIterMut<'_, T, P> {
        DirChildrenIterMut(self.children.iter_mut())
    }

    /// Pushes a new child to the end of the children list.
    ///
    /// This method takes a file name and a value, and creates a new `DirChild`
    /// with the given file name and value, then pushes it to the end of the children
    /// list.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::new();
    ///
    /// d.push("file1.txt", "file1".to_owned());
    ///
    /// let mut i = d.iter();
    /// assert_eq!(i.next(), Some(&DirChild::new("file1.txt", "file1".to_owned())));
    /// assert_eq!(i.next(), None);
    ///
    /// d.push("file2.txt", "file2".to_owned());
    ///
    /// let mut i = d.iter();
    /// assert_eq!(i.next(), Some(&DirChild::new("file1.txt", "file1".to_owned())));
    /// assert_eq!(i.next(), Some(&DirChild::new("file2.txt", "file2".to_owned())));
    /// assert_eq!(i.next(), None);
    /// ```
    pub fn push(&mut self, file_name: impl Into<P::PathSegmentOwned>, value: T) {
        self.children.push(DirChild {
            file_name: file_name.into(),
            value,
        });
    }

    /// Retains only the children specified by the predicate.
    ///
    /// The predicate is a closure that takes a reference to a `DirChild<T>`
    /// and returns `true` if the child should be kept, or `false` if it should be removed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///     ],
    /// );
    /// d.retain(|child| child.file_name() != "file1.txt");
    /// let mut i = d.iter();
    /// assert_eq!(i.next(), Some(&DirChild::new("file2.txt", "file2".to_owned())));
    /// assert_eq!(i.next(), None);
    /// ```
    pub fn retain(&mut self, f: impl FnMut(&DirChild<T, P>) -> bool) {
        self.children.retain(f);
    }

    /// Drains the children in the specified range, returning an iterator over the removed children.
    /// The range is specified using the standard Rust range syntax.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///         DirChild::new("file3.txt", "file3".to_owned()),
    ///     ],
    /// );
    /// let drained: Vec<_> = d.drain(0..1).collect();
    /// assert_eq!(drained, vec![DirChild::new("file1.txt", "file1".to_owned())]);
    /// let mut i = d.iter();
    /// assert_eq!(i.next(), Some(&DirChild::new("file2.txt", "file2".to_owned())));
    /// assert_eq!(i.next(), Some(&DirChild::new("file3.txt", "file3".to_owned())));
    /// assert_eq!(i.next(), None);
    /// ```
    pub fn drain(&mut self, range: impl RangeBounds<usize>) -> DirChildrenDrain<'_, T, P> {
        DirChildrenDrain(self.children.drain(range))
    }

    /// Extracts the children in the specified range that satisfy the given predicate,
    /// returning an iterator over the removed children.
    ///
    /// The range is specified using the standard Rust range syntax.
    /// The predicate is a closure that takes a mutable reference to a `DirChild<T>`
    /// and returns `true` if the child should be removed, or `false` if it should be kept.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{traits::sync::{DirStructure, DirStructureItem}, dir_children::{DirChildren, DirChild}};
    /// let mut d = DirChildren::<String, dir_structure::NoFilter>::with_children_from_iter(
    ///     vec![
    ///         DirChild::new("file1.txt", "file1".to_owned()),
    ///         DirChild::new("file2.txt", "file2".to_owned()),
    ///         DirChild::new("file3.txt", "file3".to_owned()),
    ///         DirChild::new("file4.txt", "file4".to_owned()),
    ///     ],
    /// );
    /// let extracted: Vec<_> = d.extract_if(1..3, |child| child.file_name() == "file2.txt").collect();
    /// assert_eq!(extracted, vec![DirChild::new("file2.txt", "file2".to_owned())]);
    ///
    /// let mut i = d.iter();
    /// assert_eq!(i.next(), Some(&DirChild::new("file1.txt", "file1".to_owned())));
    /// assert_eq!(i.next(), Some(&DirChild::new("file3.txt", "file3".to_owned())));
    /// assert_eq!(i.next(), Some(&DirChild::new("file4.txt", "file4".to_owned())));
    /// assert_eq!(i.next(), None);
    /// ```
    pub fn extract_if<'a, Fi>(
        &'a mut self,
        range: impl RangeBounds<usize>,
        filter: Fi,
    ) -> DirChildrenExtractIf<'a, T, P, Fi>
    where
        Fi: FnMut(&mut DirChild<T, P>) -> bool,
    {
        DirChildrenExtractIf(self.children.extract_if(range, filter))
    }
}

impl<T, P: PathType + ?Sized, F> From<Vec<DirChild<T, P>>> for DirChildren<T, F, P>
where
    F: Filter<P>,
{
    fn from(children: Vec<DirChild<T, P>>) -> Self {
        Self {
            children,
            filter: marker::PhantomData,
        }
    }
}

impl<T, P: PathType + ?Sized, F> Extend<DirChild<T, P>> for DirChildren<T, F, P>
where
    F: Filter<P>,
{
    fn extend<I: IntoIterator<Item = DirChild<T, P>>>(&mut self, iter: I) {
        self.children.extend(iter);
    }
}

impl<T, P: PathType + ?Sized, F> FromIterator<DirChild<T, P>> for DirChildren<T, F, P>
where
    F: Filter<P>,
{
    fn from_iter<I: IntoIterator<Item = DirChild<T, P>>>(iter: I) -> Self {
        Self::with_children_from_iter(iter)
    }
}

/// An iterator that drains the children of a [`DirChildren`].
///
/// See [`DirChildren::drain`].
pub struct DirChildrenDrain<'a, T, P: PathType + ?Sized>(vec::Drain<'a, DirChild<T, P>>);

impl<T, P: PathType + ?Sized> Iterator for DirChildrenDrain<'_, T, P> {
    type Item = DirChild<T, P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T, P: PathType + ?Sized> ExactSizeIterator for DirChildrenDrain<'_, T, P> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T, P: PathType + ?Sized> DoubleEndedIterator for DirChildrenDrain<'_, T, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<'a, T, P: PathType + ?Sized + 'a, F: Filter<P>, Vfs: vfs::Vfs<'a, Path = P>> ReadFrom<'a, Vfs>
    for DirChildren<T, F, P>
where
    T: ReadFrom<'a, Vfs>,
    F: 'a,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        let mut children = Vec::new();
        let mut walker = vfs.walk_dir(path)?;
        while let Some(child) = walker.next() {
            let DirEntryInfo {
                name,
                path: child_path,
                ..
            } = child?;

            if !F::allows(child_path.as_ref()) {
                continue;
            }

            let value = T::read_from(child_path.as_ref(), vfs)?;
            children.push(DirChild {
                file_name: name,
                value,
            });
        }

        Ok(DirChildren {
            children,
            filter: marker::PhantomData,
        })
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DirChildrenReadAsyncFutureProjOwn)]
#[doc(hidden)]
pub enum DirChildrenReadAsyncFuture<'a, T, F, Vfs: VfsAsync + 'a>
where
    T: ReadFromAsync<'a, Vfs> + 'static,
    F: Filter<Vfs::Path> + Send + 'static,
    T::Future: Future<Output = VfsResult<T, Vfs>> + Send + Unpin + 'a,
{
    Poison,
    Init(
        Pin<Box<Vfs::DirWalkFuture<'a>>>,
        Vec<DirChild<T, Vfs::Path>>,
        <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        Pin<&'a Vfs>,
        PhantomData<F>,
    ),
    Begin(
        Pin<Box<Vfs::DirWalk<'a>>>,
        Vec<DirChild<T, Vfs::Path>>,
        <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        Pin<&'a Vfs>,
    ),
    ReadAsync(
        Pin<Box<Vfs::DirWalk<'a>>>,
        Vec<DirChild<T, Vfs::Path>>,
        <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        T::Future,
        <Vfs::Path as PathType>::PathSegmentOwned,
        Pin<&'a Vfs>,
    ),
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, F, Vfs: VfsAsync + 'a> Future for DirChildrenReadAsyncFuture<'a, T, F, Vfs>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
    F: Filter<Vfs::Path> + Send + 'static,
    T::Future: Future<Output = VfsResult<T, Vfs>> + Unpin + 'static,
{
    type Output = VfsResult<DirChildren<T, F, Vfs::Path>, Vfs>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);

        match this {
            DirChildrenReadAsyncFutureProjOwn::Init(mut entries, children, path, vfs, _) => {
                match entries.as_mut().poll(cx) {
                    Poll::Ready(Ok(entries)) => {
                        self.project_replace(DirChildrenReadAsyncFuture::Begin(
                            Box::pin(entries),
                            children,
                            path,
                            vfs,
                        ));
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(DirChildrenReadAsyncFuture::Init(
                            entries,
                            children,
                            path,
                            vfs,
                            PhantomData::<F>,
                        ));
                        Poll::Pending
                    }
                }
            }
            DirChildrenReadAsyncFutureProjOwn::Begin(mut entries, children, path, vfs) => {
                use std::task::Poll;

                match entries.as_mut().poll_next(cx) {
                    Poll::Ready(Some(Ok(DirEntryInfo {
                        name,
                        path: path_child,
                        kind: _,
                    }))) => {
                        if !F::allows(path_child.as_ref()) {
                            self.project_replace(DirChildrenReadAsyncFuture::Begin(
                                entries, children, path, vfs,
                            ));
                            cx.waker().wake_by_ref();
                            return Poll::Pending;
                        }

                        let value_future = T::read_from_async(path_child, vfs);
                        self.project_replace(DirChildrenReadAsyncFuture::ReadAsync(
                            entries,
                            children,
                            path,
                            value_future,
                            name,
                            vfs,
                        ));
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(None) => Poll::Ready(Ok(DirChildren {
                        children,
                        filter: marker::PhantomData,
                    })),
                    Poll::Ready(Some(Err(e))) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(DirChildrenReadAsyncFuture::Begin(
                            entries, children, path, vfs,
                        ));
                        Poll::Pending
                    }
                }
            }
            DirChildrenReadAsyncFutureProjOwn::ReadAsync(
                entries,
                mut children,
                path,
                mut value_fut,
                file_name,
                vfs,
            ) => match Pin::<&mut T::Future>::new(&mut value_fut).poll(cx) {
                Poll::Ready(Ok(value)) => {
                    children.push(DirChild { file_name, value });
                    self.project_replace(DirChildrenReadAsyncFuture::Begin(
                        entries, children, path, vfs,
                    ));
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                Poll::Pending => {
                    self.project_replace(DirChildrenReadAsyncFuture::ReadAsync(
                        entries,
                        children,
                        path,
                        value_fut,
                        file_name.clone(),
                        vfs,
                    ));
                    Poll::Pending
                }
            },
            DirChildrenReadAsyncFutureProjOwn::Poison => {
                panic!("DirChildrenReadAsyncFuture is poisoned, this should never happen");
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, F, Vfs: VfsAsync + 'a> ReadFromAsync<'a, Vfs> for DirChildren<T, F, Vfs::Path>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
    F: Filter<Vfs::Path> + Send + 'static,
    T::Future: Future<Output = VfsResult<T, Vfs>> + Unpin + 'static,
{
    type Future = DirChildrenReadAsyncFuture<'a, T, F, Vfs>;

    fn read_from_async(
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        let f = Box::pin(vfs.walk_dir(path.clone()));
        DirChildrenReadAsyncFuture::Init(f, Vec::new(), path, vfs, PhantomData::<F>)
    }
}

impl<'a, T, F, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for DirChildren<T, F, Vfs::Path>
where
    T: WriteTo<'a, Vfs>,
    F: Filter<Vfs::Path>,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        for child in &self.children {
            let child_path = path.join_segment(&child.file_name);
            child.value.write_to(child_path.as_ref(), vfs)?;
        }

        Ok(())
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, F, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs>
    for DirChildren<T, F, Vfs::Path>
where
    T: WriteToAsync<'a, Vfs> + Send + Sync + 'static,
    F: Filter<Vfs::Path> + Send + 'static,
    T::Future: Future<Output = VfsResult<(), Vfs>> + Unpin + 'a,
{
    type Future = DirChildrenWriteAsyncFuture<'a, T, Vfs>;

    fn write_to_async(
        self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        DirChildrenWriteAsyncFuture::Init(self.into_iter(), path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DirChildrenWriteAsyncFutureProjOwn)]
#[doc(hidden)]
pub enum DirChildrenWriteAsyncFuture<'a, T, Vfs: WriteSupportingVfsAsync + 'static>
where
    T: WriteToAsync<'a, Vfs>,
    <T as WriteToAsync<'a, Vfs>>::Future: Future<Output = VfsResult<(), Vfs>> + Unpin,
{
    Poison,
    Init(
        DirChildrenIntoIter<T, <Vfs as VfsCore>::Path>,
        <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        Pin<&'a Vfs>,
    ),
    Write(
        DirChildrenIntoIter<T, <Vfs as VfsCore>::Path>,
        <T as WriteToAsync<'a, Vfs>>::Future,
        <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        Pin<&'a Vfs>,
    ),
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> Future
    for DirChildrenWriteAsyncFuture<'a, T, Vfs>
where
    T: WriteToAsync<'a, Vfs>,
    <T as WriteToAsync<'a, Vfs>>::Future: Future<Output = VfsResult<(), Vfs>> + Unpin,
{
    type Output = VfsResult<(), Vfs>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);

        match this {
            DirChildrenWriteAsyncFutureProjOwn::Init(mut iter, path, vfs) => {
                if let Some(child) = iter.next() {
                    let fut = child
                        .value
                        .write_to_async(path.as_ref().join_segment(&child.file_name), vfs);
                    self.project_replace(Self::Write(iter, fut, path.clone(), vfs));
                    cx.waker().wake_by_ref();
                    Poll::Pending
                } else {
                    Poll::Ready(Ok(()))
                }
            }
            DirChildrenWriteAsyncFutureProjOwn::Write(mut iter, mut fut, path, vfs) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(Ok(())) => {
                        if let Some(child) = iter.next() {
                            let new_fut = child
                                .value
                                .write_to_async(path.as_ref().join_segment(&child.file_name), vfs);
                            self.project_replace(Self::Write(iter, new_fut, path.clone(), vfs));
                            cx.waker().wake_by_ref();
                            Poll::Pending
                        } else {
                            Poll::Ready(Ok(()))
                        }
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::Write(iter, fut, path.clone(), vfs));
                        Poll::Pending
                    }
                }
            }
            DirChildrenWriteAsyncFutureProjOwn::Poison => {
                panic!("DirChildrenWriteAsyncFuture is poisoned, this should never happen");
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'r, T, F, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsyncRef<'r, Vfs>
    for DirChildren<T, F, Vfs::Path>
where
    T: WriteToAsyncRef<'r, Vfs> + Send + Sync + 'static,
    F: Filter<Vfs::Path> + Send + 'static,
    for<'f> <T as WriteToAsyncRef<'r, Vfs>>::Future<'f>:
        Future<Output = VfsResult<(), Vfs>> + Unpin + 'f,
{
    type Future<'a>
        = DirChildrenWriteAsyncRefFuture<'r, 'a, T, Vfs>
    where
        T: 'a,
        Vfs: 'a,
        Self: 'a,
        'r: 'a;

    fn write_to_async_ref<'a>(
        &'a self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> <Self as WriteToAsyncRef<'r, Vfs>>::Future<'a>
    where
        'r: 'a,
    {
        DirChildrenWriteAsyncRefFuture::Init(self.iter(), path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DirChildrenWriteAsyncRefFutureProjOwn)]
#[doc(hidden)]
pub enum DirChildrenWriteAsyncRefFuture<'r, 'f, T, Vfs: WriteSupportingVfsAsync + 'static>
where
    T: WriteToAsyncRef<'r, Vfs> + 'r,
    <T as WriteToAsyncRef<'r, Vfs>>::Future<'f>: Future<Output = VfsResult<(), Vfs>> + Unpin + 'f,
    'r: 'f,
{
    Poison,
    Init(
        DirChildrenIter<'f, T, Vfs::Path>,
        <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        Pin<&'f Vfs>,
    ),
    Write(
        DirChildrenIter<'f, T, Vfs::Path>,
        <T as WriteToAsyncRef<'r, Vfs>>::Future<'f>,
        <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        Pin<&'f Vfs>,
    ),
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'r, 'f, T, Vfs: WriteSupportingVfsAsync + 'static> Future
    for DirChildrenWriteAsyncRefFuture<'r, 'f, T, Vfs>
where
    T: WriteToAsyncRef<'r, Vfs>,
    <T as WriteToAsyncRef<'r, Vfs>>::Future<'f>: Future<Output = VfsResult<(), Vfs>> + Unpin + 'f,
    'r: 'f,
{
    type Output = VfsResult<(), Vfs>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);

        match this {
            DirChildrenWriteAsyncRefFutureProjOwn::Init(mut iter, path, vfs) => {
                if let Some(child) = iter.next() {
                    let fut = child
                        .value
                        .write_to_async_ref(path.as_ref().join_segment(&child.file_name), vfs);
                    self.project_replace(Self::Write(iter, fut, path.clone(), vfs));
                    cx.waker().wake_by_ref();
                    Poll::Pending
                } else {
                    Poll::Ready(Ok(()))
                }
            }
            DirChildrenWriteAsyncRefFutureProjOwn::Write(mut iter, mut fut, path, vfs) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(Ok(())) => {
                        if let Some(child) = iter.next() {
                            let new_fut = child.value.write_to_async_ref(
                                path.as_ref().join_segment(&child.file_name),
                                vfs,
                            );
                            self.project_replace(Self::Write(iter, new_fut, path.clone(), vfs));
                            cx.waker().wake_by_ref();
                            Poll::Pending
                        } else {
                            Poll::Ready(Ok(()))
                        }
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::Write(iter, fut, path.clone(), vfs));
                        Poll::Pending
                    }
                }
            }
            DirChildrenWriteAsyncRefFutureProjOwn::Poison => {
                panic!("DirChildrenWriteAsyncRefFuture is poisoned, this should never happen");
            }
        }
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<T, F, P: PathType + ?Sized> DynamicHasField for DirChildren<T, F, P>
where
    F: Filter<P>,
{
    type Inner = T;

    fn resolve_path<Pt: OwnedPathType>(mut p: Pt, name: &str) -> Pt {
        p.push_segment_str(name);
        p
    }
}

/// A single child of a [`DirChildren`] structure.
#[derive(PartialEq, Eq)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct DirChild<T, P: PathType + ?Sized = Path> {
    /// The file name of the child.
    file_name: P::PathSegmentOwned,
    /// The parsed value of the child.
    value: T,
}

impl<T, P: PathType + ?Sized> Clone for DirChild<T, P>
where
    T: Clone,
    P::PathSegmentOwned: Clone,
{
    fn clone(&self) -> Self {
        Self {
            file_name: self.file_name.clone(),
            value: self.value.clone(),
        }
    }
}

impl<T, P: PathType + ?Sized> fmt::Debug for DirChild<T, P>
where
    T: fmt::Debug,
    P::PathSegmentOwned: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DirChild")
            .field("file_name", &self.file_name)
            .field("value", &self.value)
            .finish()
    }
}

impl<T, P: PathType + ?Sized> DirChild<T, P> {
    /// Creates a new [`DirChild`] with the specified file name and value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use std::path::Path;
    /// use dir_structure::dir_children::DirChild;
    ///
    /// let d = DirChild::<_, Path>::new("file.txt".to_owned(), "file".to_owned());
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// assert_eq!(d.value(), &"file".to_owned());
    /// ```
    pub fn new(file_name: impl Into<P::PathSegmentOwned>, value: T) -> Self {
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
    /// use std::path::Path;
    /// use dir_structure::dir_children::DirChild;
    ///
    /// let d = DirChild::<_, Path>::new("file.txt".to_owned(), "file".to_owned());
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// ```
    pub fn file_name(&self) -> &P::PathSegmentOwned {
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
    /// use std::path::Path;
    /// use dir_structure::dir_children::DirChild;
    ///
    /// let mut d = DirChild::<_, Path>::new("file.txt".to_owned(), "file".to_owned());
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// *d.file_name_mut() = OsString::from("new_file.txt");
    /// assert_eq!(d.file_name(), &OsString::from("new_file.txt"));
    /// ```
    pub fn file_name_mut(&mut self) -> &mut P::PathSegmentOwned {
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
    /// use std::path::Path;
    /// use dir_structure::dir_children::DirChild;
    ///
    /// let d = DirChild::<_, Path>::new("file.txt".to_owned(), "file".to_owned());
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
    /// use std::path::Path;
    /// use dir_structure::dir_children::DirChild;
    ///
    /// let mut d = DirChild::<_, Path>::new("file.txt".to_owned(), "file".to_owned());
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
    /// use std::path::Path;
    /// use dir_structure::dir_children::DirChild;
    ///
    /// let d = DirChild::<_, Path>::new("file.txt".to_owned(), "file".to_owned());
    /// assert_eq!(d.map_file_name(|s| s.to_str().unwrap().to_uppercase()), DirChild::new("FILE.TXT", "file".to_owned()));
    /// ```
    pub fn map_file_name<F, O>(self, f: F) -> Self
    where
        F: FnOnce(P::PathSegmentOwned) -> O,
        O: Into<P::PathSegmentOwned>,
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
    /// use std::path::Path;
    /// use dir_structure::dir_children::DirChild;
    /// use dir_structure::std_types::FileString;
    ///
    /// let d = DirChild::<_, Path>::new("file.txt".to_owned(), "file".to_owned());
    /// assert_eq!(d.map_value(|v| FileString(v)), DirChild::new("file.txt".to_owned(), FileString("file".to_owned())));
    /// ```
    pub fn map_value<U, F>(self, f: F) -> DirChild<U, P>
    where
        F: FnOnce(T) -> U,
    {
        let value = f(self.value);
        DirChild {
            file_name: self.file_name,
            value,
        }
    }
}

impl<T, P: PathType + ?Sized> Deref for DirChild<T, P> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T, P: PathType + ?Sized> DerefMut for DirChild<T, P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

/// A [`DirChildren`] iterator. It iterates over the children of a
/// [`DirChildren`] structure.
///
/// See [`DirChildren::iter`] for more information.
pub struct DirChildrenIter<'a, T, P: PathType + ?Sized>(slice::Iter<'a, DirChild<T, P>>);

impl<'a, T, P: PathType + ?Sized> Iterator for DirChildrenIter<'a, T, P> {
    type Item = &'a DirChild<T, P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T, P: PathType + ?Sized> ExactSizeIterator for DirChildrenIter<'_, T, P>
where
    T: DirStructureItem,
{
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T, P: PathType + ?Sized> DoubleEndedIterator for DirChildrenIter<'_, T, P>
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
pub struct DirChildrenIterMut<'a, T, P: PathType + ?Sized>(slice::IterMut<'a, DirChild<T, P>>);

impl<'a, T, P: PathType + ?Sized> Iterator for DirChildrenIterMut<'a, T, P>
where
    T: DirStructureItem,
{
    type Item = &'a mut DirChild<T, P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T, P: PathType + ?Sized> ExactSizeIterator for DirChildrenIterMut<'_, T, P>
where
    T: DirStructureItem,
{
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T, P: PathType + ?Sized> DoubleEndedIterator for DirChildrenIterMut<'_, T, P>
where
    T: DirStructureItem,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<T, F, P: ?Sized + PathType> IntoIterator for DirChildren<T, F, P>
where
    F: Filter<P>,
{
    type Item = DirChild<T, P>;
    type IntoIter = DirChildrenIntoIter<T, P>;

    fn into_iter(self) -> Self::IntoIter {
        DirChildrenIntoIter(self.children.into_iter())
    }
}

impl<'a, T, F, P: PathType + ?Sized> IntoIterator for &'a DirChildren<T, F, P>
where
    F: Filter<P>,
{
    type Item = &'a DirChild<T, P>;
    type IntoIter = DirChildrenIter<'a, T, P>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T, F, P: PathType + ?Sized> IntoIterator for &'a mut DirChildren<T, F, P>
where
    F: Filter<P>,
{
    type Item = &'a mut DirChild<T, P>;
    type IntoIter = DirChildrenIterMut<'a, T, P>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

/// An owned iterator over the children of a [`DirChildren`] structure.
///
/// See [`DirChildren::into_iter`] for more information.
pub struct DirChildrenIntoIter<T, P: PathType + ?Sized>(vec::IntoIter<DirChild<T, P>>);

impl<T, P: PathType + ?Sized> Iterator for DirChildrenIntoIter<T, P> {
    type Item = DirChild<T, P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T, P: PathType + ?Sized> ExactSizeIterator for DirChildrenIntoIter<T, P> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T, P: PathType + ?Sized> DoubleEndedIterator for DirChildrenIntoIter<T, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

/// An iterator that extracts children from a [`DirChildren`] structure
/// that satisfy a given predicate.
///
/// See [`DirChildren::extract_if`] for more information.
pub struct DirChildrenExtractIf<'a, T, P: PathType + ?Sized, F: FnMut(&mut DirChild<T, P>) -> bool>(
    vec::ExtractIf<'a, DirChild<T, P>, F>,
);

impl<'a, T, P: PathType + ?Sized, F: FnMut(&mut DirChild<T, P>) -> bool> Iterator
    for DirChildrenExtractIf<'a, T, P, F>
{
    type Item = DirChild<T, P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

/// A wrapper around [`DirChildren`] that adds the <'vfs, Vfs> generics.
#[macro_export]
macro_rules! dir_children_wrapper_with_vfs {
    ($vis:vis $name:ident $ty:ident $(<Path=$p_ty:ty>)?) => {
        $vis struct $name<'vfs, Vfs: $crate::traits::vfs::VfsCore $(<Path = $p_ty>)? + 'vfs>(pub $crate::dir_children::DirChildren<$ty<'vfs, Vfs>, $crate::NoFilter, Vfs::Path>)
        where
            Vfs: $crate::traits::vfs::VfsCore + 'vfs;

        impl<'vfs, Vfs: $crate::traits::vfs::Vfs<'vfs $(, Path = $p_ty)?> + 'vfs> $crate::traits::sync::ReadFrom<'vfs, Vfs> for $name<'vfs, Vfs> {
            fn read_from(path: &Vfs::Path, vfs: ::std::pin::Pin<&'vfs Vfs>) -> $crate::error::VfsResult<Self, Vfs>
            where
                Self: Sized,
            {
                Ok(Self(<$crate::dir_children::DirChildren<$ty<'vfs, Vfs>, $crate::NoFilter, Vfs::Path>>::read_from(path, vfs)?))
            }
        }

        impl<'vfs, Vfs: $crate::traits::vfs::WriteSupportingVfs<'vfs $(, Path = $p_ty)?> + 'vfs> $crate::traits::sync::WriteTo<'vfs, Vfs> for $name<'vfs, Vfs> {
            fn write_to(&self, path: &Vfs::Path, vfs: ::std::pin::Pin<&'vfs Vfs>) -> $crate::error::VfsResult<(), Vfs> {
                self.0.write_to(path, vfs)
            }
        }

        impl<'vfs, Vfs: $crate::traits::vfs::VfsCore $(<Path = $p_ty>)? + 'vfs> std::ops::Deref for $name<'vfs, Vfs> {
            type Target = $crate::dir_children::DirChildren<$ty<'vfs, Vfs>, $crate::NoFilter, Vfs::Path>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl<'vfs, Vfs: $crate::traits::vfs::VfsCore $(<Path = $p_ty>)? + 'vfs> std::ops::DerefMut for $name<'vfs, Vfs> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl<'vfs, Vfs: $crate::traits::vfs::VfsCore $(<Path = $p_ty>)?+ 'vfs> std::iter::IntoIterator for $name<'vfs, Vfs> {
            type Item = $crate::dir_children::DirChild<$ty<'vfs, Vfs>, Vfs::Path>;
            type IntoIter = $crate::dir_children::DirChildrenIntoIter<$ty<'vfs, Vfs>, Vfs::Path>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.into_iter()
            }
        }

        impl<'vfs, Vfs: $crate::traits::vfs::VfsCore $(<Path = $p_ty>)? + 'vfs> $crate::traits::resolve::DynamicHasField for $name<'vfs, Vfs> where $crate::dir_children::DirChildren<$ty<'vfs, Vfs>, $crate::NoFilter, Vfs::Path>: $crate::traits::resolve::DynamicHasField {
            type Inner = <$crate::dir_children::DirChildren<$ty<'vfs, Vfs>, $crate::NoFilter, Vfs::Path> as $crate::traits::resolve::DynamicHasField>::Inner;

            fn resolve_path<P: $crate::traits::vfs::OwnedPathType>(p: P, field: &str) -> P {
                <$crate::dir_children::DirChildren<$ty<'vfs, Vfs>, $crate::NoFilter, Vfs::Path> as $crate::traits::resolve::DynamicHasField>::resolve_path(p, field)
            }
        }

        impl<'vfs, Vfs: $crate::traits::vfs::VfsCore $(<Path = $p_ty>)? + 'vfs> $crate::traits::resolve::DynamicHasFieldNoNewtype for $name<'vfs, Vfs> {}
    };
}

/// A structure that represents a directory where only one of the children pass the filter `F`.
///
/// This is useful if you want to select one specific file, by the [file prefix](Path::file_prefix) /
/// [stem](Path::file_stem), but you don't care about the extension.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct DirChildSingle<T, F: Filter<P>, P: PathType + ?Sized = Path> {
    /// The file name of the child.
    file_name: P::PathSegmentOwned,
    /// The parsed value of the child.
    value: T,
    #[cfg_attr(feature = "assert_eq", assert_eq(ignore))]
    _phantom: PhantomData<(F, P)>,
}

impl<'a, T, F, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for DirChildSingle<T, F, Vfs::Path>
where
    T: ReadFrom<'a, Vfs>,
    F: Filter<Vfs::Path> + 'a,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        let children = DirChildren::<T, F, Vfs::Path>::read_from(path, vfs)?;
        if children.len() != 1 {
            return Err(Error::UnexpectedNumberOfChildren {
                expected: "1",
                found: children.len(),
                path: path.owned(),
            });
        }

        let child = children.children.into_iter().next().unwrap();
        Ok(DirChildSingle {
            file_name: child.file_name,
            value: child.value,
            _phantom: PhantomData,
        })
    }
}

impl<'a, T, F: Filter<Vfs::Path>, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs>
    for DirChildSingle<T, F, Vfs::Path>
where
    T: WriteTo<'a, Vfs>,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        let child_path = path.join_segment(&self.file_name);
        self.value.write_to(child_path.as_ref(), vfs)
    }
}

impl<T, F: Filter<P>, P: PathType + ?Sized> DirChildSingle<T, F, P> {
    /// Creates a new [`DirChildSingle`] with the specified file name and value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::NoFilter;
    /// use dir_structure::dir_children::DirChildSingle;
    ///
    /// let d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// assert_eq!(d.value(), &"file".to_owned());
    /// ```
    pub fn new(file_name: impl Into<P::PathSegmentOwned>, value: T) -> Self {
        Self {
            file_name: file_name.into(),
            value,
            _phantom: PhantomData,
        }
    }

    /// Gets the file name of the child (or the name of the directory; the last segment in the path).
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::NoFilter;
    /// use dir_structure::dir_children::DirChildSingle;
    ///
    /// let d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// assert_eq!(d.value(), &"file".to_owned());
    /// ```
    pub fn file_name(&self) -> &P::PathSegmentOwned {
        &self.file_name
    }

    /// Gets the file name of the child (or the name of the directory; the last segment in the path). Mutable version of [`file_name`][Self::file_name].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::NoFilter;
    /// use dir_structure::dir_children::DirChildSingle;
    ///
    /// let mut d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// assert_eq!(d.file_name_mut(), &mut OsString::from("file.txt"));
    /// assert_eq!(d.value_mut(), &mut "file".to_owned());
    /// ```
    pub fn file_name_mut(&mut self) -> &mut P::PathSegmentOwned {
        &mut self.file_name
    }

    /// Gets the value of the child.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::NoFilter;
    /// use dir_structure::dir_children::DirChildSingle;
    ///
    /// let d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// assert_eq!(d.value(), &"file".to_owned());
    /// ```
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Gets the value of the child. Mutable reference version of [`Self::value`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::NoFilter;
    /// use dir_structure::dir_children::DirChildSingle;
    ///
    /// let mut d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// assert_eq!(d.value_mut(), &mut "file".to_owned());
    /// ```
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }

    /// Converts &[`DirChildSingle`]<T, F> to [`DirChildSingle`]<&T, F>.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::NoFilter;
    /// use dir_structure::dir_children::DirChildSingle;
    ///
    /// let d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// let d_ref: DirChildSingle<&String, NoFilter> = d.as_ref();
    /// assert_eq!(d_ref.file_name(), &OsString::from("file.txt"));
    /// assert_eq!(d_ref.value(), &&"file".to_owned());
    /// ```
    pub fn as_ref(&self) -> DirChildSingle<&T, F, P> {
        DirChildSingle {
            file_name: self.file_name.clone(),
            value: &self.value,
            _phantom: PhantomData,
        }
    }

    /// Converts &mut [`DirChildSingle`]<T, F> to [`DirChildSingle`]<&mut T, F>.
    ///
    /// This clones the [`OsString`] and [`PathBuf`] used for the name and path.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::NoFilter;
    /// use dir_structure::dir_children::DirChildSingle;
    ///
    /// let mut d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// let mut d_mut: DirChildSingle<&mut String, NoFilter> = d.as_mut();
    /// d_mut.value_mut().push_str("_modified");
    /// assert_eq!(d.value(), &"file_modified".to_owned());
    /// ```
    pub fn as_mut(&mut self) -> DirChildSingle<&mut T, F, P> {
        DirChildSingle {
            file_name: self.file_name.clone(),
            value: &mut self.value,
            _phantom: PhantomData,
        }
    }

    /// Maps the value of the child to a new value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::NoFilter;
    /// use dir_structure::dir_children::DirChildSingle;
    ///
    /// let d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// let d2 = d.map(|s| s.to_uppercase());
    /// assert_eq!(d2.value(), &"FILE".to_owned());
    /// ```
    pub fn map<T2, F2>(self, f: F2) -> DirChildSingle<T2, F, P>
    where
        F2: FnOnce(T) -> T2,
    {
        DirChildSingle {
            file_name: self.file_name,
            value: f(self.value),
            _phantom: PhantomData,
        }
    }

    /// Map the filter to another [`Filter`] type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::NoFilter;
    /// use dir_structure::dir_children::DirChildSingle;
    /// use dir_structure::traits::vfs::PathType;
    ///
    /// struct Filt;
    ///
    /// impl<P: PathType + ?Sized> dir_structure::dir_children::Filter<P> for Filt {
    ///     fn allows(_path: &P) -> bool {
    ///         true
    ///     }
    /// }
    ///
    /// let d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// let d2: DirChildSingle<_, Filt> = d.map_filter::<Filt>();
    /// assert_eq!(d2.file_name(), &OsString::from("file.txt"));
    /// assert_eq!(d2.value(), &"file".to_owned());
    /// ```
    pub fn map_filter<F2>(self) -> DirChildSingle<T, F2, P>
    where
        F2: Filter<P>,
    {
        DirChildSingle {
            file_name: self.file_name,
            value: self.value,
            _phantom: PhantomData,
        }
    }
}

impl<T, F, P: PathType + ?Sized> fmt::Debug for DirChildSingle<T, F, P>
where
    F: Filter<P>,
    T: fmt::Debug,
    <P as PathType>::PathSegmentOwned: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DirChildSingle")
            .field("file_name", &self.file_name)
            .field("value", &self.value)
            .finish()
    }
}

impl<T, F, P: PathType + ?Sized> Deref for DirChildSingle<T, F, P>
where
    F: Filter<P>,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T, F, P: PathType + ?Sized> DerefMut for DirChildSingle<T, F, P>
where
    F: Filter<P>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

/// A similar idea to [`DirChildSingle`], but allows for the absence of a matching entry.
#[derive(Clone, PartialEq, Eq)]
// #[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub enum DirChildSingleOpt<T, F: Filter<P>, P: PathType + ?Sized = Path> {
    /// The entry is absent.
    None,
    /// The entry is present.
    Some(DirChildSingle<T, F, P>),
}

impl<T, F: Filter<P>, P: PathType + ?Sized> hash::Hash for DirChildSingleOpt<T, F, P>
where
    T: hash::Hash,
    <P as PathType>::PathSegmentOwned: hash::Hash,
{
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            DirChildSingleOpt::None => {
                0u8.hash(state);
            }
            DirChildSingleOpt::Some(child) => {
                1u8.hash(state);
                child.hash(state);
            }
        }
    }
}

#[cfg(feature = "assert_eq")]
impl<T, F: Filter<P>, P: PathType + ?Sized> assert_eq::AssertEq for DirChildSingleOpt<T, F, P>
where
    T: assert_eq::AssertEq + fmt::Debug,
    <P as PathType>::PathSegmentOwned: assert_eq::AssertEq + fmt::Debug,
{
    fn assert_eq(
        &self,
        other: &Self,
        path: &mut ::assert_eq::AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        match (self, other) {
            (DirChildSingleOpt::None, DirChildSingleOpt::None) => {}
            (DirChildSingleOpt::Some(a), DirChildSingleOpt::Some(b)) => {
                let __g = &mut *path.__guard("[Some]");
                a.assert_eq(b, &mut *__g.__guard(".0"), init_left, init_right);
            }
            (a, b) => panic!("DirChildSingleOpt not equal: {:?} != {:?}", a, b),
        }
    }
}

impl<T, F: Filter<P>, P: PathType + ?Sized> DirChildSingleOpt<T, F, P> {
    /// Creates a new [`DirChildSingleOpt`] with the specified file name and value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::NoFilter;
    ///
    /// let DirChildSingleOpt::Some(d) = DirChildSingleOpt::<_, NoFilter>::new("file.txt", "file".to_owned()) else {
    ///    panic!("Expected Some variant");
    /// };
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// assert_eq!(d.value(), &"file".to_owned());
    /// ```
    pub fn new(file_name: impl Into<P::PathSegmentOwned>, value: T) -> Self {
        DirChildSingleOpt::Some(DirChildSingle::new(file_name, value))
    }

    /// Returns `true` if this is a [`DirChildSingleOpt::Some`].
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::dir_children::DirChildSingle;
    /// use dir_structure::NoFilter;
    ///
    /// let opt = DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned()));
    /// assert!(opt.is_some());
    ///
    /// let opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// assert!(!opt.is_some());
    /// ```
    pub fn is_some(&self) -> bool {
        matches!(self, DirChildSingleOpt::Some(_))
    }

    /// Returns `true` if this is a [`DirChildSingleOpt::None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::dir_children::DirChildSingle;
    /// use dir_structure::NoFilter;
    ///
    /// let opt = DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned()));
    /// assert!(!opt.is_none());
    ///
    /// let opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// assert!(opt.is_none());
    /// ```
    pub fn is_none(&self) -> bool {
        matches!(self, DirChildSingleOpt::None)
    }

    /// Converts a &[`DirChildSingleOpt`]<T, F> into a [`DirChildSingleOpt`]<&T, F>.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::dir_children::DirChildSingle;
    /// use dir_structure::NoFilter;
    ///
    /// let opt = DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned()));
    /// let opt_ref = opt.as_ref();
    /// assert_eq!(opt_ref, DirChildSingleOpt::Some(DirChildSingle::new("file.txt", &"file".to_owned())));
    ///
    /// let opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// let opt_ref = opt.as_ref();
    /// assert_eq!(opt_ref, DirChildSingleOpt::None);
    /// ```
    pub fn as_ref(&self) -> DirChildSingleOpt<&T, F, P> {
        match self {
            Self::Some(child) => DirChildSingleOpt::Some(child.as_ref()),
            Self::None => DirChildSingleOpt::None,
        }
    }

    /// Converts a &mut [`DirChildSingleOpt`]<T, F> into a [`DirChildSingleOpt`]<&mut T, F>.
    ///
    /// This clones the internal [`OsString`] and [`PathBuf`]` used for the name and path.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::dir_children::DirChildSingle;
    ///
    /// use dir_structure::NoFilter;
    ///
    /// let mut opt = DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned()));
    /// let mut opt_mut = opt.as_mut();
    /// if let DirChildSingleOpt::Some(child) = &mut opt_mut {
    ///     child.value_mut().push_str("_modified");
    /// }
    /// assert_eq!(opt, DirChildSingleOpt::Some(DirChildSingle::new("file.txt", "file_modified".to_owned())));
    ///
    /// let mut opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// let mut opt_mut = opt.as_mut();
    /// assert_eq!(opt_mut, DirChildSingleOpt::None);
    /// ```
    pub fn as_mut(&mut self) -> DirChildSingleOpt<&mut T, F, P> {
        match self {
            Self::Some(child) => DirChildSingleOpt::Some(child.as_mut()),
            Self::None => DirChildSingleOpt::None,
        }
    }

    /// Maps the value inside the [`DirChildSingleOpt`] if it exists.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::dir_children::DirChildSingle;
    /// use dir_structure::NoFilter;
    ///
    /// let opt = DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned()));
    /// let opt_mapped = opt.map(|child| child.map(|v| v.to_uppercase()));
    /// assert_eq!(opt_mapped, DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "FILE".to_owned())));
    ///
    /// let opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// let opt_mapped = opt.map(|child| child.map(|v| v.to_uppercase()));
    /// assert_eq!(opt_mapped, DirChildSingleOpt::None);
    /// ```
    pub fn map<U>(
        self,
        f: impl FnOnce(DirChildSingle<T, F, P>) -> DirChildSingle<U, F, P>,
    ) -> DirChildSingleOpt<U, F, P> {
        match self {
            Self::Some(child) => DirChildSingleOpt::Some(f(child)),
            Self::None => DirChildSingleOpt::None,
        }
    }

    /// Returns a new [`DirChildSingleOpt`] by applying the function `f` to the value inside
    /// the [`DirChildSingleOpt`] if it exists.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::dir_children::DirChildSingle;
    /// use dir_structure::NoFilter;
    ///
    /// let opt = DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned()));
    /// let opt_and_then = opt.and_then(|child| {
    ///     DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new(child.file_name(), child.value().to_uppercase()))
    /// });
    /// assert_eq!(opt_and_then, DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "FILE".to_owned())));
    ///
    /// let opt = DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned()));
    /// let opt_and_then = opt.and_then(|child| DirChildSingleOpt::<String, NoFilter>::None);
    /// assert_eq!(opt_and_then, DirChildSingleOpt::None);
    ///
    /// let opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// let opt_and_then = opt.and_then(|child| {
    ///     DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new(child.file_name(), child.value().to_uppercase()))
    /// });
    /// assert_eq!(opt_and_then, DirChildSingleOpt::<_, NoFilter>::None);
    /// ```
    pub fn and_then<U, F2: Filter<P2>, P2: PathType + ?Sized>(
        self,
        f: impl FnOnce(DirChildSingle<T, F, P>) -> DirChildSingleOpt<U, F2, P2>,
    ) -> DirChildSingleOpt<U, F2, P2> {
        match self {
            Self::Some(child) => f(child),
            Self::None => DirChildSingleOpt::None,
        }
    }

    /// Returns a new [`DirChildSingleOpt`] by applying the function `f` to the value inside
    /// the [`DirChildSingleOpt`] if it exists.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::dir_children::DirChildSingle;
    /// use dir_structure::NoFilter;
    ///
    /// let opt = DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned()));
    /// let opt_or_else = opt.or_else(|| {
    ///     DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "FILE".to_owned()))
    /// });
    /// assert_eq!(opt_or_else, DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned())));
    ///
    /// let opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// let opt_or_else = opt.or_else(|| {
    ///     DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "FILE".to_owned()))
    /// });
    /// assert_eq!(opt_or_else, DirChildSingleOpt::Some(DirChildSingle::<_, NoFilter>::new("file.txt", "FILE".to_owned())));
    ///
    /// let opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// let opt_or_else = opt.or_else(|| DirChildSingleOpt::<String, NoFilter>::None);
    /// assert_eq!(opt_or_else, DirChildSingleOpt::None);
    /// ```
    pub fn or_else<F2: Filter<P>>(
        self,
        f: impl FnOnce() -> DirChildSingleOpt<T, F2, P>,
    ) -> DirChildSingleOpt<T, F2, P> {
        match self {
            Self::Some(child) => DirChildSingleOpt::Some(child.map_filter()),
            Self::None => f(),
        }
    }

    /// Converts the [`DirChildSingleOpt`] into an `Option`.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::dir_children::DirChildSingle;
    /// use dir_structure::NoFilter;
    ///
    /// let opt = DirChildSingleOpt::Some(DirChildSingle::new("file.txt", "file".to_owned()));
    /// let option = opt.to_option();
    /// assert_eq!(option, Some(DirChildSingle::<String, NoFilter>::new("file.txt", "file".to_owned())));
    ///
    /// let opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// let option = opt.to_option();
    /// assert_eq!(option, None);
    /// ```
    pub fn to_option(self) -> Option<DirChildSingle<T, F, P>> {
        match self {
            DirChildSingleOpt::Some(child) => Some(child),
            DirChildSingleOpt::None => None,
        }
    }

    /// Takes the value out of the [`DirChildSingleOpt`] if the predicate `pred` returns `true`.
    ///
    /// If the predicate returns `false`, or if the [`DirChildSingleOpt`] is `None`, this returns `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_children::DirChildSingleOpt;
    /// use dir_structure::dir_children::DirChildSingle;
    /// use dir_structure::NoFilter;
    ///
    /// let mut opt = DirChildSingleOpt::<String, NoFilter>::Some(DirChildSingle::new("file.txt", "file".to_owned()));
    /// let taken = opt.take_if(|child| child.value() == "file");
    /// assert_eq!(taken, DirChildSingleOpt::Some(DirChildSingle::new("file.txt", "file".to_owned())));
    /// assert_eq!(opt, DirChildSingleOpt::None);
    ///
    /// let mut opt = DirChildSingleOpt::<String, NoFilter>::Some(DirChildSingle::new("file.txt", "file".to_owned()));
    /// let taken = opt.take_if(|child| child.value() == "other");
    /// assert_eq!(taken, DirChildSingleOpt::None);
    /// assert_eq!(opt, DirChildSingleOpt::Some(DirChildSingle::new("file.txt", "file".to_owned())));
    ///
    /// let mut opt = DirChildSingleOpt::<String, NoFilter>::None;
    /// let taken = opt.take_if(|child| child.value() == "file");
    /// assert_eq!(taken, DirChildSingleOpt::None);
    /// assert_eq!(opt, DirChildSingleOpt::None);
    /// ```
    pub fn take_if(
        &mut self,
        pred: impl FnOnce(&DirChildSingle<T, F, P>) -> bool,
    ) -> DirChildSingleOpt<T, F, P> {
        match self {
            DirChildSingleOpt::Some(child) if pred(child) => {
                mem::replace(self, DirChildSingleOpt::None)
            }
            _ => DirChildSingleOpt::None,
        }
    }
}

impl<T, F, P: PathType + ?Sized> fmt::Debug for DirChildSingleOpt<T, F, P>
where
    T: fmt::Debug,
    F: Filter<P>,
    <P as PathType>::PathSegmentOwned: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DirChildSingleOpt::Some(child) => f
                .debug_tuple("DirChildSingleOpt::Some")
                .field(child)
                .finish(),
            DirChildSingleOpt::None => write!(f, "DirChildSingleOpt::None"),
        }
    }
}

impl<'a, T, F, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for DirChildSingleOpt<T, F, Vfs::Path>
where
    T: ReadFrom<'a, Vfs>,
    F: Filter<Vfs::Path> + 'a,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        let children = DirChildren::<T, F, Vfs::Path>::read_from(path, vfs)?;
        if children.len() == 1 {
            let child = children.children.into_iter().next().unwrap();
            Ok(DirChildSingleOpt::Some(DirChildSingle {
                file_name: child.file_name,
                value: child.value,
                _phantom: PhantomData,
            }))
        } else if children.is_empty() {
            Ok(DirChildSingleOpt::None)
        } else {
            Err(Error::UnexpectedNumberOfChildren {
                expected: "0 or 1",
                found: children.len(),
                path: path.owned(),
            })
        }
    }
}

impl<'a, T, F, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs>
    for DirChildSingleOpt<T, F, Vfs::Path>
where
    T: WriteTo<'a, Vfs>,
    F: Filter<Vfs::Path>,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        match self {
            DirChildSingleOpt::Some(child) => child.write_to(path, vfs),
            DirChildSingleOpt::None => Ok(()),
        }
    }
}

/// A wrapper around [`DirChildren`] that forces the creation of the directory, even if there are no children to write.
#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct ForceCreateDirChildren<T, F = NoFilter, P: PathType + ?Sized = Path>
where
    F: Filter<P>,
{
    children: DirChildren<T, F, P>,
}

impl<T, F, P: PathType + ?Sized> fmt::Debug for ForceCreateDirChildren<T, F, P>
where
    F: Filter<P>,
    T: fmt::Debug,
    <P as PathType>::PathSegmentOwned: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ForceCreateDirChildren")
            .field("children", &self.children)
            .finish()
    }
}

impl<T, F, P> ForceCreateDirChildren<T, F, P>
where
    F: Filter<P>,
    P: PathType + ?Sized,
{
    /// Creates a new [`ForceCreateDirChildren`] with the specified children.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::dir_children::{ForceCreateDirChildren, DirChildren};
    /// use dir_structure::NoFilter;
    ///
    /// let force_create = ForceCreateDirChildren::new(DirChildren::<String, NoFilter>::new());
    ///
    /// assert_eq!(force_create.len(), 0);
    /// ```
    pub fn new(children: DirChildren<T, F, P>) -> Self {
        ForceCreateDirChildren { children }
    }

    /// Creates a new [`ForceCreateDirChildren`] from an iterator of [`DirChild`]s.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::dir_children::{ForceCreateDirChildren, DirChild, DirChildren};
    /// use dir_structure::NoFilter;
    ///
    /// let children = vec![
    ///     DirChild::new("file1.txt", "content1".to_owned()),
    ///     DirChild::new("file2.txt", "content2".to_owned()),
    /// ];
    /// let force_create = ForceCreateDirChildren::<_, NoFilter>::with_children_from_iter(children);
    ///
    /// assert_eq!(force_create.len(), 2);
    /// ```
    pub fn with_children_from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = DirChild<T, P>>,
    {
        Self::new(DirChildren::from_iter(iter))
    }
}

impl<T, F, P: PathType + ?Sized> AsRef<DirChildren<T, F, P>> for ForceCreateDirChildren<T, F, P>
where
    F: Filter<P>,
{
    fn as_ref(&self) -> &DirChildren<T, F, P> {
        &self.children
    }
}

impl<T, F, P: PathType + ?Sized> AsMut<DirChildren<T, F, P>> for ForceCreateDirChildren<T, F, P>
where
    F: Filter<P>,
{
    fn as_mut(&mut self) -> &mut DirChildren<T, F, P> {
        &mut self.children
    }
}

impl<T, F, P: PathType + ?Sized> From<DirChildren<T, F, P>> for ForceCreateDirChildren<T, F, P>
where
    F: Filter<P>,
{
    fn from(children: DirChildren<T, F, P>) -> Self {
        ForceCreateDirChildren { children }
    }
}

impl<T, F, P: PathType + ?Sized> From<ForceCreateDirChildren<T, F, P>> for DirChildren<T, F, P>
where
    F: Filter<P>,
{
    fn from(force_create: ForceCreateDirChildren<T, F, P>) -> Self {
        force_create.children
    }
}

impl<T, F, P: PathType + ?Sized> FromIterator<DirChild<T, P>> for ForceCreateDirChildren<T, F, P>
where
    F: Filter<P>,
{
    fn from_iter<I: IntoIterator<Item = DirChild<T, P>>>(iter: I) -> Self {
        ForceCreateDirChildren {
            children: DirChildren::from_iter(iter),
        }
    }
}

impl<T, F, P: PathType + ?Sized> Deref for ForceCreateDirChildren<T, F, P>
where
    F: Filter<P>,
{
    type Target = DirChildren<T, F, P>;

    fn deref(&self) -> &Self::Target {
        &self.children
    }
}

impl<T, F, P: PathType + ?Sized> DerefMut for ForceCreateDirChildren<T, F, P>
where
    F: Filter<P>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.children
    }
}

impl<'a, T, F, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for ForceCreateDirChildren<T, F, Vfs::Path>
where
    T: ReadFrom<'a, Vfs>,
    F: Filter<Vfs::Path> + 'a,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        DirChildren::<T, F, Vfs::Path>::read_from(path, vfs)
            .map(|children| ForceCreateDirChildren { children })
    }
}

impl<'a, T, F, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs>
    for ForceCreateDirChildren<T, F, Vfs::Path>
where
    T: WriteTo<'a, Vfs>,
    F: Filter<Vfs::Path>,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        vfs.create_dir_all(path)?;

        self.children.write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[pin_project]
#[doc(hidden)]
pub struct ForceCreateDirChildrenReadAsyncFuture<'a, T, F, Vfs: VfsAsync>
where
    T: ReadFromAsync<'a, Vfs> + 'static,
    F: Filter<Vfs::Path> + Send + 'static,
    T::Future: Future<Output = VfsResult<T, Vfs>> + Send + Unpin,
{
    #[pin]
    inner: DirChildrenReadAsyncFuture<'a, T, F, Vfs>,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, F, Vfs: VfsAsync> Future for ForceCreateDirChildrenReadAsyncFuture<'a, T, F, Vfs>
where
    T: ReadFromAsync<'a, Vfs> + Send + Sync + 'static,
    F: Filter<Vfs::Path> + Send + 'static,
    T::Future: Future<Output = VfsResult<T, Vfs>> + Unpin + 'static,
{
    type Output = VfsResult<ForceCreateDirChildren<T, F, Vfs::Path>, Vfs>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project();

        match this.inner.poll(cx) {
            Poll::Ready(Ok(children)) => Poll::Ready(Ok(ForceCreateDirChildren { children })),
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, F, Vfs: VfsAsync + 'a> ReadFromAsync<'a, Vfs>
    for ForceCreateDirChildren<T, F, Vfs::Path>
where
    T: ReadFromAsync<'a, Vfs> + Send + Sync + 'static,
    F: Filter<Vfs::Path> + Send + Sync + 'static,
    T::Future: Future<Output = VfsResult<T, Vfs>> + Unpin + 'static,
{
    type Future = ForceCreateDirChildrenReadAsyncFuture<'a, T, F, Vfs>;

    fn read_from_async(
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        ForceCreateDirChildrenReadAsyncFuture {
            inner: DirChildren::read_from_async(path, vfs),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, F, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs>
    for ForceCreateDirChildren<T, F, Vfs::Path>
where
    T: WriteToAsync<'a, Vfs> + Send + Sync + 'static,
    F: Filter<Vfs::Path> + Send + Sync + 'static,
    T::Future: Future<Output = VfsResult<(), Vfs>> + Unpin + 'a,
{
    type Future = Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + 'a>>;

    fn write_to_async(
        self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        Box::pin(async move {
            vfs.create_dir_all(path.clone()).await?;

            self.children.write_to_async(path, vfs).await
        })
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<T, F, P: PathType + ?Sized> DynamicHasField for ForceCreateDirChildren<T, F, P>
where
    F: Filter<P>,
{
    type Inner = T;

    fn resolve_path<Pt: OwnedPathType>(mut p: Pt, name: &str) -> Pt {
        p.push_segment_str(name);
        p
    }
}
