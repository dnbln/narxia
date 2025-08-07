use std::ffi::OsStr;
use std::ffi::OsString;
use std::marker;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::path::PathBuf;
#[cfg(feature = "async")]
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::Error;
use crate::WrapIoError;
#[cfg(feature = "tokio")]
use crate::WriteToAsyncOwned;
use crate::error::Result;
#[cfg(feature = "async")]
use crate::traits::asy::ReadFromAsync;
#[cfg(feature = "async")]
use crate::traits::asy::WriteToAsync;
#[cfg(feature = "resolve-path")]
use crate::traits::resolve::DynamicHasField;
use crate::traits::sync::DirStructureItem;
use crate::traits::sync::ReadFrom;
use crate::traits::sync::WriteTo;

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
pub struct DirChildren<T, F: Filter = NoFilter> {
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
    T: Clone,
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
        let _ = _path;
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

#[macro_export]
macro_rules! stem_filter {
    ($vis:vis $name:ident, $base_name:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        $vis struct $name;

        impl $crate::Filter for $name {
            fn make_filter() -> Self {
                Self
            }

            fn allows(&self, path: &::std::path::Path) -> bool {
                path.file_stem()
                    .and_then(|s| s.to_str())
                    .map_or(false, |s| s == $base_name)
            }
        }
    };
}

#[macro_export]
macro_rules! file_prefix_filter {
    ($vis:vis $name:ident, $file_prefix:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        $vis struct $name;

        impl $crate::Filter for $name {
            fn make_filter() -> Self {
                Self
            }

            fn allows(&self, path: &::std::path::Path) -> bool {
                path.file_prefix()
                    .and_then(|s| s.to_str())
                    .map_or(false, |s| s == $file_prefix)
            }
        }
    };
}

impl<T, F: Filter> Default for DirChildren<T, F> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, F> DirChildren<T, F>
where
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

    /// Pushes a new child to the end of the children list.
    ///
    /// This method takes a file name and a value, and creates a new `DirChild`
    /// with the given file name and value, then pushes it to the end of the children
    /// list.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::{Path, PathBuf};
    /// use dir_structure::{DirStructure, DirStructureItem, DirChildren, DirChild};
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
    pub fn push(&mut self, file_name: impl Into<OsString>, value: T) {
        self.children.push(DirChild {
            file_name: file_name.into(),
            value,
        });
    }
}

impl<T, F> ReadFrom for DirChildren<T, F>
where
    T: ReadFrom,
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

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DirChildrenReadAsyncFutureProjOwn)]
pub enum DirChildrenReadAsyncFuture<T, F>
where
    T: ReadFromAsync + 'static,
    F: Filter + Send + 'static,
    T::Future: Future<Output = Result<T>> + Send + Unpin,
{
    Poison,
    Init(
        Pin<Box<dyn Future<Output = std::io::Result<tokio::fs::ReadDir>> + Send>>,
        F,
        Vec<DirChild<T>>,
        PathBuf,
    ),
    Begin(Pin<Box<tokio::fs::ReadDir>>, F, Vec<DirChild<T>>, PathBuf),
    ReadAsync(
        Pin<Box<tokio::fs::ReadDir>>,
        F,
        Vec<DirChild<T>>,
        PathBuf,
        T::Future,
        OsString,
    ),
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T, F> Future for DirChildrenReadAsyncFuture<T, F>
where
    T: ReadFromAsync + Send + 'static,
    F: Filter + Send + 'static,
    T::Future: Future<Output = Result<T>> + Unpin + 'static,
{
    type Output = Result<DirChildren<T, F>>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);

        match this {
            DirChildrenReadAsyncFutureProjOwn::Init(mut entries, filter, children, path) => {
                match entries.as_mut().poll(cx) {
                    Poll::Ready(Ok(entries)) => {
                        self.project_replace(DirChildrenReadAsyncFuture::Begin(
                            Box::pin(entries),
                            filter,
                            children,
                            path,
                        ));
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e).wrap_io_error(|| path)),
                    Poll::Pending => {
                        self.project_replace(DirChildrenReadAsyncFuture::Init(
                            entries, filter, children, path,
                        ));
                        Poll::Pending
                    }
                }
            }
            DirChildrenReadAsyncFutureProjOwn::Begin(mut entries, filter, children, path) => {
                use std::task::Poll;

                match entries.poll_next_entry(cx) {
                    Poll::Ready(Ok(Some(entry))) => {
                        if !filter.allows(&entry.path()) {
                            return Poll::Ready(Ok(DirChildren {
                                self_path: path.clone(),
                                children,
                                filter: marker::PhantomData,
                            }));
                        }

                        let value_future = T::read_from_async(entry.path());
                        self.project_replace(DirChildrenReadAsyncFuture::ReadAsync(
                            entries,
                            filter,
                            children,
                            path,
                            value_future,
                            entry.file_name(),
                        ));
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(Ok(None)) => Poll::Ready(Ok(DirChildren {
                        self_path: path.clone(),
                        children,
                        filter: marker::PhantomData,
                    })),
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e).wrap_io_error(|| path)),
                    Poll::Pending => {
                        self.project_replace(DirChildrenReadAsyncFuture::Begin(
                            entries, filter, children, path,
                        ));
                        Poll::Pending
                    }
                }
            }
            DirChildrenReadAsyncFutureProjOwn::ReadAsync(
                entries,
                filter,
                mut children,
                path,
                mut value_fut,
                file_name,
            ) => match Pin::<&mut T::Future>::new(&mut value_fut).poll(cx) {
                Poll::Ready(Ok(value)) => {
                    children.push(DirChild { file_name, value });
                    self.project_replace(DirChildrenReadAsyncFuture::Begin(
                        entries, filter, children, path,
                    ));
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                Poll::Pending => {
                    self.project_replace(DirChildrenReadAsyncFuture::ReadAsync(
                        entries,
                        filter,
                        children,
                        path,
                        value_fut,
                        file_name.clone(),
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
impl<T, F> ReadFromAsync for DirChildren<T, F>
where
    T: ReadFromAsync + Send + 'static,
    F: Filter + Send + 'static,
    T::Future: Future<Output = Result<T>> + Unpin + 'static,
{
    type Future = DirChildrenReadAsyncFuture<T, F>;

    fn read_from_async(path: PathBuf) -> Self::Future {
        let f = Box::pin(tokio::fs::read_dir(path.clone()));
        DirChildrenReadAsyncFuture::Init(f, F::make_filter(), Vec::new(), path)
    }
}

impl<T, F> WriteTo for DirChildren<T, F>
where
    T: WriteTo,
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

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DirChildrenWriteAsyncFutureProjOwn)]
pub enum DirChildrenWriteAsyncFuture<'a, T: WriteToAsync + 'a>
where
    T::Future<'a>: Unpin,
{
    Poison,
    Begin(DirChildrenIter<'a, T>, PathBuf),
    Write(DirChildrenIter<'a, T>, PathBuf, T::Future<'a>),
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> Future for DirChildrenWriteAsyncFuture<'a, T>
where
    T: WriteToAsync + 'a,
    T::Future<'a>: Future<Output = Result<()>> + Unpin,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);

        match this {
            DirChildrenWriteAsyncFutureProjOwn::Begin(mut iter, path) => {
                if let Some(child) = iter.next() {
                    let child_path = path.join(&child.file_name);
                    let fut = child.value.write_to_async(child_path);
                    self.project_replace(Self::Write(iter, path, fut));
                    cx.waker().wake_by_ref();
                    Poll::Pending
                } else {
                    Poll::Ready(Ok(()))
                }
            }
            DirChildrenWriteAsyncFutureProjOwn::Write(mut iter, path, mut fut) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(Ok(())) => {
                        if let Some(child) = iter.next() {
                            let child_path = path.join(&child.file_name);
                            let new_fut = child.value.write_to_async(child_path);
                            self.project_replace(Self::Write(iter, path, new_fut));
                            cx.waker().wake_by_ref();
                            Poll::Pending
                        } else {
                            Poll::Ready(Ok(()))
                        }
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::Write(iter, path, fut));
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
impl<T, F> WriteToAsync for DirChildren<T, F>
where
    T: WriteToAsync + Send + Sync + 'static,
    F: Filter + Send + 'static,
    for<'a> T::Future<'a>: Future<Output = Result<()>> + Unpin + 'a,
{
    type Future<'a> = DirChildrenWriteAsyncFuture<'a, T>;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        DirChildrenWriteAsyncFuture::Begin(self.iter(), path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DirChildrenWriteAsyncOwnedFutureProjOwn)]
pub enum DirChildrenWriteAsyncOwnedFuture<'a, T>
where
    T: WriteToAsyncOwned<'a>,
    <T as WriteToAsyncOwned<'a>>::Future: Future<Output = Result<()>> + Unpin,
{
    Poison,
    Init(DirChildrenIntoIter<T>, PathBuf),
    Write(
        DirChildrenIntoIter<T>,
        <T as WriteToAsyncOwned<'a>>::Future,
        PathBuf,
    ),
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> Future for DirChildrenWriteAsyncOwnedFuture<'a, T>
where
    T: WriteToAsyncOwned<'a>,
    <T as WriteToAsyncOwned<'a>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);

        match this {
            DirChildrenWriteAsyncOwnedFutureProjOwn::Init(mut iter, path) => {
                if let Some(child) = iter.next() {
                    let fut = child
                        .value
                        .write_to_async_owned(path.join(&child.file_name));
                    self.project_replace(Self::Write(iter, fut, path.clone()));
                    cx.waker().wake_by_ref();
                    Poll::Pending
                } else {
                    Poll::Ready(Ok(()))
                }
            }
            DirChildrenWriteAsyncOwnedFutureProjOwn::Write(mut iter, mut fut, path) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(Ok(())) => {
                        if let Some(child) = iter.next() {
                            let new_fut = child
                                .value
                                .write_to_async_owned(path.join(&child.file_name));
                            self.project_replace(Self::Write(iter, new_fut, path.clone()));
                            cx.waker().wake_by_ref();
                            Poll::Pending
                        } else {
                            Poll::Ready(Ok(()))
                        }
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::Write(iter, fut, path.clone()));
                        Poll::Pending
                    }
                }
            }
            DirChildrenWriteAsyncOwnedFutureProjOwn::Poison => {
                panic!("DirChildrenWriteAsyncOwnedFuture is poisoned, this should never happen");
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, F> WriteToAsyncOwned<'a> for DirChildren<T, F>
where
    T: WriteToAsyncOwned<'a> + Send + 'a,
    F: Filter + 'a,
    T::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    type Future = DirChildrenWriteAsyncOwnedFuture<'a, T>;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        DirChildrenWriteAsyncOwnedFuture::Init(self.into_iter(), path)
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<T, F> DynamicHasField for DirChildren<T, F>
where
    F: Filter,
{
    type Inner = T;

    fn resolve_path(mut p: PathBuf, name: &str) -> PathBuf {
        p.push(name);
        p
    }
}

/// A single child of a [`DirChildren`] structure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirChild<T> {
    /// The file name of the child.
    file_name: OsString,
    /// The parsed value of the child.
    value: T,
}

impl<T> DirChild<T> {
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
    {
        let value = f(self.value);
        DirChild {
            file_name: self.file_name,
            value,
        }
    }
}

/// A [`DirChildren`] iterator. It iterates over the children of a
/// [`DirChildren`] structure.
///
/// See [`DirChildren::iter`] for more information.
pub struct DirChildrenIter<'a, T>(std::slice::Iter<'a, DirChild<T>>);

impl<'a, T> Iterator for DirChildrenIter<'a, T> {
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
pub struct DirChildrenIterMut<'a, T>(std::slice::IterMut<'a, DirChild<T>>);

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

impl<T, F> IntoIterator for DirChildren<T, F>
where
    F: Filter,
{
    type Item = DirChild<T>;
    type IntoIter = DirChildrenIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        DirChildrenIntoIter(self.children.into_iter())
    }
}

pub struct DirChildrenIntoIter<T>(std::vec::IntoIter<DirChild<T>>);

impl<T> Iterator for DirChildrenIntoIter<T> {
    type Item = DirChild<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<T> ExactSizeIterator for DirChildrenIntoIter<T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T> DoubleEndedIterator for DirChildrenIntoIter<T> {
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
            type IntoIter = $crate::DirChildrenIntoIter<$ty>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.into_iter()
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DirChildSingle<T, F: Filter> {
    /// The file name of the child.
    file_name: OsString,
    /// The parsed value of the child.
    value: T,
    _phantom: PhantomData<F>,
}

impl<T, F: Filter> ReadFrom for DirChildSingle<T, F>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        let children = DirChildren::<T, F>::read_from(path)?;
        if children.len() != 1 {
            return Err(Error::UnexpectedNumberOfChildren {
                expected: "1",
                found: children.len(),
                path: path.to_path_buf(),
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

impl<T, F: Filter> WriteTo for DirChildSingle<T, F>
where
    T: WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        let child_path = path.join(&self.file_name);
        self.value.write_to(&child_path)
    }
}

impl<T, F: Filter> DirChildSingle<T, F> {
    /// Creates a new [`DirChildSingle`] with the specified file name and value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::{DirChildSingle, ReadFrom, WriteTo, NoFilter};
    ///
    /// let d = DirChildSingle::<_, NoFilter>::new("file.txt", "file".to_owned());
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// assert_eq!(d.value(), &"file".to_owned());
    /// ```
    pub fn new(file_name: impl Into<OsString>, value: T) -> Self {
        Self {
            file_name: file_name.into(),
            value,
            _phantom: PhantomData,
        }
    }

    /// Gets the file name of the child (or the name of the directory; the last segment in the path).
    pub fn file_name(&self) -> &OsString {
        &self.file_name
    }

    /// Gets the file name of the child (or the name of the directory; the last segment in the path).
    pub fn file_name_mut(&mut self) -> &mut OsString {
        &mut self.file_name
    }

    /// Gets the value of the child.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Gets the value of the child. Mutable reference version of [`Self::value`].
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }

    pub fn as_ref(&self) -> DirChildSingle<&T, F> {
        DirChildSingle {
            file_name: self.file_name.clone(),
            value: &self.value,
            _phantom: PhantomData,
        }
    }

    pub fn map_filter<F2>(self) -> DirChildSingle<T, F2>
    where
        F2: Filter,
    {
        DirChildSingle {
            file_name: self.file_name,
            value: self.value,
            _phantom: PhantomData,
        }
    }
}

impl<T, F> Deref for DirChildSingle<T, F>
where
    F: Filter,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T, F> DerefMut for DirChildSingle<T, F>
where
    F: Filter,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DirChildSingleOpt<T, F: Filter> {
    None,
    Some(DirChildSingle<T, F>),
}

impl<T, F: Filter> DirChildSingleOpt<T, F> {
    /// Creates a new [`DirChildSingleOpt`] with the specified file name and value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::ffi::OsString;
    /// use dir_structure::{DirChildSingleOpt, ReadFrom, WriteTo, NoFilter};
    ///
    /// let DirChildSingleOpt::Some(d) = DirChildSingleOpt::<_, NoFilter>::new("file.txt", "file".to_owned()) else {
    ///    panic!("Expected Some variant");
    /// };
    /// assert_eq!(d.file_name(), &OsString::from("file.txt"));
    /// assert_eq!(d.value(), &"file".to_owned());
    /// ```
    pub fn new(file_name: impl Into<OsString>, value: T) -> Self {
        DirChildSingleOpt::Some(DirChildSingle::new(file_name, value))
    }

    /// Returns `true` if this is a [`DirChildSingleOpt::Some`].
    pub fn is_some(&self) -> bool {
        matches!(self, DirChildSingleOpt::Some(_))
    }

    /// Returns `true` if this is a [`DirChildSingleOpt::None`].
    pub fn is_none(&self) -> bool {
        matches!(self, DirChildSingleOpt::None)
    }

    pub fn as_ref(&self) -> DirChildSingleOpt<&T, F> {
        match self {
            Self::Some(child) => DirChildSingleOpt::Some(child.as_ref()),
            Self::None => DirChildSingleOpt::None,
        }
    }

    pub fn map<U>(
        self,
        f: impl FnOnce(DirChildSingle<T, F>) -> DirChildSingle<U, F>,
    ) -> DirChildSingleOpt<U, F> {
        match self {
            Self::Some(child) => DirChildSingleOpt::Some(f(child)),
            Self::None => DirChildSingleOpt::None,
        }
    }

    pub fn and_then<U, F2: Filter>(
        self,
        f: impl FnOnce(DirChildSingle<T, F>) -> DirChildSingleOpt<U, F2>,
    ) -> DirChildSingleOpt<U, F2> {
        match self {
            Self::Some(child) => f(child),
            Self::None => DirChildSingleOpt::None,
        }
    }

    pub fn or_else<F2: Filter>(
        self,
        f: impl FnOnce() -> DirChildSingleOpt<T, F2>,
    ) -> DirChildSingleOpt<T, F2> {
        match self {
            Self::Some(child) => DirChildSingleOpt::Some(child.map_filter()),
            Self::None => f(),
        }
    }

    pub fn to_option(self) -> Option<DirChildSingle<T, F>> {
        match self {
            DirChildSingleOpt::Some(child) => Some(child),
            DirChildSingleOpt::None => None,
        }
    }
}

impl<T, F> ReadFrom for DirChildSingleOpt<T, F>
where
    T: ReadFrom,
    F: Filter,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        let children = DirChildren::<T, F>::read_from(path)?;
        if children.len() == 1 {
            let child = children.children.into_iter().next().unwrap();
            Ok(DirChildSingleOpt::Some(DirChildSingle {
                file_name: child.file_name,
                value: child.value,
                _phantom: PhantomData,
            }))
        } else if children.len() == 0 {
            Ok(DirChildSingleOpt::None)
        } else {
            Err(Error::UnexpectedNumberOfChildren {
                expected: "0 or 1",
                found: children.len(),
                path: path.to_path_buf(),
            })
        }
    }
}

impl<T, F> WriteTo for DirChildSingleOpt<T, F>
where
    T: WriteTo,
    F: Filter,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        match self {
            DirChildSingleOpt::Some(child) => child.write_to(path),
            DirChildSingleOpt::None => Ok(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForceCreateDirChildren<T, F = NoFilter>
where
    F: Filter,
{
    children: DirChildren<T, F>,
}

impl<T, F> ForceCreateDirChildren<T, F>
where
    F: Filter,
{
    /// Creates a new [`ForceCreateDirChildren`] with the specified children.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use dir_structure::{ForceCreateDirChildren, DirChildren, NoFilter};
    ///
    /// let force_create = ForceCreateDirChildren::new(DirChildren::<String, NoFilter>::new());
    ///
    /// assert_eq!(force_create.len(), 0);
    /// ```
    pub fn new(children: DirChildren<T, F>) -> Self {
        ForceCreateDirChildren { children }
    }
}

impl<T, F> Deref for ForceCreateDirChildren<T, F>
where
    F: Filter,
{
    type Target = DirChildren<T, F>;

    fn deref(&self) -> &Self::Target {
        &self.children
    }
}

impl<T, F> DerefMut for ForceCreateDirChildren<T, F>
where
    F: Filter,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.children
    }
}

impl<T, F> ReadFrom for ForceCreateDirChildren<T, F>
where
    T: ReadFrom,
    F: Filter,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        DirChildren::<T, F>::read_from(path).map(|children| ForceCreateDirChildren { children })
    }
}

impl<T, F> WriteTo for ForceCreateDirChildren<T, F>
where
    T: WriteTo,
    F: Filter,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        std::fs::create_dir_all(path).wrap_io_error_with(path)?;

        self.children.write_to(path)
    }
}

#[cfg(feature = "async")]
#[pin_project]
pub struct ForceCreateDirChildrenReadAsyncFuture<T, F>
where
    T: ReadFromAsync + 'static,
    F: Filter + Send + 'static,
    T::Future: Future<Output = Result<T>> + Send + Unpin,
{
    #[pin]
    inner: DirChildrenReadAsyncFuture<T, F>,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T, F> Future for ForceCreateDirChildrenReadAsyncFuture<T, F>
where
    T: ReadFromAsync + Send + Sync + 'static,
    F: Filter + Send + Sync + 'static,
    T::Future: Future<Output = Result<T>> + Unpin + 'static,
{
    type Output = Result<ForceCreateDirChildren<T, F>>;

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
impl<T, F> ReadFromAsync for ForceCreateDirChildren<T, F>
where
    T: ReadFromAsync + Send + Sync + 'static,
    F: Filter + Send + Sync + 'static,
    T::Future: Future<Output = Result<T>> + Unpin + 'static,
{
    type Future = ForceCreateDirChildrenReadAsyncFuture<T, F>;

    fn read_from_async(path: PathBuf) -> Self::Future {
        ForceCreateDirChildrenReadAsyncFuture {
            inner: DirChildren::read_from_async(path),
        }
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<T, F> WriteToAsync for ForceCreateDirChildren<T, F>
where
    T: WriteToAsync + Send + Sync + 'static,
    F: Filter + Send + Sync + 'static,
    for<'a> T::Future<'a>: Future<Output = Result<()>> + Unpin + 'a,
{
    type Future<'a> = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        Box::pin(async move {
            tokio::fs::create_dir_all(&path)
                .await
                .wrap_io_error_with(&path)?;

            self.children.write_to_async(path).await
        })
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<'a, T, F> WriteToAsyncOwned<'a> for ForceCreateDirChildren<T, F>
where
    F: Filter + Send + 'a,
    T: WriteToAsyncOwned<'a> + Send + Sync + 'static,
    T::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        Box::pin(async move {
            tokio::fs::create_dir_all(&path)
                .await
                .wrap_io_error_with(&path)?;
            self.children.write_to_async_owned(path).await
        })
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<T, F> DynamicHasField for ForceCreateDirChildren<T, F>
where
    F: Filter,
{
    type Inner = T;

    fn resolve_path(mut p: PathBuf, name: &str) -> PathBuf {
        p.push(name);
        p
    }
}
