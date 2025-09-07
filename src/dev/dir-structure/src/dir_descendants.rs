//! A structure representing the descendants of a directory.
//!
//! See [`DirDescendants`] for more details.

use core::fmt::Debug;
use core::slice;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::fmt;
use std::marker;
use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;
use std::vec;

#[cfg(feature = "async")]
use futures::future::BoxFuture;
#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::NoFilter;
use crate::error::Result;
use crate::prelude::*;
#[cfg(feature = "async")]
use crate::traits::async_vfs::WriteSupportingVfsAsync;
#[cfg(feature = "resolve-path")]
use crate::traits::resolve::DynamicHasField;
use crate::traits::vfs;
use crate::traits::vfs::DirEntryInfo;
use crate::traits::vfs::DirWalker;

/// A structure representing the descendants of a directory.
///
/// This is different from [`DirChildren`](crate::DirChildren), as the descendants include all
/// files and folders within the directory, not just the immediate children, as is the case with
/// [`DirChildren`](crate::DirChildren).
///
/// The `F` type parameter allows for custom filtering of the descendants, as follows:
///
/// - its [`FolderFilter`] implementation tells us whether to attempt to parse a specific folder as `T`, storing it into the result set.
/// - its [`FolderRecurseFilter`] tells us whether to recurse into a specific folder, but not necessarily parse it as `T`.
/// - its [`FileFilter`] implementation tells us whether to attempt to parse a specific file as `T`, storing it into the result set.
///
/// \* note that [`FolderFilter`] and [`FolderRecurseFilter`] may both allow the same path, in which case we will both recurse
/// into the folder, and attempt to parse it as a `T`.
pub struct DirDescendants<T, F: FolderFilter + FolderRecurseFilter + FileFilter = NoFilter> {
    descendants: Vec<DirDescendant<T>>,
    _phantom: marker::PhantomData<F>,
}

impl<T: Debug, F: FolderFilter + FolderRecurseFilter + FileFilter> Debug for DirDescendants<T, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("DirDescendants")
            .field("descendants", &self.descendants)
            .finish()
    }
}

impl<T: Clone, F: FolderFilter + FolderRecurseFilter + FileFilter> Clone for DirDescendants<T, F> {
    fn clone(&self) -> Self {
        Self {
            descendants: self.descendants.clone(),
            _phantom: marker::PhantomData,
        }
    }
}

impl<T, F: FolderFilter + FolderRecurseFilter + FileFilter> DirDescendants<T, F> {
    /// Create a new [`DirDescendants`] instance from a list of [`DirDescendant`]s.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<(), NoFilter>::new(vec![]);
    /// assert!(descendants.is_empty());
    ///
    /// let descendants = DirDescendants::<(), NoFilter>::new(vec![
    ///   DirDescendant::new("child1", "child1", "child1", ()),
    ///   DirDescendant::new("child2", "child2", "child2", ()),
    /// ]);
    /// assert_eq!(descendants.len(), 2);
    /// ```
    pub fn new(descendants: Vec<DirDescendant<T>>) -> Self {
        Self {
            descendants,
            _phantom: marker::PhantomData,
        }
    }

    /// Returns the number of descendants.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<(), NoFilter>::new(vec![]);
    /// assert_eq!(descendants.len(), 0);
    ///
    /// let descendants = DirDescendants::<(), NoFilter>::new(vec![
    ///   DirDescendant::new("child1", "child1", "child1", ()),
    ///   DirDescendant::new("child2", "child2", "child2", ()),
    /// ]);
    /// assert_eq!(descendants.len(), 2);
    /// ```
    pub fn len(&self) -> usize {
        self.descendants.len()
    }

    /// Returns whether there are no descendants.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    /// let descendants = DirDescendants::<(), NoFilter>::new(vec![]);
    /// assert_eq!(descendants.is_empty(), true);
    ///
    /// let descendants = DirDescendants::<(), NoFilter>::new(vec![
    ///   DirDescendant::new("child1", "child1", "child1", ()),
    ///   DirDescendant::new("child2", "child2", "child2", ()),
    /// ]);
    /// assert_eq!(descendants.is_empty(), false);
    /// ```
    pub fn is_empty(&self) -> bool {
        self.descendants.is_empty()
    }

    /// Returns an iterator over the descendants.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<(), NoFilter>::new(vec![]);
    /// let mut i = descendants.iter();
    /// assert_eq!(i.next(), None);
    ///
    /// let descendants = DirDescendants::<(), NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "child1", "child1", ()),
    ///     DirDescendant::new("child2", "child2", "child2", ()),
    /// ]);
    /// let mut i = descendants.iter();
    /// assert_eq!(i.next(), Some(&DirDescendant::new("child1", "child1", "child1", ())));
    /// assert_eq!(i.next(), Some(&DirDescendant::new("child2", "child2", "child2", ())));
    /// assert_eq!(i.next(), None);
    /// ```
    pub fn iter(&self) -> DirDescendantsIter<'_, T> {
        DirDescendantsIter(self.descendants.iter())
    }

    /// Returns an iterator over the mutable descendants.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let mut descendants = DirDescendants::<(), NoFilter>::new(vec![]);
    /// let mut i = descendants.iter_mut();
    /// assert_eq!(i.next(), None);
    ///
    /// let mut descendants = DirDescendants::<(), NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "child1", "child1", ()),
    ///     DirDescendant::new("child2", "child2", "child2", ()),
    /// ]);
    /// let mut i = descendants.iter_mut();
    /// assert_eq!(i.next(), Some(&mut DirDescendant::new("child1", "child1", "child1", ())));
    /// assert_eq!(i.next(), Some(&mut DirDescendant::new("child2", "child2", "child2", ())));
    /// assert_eq!(i.next(), None);
    /// ```
    pub fn iter_mut(&mut self) -> DirDescendantsIterMut<'_, T> {
        DirDescendantsIterMut(self.descendants.iter_mut())
    }

    /// Returns the descendant at the given index, or `None` if out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<(), NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "child1", "child1", ()),
    ///     DirDescendant::new("child2", "child2", "child2", ()),
    /// ]);
    ///
    /// assert_eq!(descendants.len(), 2);
    /// assert_eq!(descendants.get(0), Some(&DirDescendant::new("child1", "child1", "child1", ())));
    /// assert_eq!(descendants.get(1), Some(&DirDescendant::new("child2", "child2", "child2", ())));
    /// assert_eq!(descendants.get(2), None);
    /// assert_eq!(descendants.get(100), None);
    /// ```
    pub fn get(&self, index: usize) -> Option<&DirDescendant<T>> {
        self.descendants.get(index)
    }

    /// Returns a mutable reference to the descendant at the given index, or `None` if out of bounds.
    /// This is a mutable version of [`get`](Self::get).
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let mut descendants = DirDescendants::<(), NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "child1", "child1", ()),
    ///     DirDescendant::new("child2", "child2", "child2", ()),
    /// ]);
    ///
    /// assert_eq!(descendants.len(), 2);
    /// assert_eq!(descendants.get_mut(0), Some(&mut DirDescendant::new("child1", "child1", "child1", ())));
    /// assert_eq!(descendants.get_mut(1), Some(&mut DirDescendant::new("child2", "child2", "child2", ())));
    /// assert_eq!(descendants.get_mut(2), None);
    /// assert_eq!(descendants.get_mut(100), None);
    /// ```
    pub fn get_mut(&mut self, index: usize) -> Option<&mut DirDescendant<T>> {
        self.descendants.get_mut(index)
    }

    /// Returns the descendant with the given name, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_by_name("child1"), Some(&DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into())));
    /// assert_eq!(descendants.get_by_name("child2"), Some(&DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into())));
    /// assert_eq!(descendants.get_by_name("child3"), None);
    /// assert_eq!(descendants.get_by_name("nonexistent"), None);
    /// ```
    pub fn get_by_name(&self, name: impl AsRef<OsStr>) -> Option<&DirDescendant<T>> {
        self.iter().find(|d| d.name == name.as_ref())
    }

    /// Returns a mutable reference to the descendant with the given name, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let mut descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_by_name_mut("child1"), Some(&mut DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into())));
    /// assert_eq!(descendants.get_by_name_mut("child2"), Some(&mut DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into())));
    /// assert_eq!(descendants.get_by_name_mut("child3"), None);
    /// assert_eq!(descendants.get_by_name_mut("nonexistent"), None);
    /// ```
    pub fn get_by_name_mut(&mut self, name: impl AsRef<OsStr>) -> Option<&mut DirDescendant<T>> {
        self.iter_mut().find(|d| d.name == name.as_ref())
    }

    /// Returns the value of the descendant with the given name, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_value_by_name("child1"), Some(&"value1".into()));
    /// assert_eq!(descendants.get_value_by_name("child2"), Some(&"value2".into()));
    /// assert_eq!(descendants.get_value_by_name("child3"), None);
    /// assert_eq!(descendants.get_value_by_name("nonexistent"), None);
    /// ```
    pub fn get_value_by_name(&self, name: impl AsRef<OsStr>) -> Option<&T> {
        self.get_by_name(name).map(|d| &d.value)
    }

    /// Returns a mutable reference to the value of the descendant with the given name, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let mut descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_value_by_name_mut("child1"), Some(&mut "value1".into()));
    /// assert_eq!(descendants.get_value_by_name_mut("child2"), Some(&mut "value2".into()));
    /// assert_eq!(descendants.get_value_by_name_mut("child3"), None);
    /// assert_eq!(descendants.get_value_by_name_mut("nonexistent"), None);
    /// ```
    pub fn get_value_by_name_mut(&mut self, name: impl AsRef<OsStr>) -> Option<&mut T> {
        self.get_by_name_mut(name).map(|d| &mut d.value)
    }

    /// Returns the descendant with the given path, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_by_path("root/a/b/child1"), Some(&DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into())));
    /// assert_eq!(descendants.get_by_path("root/a/b/child2"), Some(&DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into())));
    /// assert_eq!(descendants.get_by_path("root/a/b/child3"), None);
    /// assert_eq!(descendants.get_by_path("nonexistent"), None);
    /// ```
    pub fn get_by_path(&self, path: impl AsRef<Path>) -> Option<&DirDescendant<T>> {
        self.iter().find(|d| d.path == path.as_ref())
    }

    /// Returns a mutable reference to the descendant with the given path, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let mut descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_by_path_mut("root/a/b/child1"), Some(&mut DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into())));
    /// assert_eq!(descendants.get_by_path_mut("root/a/b/child2"), Some(&mut DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into())));
    /// assert_eq!(descendants.get_by_path_mut("root/a/b/child3"), None);
    /// assert_eq!(descendants.get_by_path_mut("nonexistent"), None);
    /// ```
    pub fn get_by_path_mut(&mut self, path: impl AsRef<Path>) -> Option<&mut DirDescendant<T>> {
        self.iter_mut().find(|d| d.path == path.as_ref())
    }

    /// Returns the value of the descendant with the given path, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_value_by_path("root/a/b/child1"), Some(&"value1".into()));
    /// assert_eq!(descendants.get_value_by_path("root/a/b/child2"), Some(&"value2".into()));
    /// assert_eq!(descendants.get_value_by_path("root/a/b/child3"), None);
    /// assert_eq!(descendants.get_value_by_path("nonexistent"), None);
    /// ```
    pub fn get_value_by_path(&self, path: impl AsRef<Path>) -> Option<&T> {
        self.get_by_path(path).map(|d| &d.value)
    }

    /// Returns a mutable reference to the value of the descendant with the given path, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let mut descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_value_by_path_mut("root/a/b/child1"), Some(&mut "value1".into()));
    /// assert_eq!(descendants.get_value_by_path_mut("root/a/b/child2"), Some(&mut "value2".into()));
    /// assert_eq!(descendants.get_value_by_path_mut("root/a/b/child3"), None);
    /// assert_eq!(descendants.get_value_by_path_mut("nonexistent"), None);
    /// ```
    pub fn get_value_by_path_mut(&mut self, path: impl AsRef<Path>) -> Option<&mut T> {
        self.get_by_path_mut(path).map(|d| &mut d.value)
    }

    /// Returns the descendant with the given path relative to the ascendant, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_by_relative_path("a/b/child1"), Some(&DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into())));
    /// assert_eq!(descendants.get_by_relative_path("a/b/child2"), Some(&DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into())));
    /// assert_eq!(descendants.get_by_relative_path("a/b/child3"), None);
    /// assert_eq!(descendants.get_by_relative_path("nonexistent"), None);
    /// ```
    pub fn get_by_relative_path(&self, path: impl AsRef<Path>) -> Option<&DirDescendant<T>> {
        self.iter()
            .find(|d| d.path_relative_to_ascendant == path.as_ref())
    }

    /// Returns a mutable reference to the descendant with the given path relative to the ascendant, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let mut descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_by_relative_path_mut("a/b/child1"), Some(&mut DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into())));
    /// assert_eq!(descendants.get_by_relative_path_mut("a/b/child2"), Some(&mut DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into())));
    /// assert_eq!(descendants.get_by_relative_path_mut("a/b/child3"), None);
    /// assert_eq!(descendants.get_by_relative_path_mut("nonexistent"), None);
    /// ```
    pub fn get_by_relative_path_mut(
        &mut self,
        path: impl AsRef<Path>,
    ) -> Option<&mut DirDescendant<T>> {
        self.iter_mut()
            .find(|d| d.path_relative_to_ascendant == path.as_ref())
    }

    /// Returns the value of the descendant with the given path relative to the ascendant, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_value_by_relative_path("a/b/child1"), Some(&"value1".into()));
    /// assert_eq!(descendants.get_value_by_relative_path("a/b/child2"), Some(&"value2".into()));
    /// assert_eq!(descendants.get_value_by_relative_path("a/b/child3"), None);
    /// assert_eq!(descendants.get_value_by_relative_path("nonexistent"), None);
    /// ```
    pub fn get_value_by_relative_path(&self, path: impl AsRef<Path>) -> Option<&T> {
        self.get_by_relative_path(path).map(|d| &d.value)
    }

    /// Returns a mutable reference to the value of the descendant with the given path relative to the ascendant, or `None` if not found.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let mut descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".into()),
    ///     DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2".into()),
    /// ]);
    ///
    /// assert_eq!(descendants.get_value_by_relative_path_mut("a/b/child1"), Some(&mut "value1".into()));
    /// assert_eq!(descendants.get_value_by_relative_path_mut("a/b/child2"), Some(&mut "value2".into()));
    /// assert_eq!(descendants.get_value_by_relative_path_mut("a/b/child3"), None);
    /// assert_eq!(descendants.get_value_by_relative_path_mut("nonexistent"), None);
    /// ```
    pub fn get_value_by_relative_path_mut(&mut self, path: impl AsRef<Path>) -> Option<&mut T> {
        self.get_by_relative_path_mut(path).map(|d| &mut d.value)
    }

    /// Maps the descendants to another type.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant}, NoFilter};
    ///
    /// let descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///   DirDescendant::new("child1", "root/a/b/child1", "a/b/child1", "value1".to_string()),
    ///   DirDescendant::new("child2", "root/a/b/child2", "a/b/child2", "value2abc1000".to_string()),
    /// ]);
    ///
    /// let mapped = descendants.map(|s| s.len());
    /// assert_eq!(mapped.len(), 2);
    /// assert_eq!(mapped.get(0).unwrap().value(), &6); // "value1".len() == 6
    /// assert_eq!(mapped.get(1).unwrap().value(), &13); // "value2abc1000".len() == 13
    /// assert_eq!(mapped.get(2), None);
    /// assert_eq!(mapped.get(100), None);
    /// ```
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> DirDescendants<U, F> {
        DirDescendants {
            descendants: self
                .descendants
                .into_iter()
                .map(move |d| DirDescendant {
                    name: d.name,
                    path: d.path,
                    path_relative_to_ascendant: d.path_relative_to_ascendant,
                    value: f(d.value),
                })
                .collect(),
            _phantom: marker::PhantomData,
        }
    }

    /// Maps the filter type to another type.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{dir_descendants::{DirDescendants, DirDescendant, FileFilter, FolderFilter, FolderRecurseFilter}, NoFilter};
    ///
    /// struct MyFilter;
    ///
    /// impl FileFilter for MyFilter {
    ///     fn allows(_file: &std::path::Path) -> bool { true }
    /// }
    /// impl FolderFilter for MyFilter {
    ///     fn allows(_folder: &std::path::Path) -> bool { true }
    /// }
    /// impl FolderRecurseFilter for MyFilter {
    ///     fn allows(_folder: &std::path::Path) -> bool { true }
    /// }
    ///
    /// let descendants = DirDescendants::<String, NoFilter>::new(vec![
    ///     DirDescendant::new("child1", "child1", "child1", "value1".to_string()),
    ///     DirDescendant::new("child2", "child2", "child2", "value2".to_string()),
    /// ]);
    /// let descendants_with_my_filter: DirDescendants<String, MyFilter> = descendants.map_filter::<MyFilter>();
    /// ```
    pub fn map_filter<F2: FolderFilter + FolderRecurseFilter + FileFilter>(
        self,
    ) -> DirDescendants<T, F2> {
        DirDescendants {
            descendants: self.descendants,
            _phantom: marker::PhantomData,
        }
    }
}

impl<'a, T, F: FolderFilter + FolderRecurseFilter + FileFilter> IntoIterator
    for &'a DirDescendants<T, F>
{
    type Item = &'a DirDescendant<T>;
    type IntoIter = DirDescendantsIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T, F: FolderFilter + FolderRecurseFilter + FileFilter> IntoIterator
    for &'a mut DirDescendants<T, F>
{
    type Item = &'a mut DirDescendant<T>;
    type IntoIter = DirDescendantsIterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<T, F: FolderFilter + FolderRecurseFilter + FileFilter> IntoIterator for DirDescendants<T, F> {
    type Item = DirDescendant<T>;
    type IntoIter = DirDescendantsIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        DirDescendantsIntoIter(self.descendants.into_iter())
    }
}

/// An iterator over the immutable descendants.
pub struct DirDescendantsIter<'a, T>(slice::Iter<'a, DirDescendant<T>>);

impl<'a, T> Iterator for DirDescendantsIter<'a, T> {
    type Item = &'a DirDescendant<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for DirDescendantsIter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<'a, T> ExactSizeIterator for DirDescendantsIter<'a, T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

/// An iterator over the mutable descendants.
pub struct DirDescendantsIterMut<'a, T>(slice::IterMut<'a, DirDescendant<T>>);

impl<'a, T> Iterator for DirDescendantsIterMut<'a, T> {
    type Item = &'a mut DirDescendant<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for DirDescendantsIterMut<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<'a, T> ExactSizeIterator for DirDescendantsIterMut<'a, T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

/// An iterator over the owned descendants.
pub struct DirDescendantsIntoIter<T>(vec::IntoIter<DirDescendant<T>>);

impl<'a, T> Iterator for DirDescendantsIntoIter<T> {
    type Item = DirDescendant<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for DirDescendantsIntoIter<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }
}

impl<'a, T> ExactSizeIterator for DirDescendantsIntoIter<T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<
    'vfs,
    Vfs: vfs::Vfs,
    T: ReadFrom<'vfs, Vfs>,
    F: FolderFilter + FolderRecurseFilter + FileFilter + 'vfs,
> ReadFrom<'vfs, Vfs> for DirDescendants<T, F>
{
    fn read_from(path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<Self> {
        let mut descendants = Vec::new();

        if path.is_dir() {
            let mut walker = vfs.walk_dir(path)?;
            while let Some(entry) = walker.next() {
                let DirEntryInfo {
                    name,
                    path: entry_path,
                    kind,
                } = entry?;

                if kind.is_dir() {
                    if <F as FolderRecurseFilter>::allows(&entry_path) {
                        let sub_descendants = DirDescendants::<T, F>::read_from(&entry_path, vfs)?;
                        descendants.extend(sub_descendants.descendants.into_iter().map(
                            |mut it| {
                                let mut p = PathBuf::from(name.clone());
                                p.push(&it.path_relative_to_ascendant);
                                it.path_relative_to_ascendant = p;
                                it
                            },
                        ));
                    }

                    if <F as FolderFilter>::allows(&entry_path) {
                        let value = T::read_from(&entry_path, vfs)?;
                        descendants.push(DirDescendant {
                            name,
                            path_relative_to_ascendant: entry_path
                                .strip_prefix(path)
                                .unwrap()
                                .to_path_buf(),
                            path: entry_path,
                            value,
                        });
                    }
                } else if kind.is_file() && <F as FileFilter>::allows(&entry_path) {
                    let value = T::read_from(&entry_path, vfs)?;
                    descendants.push(DirDescendant {
                        name,
                        path_relative_to_ascendant: entry_path
                            .strip_prefix(path)
                            .unwrap()
                            .to_path_buf(),
                        path: entry_path,
                        value,
                    });
                }
            }
        }

        Ok(DirDescendants::new(descendants))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<
    'vfs,
    Vfs: VfsAsync + 'static,
    T: ReadFromAsync<'vfs, Vfs> + Send + 'static,
    F: FolderFilter + FolderRecurseFilter + FileFilter + 'vfs,
> ReadFromAsync<'vfs, Vfs> for DirDescendants<T, F>
{
    type Future
        = BoxFuture<'vfs, Result<Self>>
    where
        Self: 'vfs;
    fn read_from_async(path: PathBuf, vfs: Pin<&'vfs Vfs>) -> Self::Future {
        Box::pin(async move {
            let mut descendants = Vec::new();

            if path.is_dir() {
                use std::pin::pin;

                use futures::StreamExt;

                let mut walker = pin!(vfs.walk_dir(path.clone()).await?);
                while let Some(entry) = walker.next().await {
                    let DirEntryInfo {
                        name,
                        path: entry_path,
                        kind,
                    } = entry?;

                    if kind.is_dir() {
                        if <F as FolderRecurseFilter>::allows(&entry_path) {
                            let sub_descendants =
                                DirDescendants::<T, F>::read_from_async(entry_path.clone(), vfs)
                                    .await?;
                            descendants.extend(sub_descendants.descendants.into_iter().map(
                                |mut it| {
                                    let mut p = PathBuf::from(name.clone());
                                    p.push(&it.path_relative_to_ascendant);
                                    it.path_relative_to_ascendant = p;
                                    it
                                },
                            ));
                        }

                        if <F as FolderFilter>::allows(&entry_path) {
                            let value = T::read_from_async(entry_path.clone(), vfs).await?;
                            descendants.push(DirDescendant {
                                name,
                                path_relative_to_ascendant: entry_path
                                    .strip_prefix(&path)
                                    .unwrap()
                                    .to_path_buf(),
                                path: entry_path,
                                value,
                            });
                        }
                    } else if kind.is_file() && <F as FileFilter>::allows(&entry_path) {
                        let value = T::read_from_async(entry_path.clone(), vfs).await?;
                        descendants.push(DirDescendant {
                            name,
                            path_relative_to_ascendant: entry_path
                                .strip_prefix(&path)
                                .unwrap()
                                .to_path_buf(),
                            path: entry_path,
                            value,
                        });
                    }
                }
            }

            Ok(DirDescendants::new(descendants))
        })
    }
}

impl<
    'vfs,
    Vfs: vfs::WriteSupportingVfs,
    T: WriteTo<Vfs> + 'vfs,
    F: FileFilter + FolderRecurseFilter + FolderFilter + 'vfs,
> WriteTo<Vfs> for DirDescendants<T, F>
{
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        for descendant in &self.descendants {
            descendant
                .value
                .write_to(&path.join(&descendant.path_relative_to_ascendant), vfs)?;
        }
        Ok(())
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<
    'vfs,
    Vfs: WriteSupportingVfsAsync + 'vfs,
    T: WriteToAsync<'vfs, Vfs> + Send + 'vfs,
    F: FileFilter + FolderRecurseFilter + FolderFilter + Send + 'vfs,
> WriteToAsync<'vfs, Vfs> for DirDescendants<T, F>
{
    type Future = BoxFuture<'vfs, Result<()>>;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'vfs Vfs>) -> Self::Future {
        Box::pin(async move {
            for descendant in self {
                descendant
                    .value
                    .write_to_async(path.join(&descendant.path_relative_to_ascendant), vfs)
                    .await?;
            }
            Ok(())
        })
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DirDescendantsWriteRefFutureProj)]
#[doc(hidden)]
pub enum DirDescendantsWriteRefFuture<
    'a,
    'vfs: 'a,
    Vfs: WriteSupportingVfsAsync + 'vfs,
    T: WriteToAsyncRef<'vfs, Vfs> + 'vfs,
> where
    T::Future<'a>: Future<Output = Result<()>> + Unpin + 'a,
{
    Poison,
    Writing {
        vfs: Pin<&'a Vfs>,
        path: PathBuf,
        iter: DirDescendantsIter<'a, T>,
        future: T::Future<'a>,
    },
    NoElems,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, 'vfs: 'a, Vfs: WriteSupportingVfsAsync + 'vfs, T: WriteToAsyncRef<'vfs, Vfs> + 'vfs> Future
    for DirDescendantsWriteRefFuture<'a, 'vfs, Vfs, T>
where
    for<'r> T::Future<'r>: Future<Output = Result<()>> + Unpin + 'r,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);
        match this {
            DirDescendantsWriteRefFutureProj::Poison => {
                panic!("polled after completion")
            }
            DirDescendantsWriteRefFutureProj::Writing {
                vfs,
                path,
                mut iter,
                mut future,
            } => match Pin::new(&mut future).poll(cx) {
                Poll::Ready(Ok(())) => {
                    let next = iter.next();
                    if let Some(descendant) = next {
                        let future = descendant.value.write_to_async_ref(
                            path.join(&descendant.path_relative_to_ascendant),
                            vfs,
                        );
                        self.as_mut()
                            .project_replace(DirDescendantsWriteRefFuture::Writing {
                                vfs,
                                path,
                                iter,
                                future,
                            });
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    } else {
                        Poll::Ready(Ok(()))
                    }
                }
                Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                Poll::Pending => {
                    self.as_mut()
                        .project_replace(DirDescendantsWriteRefFuture::Writing {
                            vfs,
                            path,
                            iter,
                            future,
                        });
                    Poll::Pending
                }
            },
            DirDescendantsWriteRefFutureProj::NoElems => Poll::Ready(Ok(())),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<
    'vfs,
    Vfs: WriteSupportingVfsAsync + 'static,
    T: WriteToAsyncRef<'vfs, Vfs> + Sync + 'static,
    F: FileFilter + FolderRecurseFilter + FolderFilter + Sync + 'vfs,
> WriteToAsyncRef<'vfs, Vfs> for DirDescendants<T, F>
where
    for<'r> T::Future<'r>: Future<Output = Result<()>> + Unpin + 'r,
{
    type Future<'r>
        = DirDescendantsWriteRefFuture<'r, 'vfs, Vfs, T>
    where
        'vfs: 'r,
        Self: 'r,
        Vfs: 'r,
        T: 'r;

    fn write_to_async_ref<'r>(&'r self, path: PathBuf, vfs: Pin<&'r Vfs>) -> Self::Future<'r>
    where
        'vfs: 'r,
    {
        let mut iter = self.iter();
        if let Some(first) = iter.next() {
            let future = first
                .value
                .write_to_async_ref(path.join(&first.path_relative_to_ascendant), vfs);
            DirDescendantsWriteRefFuture::Writing {
                vfs,
                iter,
                future,
                path,
            }
        } else {
            DirDescendantsWriteRefFuture::NoElems
        }
    }
}

/// A filter for folders, see the [`DirDescendants`] documentation.
pub trait FolderFilter {
    /// Whether to allow the given path.
    fn allows(folder: &Path) -> bool;
}

impl FolderFilter for NoFilter {
    fn allows(_folder: &Path) -> bool {
        true
    }
}

/// A filter to tell the reading logic whether to recurse into a folder, see the [`DirDescendants`] documentation.
pub trait FolderRecurseFilter {
    /// Whether to allow the given path.
    fn allows(folder: &Path) -> bool;
}

impl FolderRecurseFilter for NoFilter {
    fn allows(_folder: &Path) -> bool {
        true
    }
}

/// A filter for files, see the [`DirDescendants`] documentation.
pub trait FileFilter {
    /// Whether to allow the given path.
    fn allows(file: &Path) -> bool;
}

impl FileFilter for NoFilter {
    fn allows(_file: &Path) -> bool {
        true
    }
}

/// A single directory descendant, identified by its path relative to the ascendant.
///
/// It also stores the path relative to the "root."
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DirDescendant<T> {
    name: OsString,
    path: PathBuf,
    path_relative_to_ascendant: PathBuf,
    value: T,
}

impl<T> DirDescendant<T> {
    /// Create a new directory descendant from its parts.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use std::path::PathBuf;
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// assert_eq!(descendant.name(), &OsString::from("child"));
    /// assert_eq!(descendant.path(), &PathBuf::from("root/a/b/child"));
    /// assert_eq!(descendant.path_relative_to_ascendant(), &PathBuf::from("a/b/child"));
    /// assert_eq!(descendant.value(), &String::from("child_value"));
    /// ```
    pub fn new(
        name: impl Into<OsString>,
        path: impl Into<PathBuf>,
        path_relative_to_ascendant: impl Into<PathBuf>,
        value: T,
    ) -> Self {
        Self {
            name: name.into(),
            path: path.into(),
            path_relative_to_ascendant: path_relative_to_ascendant.into(),
            value,
        }
    }

    /// Get the name of the directory descendant.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// assert_eq!(descendant.name(), &OsString::from("child"));
    /// ```
    pub fn name(&self) -> &OsString {
        &self.name
    }

    /// Get the path of the directory descendant.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// assert_eq!(descendant.path(), &PathBuf::from("root/a/b/child"));
    /// ```
    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Get the path relative to the ascendant.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// assert_eq!(descendant.path_relative_to_ascendant(), &PathBuf::from("a/b/child"));
    /// ```
    pub fn path_relative_to_ascendant(&self) -> &PathBuf {
        &self.path_relative_to_ascendant
    }

    /// Get the value of the descendant.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// assert_eq!(descendant.value(), &String::from("child_value"));
    /// ```
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Turns the descendant into its value.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// assert_eq!(descendant.into_value(), String::from("child_value"));
    /// ```
    pub fn into_value(self) -> T {
        self.value
    }

    /// Turns the descendant into its name.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// assert_eq!(descendant.into_name(), OsString::from("child"));
    /// ```
    pub fn into_name(self) -> OsString {
        self.name
    }

    /// Turns the descendant into its path.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// assert_eq!(descendant.into_path(), PathBuf::from("root/a/b/child"));
    /// ```
    pub fn into_path(self) -> PathBuf {
        self.path
    }

    /// Get a mutable reference to the name of the directory descendant.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::ffi::OsString;
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let mut descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// *descendant.name_mut() = OsString::from("new_child");
    /// assert_eq!(descendant.name(), &OsString::from("new_child"));
    /// ```
    pub fn name_mut(&mut self) -> &mut OsString {
        &mut self.name
    }

    /// Get a mutable reference to the value of the directory descendant.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let mut descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// *descendant.value_mut() = String::from("new_child_value");
    /// assert_eq!(descendant.value(), &String::from("new_child_value"));
    /// ```
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }

    /// Clones the directory name and paths, but makes the value a reference to the original value.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    ///
    /// let ref_descendant = descendant.as_ref();
    /// assert_eq!(ref_descendant.name(), descendant.name());
    /// assert_eq!(ref_descendant.path(), descendant.path());
    /// assert_eq!(ref_descendant.path_relative_to_ascendant(), descendant.path_relative_to_ascendant());
    /// assert_eq!(ref_descendant.value(), &descendant.value());
    /// ```
    pub fn as_ref(&self) -> DirDescendant<&T> {
        DirDescendant::new(
            self.name.clone(),
            self.path.clone(),
            self.path_relative_to_ascendant.clone(),
            &self.value,
        )
    }

    /// Clones the directory name and paths, but makes the value a mutable reference to the original value.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let mut descendant = DirDescendant::new(
    ///    "child",
    ///   "root/a/b/child",
    ///  "a/b/child",
    ///   String::from("child_value"),
    /// );
    ///
    /// let mut mut_ref_descendant = descendant.as_mut();
    /// mut_ref_descendant.value_mut().push_str("_new");
    /// assert_eq!(descendant.value(), &String::from("child_value_new"));
    /// ```
    pub fn as_mut(&mut self) -> DirDescendant<&mut T> {
        DirDescendant::new(
            self.name.clone(),
            self.path.clone(),
            self.path_relative_to_ascendant.clone(),
            &mut self.value,
        )
    }

    /// Maps the value of the directory descendant.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::dir_descendants::DirDescendant;
    ///
    /// let descendant = DirDescendant::new(
    ///     "child",
    ///     "root/a/b/child",
    ///     "a/b/child",
    ///     String::from("child_value"),
    /// );
    /// let mapped = descendant.map(|v| v.len());
    /// assert_eq!(mapped.value(), &11);
    /// ```
    pub fn map<F, U>(self, f: F) -> DirDescendant<U>
    where
        F: FnOnce(T) -> U,
    {
        DirDescendant::new(
            self.name,
            self.path,
            self.path_relative_to_ascendant,
            f(self.value),
        )
    }
}

impl<T> Deref for DirDescendant<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for DirDescendant<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

#[cfg(feature = "resolve-path")]
impl<T, F: FolderFilter + FolderRecurseFilter + FileFilter> DynamicHasField
    for DirDescendants<T, F>
{
    type Inner = T;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        p.join(name)
    }
}
