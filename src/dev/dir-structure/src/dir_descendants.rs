use core::fmt::Debug;
use core::slice;
use std::ffi::OsString;
use std::fmt;
use std::marker;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
use std::vec;

use crate::DirEntryInfo;
use crate::DirWalker;
#[cfg(feature = "resolve-path")]
use crate::DynamicHasField;
use crate::NoFilter;
use crate::ReadFrom;
use crate::WriteTo;

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
    pub fn new(descendants: Vec<DirDescendant<T>>) -> Self {
        Self {
            descendants,
            _phantom: marker::PhantomData,
        }
    }

    /// Returns an iterator over the descendants.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::{DirDescendants, DirDescendant, NoFilter};
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
    /// use dir_structure::{DirDescendants, DirDescendant, NoFilter};
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
    Vfs: crate::Vfs,
    T: ReadFrom<'vfs, Vfs>,
    F: FolderFilter + FolderRecurseFilter + FileFilter + 'vfs,
> ReadFrom<'vfs, Vfs> for DirDescendants<T, F>
{
    fn read_from(path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<Self, crate::Error> {
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

impl<
    'vfs,
    Vfs: crate::WriteSupportingVfs,
    T: WriteTo<Vfs> + 'vfs,
    F: FileFilter + FolderRecurseFilter + FolderFilter + 'vfs,
> WriteTo<Vfs> for DirDescendants<T, F>
{
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<(), crate::Error> {
        for descendant in &self.descendants {
            descendant
                .value
                .write_to(&path.join(&descendant.path_relative_to_ascendant), vfs)?;
        }
        Ok(())
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
    /// use dir_structure::DirDescendant;
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
    /// use dir_structure::DirDescendant;
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
    /// use dir_structure::DirDescendant;
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
    /// use dir_structure::DirDescendant;
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
    /// use dir_structure::DirDescendant;
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
    /// use dir_structure::DirDescendant;
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
    /// use dir_structure::DirDescendant;
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
    /// use dir_structure::DirDescendant;
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
    /// use dir_structure::DirDescendant;
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

    /// Maps the value of the directory descendant.
    ///
    /// # Examples
    ///
    /// ```
    /// use dir_structure::DirDescendant;
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

#[cfg(feature = "resolve-path")]
impl<T, F: FolderFilter + FolderRecurseFilter + FileFilter> DynamicHasField
    for DirDescendants<T, F>
{
    type Inner = T;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        p.join(name)
    }
}
