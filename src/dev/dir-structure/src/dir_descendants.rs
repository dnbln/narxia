use core::slice;
use std::ffi::OsString;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::DirEntryInfo;
use crate::DirWalker;
use crate::NoFilter;
use crate::ReadFrom;
use crate::WriteTo;

pub struct DirDescendants<T, F: FolderFilter + FileFilter = NoFilter> {
    descendants: Vec<DirDescendant<T>>,
    _phantom: std::marker::PhantomData<F>,
}

impl<T: Clone, F: FolderFilter + FileFilter> Clone for DirDescendants<T, F> {
    fn clone(&self) -> Self {
        Self {
            descendants: self.descendants.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T, F: FolderFilter + FileFilter> DirDescendants<T, F> {
    pub fn new(descendants: Vec<DirDescendant<T>>) -> Self {
        Self {
            descendants,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn iter(&self) -> DirDescendantsIter<'_, T> {
        DirDescendantsIter(self.descendants.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut DirDescendant<T>> {
        self.descendants.iter_mut()
    }
}

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

pub struct DirDescendantsIntoIter<T>(std::vec::IntoIter<DirDescendant<T>>);

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

impl<'vfs, Vfs: crate::Vfs, T: ReadFrom<'vfs, Vfs>, F: FolderFilter + FileFilter + 'vfs>
    ReadFrom<'vfs, Vfs> for DirDescendants<T, F>
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
                    if <F as FolderFilter>::allows(&entry_path) {
                        let sub_descendants = DirDescendants::<T, F>::read_from(&entry_path, vfs)?;
                        descendants.extend(sub_descendants.descendants);
                    }
                } else if kind.is_file() {
                    if <F as FileFilter>::allows(&entry_path) {
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
        }

        Ok(DirDescendants::new(descendants))
    }
}

impl<'vfs, Vfs: crate::Vfs, T: WriteTo<Vfs> + 'vfs, F: FileFilter + FolderFilter + 'vfs>
    WriteTo<Vfs> for DirDescendants<T, F>
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

pub trait FolderFilter {
    fn allows(folder: &Path) -> bool;
}

impl FolderFilter for NoFilter {
    fn allows(_folder: &Path) -> bool {
        true
    }
}

pub trait FileFilter {
    fn allows(file: &Path) -> bool;
}

impl FileFilter for NoFilter {
    fn allows(_file: &Path) -> bool {
        true
    }
}

#[derive(Clone)]
pub struct DirDescendant<T> {
    name: OsString,
    path: PathBuf,
    path_relative_to_ascendant: PathBuf,
    value: T,
}

impl<T> DirDescendant<T> {
    pub fn new(
        name: OsString,
        path: PathBuf,
        path_relative_to_ascendant: PathBuf,
        value: T,
    ) -> Self {
        Self {
            name,
            path,
            path_relative_to_ascendant,
            value,
        }
    }

    pub fn name(&self) -> &OsString {
        &self.name
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn into_value(self) -> T {
        self.value
    }

    pub fn into_name(self) -> OsString {
        self.name
    }

    pub fn into_path(self) -> PathBuf {
        self.path
    }

    pub fn name_mut(&mut self) -> &mut OsString {
        &mut self.name
    }

    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }
}
