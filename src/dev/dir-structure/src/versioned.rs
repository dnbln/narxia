//! A versioned value.
//!
//! See [`Versioned`] for more details.

use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::error::Result;
use crate::prelude::*;
#[cfg(feature = "async")]
use crate::traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::WriteSupportingVfsAsync;
#[cfg(feature = "resolve-path")]
use crate::traits::resolve::DynamicHasField;
#[cfg(feature = "resolve-path")]
use crate::traits::resolve::HAS_FIELD_MAX_LEN;
#[cfg(feature = "resolve-path")]
use crate::traits::resolve::HasField;
use crate::traits::vfs;

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
/// use dir_structure::versioned::VersionedString;
///
/// let mut v = VersionedString::new("value".to_owned(), "path");
/// assert!(v.is_clean());
/// assert!(!v.is_dirty());
///
/// *v = "new value".to_owned();
/// assert!(v.is_dirty());
/// ```
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct Versioned<T> {
    value: T,
    version: usize,
    path: PathBuf,
}

impl<T> Versioned<T> {
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
    /// use dir_structure::versioned::VersionedString;
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
    /// use dir_structure::versioned::VersionedString;
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
    /// use dir_structure::{traits::sync::DirStructureItem, versioned::VersionedString};
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
    #[expect(unsafe_code, reason = "This function is unsafe by design")]
    pub unsafe fn reset(&mut self) {
        // This is unsafe because it allows us to reset the version to 0,
        // which means that the value will be considered clean.
        // Use with caution!
        self.version = Self::DEFAULT_VERSION;
    }
}

impl<'a, Vfs: vfs::Vfs, T> ReadFrom<'a, Vfs> for Versioned<T>
where
    T: ReadFrom<'a, Vfs>,
{
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        T::read_from(path, vfs).map(|it| Self::new(it, path))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project]
#[doc(hidden)]
pub struct VersionedReadFuture<'a, Vfs: VfsAsync, T: ReadFromAsync<'a, Vfs> + Send + 'static> {
    #[pin]
    inner: T::Future,
    path: PathBuf,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: VfsAsync + 'static, T> Future for VersionedReadFuture<'a, Vfs, T>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    type Output = Result<Versioned<T>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let projection = self.project();
        <T::Future as Future>::poll(projection.inner, cx)
            .map_ok(|value| Versioned::new(value, projection.path.to_path_buf()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: VfsAsync + 'static, T: ReadFromAsync<'a, Vfs> + Send + 'static> ReadFromAsync<'a, Vfs>
    for Versioned<T>
{
    type Future = VersionedReadFuture<'a, Vfs, T>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        VersionedReadFuture {
            inner: T::read_from_async(path.clone(), vfs),
            path,
        }
    }
}

impl<Vfs: vfs::WriteSupportingVfs, T: WriteTo<Vfs>> WriteTo<Vfs> for Versioned<T> {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        if self.path == path && self.is_clean() {
            return Ok(());
        }

        self.value.write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = VersionedWriteFutureProj)]
#[doc(hidden)]
pub enum VersionedWriteFuture<'a, T, Vfs: WriteSupportingVfsAsync + 'a>
where
    T: WriteToAsync<'a, Vfs> + Send + Sync + 'static,
    <T as WriteToAsync<'a, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    Poisson,
    NotTouched,
    Writing {
        inner: <T as WriteToAsync<'a, Vfs>>::Future,
    },
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'a> Future for VersionedWriteFuture<'a, T, Vfs>
where
    T: WriteToAsync<'a, Vfs> + Send + Sync + 'static,
    <T as WriteToAsync<'a, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poisson);
        match this {
            VersionedWriteFutureProj::NotTouched => Poll::Ready(Ok(())),
            VersionedWriteFutureProj::Writing { mut inner } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(res) => Poll::Ready(res),
                    Poll::Pending => {
                        self.project_replace(Self::Writing { inner });
                        Poll::Pending
                    }
                }
            }
            VersionedWriteFutureProj::Poisson => {
                panic!("VersionedWriteFuture is in an invalid state. This is a bug in the code.");
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for Versioned<T>
where
    T: WriteToAsync<'a, Vfs> + Send + Sync + 'static,
    <T as WriteToAsync<'a, Vfs>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Future = VersionedWriteFuture<'a, T, Vfs>;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        if self.path == path && self.is_clean() {
            return VersionedWriteFuture::NotTouched;
        }

        VersionedWriteFuture::Writing {
            inner: self.value.write_to_async(path, vfs),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = VersionedWriteRefFutureProj)]
#[doc(hidden)]
pub enum VersionedWriteRefFuture<'a, 'f, T, Vfs: WriteSupportingVfsAsync + 'a>
where
    T: WriteToAsyncRef<'a, Vfs> + Send + Sync + 'static,
    <T as WriteToAsyncRef<'a, Vfs>>::Future<'f>: Future<Output = Result<()>> + Unpin + 'f,
    'a: 'f,
{
    Poisson,
    NotTouched,
    Writing {
        inner: <T as WriteToAsyncRef<'a, Vfs>>::Future<'f>,
    },
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, 'f, T, Vfs: WriteSupportingVfsAsync + 'a> Future
    for VersionedWriteRefFuture<'a, 'f, T, Vfs>
where
    T: WriteToAsyncRef<'a, Vfs> + Send + Sync + 'static,
    <T as WriteToAsyncRef<'a, Vfs>>::Future<'f>: Future<Output = Result<()>> + Unpin + 'f,
    'a: 'f,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poisson);
        match this {
            VersionedWriteRefFutureProj::NotTouched => Poll::Ready(Ok(())),
            VersionedWriteRefFutureProj::Writing { mut inner } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(res) => Poll::Ready(res),
                    Poll::Pending => {
                        self.project_replace(Self::Writing { inner });
                        Poll::Pending
                    }
                }
            }
            VersionedWriteRefFutureProj::Poisson => {
                panic!(
                    "VersionedWriteRefFuture is in an invalid state. This is a bug in the code."
                );
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'r, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsyncRef<'r, Vfs> for Versioned<T>
where
    T: WriteToAsyncRef<'r, Vfs> + Send + Sync + 'static,
    for<'f> <T as WriteToAsyncRef<'r, Vfs>>::Future<'f>: Future<Output = Result<()>> + Unpin + 'f,
{
    type Future<'a>
        = VersionedWriteRefFuture<'r, 'a, T, Vfs>
    where
        Self: 'a,
        'r: 'a,
        Vfs: 'a;

    fn write_to_async_ref<'a>(
        &'a self,
        path: PathBuf,
        vfs: Pin<&'a Vfs>,
    ) -> <Self as WriteToAsyncRef<'r, Vfs>>::Future<'a>
    where
        'r: 'a,
    {
        if self.path == path && self.is_clean() {
            return VersionedWriteRefFuture::NotTouched;
        }

        VersionedWriteRefFuture::Writing {
            inner: self.value.write_to_async_ref(path, vfs),
        }
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<const NAME: [char; HAS_FIELD_MAX_LEN], T> HasField<NAME> for Versioned<T>
where
    T: HasField<NAME>,
{
    type Inner = <T as HasField<NAME>>::Inner;

    fn resolve_path(p: PathBuf) -> PathBuf {
        T::resolve_path(p)
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<T> DynamicHasField for Versioned<T>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        T::resolve_path(p, name)
    }
}

impl<T> Deref for Versioned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Versioned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // We will assume that the value has changed, if `deref_mut` was called.
        // So we increment the version.
        self.version += 1;

        &mut self.value
    }
}

/// A [`Versioned`] [`String`].
pub type VersionedString = Versioned<String>;
/// A [`Versioned`] `Vec<u8>`.
pub type VersionedBytes = Versioned<Vec<u8>>;
