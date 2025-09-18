//! A versioned value.
//!
//! See [`Versioned`] for more details.

use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
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
#[cfg(feature = "resolve-path")]
use crate::traits::vfs::OwnedPathType;
use crate::traits::vfs::PathType;
#[cfg(feature = "async")]
use crate::traits::vfs::VfsCore;

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
/// use std::path::Path;
/// use dir_structure::versioned::VersionedString;
///
/// let mut v = VersionedString::<Path>::new("value".to_owned(), "path".to_owned());
/// assert!(v.is_clean());
/// assert!(!v.is_dirty());
///
/// *v = "new value".to_owned();
/// assert!(v.is_dirty());
/// ```
#[derive(Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct Versioned<T, P: PathType + ?Sized = Path> {
    value: T,
    version: usize,
    path: P::OwnedPath,
}

impl<T, P: PathType + ?Sized> Clone for Versioned<T, P>
where
    T: Clone,
    P::OwnedPath: Clone,
{
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            version: self.version,
            path: self.path.clone(),
        }
    }
}

impl<T, P: PathType + ?Sized> Versioned<T, P> {
    const DEFAULT_VERSION: usize = 0;

    /// Creates a new [`Versioned`] with the specified value.
    ///
    /// The version is set to the default value.
    pub fn new(value: T, path: impl Into<P::OwnedPath>) -> Self {
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
    /// use std::path::Path;
    /// use dir_structure::versioned::VersionedString;
    ///
    /// let v = VersionedString::<Path>::new_dirty("value".to_owned(), "path".to_owned());
    /// assert!(v.is_dirty());
    /// ```
    pub fn new_dirty(value: T, path: impl Into<P::OwnedPath>) -> Self {
        Self {
            value,
            version: Self::DEFAULT_VERSION + 1,
            path: path.into(),
        }
    }

    /// Checks if the value has been changed.
    ///
    /// # Example
    ///
    /// ```
    /// use std::path::Path;
    /// use dir_structure::versioned::VersionedString;
    ///
    /// let mut v = VersionedString::<Path>::new("value".to_owned(), "path".to_owned());
    /// assert!(!v.is_dirty());
    /// *v = "new value".to_owned();
    /// assert!(v.is_dirty());
    /// ```
    pub fn is_dirty(&self) -> bool {
        !self.is_clean()
    }

    /// Checks if the value has not been changed.
    ///
    /// # Example
    ///
    /// ```
    /// use std::path::Path;
    /// use dir_structure::versioned::VersionedString;
    ///
    /// let mut v = VersionedString::<Path>::new("value".to_owned(), "path".to_owned());
    /// assert!(v.is_clean());
    /// *v = "new value".to_owned();
    /// assert!(!v.is_clean());
    /// ```
    pub fn is_clean(&self) -> bool {
        self.version == Self::DEFAULT_VERSION
    }

    /// Edits the value using the provided closure, and increments the version
    /// if the value has changed.
    ///
    /// # Example
    ///
    /// ```
    /// use std::path::Path;
    /// use dir_structure::versioned::VersionedString;
    ///
    /// let mut v = VersionedString::<Path>::new("value".to_owned(), "path".to_owned());
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
    /// use std::path::Path;
    /// use dir_structure::{traits::sync::DirStructureItem, versioned::VersionedString};
    /// std::fs::write("path", "value").unwrap();
    ///
    /// let mut v = VersionedString::<Path>::new("value".to_owned(), "path".to_owned());
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

impl<'a, Vfs: vfs::Vfs<'a>, T> ReadFrom<'a, Vfs> for Versioned<T, Vfs::Path>
where
    T: ReadFrom<'a, Vfs>,
{
    fn read_from(
        path: &Vfs::Path,
        vfs: Pin<&'a Vfs>,
    ) -> Result<Self, <Vfs::Path as PathType>::OwnedPath>
    where
        Self: Sized,
    {
        T::read_from(path, vfs).map(|it| Self::new(it, path.owned()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project]
#[doc(hidden)]
pub struct VersionedReadFuture<'a, Vfs: VfsAsync, T: ReadFromAsync<'a, Vfs> + Send + 'static> {
    #[pin]
    inner: T::Future,
    path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: VfsAsync + 'static, T> Future for VersionedReadFuture<'a, Vfs, T>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    type Output = Result<Versioned<T, Vfs::Path>, <<Vfs as VfsCore>::Path as PathType>::OwnedPath>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let projection = self.project();
        <T::Future as Future>::poll(projection.inner, cx)
            .map_ok(|value| Versioned::new(value, projection.path.clone()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: VfsAsync + 'static, T: ReadFromAsync<'a, Vfs> + Send + 'static> ReadFromAsync<'a, Vfs>
    for Versioned<T, Vfs::Path>
{
    type Future = VersionedReadFuture<'a, Vfs, T>;

    fn read_from_async(
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        VersionedReadFuture {
            inner: T::read_from_async(path.clone(), vfs),
            path,
        }
    }
}

impl<'a, Vfs: vfs::WriteSupportingVfs<'a>, T: WriteTo<'a, Vfs>> WriteTo<'a, Vfs>
    for Versioned<T, Vfs::Path>
where
    Vfs::Path: PartialEq,
{
    fn write_to(
        &self,
        path: &Vfs::Path,
        vfs: Pin<&'a Vfs>,
    ) -> Result<(), <Vfs::Path as PathType>::OwnedPath> {
        if self.path.as_ref() == path && self.is_clean() {
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
    <T as WriteToAsync<'a, Vfs>>::Future:
        Future<Output = Result<(), <<Vfs as VfsCore>::Path as PathType>::OwnedPath>> + Unpin + 'a,
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
    <T as WriteToAsync<'a, Vfs>>::Future:
        Future<Output = Result<(), <<Vfs as VfsCore>::Path as PathType>::OwnedPath>> + Unpin + 'a,
{
    type Output = Result<(), <<Vfs as VfsCore>::Path as PathType>::OwnedPath>;

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
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs>
    for Versioned<T, Vfs::Path>
where
    T: WriteToAsync<'a, Vfs> + Send + Sync + 'static,
    <T as WriteToAsync<'a, Vfs>>::Future:
        Future<Output = Result<(), <<Vfs as VfsCore>::Path as PathType>::OwnedPath>> + Unpin,
    Vfs::Path: PartialEq,
{
    type Future = VersionedWriteFuture<'a, T, Vfs>;

    fn write_to_async(
        self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        if self.path.as_ref() == path.as_ref() && self.is_clean() {
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
    <T as WriteToAsyncRef<'a, Vfs>>::Future<'f>:
        Future<Output = Result<(), <<Vfs as VfsCore>::Path as PathType>::OwnedPath>> + Unpin + 'f,
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
    <T as WriteToAsyncRef<'a, Vfs>>::Future<'f>:
        Future<Output = Result<(), <<Vfs as VfsCore>::Path as PathType>::OwnedPath>> + Unpin + 'f,
    'a: 'f,
{
    type Output = Result<(), <<Vfs as VfsCore>::Path as PathType>::OwnedPath>;

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
impl<'r, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsyncRef<'r, Vfs>
    for Versioned<T, Vfs::Path>
where
    T: WriteToAsyncRef<'r, Vfs> + Send + Sync + 'static,
    for<'f> <T as WriteToAsyncRef<'r, Vfs>>::Future<'f>:
        Future<Output = Result<(), <<Vfs as VfsCore>::Path as PathType>::OwnedPath>> + Unpin + 'f,
    Vfs::Path: PartialEq,
{
    type Future<'a>
        = VersionedWriteRefFuture<'r, 'a, T, Vfs>
    where
        Self: 'a,
        'r: 'a,
        Vfs: 'a;

    fn write_to_async_ref<'a>(
        &'a self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> <Self as WriteToAsyncRef<'r, Vfs>>::Future<'a>
    where
        'r: 'a,
    {
        if self.path.as_ref() == path.as_ref() && self.is_clean() {
            return VersionedWriteRefFuture::NotTouched;
        }

        VersionedWriteRefFuture::Writing {
            inner: self.value.write_to_async_ref(path, vfs),
        }
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<const NAME: [char; HAS_FIELD_MAX_LEN], T, P: PathType + ?Sized> HasField<NAME>
    for Versioned<T, P>
where
    T: HasField<NAME>,
{
    type Inner = <T as HasField<NAME>>::Inner;

    fn resolve_path<Pt: OwnedPathType>(p: Pt) -> Pt {
        T::resolve_path(p)
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<T, P: PathType + ?Sized> DynamicHasField for Versioned<T, P>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path<Pt: OwnedPathType>(p: Pt, name: &str) -> Pt {
        T::resolve_path(p, name)
    }
}

impl<T, P: PathType + ?Sized> Deref for Versioned<T, P> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T, P: PathType + ?Sized> DerefMut for Versioned<T, P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // We will assume that the value has changed, if `deref_mut` was called.
        // So we increment the version.
        self.version += 1;

        &mut self.value
    }
}

/// A [`Versioned`] [`String`].
pub type VersionedString<P: PathType + ?Sized = Path> = Versioned<String, P>;
/// A [`Versioned`] `Vec<u8>`.
pub type VersionedBytes<P: PathType + ?Sized = Path> = Versioned<Vec<u8>, P>;

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::pin::Pin;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;

    use super::*;

    struct WriteCounter<T> {
        count: AtomicUsize,
        inner: T,
    }

    impl<T> WriteCounter<T> {
        fn write_count(&self) -> usize {
            self.count.load(Ordering::SeqCst)
        }
    }

    impl<T> Deref for WriteCounter<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.inner
        }
    }

    impl<T> DerefMut for WriteCounter<T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.inner
        }
    }

    impl<'a, Vfs: vfs::Vfs<'a>, T: ReadFrom<'a, Vfs>> ReadFrom<'a, Vfs> for WriteCounter<T> {
        fn read_from(
            path: &Vfs::Path,
            vfs: Pin<&'a Vfs>,
        ) -> Result<Self, <Vfs::Path as vfs::PathType>::OwnedPath> {
            Ok(Self {
                count: AtomicUsize::new(0),
                inner: T::read_from(path, vfs)?,
            })
        }
    }

    impl<'a, Vfs: vfs::WriteSupportingVfs<'a>, T: WriteTo<'a, Vfs>> WriteTo<'a, Vfs>
        for WriteCounter<T>
    {
        fn write_to(
            &self,
            path: &Vfs::Path,
            vfs: Pin<&'a Vfs>,
        ) -> Result<(), <Vfs::Path as PathType>::OwnedPath> {
            self.inner.write_to(path, vfs)?;
            self.count.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }
    }

    #[test]
    fn versioned_works() {
        let s = VersionedString::<Path>::new("value".to_owned(), "path");
        assert!(s.is_clean());
        assert!(!s.is_dirty());

        let mut s = s;
        *s = "new value".to_owned();
        assert!(s.is_dirty());
        assert!(!s.is_clean());

        s.edit_eq_check(|v| *v = "new value".to_owned());
        assert!(s.is_dirty());
        assert!(!s.is_clean());
        s.edit_eq_check(|v| *v = "value".to_owned());
        assert!(s.is_dirty());
        assert!(!s.is_clean());

        #[expect(unsafe_code, reason = "This function is unsafe by design")]
        unsafe {
            s.reset();
        }

        assert!(s.is_clean());
        assert!(!s.is_dirty());

        s.edit_eq_check(|v| *v = "value".to_owned());
        assert!(s.is_clean());
        assert!(!s.is_dirty());

        s.edit_eq_check(|v| *v = "new value".to_owned());
        assert!(s.is_dirty());
        assert!(!s.is_clean());
    }

    #[test]
    fn type_checks() {
        crate::test_utils::assert_is_read_from::<crate::vfs::fs_vfs::FsVfs, VersionedString<Path>>(
        );
        crate::test_utils::assert_is_write_to::<crate::vfs::fs_vfs::FsVfs, VersionedString<Path>>();
    }
}
