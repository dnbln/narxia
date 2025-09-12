//! A wrapper that defers the reading of a file until it is actually needed.
//!
//! See [`DeferredRead::perform_read`] for more details.

use core::fmt;
use core::fmt::Debug;
use std::io;
use std::marker;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::error::Error;
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
use crate::vfs::fs_vfs;

/// A wrapper that defers the reading of a file until it is actually needed.
///
/// The only thing you can do with a [`DeferredRead`] is to call [`DeferredRead::perform_read`],
/// which will read the file and return the value.
///
/// See the [`DeferredRead::perform_read`] method for more details.
#[derive(Clone, Hash)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct DeferredRead<'a, T, Vfs = fs_vfs::FsVfs, const CHECK_ON_READ: bool = false>(
    pub PathBuf,
    #[cfg_attr(feature = "assert_eq", assert_eq(ignore))]
    Pin<&'a Vfs>,
    #[cfg_attr(feature = "assert_eq", assert_eq(ignore))]
    marker::PhantomData<T>,
);

impl<'a, const CHECK_ON_READ: bool, T, Vfs> Debug for DeferredRead<'a, T, Vfs, CHECK_ON_READ>
where
    T: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::any::type_name;
        write!(
            f,
            "deferred{} {} @ {} (vfs = {})",
            if CHECK_ON_READ { "[checked]" } else { "" },
            type_name::<T>(),
            self.0.display(),
            type_name::<Vfs>()
        )
    }
}

impl<'a, const CHECK_ON_READ: bool, T, Vfs: vfs::Vfs> ReadFrom<'a, Vfs>
    for DeferredRead<'a, T, Vfs, CHECK_ON_READ>
where
    T: ReadFrom<'a, Vfs>,
{
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        if CHECK_ON_READ && !vfs.exists(path)? {
            return Err(Error::Io(
                path.to_path_buf(),
                io::ErrorKind::NotFound.into(),
            ));
        }

        Ok(Self(path.to_path_buf(), vfs, marker::PhantomData))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, const CHECK_ON_READ: bool, T, Vfs: VfsAsync + 'static> ReadFromAsync<'a, Vfs>
    for DeferredRead<'a, T, Vfs, CHECK_ON_READ>
where
    T: Send + ReadFromAsync<'a, Vfs> + 'static,
{
    type Future
        = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'a>>
    where
        Self: 'a;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move {
            if CHECK_ON_READ && !vfs.exists(path.clone()).await? {
                return Err(Error::Io(path, io::ErrorKind::NotFound.into()));
            }

            Ok(Self(path, vfs, marker::PhantomData))
        })
    }
}

impl<'a, const CHECK_ON_READ: bool, T, Vfs: vfs::Vfs> DeferredRead<'a, T, Vfs, CHECK_ON_READ>
where
    T: ReadFrom<'a, Vfs>,
{
    /// Performs the read and returns the value.
    ///
    /// If the value changed on disk since the [`DeferredRead`] was created, then the
    /// new value will be read from disk and returned.
    ///
    /// For a cached version see [`DeferredReadOrOwn`][crate::DeferredReadOrOwn].
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::Path;
    /// use std::pin::Pin;
    /// use dir_structure::traits::sync::DirStructureItem;
    /// use dir_structure::deferred_read::DeferredRead;
    ///
    /// #[derive(dir_structure::DirStructure)]
    /// struct Dir<'vfs, Vfs> {
    ///     #[dir_structure(path = "f.txt")]
    ///     f: DeferredRead<'vfs, String, Vfs>,
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
        T::read_from(&self.0, self.1)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, const CHECK_ON_READ: bool, T, Vfs: VfsAsync> DeferredRead<'a, T, Vfs, CHECK_ON_READ>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    /// Performs the read asynchronously and returns the value.
    ///
    /// If the value changed on disk since the [`DeferredRead`] was created, then the
    /// new value will be read from disk and returned.
    ///
    /// For a cached version see [`DeferredReadOrOwn`].
    ///
    /// Asynchronous version of [`DeferredRead::perform_read`].
    pub async fn perform_read_async(&self) -> Result<T> {
        T::read_from_async(self.0.clone(), self.1).await
    }
}

impl<'a, const CHECK_ON_READ: bool, T, SelfVfs: vfs::Vfs, TargetVfs: vfs::WriteSupportingVfs>
    WriteTo<TargetVfs> for DeferredRead<'a, T, SelfVfs, CHECK_ON_READ>
where
    T: ReadFrom<'a, SelfVfs> + WriteTo<TargetVfs>,
{
    fn write_to(&self, path: &Path, vfs: Pin<&TargetVfs>) -> Result<()> {
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
        r.write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DeferredReadWriteFutureProj)]
#[doc(hidden)]
pub enum DeferredReadWriteFuture<
    'a,
    T,
    SelfVfs: VfsAsync + 'a,
    TargetVfs: WriteSupportingVfsAsync + 'a,
> where
    T: ReadFromAsync<'a, SelfVfs> + WriteToAsync<'a, TargetVfs> + Send + 'static,
    <T as ReadFromAsync<'a, SelfVfs>>::Future: Future<Output = Result<T>> + Unpin + 'a,
    <T as WriteToAsync<'a, TargetVfs>>::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    Poisson,
    SamePath,
    Reading {
        self_vfs: Pin<&'a SelfVfs>,
        target_vfs: Pin<&'a TargetVfs>,
        inner: <T as ReadFromAsync<'a, SelfVfs>>::Future,
        path: PathBuf,
    },
    Writing {
        inner: <T as WriteToAsync<'a, TargetVfs>>::Future,
    },
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, SelfVfs: VfsAsync + 'a, TargetVfs: WriteSupportingVfsAsync + 'a> Future
    for DeferredReadWriteFuture<'a, T, SelfVfs, TargetVfs>
where
    T: ReadFromAsync<'a, SelfVfs> + WriteToAsync<'a, TargetVfs> + Send + 'static,
    <T as ReadFromAsync<'a, SelfVfs>>::Future: Future<Output = Result<T>> + Unpin,
    <T as WriteToAsync<'a, TargetVfs>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poisson);
        match this {
            DeferredReadWriteFutureProj::SamePath => Poll::Ready(Ok(())),
            DeferredReadWriteFutureProj::Reading {
                mut inner,
                self_vfs,
                target_vfs,
                path,
            } => match Pin::new(&mut inner).poll(cx) {
                Poll::Ready(Ok(v)) => {
                    self.project_replace(Self::Writing {
                        inner: v.write_to_async(path, target_vfs),
                    });
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                Poll::Pending => {
                    self.project_replace(Self::Reading {
                        inner,
                        path,
                        self_vfs,
                        target_vfs,
                    });
                    Poll::Pending
                }
            },
            DeferredReadWriteFutureProj::Writing { mut inner } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(Ok(())) => Poll::Ready(Ok(())),
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::Writing { inner });
                        Poll::Pending
                    }
                }
            }
            DeferredReadWriteFutureProj::Poisson => {
                panic!(
                    "DeferredReadWriteFuture is in an invalid state. This is a bug in the code."
                );
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<
    'f,
    const CHECK_ON_READ: bool,
    T,
    SelfVfs: VfsAsync + 'f,
    TargetVfs: WriteSupportingVfsAsync + 'f,
> WriteToAsync<'f, TargetVfs> for DeferredRead<'f, T, SelfVfs, CHECK_ON_READ>
where
    T: for<'a> ReadFromAsync<'a, SelfVfs> + for<'a> WriteToAsync<'a, TargetVfs> + Send + 'static,
    for<'a> <T as ReadFromAsync<'a, SelfVfs>>::Future: Future<Output = Result<T>> + Unpin + 'a,
    for<'a> <T as WriteToAsync<'a, TargetVfs>>::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    type Future = DeferredReadWriteFuture<'f, T, SelfVfs, TargetVfs>;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'f TargetVfs>) -> Self::Future {
        if path == self.0 {
            // Optimization: We were asked to write to the same path
            // we are supposed to read from. We can just ignore it, since
            // the file / directory should already be in the given state.

            // If `T` has trivial `ReadFromAsync` / `WriteToAsync` implementations,
            // this should not be a problem, but if it is, a custom `DeferredRead`
            // implementation should be written for it.
            return DeferredReadWriteFuture::SamePath;
        }

        DeferredReadWriteFuture::Reading {
            inner: T::read_from_async(self.0.clone(), self.1),
            path,
            self_vfs: self.1,
            target_vfs: vfs,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DeferredReadWriteRefFutureProj)]
#[doc(hidden)]
pub enum DeferredReadWriteRefFuture<
    'f,
    T,
    SelfVfs: VfsAsync + 'f,
    TargetVfs: WriteSupportingVfsAsync + 'f,
> where
    T: for<'a> ReadFromAsync<'a, SelfVfs> + for<'a> WriteToAsync<'a, TargetVfs> + Send + 'static,
    for<'a> <T as ReadFromAsync<'a, SelfVfs>>::Future: Future<Output = Result<T>> + Unpin + 'a,
    for<'a> <T as WriteToAsync<'a, TargetVfs>>::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    Poisson,
    SamePath,
    Reading {
        self_vfs: Pin<&'f SelfVfs>,
        target_vfs: Pin<&'f TargetVfs>,
        inner: <T as ReadFromAsync<'f, SelfVfs>>::Future,
        path: PathBuf,
    },
    Writing {
        inner: <T as WriteToAsync<'f, TargetVfs>>::Future,
    },
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'f, T, SelfVfs: VfsAsync + 'f, TargetVfs: WriteSupportingVfsAsync + 'f> Future
    for DeferredReadWriteRefFuture<'f, T, SelfVfs, TargetVfs>
where
    T: for<'a> ReadFromAsync<'a, SelfVfs> + for<'a> WriteToAsync<'a, TargetVfs> + Send + 'static,
    for<'a> <T as ReadFromAsync<'a, SelfVfs>>::Future: Future<Output = Result<T>> + Unpin + 'a,
    for<'a> <T as WriteToAsync<'a, TargetVfs>>::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poisson);
        match this {
            DeferredReadWriteRefFutureProj::SamePath => Poll::Ready(Ok(())),
            DeferredReadWriteRefFutureProj::Reading {
                mut inner,
                self_vfs,
                target_vfs,
                path,
            } => match Pin::new(&mut inner).poll(cx) {
                Poll::Ready(Ok(v)) => {
                    let write_fut = v.write_to_async(path, target_vfs);
                    self.as_mut()
                        .project_replace(Self::Writing { inner: write_fut });
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                Poll::Pending => {
                    self.project_replace(Self::Reading {
                        inner,
                        self_vfs,
                        target_vfs,
                        path,
                    });
                    Poll::Pending
                }
            },
            DeferredReadWriteRefFutureProj::Writing { mut inner } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(r) => Poll::Ready(r),
                    Poll::Pending => {
                        self.as_mut().project_replace(Self::Writing { inner });
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                }
            }
            DeferredReadWriteRefFutureProj::Poisson => {
                panic!(
                    "DeferredReadWriteRefFuture is in an invalid state. This is a bug in the code."
                );
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<
    'f,
    const CHECK_ON_READ: bool,
    T,
    SelfVfs: VfsAsync + 'f,
    TargetVfs: WriteSupportingVfsAsync + 'f,
> WriteToAsyncRef<'f, TargetVfs> for DeferredRead<'f, T, SelfVfs, CHECK_ON_READ>
where
    for<'a> T: ReadFromAsync<'a, SelfVfs> + WriteToAsync<'a, TargetVfs> + Send + 'a,
    for<'a> <T as ReadFromAsync<'a, SelfVfs>>::Future: Future<Output = Result<T>> + Unpin + 'a,
    for<'a> <T as WriteToAsync<'a, TargetVfs>>::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    type Future<'a>
        = DeferredReadWriteFuture<'a, T, SelfVfs, TargetVfs>
    where
        Self: 'a,
        'f: 'a;

    fn write_to_async_ref<'a>(
        self: &'a Self,
        path: PathBuf,
        vfs: Pin<&'a TargetVfs>,
    ) -> Self::Future<'a>
    where
        'f: 'a,
    {
        if path == self.0 {
            // Optimization: We were asked to write to the same path
            // we are supposed to read from. We can just ignore it, since
            // the file / directory should already be in the given state.

            // If `T` has trivial `ReadFromAsync` / `WriteToAsyncRef` implementations,
            // this should not be a problem, but if it is, a custom `DeferredRead`
            // implementation should be written for it.
            return DeferredReadWriteFuture::SamePath;
        }

        DeferredReadWriteFuture::Reading {
            inner: T::read_from_async(self.0.clone(), self.1),
            path,
            self_vfs: self.1,
            target_vfs: vfs,
        }
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<const CHECK_ON_READ: bool, const NAME: [char; HAS_FIELD_MAX_LEN], T, Vfs> HasField<NAME>
    for DeferredRead<'_, T, Vfs, CHECK_ON_READ>
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
impl<const CHECK_ON_READ: bool, T, Vfs> DynamicHasField for DeferredRead<'_, T, Vfs, CHECK_ON_READ>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        T::resolve_path(p, name)
    }
}
