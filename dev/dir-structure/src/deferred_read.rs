#[cfg(feature = "async")]
use std::future;
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

#[cfg(feature = "resolve-path")]
use crate::DynamicHasField;
#[cfg(feature = "resolve-path")]
use crate::HAS_FIELD_MAX_LEN;
#[cfg(feature = "resolve-path")]
use crate::HasField;
use crate::Result;
use crate::prelude::*;

/// A wrapper that defers the reading of a file until it is actually needed.
///
/// The only thing you can do with a [`DeferredRead`] is to call [`DeferredRead::perform_read`],
/// which will read the file and return the value.
///
/// See the [`DeferredRead::perform_read`] method for more details.
#[derive(Debug, Clone, Hash)]
pub struct DeferredRead<'a, T, Vfs = crate::FsVfs>(
    pub PathBuf,
    Pin<&'a Vfs>,
    marker::PhantomData<T>,
);

impl<'a, T, Vfs: crate::Vfs> ReadFrom<'a, Vfs> for DeferredRead<'a, T, Vfs>
where
    T: ReadFrom<'a, Vfs>,
{
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(path.to_path_buf(), vfs, marker::PhantomData))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync> ReadFromAsync<'a, Vfs> for DeferredRead<'a, T, Vfs>
where
    T: Send + ReadFromAsync<'a, Vfs> + 'static,
{
    type Future
        = future::Ready<Result<Self>>
    where
        Self: 'a;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        future::ready(Ok(Self(path, vfs, marker::PhantomData)))
    }
}

impl<'a, T, Vfs: crate::Vfs> DeferredRead<'a, T, Vfs>
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
    /// use dir_structure::DirStructureItem;
    /// use dir_structure::DeferredRead;
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
impl<'a, T, Vfs: crate::VfsAsync> DeferredRead<'a, T, Vfs>
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

impl<'a, T, Vfs: crate::Vfs> WriteTo<Vfs> for DeferredRead<'a, T, Vfs>
where
    T: ReadFrom<'a, Vfs> + WriteTo<Vfs>,
{
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
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
pub enum DeferredReadWriteFuture<'a, T, Vfs: crate::VfsAsync + 'a>
where
    T: ReadFromAsync<'a, Vfs> + WriteToAsync<'a, Vfs> + Send + 'static,
    <T as ReadFromAsync<'a, Vfs>>::Future: Future<Output = Result<T>> + Unpin + 'a,
    <T as WriteToAsync<'a, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    Poisson,
    SamePath,
    Reading {
        vfs: Pin<&'a Vfs>,
        inner: <T as ReadFromAsync<'a, Vfs>>::Future,
        path: PathBuf,
    },
    Writing {
        inner: <T as WriteToAsync<'a, Vfs>>::Future,
    },
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'a> Future for DeferredReadWriteFuture<'a, T, Vfs>
where
    T: ReadFromAsync<'a, Vfs> + WriteToAsync<'a, Vfs> + Send + 'static,
    <T as ReadFromAsync<'a, Vfs>>::Future: Future<Output = Result<T>> + Unpin,
    <T as WriteToAsync<'a, Vfs>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poisson);
        match this {
            DeferredReadWriteFutureProj::SamePath => Poll::Ready(Ok(())),
            DeferredReadWriteFutureProj::Reading {
                mut inner,
                vfs,
                path,
            } => match Pin::new(&mut inner).poll(cx) {
                Poll::Ready(Ok(v)) => {
                    self.project_replace(Self::Writing {
                        inner: v.write_to_async(path, vfs),
                    });
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                Poll::Pending => {
                    self.project_replace(Self::Reading { inner, path, vfs });
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
impl<'f, T, Vfs: crate::VfsAsync + 'f> WriteToAsync<'f, Vfs> for DeferredRead<'f, T, Vfs>
where
    T: for<'a> ReadFromAsync<'a, Vfs> + for<'a> WriteToAsync<'a, Vfs> + Send + 'static,
    for<'a> <T as ReadFromAsync<'a, Vfs>>::Future: Future<Output = Result<T>> + Unpin + 'a,
    for<'a> <T as WriteToAsync<'a, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'a,
{
    type Future = DeferredReadWriteFuture<'f, T, Vfs>;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'f Vfs>) -> Self::Future {
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
            inner: T::read_from_async(self.0.clone(), vfs),
            path,
            vfs,
        }
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<const NAME: [char; HAS_FIELD_MAX_LEN], T, Vfs> HasField<NAME> for DeferredRead<'_, T, Vfs>
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
impl<T, Vfs> DynamicHasField for DeferredRead<'_, T, Vfs>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        T::resolve_path(p, name)
    }
}
