#[cfg(feature = "async")]
use std::future;
use std::marker;
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
pub struct DeferredRead<T>(pub PathBuf, marker::PhantomData<T>);

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

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T> ReadFromAsync for DeferredRead<T>
where
    T: Send + 'static,
{
    type Future = future::Ready<Result<Self>>;

    fn read_from_async(path: PathBuf) -> Self::Future {
        future::ready(Ok(Self(path, marker::PhantomData)))
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
    /// For a cached version see [`DeferredReadOrOwn`][crate::DeferredReadOrOwn].
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

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T> DeferredRead<T>
where
    T: ReadFromAsync + Send + 'static,
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
        T::read_from_async(self.0.clone()).await
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

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DeferredReadWriteFutureProj)]
pub enum DeferredReadWriteFuture<'a, T>
where
    T: ReadFromAsync + WriteToAsyncOwned<'a> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    <T as WriteToAsyncOwned<'a>>::Future: Future<Output = Result<()>> + Unpin,
{
    Poisson,
    SamePath,
    Reading {
        inner: <T as ReadFromAsync>::Future,
        path: PathBuf,
    },
    Writing {
        inner: <T as WriteToAsyncOwned<'a>>::Future,
    },
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> Future for DeferredReadWriteFuture<'a, T>
where
    T: ReadFromAsync + WriteToAsyncOwned<'a> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    <T as WriteToAsyncOwned<'a>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poisson);
        match this {
            DeferredReadWriteFutureProj::SamePath => Poll::Ready(Ok(())),
            DeferredReadWriteFutureProj::Reading { mut inner, path } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(Ok(v)) => {
                        self.project_replace(Self::Writing {
                            inner: v.write_to_async_owned(path),
                        });
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::Reading { inner, path });
                        Poll::Pending
                    }
                }
            }
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
impl<T> WriteToAsync for DeferredRead<T>
where
    T: ReadFromAsync + for<'a> WriteToAsyncOwned<'a> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    for<'a> <T as WriteToAsyncOwned<'a>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Future<'a>
        = DeferredReadWriteFuture<'a, T>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
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
            inner: T::read_from_async(self.0.clone()),
            path,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> WriteToAsyncOwned<'a> for DeferredRead<T>
where
    T: ReadFromAsync + for<'b> WriteToAsyncOwned<'b> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    for<'b> <T as WriteToAsyncOwned<'b>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Future = DeferredReadWriteFuture<'a, T>;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
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
            inner: T::read_from_async(self.0),
            path,
        }
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<const NAME: [char; HAS_FIELD_MAX_LEN], T> HasField<NAME> for DeferredRead<T>
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
impl<T> DynamicHasField for DeferredRead<T>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        T::resolve_path(p, name)
    }
}
