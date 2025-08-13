use std::path::Path;
#[cfg(any(feature = "resolve-path", feature = "async"))]
use std::path::PathBuf;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::DeferredRead;
#[cfg(feature = "resolve-path")]
use crate::DynamicHasField;
#[cfg(feature = "resolve-path")]
use crate::HAS_FIELD_MAX_LEN;
#[cfg(feature = "resolve-path")]
use crate::HasField;
use crate::Result;
use crate::prelude::*;

/// A wrapper that defers the reading of a file until it is actually needed,
/// but can also store the value.
///
/// It allows us to read the value from disk, and then store it in memory,
/// and if we ever need it again, we can just return the stored value.
///
/// This type exposes 2 functions: [`DeferredReadOrOwn::get`] and
/// [`DeferredReadOrOwn::perform_and_store_read`].
///
/// The table below summarizes the differences between the two functions:
///
/// | State             | [`DeferredReadOrOwn::get`]               | [`DeferredReadOrOwn::perform_and_store_read`]     |
/// |-------------------|------------------------------------------|---------------------------------------------------|
/// | New, not cached   | Reads the value, does not cache          | Reads the value, and caches it, returns reference |
/// | Cached            | Clones the cached value                  | Returns a reference to the cached value           |
///
/// As such, [`DeferredReadOrOwn::get`] has the signature of `fn(&self) -> Result<T>` and
/// [`DeferredReadOrOwn::perform_and_store_read`] has the signature of `fn(&mut self) -> Result<&mut T>`.
///
/// If you never call [`DeferredReadOrOwn::perform_and_store_read`], and only ever call [`DeferredReadOrOwn::get`],
/// that would effectively be the same as using a [`DeferredRead`], and that should be preferred instead.
#[derive(Debug, Clone, Hash)]
pub enum DeferredReadOrOwn<'a, T, Vfs = crate::FsVfs> {
    Own(T),
    Deferred(DeferredRead<'a, T, Vfs>),
}

impl<'a, T, Vfs: crate::Vfs> DeferredReadOrOwn<'a, T, Vfs>
where
    T: ReadFrom<'a, Vfs>,
{
    /// Gets the value. If it is not already read, it will read it, but without saving it.
    ///
    /// This is useful if you want to read the value, but you don't want to store it.
    ///
    /// Though never calling [`DeferredReadOrOwn::perform_and_store_read`] and only calling
    /// [`DeferredReadOrOwn::get`] is equivalent to using a [`DeferredRead`], and that should be preferred.
    ///
    /// See [`DeferredReadOrOwn`] for more details.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::Path;
    /// use std::pin::Pin;
    /// use dir_structure::DirStructureItem;
    /// use dir_structure::DeferredRead;
    /// use dir_structure::DeferredReadOrOwn;
    /// use dir_structure::ReadFrom;
    /// use dir_structure::FsVfs;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let d = Path::new("dir");
    ///     std::fs::create_dir_all(&d)?;
    ///     let deferred = DeferredReadOrOwn::<String, FsVfs>::Deferred(
    ///         DeferredRead::read_from(&d.join("f.txt"), Pin::new(&FsVfs)).unwrap()
    ///     );
    ///     assert!(deferred.get().is_err());
    ///     std::fs::write(d.join("f.txt"), "Hello, world!")?;
    ///     assert_eq!(deferred.get()?, "Hello, world!");
    ///     std::fs::write(d.join("f.txt"), "Goodbye, world!")?;
    ///     assert_eq!(deferred.get()?, "Goodbye, world!");
    ///     # std::fs::remove_dir_all(&d)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn get(&self) -> Result<T>
    where
        T: Clone,
    {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own.clone()),
            DeferredReadOrOwn::Deferred(d) => Ok(d.perform_read()?),
        }
    }

    /// Performs the read and stores the value. If the value is already read, it will
    /// just return a reference to it.
    ///
    /// See [`DeferredReadOrOwn`] for more details.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::path::Path;
    /// use std::pin::Pin;
    /// use dir_structure::DirStructureItem;
    /// use dir_structure::DeferredRead;
    /// use dir_structure::DeferredReadOrOwn;
    /// use dir_structure::ReadFrom;
    /// use dir_structure::FsVfs;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let d = Path::new("dir");
    ///     std::fs::create_dir_all(&d)?;
    ///     let mut deferred = DeferredReadOrOwn::<String, FsVfs>::Deferred(
    ///         DeferredRead::read_from(&d.join("f.txt"), Pin::new(&FsVfs)).unwrap()
    ///     );
    ///     assert!(deferred.perform_and_store_read().is_err());
    ///     std::fs::write(d.join("f.txt"), "Hello, world!")?;
    ///     assert_eq!(deferred.perform_and_store_read()?, "Hello, world!");
    ///     std::fs::write(d.join("f.txt"), "Goodbye, world!")?;
    ///     assert_eq!(deferred.perform_and_store_read()?, "Hello, world!");
    ///     # std::fs::remove_dir_all(&d)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn perform_and_store_read(&mut self) -> Result<&mut T> {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own),
            DeferredReadOrOwn::Deferred(d) => {
                let value = d.perform_read()?;
                *self = DeferredReadOrOwn::Own(value);
                let DeferredReadOrOwn::Own(own) = self else {
                    unreachable!()
                };
                Ok(own)
            }
        }
    }
}

impl<'a, T, Vfs: crate::Vfs> ReadFrom<'a, Vfs> for DeferredReadOrOwn<'a, T, Vfs>
where
    T: ReadFrom<'a, Vfs>,
{
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        ReadFrom::read_from(path, vfs).map(Self::Deferred)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'a> DeferredReadOrOwn<'a, T, Vfs>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    pub async fn get_async(&'a self) -> Result<T>
    where
        T: Clone,
    {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own.clone()),
            DeferredReadOrOwn::Deferred(d) => d.perform_read_async().await,
        }
    }

    pub async fn perform_and_store_read_async(&'a mut self) -> Result<&'a mut T> {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own),
            DeferredReadOrOwn::Deferred(d) => {
                let value = d.perform_read_async().await?;
                *self = DeferredReadOrOwn::Own(value);
                let DeferredReadOrOwn::Own(own) = self else {
                    unreachable!()
                };
                Ok(own)
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'a> ReadFromAsync<'a, Vfs> for DeferredReadOrOwn<'a, T, Vfs>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'a>>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move {
            DeferredRead::read_from_async(path, vfs)
                .await
                .map(Self::Deferred)
        })
    }
}

impl<'a, T, Vfs: crate::Vfs> WriteTo<Vfs> for DeferredReadOrOwn<'a, T, Vfs>
where
    T: ReadFrom<'a, Vfs> + WriteTo<Vfs>,
{
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        match self {
            DeferredReadOrOwn::Own(own) => own.write_to(path, vfs),
            DeferredReadOrOwn::Deferred(d) => d.write_to(path, vfs),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DeferredReadOrOwnWriteFutureProj)]
pub enum DeferredReadOrOwnWriteFuture<'a, T, Vfs: crate::VfsAsync + 'static>
where
    T: for<'b> ReadFromAsync<'b, Vfs>
        + WriteToAsync<Vfs>
        + for<'b> WriteToAsyncOwned<'b, Vfs>
        + Send
        + 'static,
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = Result<T>> + Unpin + 'b,
    <T as WriteToAsync<Vfs>>::Future<'a>: Future<Output = Result<()>> + Unpin + 'a,
    for<'b> <T as WriteToAsyncOwned<'b, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'b,
{
    Poisson,
    Own {
        inner: <T as WriteToAsync<Vfs>>::Future<'a>,
    },
    OwnOwned {
        inner: <T as WriteToAsyncOwned<'a, Vfs>>::Future,
    },
    Deferred {
        inner: <DeferredRead<'a, T, Vfs> as WriteToAsync<Vfs>>::Future<'a>,
    },
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'a> Future for DeferredReadOrOwnWriteFuture<'a, T, Vfs>
where
    T: for<'b> ReadFromAsync<'b, Vfs>
        + WriteToAsync<Vfs>
        + for<'b> WriteToAsyncOwned<'b, Vfs>
        + Send
        + 'static,
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = Result<T>> + Unpin + 'b,
    <T as WriteToAsync<Vfs>>::Future<'a>: Future<Output = Result<()>> + Unpin + 'a,
    for<'b> <T as WriteToAsyncOwned<'b, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'b,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poisson);
        match this {
            DeferredReadOrOwnWriteFutureProj::Own { mut inner } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(v) => Poll::Ready(v),
                    Poll::Pending => Poll::Pending,
                }
            }
            DeferredReadOrOwnWriteFutureProj::OwnOwned { mut inner } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(v) => Poll::Ready(v),
                    Poll::Pending => Poll::Pending,
                }
            }
            DeferredReadOrOwnWriteFutureProj::Deferred { mut inner } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(v) => Poll::Ready(v),
                    Poll::Pending => Poll::Pending,
                }
            }
            DeferredReadOrOwnWriteFutureProj::Poisson => {
                panic!(
                    "DeferredReadOrOwnWriteFuture is in an invalid state. This is a bug in the code."
                );
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for DeferredReadOrOwn<'a, T, Vfs>
where
    T: for<'b> ReadFromAsync<'b, Vfs>
        + WriteToAsync<Vfs>
        + for<'b> WriteToAsyncOwned<'b, Vfs>
        + Send
        + 'static,
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = Result<T>> + Unpin + 'b,
    for<'b> <T as WriteToAsync<Vfs>>::Future<'b>: Future<Output = Result<()>> + Unpin + 'b,
    for<'b> <T as WriteToAsyncOwned<'b, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'b,
{
    type Future<'b>
        = DeferredReadOrOwnWriteFuture<'b, T, Vfs>
    where
        Self: 'b,
        Vfs: 'b;

    fn write_to_async<'b>(&'b self, path: PathBuf, vfs: Pin<&'b Vfs>) -> Self::Future<'b> {
        match self {
            DeferredReadOrOwn::Own(own) => DeferredReadOrOwnWriteFuture::Own {
                inner: own.write_to_async(path, vfs),
            },
            DeferredReadOrOwn::Deferred(d) => DeferredReadOrOwnWriteFuture::Deferred {
                inner: d.write_to_async(path, vfs),
            },
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'static> WriteToAsyncOwned<'a, Vfs>
    for DeferredReadOrOwn<'a, T, Vfs>
where
    T: for<'b> ReadFromAsync<'b, Vfs>
        + WriteToAsync<Vfs>
        + for<'b> WriteToAsyncOwned<'b, Vfs>
        + Send
        + 'static,
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = Result<T>> + Unpin + 'b,
    for<'b> <T as WriteToAsync<Vfs>>::Future<'b>: Future<Output = Result<()>> + Unpin + 'b,
    for<'b> <T as WriteToAsyncOwned<'b, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'b,
{
    type Future
        = DeferredReadOrOwnWriteFuture<'a, T, Vfs>
    where
        Self: 'a,
        Vfs: 'a;

    fn write_to_async_owned(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        match self {
            DeferredReadOrOwn::Own(own) => DeferredReadOrOwnWriteFuture::OwnOwned {
                inner: own.write_to_async_owned(path, vfs),
            },
            DeferredReadOrOwn::Deferred(d) => DeferredReadOrOwnWriteFuture::Deferred {
                inner: d.write_to_async_owned(path, vfs),
            },
        }
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<'a, const NAME: [char; HAS_FIELD_MAX_LEN], T, Vis> HasField<NAME>
    for DeferredReadOrOwn<'a, T, Vis>
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
impl<'a, T, Vis> DynamicHasField for DeferredReadOrOwn<'a, T, Vis>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        T::resolve_path(p, name)
    }
}
