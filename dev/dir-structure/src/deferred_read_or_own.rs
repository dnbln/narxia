#[cfg(feature = "async")]
use std::future;
use std::path::Path;
#[cfg(any(feature = "resolve-path", feature = "async"))]
use std::path::PathBuf;
#[cfg(feature = "async")]
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
pub enum DeferredReadOrOwn<T> {
    Own(T),
    Deferred(DeferredRead<T>),
}

impl<T> DeferredReadOrOwn<T>
where
    T: ReadFrom,
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
    /// use dir_structure::DirStructureItem;
    /// use dir_structure::DeferredRead;
    /// use dir_structure::DeferredReadOrOwn;
    /// use dir_structure::ReadFrom;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let d = Path::new("dir");
    ///     std::fs::create_dir_all(&d)?;
    ///     let deferred = DeferredReadOrOwn::<String>::Deferred(
    ///         DeferredRead::read_from(&d.join("f.txt")).unwrap()
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
    /// use dir_structure::DirStructureItem;
    /// use dir_structure::DeferredRead;
    /// use dir_structure::DeferredReadOrOwn;
    /// use dir_structure::ReadFrom;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let d = Path::new("dir");
    ///     std::fs::create_dir_all(&d)?;
    ///     let mut deferred = DeferredReadOrOwn::<String>::Deferred(
    ///         DeferredRead::read_from(&d.join("f.txt")).unwrap()
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

impl<T> ReadFrom for DeferredReadOrOwn<T>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        ReadFrom::read_from(path).map(Self::Deferred)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T> DeferredReadOrOwn<T>
where
    T: ReadFromAsync + Send + 'static,
{
    pub async fn get_async(&self) -> Result<T>
    where
        T: Clone,
    {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own.clone()),
            DeferredReadOrOwn::Deferred(d) => d.perform_read_async().await,
        }
    }

    pub async fn perform_and_store_read_async(&mut self) -> Result<&mut T> {
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
impl<T> ReadFromAsync for DeferredReadOrOwn<T>
where
    T: ReadFrom + Send + 'static,
{
    type Future = future::Ready<Result<Self>>;

    fn read_from_async(path: PathBuf) -> Self::Future {
        future::ready(DeferredRead::read_from(&path).map(Self::Deferred))
    }
}

impl<T> WriteTo for DeferredReadOrOwn<T>
where
    T: ReadFrom + WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        match self {
            DeferredReadOrOwn::Own(own) => own.write_to(path),
            DeferredReadOrOwn::Deferred(d) => d.write_to(path),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DeferredReadOrOwnWriteFutureProj)]
pub enum DeferredReadOrOwnWriteFuture<'a, T>
where
    T: ReadFromAsync + WriteToAsync + for<'b> WriteToAsyncOwned<'b> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    <T as WriteToAsync>::Future<'a>: Future<Output = Result<()>> + Unpin,
    for<'b> <T as WriteToAsyncOwned<'b>>::Future: Future<Output = Result<()>> + Unpin,
{
    Poisson,
    Own {
        inner: <T as WriteToAsync>::Future<'a>,
    },
    OwnOwned {
        inner: <T as WriteToAsyncOwned<'a>>::Future,
    },
    Deferred {
        inner: <DeferredRead<T> as WriteToAsync>::Future<'a>,
    },
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> Future for DeferredReadOrOwnWriteFuture<'a, T>
where
    T: ReadFromAsync + WriteToAsync + for<'b> WriteToAsyncOwned<'b> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    <T as WriteToAsync>::Future<'a>: Future<Output = Result<()>> + Unpin,
    for<'b> <T as WriteToAsyncOwned<'b>>::Future: Future<Output = Result<()>> + Unpin,
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
impl<T> WriteToAsync for DeferredReadOrOwn<T>
where
    T: ReadFromAsync + WriteToAsync + for<'a> WriteToAsyncOwned<'a> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    for<'a> <T as WriteToAsync>::Future<'a>: Future<Output = Result<()>> + Unpin,
    for<'a> <T as WriteToAsyncOwned<'a>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Future<'a>
        = DeferredReadOrOwnWriteFuture<'a, T>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        match self {
            DeferredReadOrOwn::Own(own) => DeferredReadOrOwnWriteFuture::Own {
                inner: own.write_to_async(path),
            },
            DeferredReadOrOwn::Deferred(d) => DeferredReadOrOwnWriteFuture::Deferred {
                inner: d.write_to_async(path),
            },
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> WriteToAsyncOwned<'a> for DeferredReadOrOwn<T>
where
    T: ReadFromAsync + WriteToAsync + for<'b> WriteToAsyncOwned<'b> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    for<'b> <T as WriteToAsync>::Future<'b>: Future<Output = Result<()>> + Unpin,
    for<'b> <T as WriteToAsyncOwned<'b>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Future = DeferredReadOrOwnWriteFuture<'a, T>;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        match self {
            DeferredReadOrOwn::Own(own) => DeferredReadOrOwnWriteFuture::OwnOwned {
                inner: own.write_to_async_owned(path),
            },
            DeferredReadOrOwn::Deferred(d) => DeferredReadOrOwnWriteFuture::Deferred {
                inner: d.write_to_async_owned(path),
            },
        }
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<const NAME: [char; HAS_FIELD_MAX_LEN], T> HasField<NAME> for DeferredReadOrOwn<T>
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
impl<T> DynamicHasField for DeferredReadOrOwn<T>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        T::resolve_path(p, name)
    }
}
