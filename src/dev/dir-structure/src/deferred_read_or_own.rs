//! A wrapper that defers the reading of a file until it is actually needed.
//!
//! See [`DeferredReadOrOwn`] for more details.

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

use crate::deferred_read::DeferredRead;
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
#[derive(Clone, Hash)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub enum DeferredReadOrOwn<'a, T, Vfs = fs_vfs::FsVfs, const CHECK_ON_READ: bool = false> {
    /// An owned value.
    Own(T),
    /// A deferred read.
    Deferred(DeferredRead<'a, T, Vfs, CHECK_ON_READ>),
}

impl<'a, const CHECK_ON_READ: bool, T, Vfs> std::fmt::Debug
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeferredReadOrOwn::Own(own) => f.debug_tuple("Own").field(own).finish(),
            DeferredReadOrOwn::Deferred(d) => f.debug_tuple("Deferred").field(d).finish(),
        }
    }
}

impl<'a, const CHECK_ON_READ: bool, T, Vfs: vfs::Vfs> DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
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
    /// use dir_structure::traits::sync::DirStructureItem;
    /// use dir_structure::deferred_read::DeferredRead;
    /// use dir_structure::deferred_read_or_own::DeferredReadOrOwn;
    /// use dir_structure::prelude::*;
    /// use dir_structure::vfs::fs_vfs::FsVfs;
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
    /// use dir_structure::traits::sync::DirStructureItem;
    /// use dir_structure::deferred_read::DeferredRead;
    /// use dir_structure::deferred_read_or_own::DeferredReadOrOwn;
    /// use dir_structure::prelude::*;
    /// use dir_structure::vfs::fs_vfs::FsVfs;
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

    /// Flushes the current value to the specified path.
    pub fn flush_to<TargetVfs: vfs::WriteSupportingVfs>(
        &self,
        path: &Path,
        vfs: Pin<&TargetVfs>,
    ) -> Result<()>
    where
        T: WriteTo<TargetVfs>,
    {
        match self {
            DeferredReadOrOwn::Own(own) => own.write_to(path, vfs),
            DeferredReadOrOwn::Deferred(d) => d.write_to(path, vfs),
        }
    }
}

impl<'a, const CHECK_ON_READ: bool, T, Vfs: vfs::Vfs> ReadFrom<'a, Vfs>
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
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
impl<'a, const CHECK_ON_READ: bool, T, Vfs: VfsAsync + 'a>
    DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    /// Gets the value, asynchronously. This is an async version of [`get`](Self::get).
    pub async fn get_async(&'a self) -> Result<T>
    where
        T: Clone,
    {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own.clone()),
            DeferredReadOrOwn::Deferred(d) => d.perform_read_async().await,
        }
    }

    /// Performs the read and stores the value. If the value is already read, it will
    /// just return a reference to it.
    ///
    /// See [`DeferredReadOrOwn`] for more details.
    ///
    /// This is an async version of [`perform_and_store_read`](Self::perform_and_store_read).
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
impl<'a, const CHECK_ON_READ: bool, T, Vfs: VfsAsync + 'a>
    DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
{
    /// Flushes the current value to the specified path. Async version of [`flush_to`](Self::flush_to).
    pub async fn flush_to_async<TargetVfs: WriteSupportingVfsAsync + 'a, ReadFutTy>(
        &'a self,
        path: PathBuf,
        vfs: Pin<&'a TargetVfs>,
    ) -> Result<()>
    where
        for<'b> T: ReadFromAsync<'b, Vfs, Future = ReadFutTy>
            + WriteToAsync<'b, TargetVfs>
            + WriteToAsyncRef<'b, TargetVfs>
            + Send
            + 'b,
        ReadFutTy: Future<Output = Result<T>> + Unpin + 'static,
    {
        match self {
            DeferredReadOrOwn::Own(own) => own.write_to_async_ref(path, vfs).await,
            DeferredReadOrOwn::Deferred(d) => d.write_to_async_ref(path, vfs).await,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, const CHECK_ON_READ: bool, T, Vfs: VfsAsync + 'static> ReadFromAsync<'a, Vfs>
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'a>>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        use std::future::poll_fn;

        let mut fut = Box::pin(DeferredRead::<T, Vfs, CHECK_ON_READ>::read_from_async(
            path, vfs,
        ));

        Box::pin(poll_fn(move |cx| {
            fut.as_mut().poll(cx).map_ok(Self::Deferred)
        }))
    }
}

impl<'a, const CHECK_ON_READ: bool, T, SelfVfs: vfs::Vfs, TargetVfs: vfs::WriteSupportingVfs>
    WriteTo<TargetVfs> for DeferredReadOrOwn<'a, T, SelfVfs, CHECK_ON_READ>
where
    T: ReadFrom<'a, SelfVfs> + WriteTo<TargetVfs>,
{
    fn write_to(&self, path: &Path, vfs: Pin<&TargetVfs>) -> Result<()> {
        match self {
            DeferredReadOrOwn::Own(own) => own.write_to(path, vfs),
            DeferredReadOrOwn::Deferred(d) => d.write_to(path, vfs),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = DeferredReadOrOwnWriteFutureProj)]
#[doc(hidden)]
pub enum DeferredReadOrOwnWriteFuture<
    'a,
    T,
    Vfs: WriteSupportingVfsAsync + 'static,
    const CHECK_ON_READ: bool,
> where
    T: for<'b> ReadFromAsync<'b, Vfs> + for<'b> WriteToAsync<'b, Vfs> + Send + 'static,
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = Result<T>> + Unpin + 'b,
    for<'b> <T as WriteToAsync<'b, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'b,
{
    Poisson,
    Own {
        inner: <T as WriteToAsync<'a, Vfs>>::Future,
    },
    Deferred {
        inner: <DeferredRead<'a, T, Vfs, CHECK_ON_READ> as WriteToAsync<'a, Vfs>>::Future,
    },
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, const CHECK_ON_READ: bool, T, Vfs: WriteSupportingVfsAsync + 'a> Future
    for DeferredReadOrOwnWriteFuture<'a, T, Vfs, CHECK_ON_READ>
where
    T: for<'b> ReadFromAsync<'b, Vfs> + for<'b> WriteToAsync<'b, Vfs> + Send + 'static,
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = Result<T>> + Unpin + 'b,
    for<'b> <T as WriteToAsync<'b, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'b,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poisson);
        match this {
            DeferredReadOrOwnWriteFutureProj::Own { mut inner } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(v) => Poll::Ready(v),
                    Poll::Pending => {
                        self.project_replace(Self::Own { inner });
                        Poll::Pending
                    }
                }
            }
            DeferredReadOrOwnWriteFutureProj::Deferred { mut inner } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(v) => Poll::Ready(v),
                    Poll::Pending => {
                        self.project_replace(Self::Deferred { inner });
                        Poll::Pending
                    }
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
impl<'a, const CHECK_ON_READ: bool, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs>
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: for<'b> ReadFromAsync<'b, Vfs> + for<'b> WriteToAsync<'b, Vfs> + Send + 'static,
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = Result<T>> + Unpin + 'b,
    for<'b> <T as WriteToAsync<'b, Vfs>>::Future: Future<Output = Result<()>> + Unpin + 'b,
{
    type Future = DeferredReadOrOwnWriteFuture<'a, T, Vfs, CHECK_ON_READ>;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
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

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<'a, const CHECK_ON_READ: bool, const NAME: [char; HAS_FIELD_MAX_LEN], T, Vis> HasField<NAME>
    for DeferredReadOrOwn<'a, T, Vis, CHECK_ON_READ>
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
impl<'a, const CHECK_ON_READ: bool, T, Vis> DynamicHasField
    for DeferredReadOrOwn<'a, T, Vis, CHECK_ON_READ>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        T::resolve_path(p, name)
    }
}
