//! A wrapper that defers the reading of a file until it is actually needed.
//!
//! See [`DeferredReadOrOwn`] for more details.

use std::fmt;
use std::hash;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::deferred_read::DeferredRead;
use crate::error::Result;
use crate::error::VfsResult;
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
#[derive(Clone)]
pub enum DeferredReadOrOwn<'a, T, Vfs: VfsCore = fs_vfs::FsVfs, const CHECK_ON_READ: bool = false> {
    /// An owned value.
    Own(T),
    /// A deferred read.
    Deferred(DeferredRead<'a, T, Vfs, CHECK_ON_READ>),
}

impl<'a, T, Vfs: VfsCore, const CHECK_ON_READ: bool> hash::Hash
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: hash::Hash,
    <Vfs::Path as PathType>::OwnedPath: hash::Hash,
{
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            DeferredReadOrOwn::Own(own) => {
                state.write_u8(0);
                own.hash(state);
            }
            DeferredReadOrOwn::Deferred(d) => {
                state.write_u8(1);
                d.hash(state);
            }
        }
    }
}

#[cfg(feature = "assert_eq")]
impl<'a, T, Vfs: VfsCore<Path = P>, P, const CHECK_ON_READ: bool> assert_eq::AssertEq
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: assert_eq::AssertEq,
    P: PathType + ?Sized,
    P::OwnedPath: assert_eq::AssertEq + fmt::Debug,
{
    fn assert_eq(
        &self,
        other: &Self,
        path: &mut assert_eq::AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        match (self, other) {
            (Self::Own(l), Self::Own(r)) => {
                T::assert_eq(l, r, &mut *path.__guard("[Own]"), init_left, init_right);
            }
            (Self::Deferred(l), Self::Deferred(r)) => {
                DeferredRead::<T, Vfs, CHECK_ON_READ>::assert_eq(
                    l,
                    r,
                    &mut *path.__guard("[Deferred]"),
                    init_left,
                    init_right,
                );
            }
            (Self::Own(_), Self::Deferred(_)) => {
                panic!("Left is Own, right is Deferred: at: {:?}", path);
            }
            (Self::Deferred(_), Self::Own(_)) => {
                panic!("Left is Deferred, right is Own: at: {:?}", path);
            }
        }
    }
}

impl<'a, const CHECK_ON_READ: bool, T, Vfs: VfsCore<Path = P>, P> fmt::Debug
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: fmt::Debug,
    P: PathType + ?Sized,
    P::OwnedPath: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DeferredReadOrOwn::Own(own) => f.debug_tuple("Own").field(own).finish(),
            DeferredReadOrOwn::Deferred(d) => f.debug_tuple("Deferred").field(d).finish(),
        }
    }
}

impl<'a, const CHECK_ON_READ: bool, T, Vfs: vfs::Vfs<'a, Path = P>, P: PathType + ?Sized + 'a>
    DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
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
    ///         DeferredRead::read_from(d.join("f.txt").as_ref(), Pin::new(&FsVfs)).unwrap()
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
    pub fn get(&self) -> VfsResult<T, Vfs>
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
    ///         DeferredRead::read_from(d.join("f.txt").as_ref(), Pin::new(&FsVfs)).unwrap()
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
    pub fn perform_and_store_read(&mut self) -> VfsResult<&mut T, Vfs> {
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
    pub fn flush_to<'t, TargetVfs: vfs::WriteSupportingVfs<'t, Path = P>>(
        &self,
        path: &P,
        vfs: Pin<&'t TargetVfs>,
    ) -> VfsResult<(), Vfs>
    where
        T: WriteTo<'t, TargetVfs>,
    {
        match self {
            DeferredReadOrOwn::Own(own) => own.write_to(path, vfs),
            DeferredReadOrOwn::Deferred(d) => d.write_to(path, vfs),
        }
    }
}

impl<'a, const CHECK_ON_READ: bool, T, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs>
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: ReadFrom<'a, Vfs>,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        ReadFrom::read_from(path, vfs).map(Self::Deferred)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, const CHECK_ON_READ: bool, T, Vfs: VfsAsync<Path = P> + 'a, P: PathType + ?Sized + 'a>
    DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    /// Gets the value, asynchronously. This is an async version of [`get`](Self::get).
    pub async fn get_async(&'a self) -> VfsResult<T, Vfs>
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
    pub async fn perform_and_store_read_async(&'a mut self) -> VfsResult<&'a mut T, Vfs> {
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
impl<'a, const CHECK_ON_READ: bool, T, Vfs: VfsAsync<Path = P> + 'a, P: PathType + ?Sized + 'a>
    DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    P::OwnedPath: 'a,
{
    /// Flushes the current value to the specified path. Async version of [`flush_to`](Self::flush_to).
    pub async fn flush_to_async<TargetVfs: WriteSupportingVfsAsync<Path = P> + 'a, ReadFutTy>(
        &'a self,
        path: P::OwnedPath,
        vfs: Pin<&'a TargetVfs>,
    ) -> VfsResult<(), Vfs>
    where
        for<'b> T: ReadFromAsync<'b, Vfs, Future = ReadFutTy>
            + WriteToAsync<'b, TargetVfs>
            + WriteToAsyncRef<'b, TargetVfs>
            + Send
            + 'b,
        ReadFutTy: Future<Output = VfsResult<T, Vfs>> + Unpin + 'static,
    {
        match self {
            DeferredReadOrOwn::Own(own) => own.write_to_async_ref(path, vfs).await,
            DeferredReadOrOwn::Deferred(d) => d.write_to_async_ref(path, vfs).await,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, const CHECK_ON_READ: bool, T, Vfs: VfsAsync<Path = P> + 'static, P: PathType + ?Sized + 'a>
    ReadFromAsync<'a, Vfs> for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
    P::OwnedPath: Send + Sync,
{
    type Future = Pin<Box<dyn Future<Output = VfsResult<Self, Vfs>> + Send + 'a>>;

    fn read_from_async(path: P::OwnedPath, vfs: Pin<&'a Vfs>) -> Self::Future {
        use std::future::poll_fn;

        let mut fut = DeferredRead::<T, Vfs, CHECK_ON_READ>::read_from_async(path, vfs);

        Box::pin(poll_fn(move |cx| {
            fut.as_mut().poll(cx).map_ok(Self::Deferred)
        }))
    }
}

impl<
    'a,
    't,
    const CHECK_ON_READ: bool,
    T,
    P: PathType + ?Sized + 'a,
    SelfVfs: vfs::Vfs<'a, Path = P>,
    TargetVfs: vfs::WriteSupportingVfs<'t, Path = P>,
> WriteTo<'t, TargetVfs> for DeferredReadOrOwn<'a, T, SelfVfs, CHECK_ON_READ>
where
    T: ReadFrom<'a, SelfVfs> + WriteTo<'t, TargetVfs>,
{
    fn write_to(&self, path: &P, vfs: Pin<&'t TargetVfs>) -> Result<(), P::OwnedPath> {
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
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = VfsResult<T, Vfs>> + Unpin + 'b,
    for<'b> <T as WriteToAsync<'b, Vfs>>::Future: Future<Output = VfsResult<(), Vfs>> + Unpin + 'b,
{
    Poisson,
    Own {
        inner: <T as WriteToAsync<'a, Vfs>>::Future,
    },
    Deferred {
        inner: <DeferredRead<'a, T, Vfs, CHECK_ON_READ> as WriteToAsync<'a, Vfs>>::Future,
    },
}

// needed to avoid ICE in rustdoc, see https://github.com/rust-lang/rust/issues/144918
#[cfg(all(feature = "async", doc))]
impl<T, Vfs, const CHECK_ON_READ: bool> core::marker::Unpin
    for DeferredReadOrOwnWriteFutureProj<'_, T, Vfs, CHECK_ON_READ>
where
    T: for<'b> ReadFromAsync<'b, Vfs> + for<'b> WriteToAsync<'b, Vfs> + Send + 'static,
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = VfsResult<T, Vfs>> + Unpin + 'b,
    for<'b> <T as WriteToAsync<'b, Vfs>>::Future: Future<Output = VfsResult<(), Vfs>> + Unpin + 'b,
    Vfs: WriteSupportingVfsAsync + 'static,
{
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<
    'a,
    const CHECK_ON_READ: bool,
    T,
    P: PathType + ?Sized + 'a,
    Vfs: WriteSupportingVfsAsync<Path = P> + 'a,
> Future for DeferredReadOrOwnWriteFuture<'a, T, Vfs, CHECK_ON_READ>
where
    T: for<'b> ReadFromAsync<'b, Vfs> + for<'b> WriteToAsync<'b, Vfs> + Send + 'static,
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = VfsResult<T, Vfs>> + Unpin + 'b,
    for<'b> <T as WriteToAsync<'b, Vfs>>::Future: Future<Output = VfsResult<(), Vfs>> + Unpin + 'b,
{
    type Output = VfsResult<(), Vfs>;

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
    for<'b> <T as ReadFromAsync<'b, Vfs>>::Future: Future<Output = VfsResult<T, Vfs>> + Unpin + 'b,
    for<'b> <T as WriteToAsync<'b, Vfs>>::Future: Future<Output = VfsResult<(), Vfs>> + Unpin + 'b,
{
    type Future = DeferredReadOrOwnWriteFuture<'a, T, Vfs, CHECK_ON_READ>;

    fn write_to_async(
        self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
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
impl<'a, const CHECK_ON_READ: bool, const NAME: [char; HAS_FIELD_MAX_LEN], T, Vfs> HasField<NAME>
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: HasField<NAME>,
    Vfs: VfsCore,
{
    type Inner = <T as HasField<NAME>>::Inner;

    fn resolve_path<P: OwnedPathType>(p: P) -> P {
        T::resolve_path(p)
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<'a, const CHECK_ON_READ: bool, T, Vfs> DynamicHasField
    for DeferredReadOrOwn<'a, T, Vfs, CHECK_ON_READ>
where
    T: DynamicHasField,
    Vfs: VfsCore,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path<P: OwnedPathType>(p: P, name: &str) -> P {
        T::resolve_path(p, name)
    }
}
