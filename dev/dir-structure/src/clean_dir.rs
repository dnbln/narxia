use std::marker;
use std::path::Path;
#[cfg(any(feature = "async", feature = "resolve-path"))]
use std::path::PathBuf;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::DirStructureItem;
#[cfg(feature = "resolve-path")]
use crate::DynamicHasField;
use crate::FromRefForWriter;
#[cfg(feature = "async")]
use crate::FromRefForWriterAsync;
#[cfg(feature = "resolve-path")]
use crate::HAS_FIELD_MAX_LEN;
#[cfg(feature = "resolve-path")]
use crate::HasField;
use crate::NewtypeToInner;
use crate::Result;
use crate::prelude::*;

/// A newtype that will clean the directory it is written to, before writing
/// the value.
///
/// This is useful when we want to write a directory structure, but we want
/// to make sure that the directory is clean before writing it, so that there
/// are no old files / directories left in it.
///
/// ```rust
/// use std::path::Path;
///
/// use dir_structure::DirStructureItem;
/// use dir_structure::CleanDir;
///
/// #[derive(dir_structure::DirStructure)]
/// struct Dir {
///    #[dir_structure(path = "f.txt")]
///    f: String,
/// }
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let d = Path::new("dir");
///     std::fs::create_dir_all(&d)?;
///     std::fs::write(d.join("f.txt"), "Hello, world!")?;
///     std::fs::write(d.join("f2.txt"), "Hello, world! (2)")?;
///     let dir = Dir::read(&d)?;
///     assert_eq!(dir.f, "Hello, world!");
///     assert_eq!(std::fs::read_to_string(d.join("f2.txt"))?, "Hello, world! (2)");
///     CleanDir(dir).write(&d)?;
///     assert_eq!(std::fs::read_to_string(d.join("f.txt"))?, "Hello, world!");
///     assert!(!d.join("f2.txt").exists());
///     # std::fs::remove_dir_all(&d)?;
///     Ok(())
/// }
/// ```
#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct CleanDir<T>(pub T);

impl<'a, T, Vfs: crate::Vfs> ReadFrom<'a, Vfs> for CleanDir<T>
where
    T: ReadFrom<'a, Vfs>,
{
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(T::read_from(path, vfs)?))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project]
pub struct CleanDirReadFuture<'a, T, Vfs>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
    Vfs: crate::VfsAsync,
{
    #[pin]
    inner: T::Future,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync> Future for CleanDirReadFuture<'a, T, Vfs>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    type Output = Result<CleanDir<T>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        match this.inner.poll(cx) {
            Poll::Ready(v) => Poll::Ready(v.map(CleanDir)),
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'a> ReadFromAsync<'a, Vfs> for CleanDir<T>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'a>>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move { T::read_from_async(path, vfs).await.map(Self) })
    }
}

impl<T, Vfs: crate::Vfs> WriteTo<Vfs> for CleanDir<T>
where
    T: WriteTo<Vfs>,
{
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T, Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for CleanDir<T>
where
    T: WriteToAsync<Vfs> + Send + Sync + 'static,
{
    type Future<'a>
        = <CleanDirRefWr<'a, T, Vfs> as WriteToAsync<Vfs>>::Future<'a>
    where
        Self: 'a;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        Self::from_ref_for_writer_async(&self.0).write_to_async_owned(path, vfs)
    }
}

impl<'a, T, Vfs: crate::Vfs> FromRefForWriter<'a, Vfs> for CleanDir<T>
where
    T: WriteTo<Vfs> + 'a,
    Vfs: 'a,
{
    type Inner = T;
    type Wr = CleanDirRefWr<'a, T, Vfs>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        CleanDirRefWr(value, marker::PhantomData)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'static> FromRefForWriterAsync<'a, Vfs> for CleanDir<T>
where
    T: WriteToAsync<Vfs> + Send + Sync + 'static,
{
    type Inner = T;
    type Wr = CleanDirRefWr<'a, T, Vfs>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        CleanDirRefWr(value, marker::PhantomData)
    }
}

impl<T> NewtypeToInner for CleanDir<T>
where
    T: DirStructureItem,
{
    type Inner = T;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<const NAME: [char; HAS_FIELD_MAX_LEN], T> HasField<NAME> for CleanDir<T>
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
impl<T> DynamicHasField for CleanDir<T>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        T::resolve_path(p, name)
    }
}

/// [`WriteTo`] impl for [`CleanDir`]
pub struct CleanDirRefWr<'a, T: ?Sized, Vfs: 'a>(&'a T, marker::PhantomData<Vfs>);

impl<T, Vfs: crate::Vfs> WriteTo<Vfs> for CleanDirRefWr<'_, T, Vfs>
where
    T: ?Sized + WriteTo<Vfs>,
{
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        if vfs.exists(path)? {
            vfs.remove_dir_all(path)?;
        } else {
            vfs.create_parent_dir(path)?;
        }
        self.0.write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T, Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for CleanDirRefWr<'_, T, Vfs>
where
    T: ?Sized + WriteToAsync<Vfs> + Send + Sync + 'static,
{
    type Future<'a>
        = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>
    where
        Self: 'a;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        Box::pin(async move {
            if vfs.exists(path.clone()).await? {
                vfs.remove_dir_all(path.clone()).await?;
            } else {
                vfs.create_parent_dir(path.clone()).await?;
            }
            self.0.write_to_async(path, vfs).await
        })
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'static> WriteToAsyncOwned<'a, Vfs> for CleanDirRefWr<'a, T, Vfs>
where
    T: ?Sized + WriteToAsync<Vfs> + Send + Sync + 'static,
{
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move {
            if vfs.exists(path.clone()).await? {
                vfs.remove_dir_all(path.clone()).await?;
            } else {
                vfs.create_parent_dir(path.clone()).await?;
            }
            self.0.write_to_async(path, vfs).await
        })
    }
}
