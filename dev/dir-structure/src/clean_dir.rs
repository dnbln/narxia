use std::path::Path;
#[cfg(any(feature = "async", feature = "resolve-path"))]
use std::path::PathBuf;
#[cfg(feature = "async")]
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
use crate::WrapIoError;
use crate::prelude::*;
use crate::utils;

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

impl<T> ReadFrom for CleanDir<T>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(T::read_from(path)?))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project]
pub struct CleanDirReadFuture<T>
where
    T: ReadFromAsync + Send + 'static,
{
    #[pin]
    inner: T::Future,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T> Future for CleanDirReadFuture<T>
where
    T: ReadFromAsync + Send + 'static,
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
impl<T> ReadFromAsync for CleanDir<T>
where
    T: ReadFromAsync + Send + 'static,
{
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send>>;

    fn read_from_async(path: PathBuf) -> Self::Future {
        Box::pin(async move { T::read_from_async(path).await.map(Self) })
    }
}

impl<T> WriteTo for CleanDir<T>
where
    T: WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T> WriteToAsync for CleanDir<T>
where
    T: WriteToAsync + Send + Sync + 'static,
{
    type Future<'a>
        = <CleanDirRefWr<'a, T> as WriteToAsync>::Future<'a>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        Self::from_ref_for_writer_async(&self.0).write_to_async_owned(path)
    }
}

impl<'a, T> FromRefForWriter<'a> for CleanDir<T>
where
    T: WriteTo + 'a,
{
    type Inner = T;
    type Wr = CleanDirRefWr<'a, T>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        CleanDirRefWr(value)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> FromRefForWriterAsync<'a> for CleanDir<T>
where
    T: WriteToAsync + Send + Sync + 'static,
{
    type Inner = T;
    type Wr = CleanDirRefWr<'a, T>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        CleanDirRefWr(value)
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
pub struct CleanDirRefWr<'a, T: ?Sized>(&'a T);

impl<T> WriteTo for CleanDirRefWr<'_, T>
where
    T: ?Sized + WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        if path.exists() {
            std::fs::remove_dir_all(path).wrap_io_error_with(path)?;
        } else {
            utils::create_parent_dir(path)?;
        }
        self.0.write_to(path)
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<T> WriteToAsync for CleanDirRefWr<'_, T>
where
    T: ?Sized + WriteToAsync + Send + Sync + 'static,
{
    type Future<'a>
        = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        Box::pin(async move {
            if tokio::fs::try_exists(&path)
                .await
                .wrap_io_error_with(&path)?
            {
                tokio::fs::remove_dir_all(&path)
                    .await
                    .wrap_io_error_with(&path)?;
            } else {
                utils::create_parent_dir_async(&path).await?;
            }
            self.0.write_to_async(path).await
        })
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<'a, T> WriteToAsyncOwned<'a> for CleanDirRefWr<'a, T>
where
    T: ?Sized + WriteToAsync + Send + Sync + 'static,
{
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        Box::pin(async move {
            if tokio::fs::try_exists(&path)
                .await
                .wrap_io_error_with(&path)?
            {
                tokio::fs::remove_dir_all(&path)
                    .await
                    .wrap_io_error_with(&path)?;
            } else {
                utils::create_parent_dir_async(&path).await?;
            }
            self.0.write_to_async(path).await
        })
    }
}
