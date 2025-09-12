//! A newtype that will clean the directory it is written to, before writing
//! the value.

#[cfg(feature = "async")]
use std::future::Future;
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

use crate::error::Result;
use crate::prelude::*;
#[cfg(feature = "async")]
use crate::traits::asy::FromRefForWriterAsync;
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
use crate::traits::sync::DirStructureItem;
use crate::traits::sync::FromRefForWriter;
use crate::traits::sync::NewtypeToInner;
use crate::traits::vfs;

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
/// use dir_structure::traits::sync::DirStructureItem;
/// use dir_structure::clean_dir::CleanDir;
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
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct CleanDir<T>(pub T);

impl<'a, T, Vfs: vfs::Vfs> ReadFrom<'a, Vfs> for CleanDir<T>
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
#[doc(hidden)]
pub struct CleanDirReadFuture<'a, T, Vfs>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
    Vfs: VfsAsync,
{
    #[pin]
    inner: T::Future,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: VfsAsync> Future for CleanDirReadFuture<'a, T, Vfs>
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
impl<'a, T, Vfs: VfsAsync + 'a> ReadFromAsync<'a, Vfs> for CleanDir<T>
where
    T: ReadFromAsync<'a, Vfs> + Send + 'static,
{
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'a>>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move { T::read_from_async(path, vfs).await.map(Self) })
    }
}

impl<T, Vfs: vfs::WriteSupportingVfs> WriteTo<Vfs> for CleanDir<T>
where
    T: WriteTo<Vfs>,
{
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for CleanDir<T>
where
    T: WriteToAsync<'a, Vfs> + Send + Sync + 'static,
{
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
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
#[pin_project(project_replace = CleanDirWriteRefFutureProjOwn)]
#[doc(hidden)]
pub enum CleanDirWriteRefFuture<'a, 'f, T, Vfs: WriteSupportingVfsAsync + 'static>
where
    T: WriteToAsyncRef<'a, Vfs> + Send + Sync + 'a,
    <T as WriteToAsyncRef<'a, Vfs>>::Future<'f>: Future<Output = Result<()>> + Unpin + 'f,
    <Vfs as VfsAsync>::ExistsFuture<'f>: Future<Output = Result<bool>> + Unpin + 'f,
    <Vfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'f>:
        Future<Output = Result<()>> + Unpin + 'f,
    'a: 'f,
{
    Poison,
    ExistsCheck(
        <Vfs as VfsAsync>::ExistsFuture<'f>,
        PathBuf,
        Pin<&'f Vfs>,
        &'f T,
    ),
    RemoveDirAll(
        <Vfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'f>,
        PathBuf,
        Pin<&'f Vfs>,
        &'f T,
    ),
    Inner(<T as WriteToAsyncRef<'a, Vfs>>::Future<'f>),
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, 'f, T, Vfs: WriteSupportingVfsAsync + 'static> Future
    for CleanDirWriteRefFuture<'a, 'f, T, Vfs>
where
    T: WriteToAsyncRef<'a, Vfs> + Send + Sync + 'static,
    <T as WriteToAsyncRef<'a, Vfs>>::Future<'f>: Future<Output = Result<()>> + Unpin + 'f,
    <Vfs as VfsAsync>::ExistsFuture<'f>: Future<Output = Result<bool>> + Unpin + 'f,
    <Vfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'f>:
        Future<Output = Result<()>> + Unpin + 'f,
    'a: 'f,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);
        match this {
            CleanDirWriteRefFutureProjOwn::Poison => {
                panic!("polled after completion")
            }
            CleanDirWriteRefFutureProjOwn::ExistsCheck(mut fut, path, vfs, item) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(Ok(exists)) => {
                        if exists {
                            let fut = vfs.remove_dir_all(path.clone());
                            self.project_replace(Self::RemoveDirAll(fut, path.clone(), vfs, item));
                            cx.waker().wake_by_ref();
                            Poll::Pending
                        } else {
                            let fut = item.write_to_async_ref(path, vfs);
                            self.project_replace(Self::Inner(fut));
                            cx.waker().wake_by_ref();
                            Poll::Pending
                        }
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::ExistsCheck(fut, path, vfs, item));
                        Poll::Pending
                    }
                }
            }
            CleanDirWriteRefFutureProjOwn::RemoveDirAll(mut fut, path, vfs, item) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(Ok(())) => {
                        let fut = item.write_to_async_ref(path.clone(), vfs);
                        self.project_replace(Self::Inner(fut));
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::RemoveDirAll(fut, path, vfs, item));
                        Poll::Pending
                    }
                }
            }
            CleanDirWriteRefFutureProjOwn::Inner(mut fut) => match Pin::new(&mut fut).poll(cx) {
                Poll::Ready(v) => Poll::Ready(v),
                Poll::Pending => {
                    self.project_replace(Self::Inner(fut));
                    Poll::Pending
                }
            },
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsyncRef<'a, Vfs> for CleanDir<T>
where
    T: WriteToAsyncRef<'a, Vfs> + Send + Sync + 'static,
    for<'f> <T as WriteToAsyncRef<'a, Vfs>>::Future<'f>: Future<Output = Result<()>> + Unpin + 'f,
    for<'f> <Vfs as VfsAsync>::ExistsFuture<'f>: Future<Output = Result<bool>> + Unpin + 'f,
    for<'f> <Vfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'f>:
        Future<Output = Result<()>> + Unpin + 'f,
{
    type Future<'b>
        = CleanDirWriteRefFuture<'a, 'b, T, Vfs>
    where
        Self: 'b,
        'a: 'b,
        Vfs: 'b;

    fn write_to_async_ref<'b>(&'b self, path: PathBuf, vfs: Pin<&'b Vfs>) -> Self::Future<'b>
    where
        'a: 'b,
    {
        let exists_future = vfs.exists(path.clone());
        CleanDirWriteRefFuture::<'a, 'b, T, Vfs>::ExistsCheck(exists_future, path, vfs, &self.0)
    }
}

impl<'a, T, Vfs: vfs::WriteSupportingVfs> FromRefForWriter<'a, Vfs> for CleanDir<T>
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
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> FromRefForWriterAsync<'a, Vfs> for CleanDir<T>
where
    T: WriteToAsyncRef<'a, Vfs> + Send + Sync + 'static,
    for<'f> <Vfs as VfsAsync>::ExistsFuture<'f>: Future<Output = Result<bool>> + Unpin + 'f,
    for<'f> <Vfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'f>:
        Future<Output = Result<()>> + Unpin + 'f,
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
pub struct CleanDirRefWr<'a, T: ?Sized + 'a, Vfs: 'a>(&'a T, marker::PhantomData<Vfs>);

impl<T, Vfs: vfs::WriteSupportingVfs> WriteTo<Vfs> for CleanDirRefWr<'_, T, Vfs>
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
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs>
    for CleanDirRefWr<'a, T, Vfs>
where
    T: WriteToAsyncRef<'a, Vfs> + Send + Sync + 'static,
    for<'f> <Vfs as VfsAsync>::ExistsFuture<'f>: Future<Output = Result<bool>> + Unpin + 'f,
    for<'f> <Vfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'f>:
        Future<Output = Result<()>> + Unpin + 'f,
{
    type Future = CleanDirRefWrWriteFuture<'a, 'a, T, Vfs>;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        let exists_future = vfs.exists(path.clone());
        CleanDirRefWrWriteFuture::ExistsCheck(exists_future, path, vfs, self.0)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = CleanDirRefWrWriteFutureProjOwn)]
#[doc(hidden)]
pub enum CleanDirRefWrWriteFuture<'a, 'f, T, Vfs: WriteSupportingVfsAsync + 'static>
where
    T: WriteToAsyncRef<'a, Vfs> + ?Sized + 'a,
    T::Future<'f>: Future<Output = Result<()>> + Unpin + 'f,
    <Vfs as VfsAsync>::ExistsFuture<'f>: Future<Output = Result<bool>> + Unpin + 'f,
    <Vfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'f>:
        Future<Output = Result<()>> + Unpin + 'f,
    'a: 'f,
{
    Poison,
    ExistsCheck(
        <Vfs as VfsAsync>::ExistsFuture<'f>,
        PathBuf,
        Pin<&'a Vfs>,
        &'f T,
    ),
    RemoveDirAll(
        <Vfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'f>,
        PathBuf,
        Pin<&'f Vfs>,
        &'f T,
    ),
    Write(
        <T as WriteToAsyncRef<'a, Vfs>>::Future<'f>,
        PathBuf,
        Pin<&'f Vfs>,
    ),
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, 'f, T, Vfs: WriteSupportingVfsAsync + 'static> Future
    for CleanDirRefWrWriteFuture<'a, 'f, T, Vfs>
where
    T: WriteToAsyncRef<'a, Vfs> + ?Sized + 'a,
    T::Future<'f>: Future<Output = Result<()>> + Unpin + 'f,
    <Vfs as VfsAsync>::ExistsFuture<'f>: Future<Output = Result<bool>> + Unpin + 'f,
    <Vfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'f>:
        Future<Output = Result<()>> + Unpin + 'f,
    'a: 'f,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);
        match this {
            CleanDirRefWrWriteFutureProjOwn::Poison => {
                panic!("polled after completion")
            }
            CleanDirRefWrWriteFutureProjOwn::ExistsCheck(mut fut, path, vfs, v) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(Ok(exists)) => {
                        if exists {
                            let fut = vfs.remove_dir_all(path.clone());
                            self.project_replace(Self::RemoveDirAll(fut, path.clone(), vfs, v));
                            cx.waker().wake_by_ref();
                            Poll::Pending
                        } else {
                            let fut = T::write_to_async_ref(v, path.clone(), vfs);
                            self.project_replace(Self::Write(fut, path.clone(), vfs));
                            cx.waker().wake_by_ref();
                            Poll::Pending
                        }
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::ExistsCheck(fut, path, vfs, v));
                        Poll::Pending
                    }
                }
            }
            CleanDirRefWrWriteFutureProjOwn::RemoveDirAll(mut fut, path, vfs, v) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(Ok(())) => {
                        let fut = T::write_to_async_ref(v, path.clone(), vfs);
                        self.project_replace(Self::Write(fut, path.clone(), vfs));
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.project_replace(Self::RemoveDirAll(fut, path, vfs, v));
                        Poll::Pending
                    }
                }
            }
            CleanDirRefWrWriteFutureProjOwn::Write(mut fut, _path, _vfs) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(v) => Poll::Ready(v),
                    Poll::Pending => {
                        self.project_replace(Self::Write(fut, _path, _vfs));
                        Poll::Pending
                    }
                }
            }
        }
    }
}
