#[cfg(feature = "async")]
use std::future;
use std::marker;
use std::path::Path;
#[cfg(feature = "async")]
use std::path::PathBuf;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::FromRefForWriter;
#[cfg(feature = "async")]
use crate::FromRefForWriterAsync;
use crate::NewtypeToInner;
use crate::ReadFrom;
#[cfg(feature = "tokio")]
use crate::ReadFromAsync;
use crate::Result;
use crate::WrapIoError;
use crate::WriteTo;
#[cfg(feature = "async")]
use crate::WriteToAsync;
#[cfg(feature = "async")]
use crate::WriteToAsyncOwned;

/// A newtype around a `Vec<u8>`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileBytes(pub Vec<u8>);

impl FileBytes {
    /// Creates a new [`FileBytes`] from the specified `Vec<u8>`.
    pub fn new(v: impl Into<Vec<u8>>) -> Self {
        Self(v.into())
    }
}

impl<'a, Vfs: crate::Vfs> ReadFrom<'a, Vfs> for FileBytes {
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        vfs.read(path).map(Self)
    }
}

#[cfg(feature = "tokio")]
impl<'a, Vfs: crate::VfsAsync + 'static> ReadFromAsync<'a, Vfs> for FileBytes {
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'a>>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move { vfs.read(path).await.map(Self::new) })
    }
}

impl<Vfs: crate::Vfs> WriteTo<Vfs> for FileBytes {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path, vfs)
    }
}

impl From<FileBytes> for Vec<u8> {
    fn from(value: FileBytes) -> Self {
        value.0
    }
}

impl From<Vec<u8>> for FileBytes {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl NewtypeToInner for FileBytes {
    type Inner = Vec<u8>;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

impl<'a, Vfs: crate::Vfs + 'a> FromRefForWriter<'a, Vfs> for FileBytes {
    type Inner = [u8];
    type Wr = FileBytesRefWr<'a, Vfs>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileBytesRefWr(value, marker::PhantomData)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'static> FromRefForWriterAsync<'a, Vfs> for FileBytes {
    type Inner = [u8];
    type Wr = FileBytesRefWr<'a, Vfs>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        FileBytesRefWr(value, marker::PhantomData)
    }
}

/// The [`WriteTo`] wrapper around a reference to a `[u8]`.
pub struct FileBytesRefWr<'a, Vfs: 'a>(&'a [u8], marker::PhantomData<Vfs>);

impl<Vfs: crate::Vfs> WriteTo<Vfs> for FileBytesRefWr<'_, Vfs> {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        vfs.create_parent_dir(path)?;
        std::fs::write(path, self.0).wrap_io_error_with(path)?;
        Ok(())
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for FileBytesRefWr<'_, Vfs> {
    type Future<'a>
        = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>
    where
        Self: 'a;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        Box::pin(async move {
            vfs.create_parent_dir(path.clone()).await?;
            vfs.write(path, self.0).await?;
            Ok(())
        })
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<'a, Vfs: crate::VfsAsync + 'static> WriteToAsyncOwned<'a, Vfs> for FileBytesRefWr<'a, Vfs> {
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move {
            vfs.create_parent_dir(path.clone()).await?;
            vfs.write(path, self.0).await?;
            Ok(())
        })
    }
}

/// A newtype around a [`String`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileString(pub String);

impl FileString {
    /// Creates a new [`FileString`] from the specified [`String`].
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }
}

impl From<FileString> for String {
    fn from(value: FileString) -> Self {
        value.0
    }
}

impl From<String> for FileString {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl NewtypeToInner for FileString {
    type Inner = String;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

impl<'a, Vfs: crate::Vfs> ReadFrom<'a, Vfs> for FileString {
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        vfs.read_string(path).map(Self)
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<'a, Vfs: crate::VfsAsync + 'static> ReadFromAsync<'a, Vfs> for FileString {
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'a>>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move { vfs.read_string(path).await.map(Self) })
    }
}

impl<Vfs: crate::Vfs> WriteTo<Vfs> for FileString {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for FileString {
    type Future<'a>
        = <FileStrWr<'a, Vfs> as WriteToAsync<Vfs>>::Future<'a>
    where
        Self: 'a;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        Self::from_ref_for_writer_async(&self.0).write_to_async_owned(path, vfs)
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<'a, Vfs: crate::VfsAsync + 'static> WriteToAsyncOwned<'a, Vfs> for FileString {
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move {
            vfs.create_parent_dir(path.clone()).await?;
            vfs.write(path, self.0.as_bytes()).await?;
            Ok(())
        })
    }
}

impl<'a, Vfs: crate::Vfs + 'a> FromRefForWriter<'a, Vfs> for FileString {
    type Inner = str;
    type Wr = FileStrWr<'a, Vfs>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileStrWr(value, marker::PhantomData)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'static> FromRefForWriterAsync<'a, Vfs> for FileString {
    type Inner = str;
    type Wr = FileStrWr<'a, Vfs>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        FileStrWr(value, marker::PhantomData)
    }
}

/// The [`WriteTo`] wrapper around a reference to a [`str`].
pub struct FileStrWr<'a, Vfs: 'a>(&'a str, marker::PhantomData<Vfs>);

impl<Vfs: crate::Vfs> WriteTo<Vfs> for FileStrWr<'_, Vfs> {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        FileBytes::from_ref_for_writer(self.0.as_bytes()).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for FileStrWr<'_, Vfs> {
    type Future<'b>
        = <FileBytesRefWr<'b, Vfs> as WriteToAsync<Vfs>>::Future<'b>
    where
        Self: 'b,
        Vfs: 'b;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        FileBytes::from_ref_for_writer_async(self.0.as_bytes()).write_to_async_owned(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'static> WriteToAsyncOwned<'a, Vfs> for FileStrWr<'a, Vfs> {
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        FileBytes::from_ref_for_writer_async(self.0.as_bytes()).write_to_async_owned(path, vfs)
    }
}

// Impls for std types.

impl<'a, Vfs: crate::Vfs> ReadFrom<'a, Vfs> for String {
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        FileString::read_from(path, vfs).map(|v| v.0)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project]
pub struct StringReadFuture<'a, Vfs: crate::VfsAsync + 'static>(
    #[pin] <FileString as ReadFromAsync<'a, Vfs>>::Future,
);

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'static> Future for StringReadFuture<'a, Vfs> {
    type Output = Result<String>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let projection = self.project();
        let res = <FileString as ReadFromAsync<'a, Vfs>>::Future::poll(projection.0, cx);
        match res {
            Poll::Ready(res) => Poll::Ready(res.map(|inner| inner.0)),
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'static> ReadFromAsync<'a, Vfs> for String {
    type Future = StringReadFuture<'a, Vfs>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        StringReadFuture(FileString::read_from_async(path, vfs))
    }
}

impl<Vfs: crate::Vfs> WriteTo<Vfs> for String {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        FileString::from_ref_for_writer(self).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for String {
    type Future<'b>
        = <FileStrWr<'b, Vfs> as WriteToAsync<Vfs>>::Future<'b>
    where
        Self: 'b;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        FileString::from_ref_for_writer_async(self).write_to_async_owned(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'static> WriteToAsyncOwned<'a, Vfs> for String {
    type Future = <FileString as WriteToAsyncOwned<'a, Vfs>>::Future;

    fn write_to_async_owned(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        FileString::new(self).write_to_async_owned(path, vfs)
    }
}

impl<'a, Vfs: crate::Vfs> ReadFrom<'a, Vfs> for Vec<u8> {
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        FileBytes::read_from(path, vfs).map(|v| v.0)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project]
pub struct VecReadFuture<'a, Vfs: crate::VfsAsync + 'static>(
    #[pin] <FileBytes as ReadFromAsync<'a, Vfs>>::Future,
);

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'static> Future for VecReadFuture<'a, Vfs> {
    type Output = Result<Vec<u8>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let projection = self.project();
        let res = <FileBytes as ReadFromAsync<'a, Vfs>>::Future::poll(projection.0, cx);
        match res {
            Poll::Ready(res) => Poll::Ready(res.map(|inner| inner.0)),
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'static> ReadFromAsync<'a, Vfs> for Vec<u8> {
    type Future = VecReadFuture<'a, Vfs>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        VecReadFuture(FileBytes::read_from_async(path, vfs))
    }
}

impl<Vfs: crate::Vfs> WriteTo<Vfs> for Vec<u8> {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        FileBytes::from_ref_for_writer(self).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for Vec<u8> {
    type Future<'b>
        = <FileBytesRefWr<'b, Vfs> as WriteToAsync<Vfs>>::Future<'b>
    where
        Self: 'b,
        Vfs: 'b;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        FileBytes::from_ref_for_writer_async(self).write_to_async_owned(path, vfs)
    }
}

impl<Vfs: crate::Vfs> WriteTo<Vfs> for str {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        FileStrWr(self, marker::PhantomData).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for str {
    type Future<'a>
        = <FileStrWr<'a, Vfs> as WriteToAsync<Vfs>>::Future<'a>
    where
        Self: 'a,
        Vfs: 'a;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        FileStrWr(self, marker::PhantomData).write_to_async_owned(path, vfs)
    }
}

impl<Vfs: crate::Vfs> WriteTo<Vfs> for &str {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        FileStrWr(self, marker::PhantomData).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for &str {
    type Future<'a>
        = <FileStrWr<'a, Vfs> as WriteToAsync<Vfs>>::Future<'a>
    where
        Self: 'a,
        Vfs: 'a;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        FileStrWr(self, marker::PhantomData).write_to_async_owned(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'static> WriteToAsyncOwned<'a, Vfs> for &'a str {
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        FileStrWr(self, marker::PhantomData).write_to_async_owned(path, vfs)
    }
}

impl<Vfs: crate::Vfs> WriteTo<Vfs> for [u8] {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        FileBytesRefWr(self, marker::PhantomData).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for [u8] {
    type Future<'a>
        = <FileBytesRefWr<'a, Vfs> as WriteToAsyncOwned<'a, Vfs>>::Future
    where
        Self: 'a,
        Vfs: 'a;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        FileBytesRefWr(self, marker::PhantomData).write_to_async_owned(path, vfs)
    }
}

impl<Vfs: crate::Vfs> WriteTo<Vfs> for &[u8] {
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        FileBytesRefWr(self, marker::PhantomData).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for &[u8] {
    type Future<'a>
        = <FileBytesRefWr<'a, Vfs> as WriteToAsyncOwned<'a, Vfs>>::Future
    where
        Self: 'a,
        Vfs: 'a;

    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        FileBytesRefWr(self, marker::PhantomData).write_to_async_owned(path, vfs)
    }
}

impl<'a, T: 'a, Vfs: crate::Vfs> ReadFrom<'a, Vfs> for marker::PhantomData<T> {
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(Self)
    }
}

impl<T, Vfs: crate::Vfs> WriteTo<Vfs> for marker::PhantomData<T> {
    fn write_to(&self, _path: &Path, _vfs: Pin<&Vfs>) -> Result<()> {
        Ok(())
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: crate::VfsAsync + 'a> ReadFromAsync<'a, Vfs> for marker::PhantomData<T>
where
    T: Send + Sync + 'static,
{
    type Future = future::Ready<Result<Self>>;

    fn read_from_async(_path: PathBuf, _vfs: Pin<&Vfs>) -> Self::Future {
        future::ready(Ok(Self))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T, Vfs: crate::VfsAsync + 'static> WriteToAsync<Vfs> for marker::PhantomData<T>
where
    T: Send + Sync + 'static,
{
    type Future<'a> = future::Ready<Result<()>>;

    fn write_to_async<'a>(&'a self, _path: PathBuf, _vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        future::ready(Ok(()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T, Vfs: crate::VfsAsync> WriteToAsyncOwned<'_, Vfs> for marker::PhantomData<T>
where
    T: Send + Sync + 'static,
{
    type Future = future::Ready<Result<()>>;

    fn write_to_async_owned(self, _path: PathBuf, _vfs: Pin<&Vfs>) -> Self::Future {
        future::ready(Ok(()))
    }
}
