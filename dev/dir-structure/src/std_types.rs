use std::path::Path;
#[cfg(feature = "async")]
use std::path::PathBuf;
#[cfg(feature = "async")]
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
use crate::utils;

/// A newtype around a `Vec<u8>`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileBytes(pub Vec<u8>);

impl FileBytes {
    /// Creates a new [`FileBytes`] from the specified `Vec<u8>`.
    pub fn new(v: impl Into<Vec<u8>>) -> Self {
        Self(v.into())
    }
}

impl ReadFrom for FileBytes {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        std::fs::read(path).wrap_io_error_with(path).map(Self)
    }
}

#[cfg(feature = "tokio")]
impl ReadFromAsync for FileBytes {
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send>>;

    fn read_from_async(path: PathBuf) -> Self::Future {
        Box::pin(async move {
            let bytes = tokio::fs::read(&path).await.wrap_io_error(|| path)?;
            Ok(Self::new(bytes))
        })
    }
}

impl WriteTo for FileBytes {
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
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

impl<'a> FromRefForWriter<'a> for FileBytes {
    type Inner = [u8];
    type Wr = FileBytesRefWr<'a>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileBytesRefWr(value)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a> FromRefForWriterAsync<'a> for FileBytes {
    type Inner = [u8];
    type Wr = FileBytesRefWr<'a>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        FileBytesRefWr(value)
    }
}

/// The [`WriteTo`] wrapper around a reference to a `[u8]`.
pub struct FileBytesRefWr<'a>(&'a [u8]);

impl WriteTo for FileBytesRefWr<'_> {
    fn write_to(&self, path: &Path) -> Result<()> {
        utils::create_parent_dir(path)?;
        std::fs::write(path, self.0).wrap_io_error_with(path)?;
        Ok(())
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl WriteToAsync for FileBytesRefWr<'_> {
    type Future<'a>
        = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        Box::pin(async move {
            utils::create_parent_dir_async(&path).await?;
            tokio::fs::write(&path, self.0)
                .await
                .wrap_io_error_with(&path)?;
            Ok(())
        })
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<'a> WriteToAsyncOwned<'a> for FileBytesRefWr<'a> {
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        Box::pin(async move {
            utils::create_parent_dir_async(&path).await?;
            tokio::fs::write(&path, self.0)
                .await
                .wrap_io_error_with(&path)?;
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

impl ReadFrom for FileString {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        std::fs::read_to_string(path)
            .wrap_io_error_with(path)
            .map(Self)
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl ReadFromAsync for FileString {
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send>>;

    fn read_from_async(path: PathBuf) -> Self::Future {
        Box::pin(async move {
            Ok(Self(
                tokio::fs::read_to_string(&path)
                    .await
                    .wrap_io_error_with(&path)?,
            ))
        })
    }
}

impl WriteTo for FileString {
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl WriteToAsync for FileString {
    type Future<'a>
        = <FileStrWr<'a> as WriteToAsync>::Future<'a>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        Self::from_ref_for_writer_async(&self.0).write_to_async_owned(path)
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
impl<'a> WriteToAsyncOwned<'a> for FileString {
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'static>>;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        Box::pin(async move {
            utils::create_parent_dir_async(&path).await?;
            tokio::fs::write(&path, self.0)
                .await
                .wrap_io_error_with(&path)?;
            Ok(())
        })
    }
}

impl<'a> FromRefForWriter<'a> for FileString {
    type Inner = str;
    type Wr = FileStrWr<'a>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileStrWr(value)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a> FromRefForWriterAsync<'a> for FileString {
    type Inner = str;
    type Wr = FileStrWr<'a>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        FileStrWr(value)
    }
}

/// The [`WriteTo`] wrapper around a reference to a [`str`].
pub struct FileStrWr<'a>(&'a str);

impl WriteTo for FileStrWr<'_> {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytes::from_ref_for_writer(self.0.as_bytes()).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl WriteToAsync for FileStrWr<'_> {
    type Future<'a>
        = <FileBytesRefWr<'a> as WriteToAsync>::Future<'a>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        FileBytes::from_ref_for_writer_async(self.0.as_bytes()).write_to_async_owned(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a> WriteToAsyncOwned<'a> for FileStrWr<'a> {
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        FileBytes::from_ref_for_writer_async(self.0.as_bytes()).write_to_async_owned(path)
    }
}

// Impls for std types.

impl ReadFrom for String {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        FileString::read_from(path).map(|v| v.0)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project]
pub struct StringReadFuture(#[pin] <FileString as ReadFromAsync>::Future);

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl Future for StringReadFuture {
    type Output = Result<String>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let projection = self.project();
        let res = <FileString as ReadFromAsync>::Future::poll(projection.0, cx);
        match res {
            Poll::Ready(res) => Poll::Ready(res.map(|inner| inner.0)),
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl ReadFromAsync for String {
    type Future = StringReadFuture;

    fn read_from_async(path: PathBuf) -> Self::Future {
        StringReadFuture(FileString::read_from_async(path))
    }
}

impl WriteTo for String {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileString::from_ref_for_writer(self).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl WriteToAsync for String {
    type Future<'a>
        = <FileStrWr<'a> as WriteToAsync>::Future<'a>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        FileString::from_ref_for_writer_async(self).write_to_async_owned(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a> WriteToAsyncOwned<'a> for String {
    type Future = <FileString as WriteToAsyncOwned<'a>>::Future;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        FileString::new(self).write_to_async_owned(path)
    }
}

impl ReadFrom for Vec<u8> {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        FileBytes::read_from(path).map(|v| v.0)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project]
pub struct VecReadFuture(#[pin] <FileBytes as ReadFromAsync>::Future);

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl Future for VecReadFuture {
    type Output = Result<Vec<u8>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let projection = self.project();
        let res = <FileBytes as ReadFromAsync>::Future::poll(projection.0, cx);
        match res {
            Poll::Ready(res) => Poll::Ready(res.map(|inner| inner.0)),
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl ReadFromAsync for Vec<u8> {
    type Future = VecReadFuture;

    fn read_from_async(path: PathBuf) -> Self::Future {
        VecReadFuture(FileBytes::read_from_async(path))
    }
}

impl WriteTo for Vec<u8> {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytes::from_ref_for_writer(self).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl WriteToAsync for Vec<u8> {
    type Future<'a>
        = <FileBytesRefWr<'a> as WriteToAsync>::Future<'a>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        FileBytes::from_ref_for_writer_async(self).write_to_async_owned(path)
    }
}

impl WriteTo for str {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileStrWr(self).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl WriteToAsync for str {
    type Future<'a>
        = <FileStrWr<'a> as WriteToAsync>::Future<'a>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        FileStrWr(self).write_to_async_owned(path)
    }
}

impl WriteTo for &str {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileStrWr(self).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl WriteToAsync for &str {
    type Future<'a>
        = <FileStrWr<'a> as WriteToAsync>::Future<'a>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        FileStrWr(self).write_to_async_owned(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a> WriteToAsyncOwned<'a> for &'a str {
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        FileStrWr(self).write_to_async_owned(path)
    }
}

impl WriteTo for [u8] {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytesRefWr(self).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl WriteToAsync for [u8] {
    type Future<'a>
        = <FileBytesRefWr<'a> as WriteToAsync>::Future<'a>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        FileBytesRefWr(self).write_to_async_owned(path)
    }
}

impl WriteTo for &[u8] {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytesRefWr(self).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl WriteToAsync for &[u8] {
    type Future<'a>
        = <FileBytesRefWr<'a> as WriteToAsync>::Future<'a>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        FileBytesRefWr(self).write_to_async_owned(path)
    }
}
