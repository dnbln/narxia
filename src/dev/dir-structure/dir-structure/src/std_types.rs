//! Implementations for standard library types.

#[cfg(feature = "async")]
use std::future;
use std::marker;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

use crate::error::VfsResult;
use crate::prelude::*;
#[cfg(feature = "async")]
use crate::traits::asy::FromRefForWriterAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::WriteSupportingVfsAsync;
use crate::traits::sync::FromRefForWriter;
use crate::traits::sync::NewtypeToInner;
use crate::traits::vfs;
#[cfg(feature = "async")]
use crate::traits::vfs::PathType;

/// A newtype around a `Vec<u8>`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct FileBytes(pub Vec<u8>);

impl FileBytes {
    /// Creates a new [`FileBytes`] from the specified `Vec<u8>`.
    pub fn new(v: impl Into<Vec<u8>>) -> Self {
        Self(v.into())
    }
}

impl<'a, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for FileBytes {
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        vfs.read(path).map(Self)
    }
}

#[cfg(feature = "async")]
impl<'a, Vfs: VfsAsync + 'static> ReadFromAsync<'a, Vfs> for FileBytes {
    type Future = Pin<Box<dyn Future<Output = VfsResult<Self, Vfs>> + Send + 'a>>;

    fn read_from_async(
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        Box::pin(async move { vfs.read(path).await.map(Self::new) })
    }
}

impl<'a, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for FileBytes {
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
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

impl<'a, 'vfs, Vfs: vfs::WriteSupportingVfs<'vfs> + 'vfs> FromRefForWriter<'a, 'vfs, Vfs>
    for FileBytes
where
    'vfs: 'a,
{
    type Inner = [u8];
    type Wr = FileBytesRefWr<'a, 'vfs, Vfs>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileBytesRefWr(value, marker::PhantomData)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> FromRefForWriterAsync<'a, Vfs> for FileBytes {
    type Inner = [u8];
    type Wr = FileBytesRefWr<'a, 'a, Vfs>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        FileBytesRefWr(value, marker::PhantomData)
    }
}

/// The [`WriteTo`] wrapper around a reference to a `[u8]`.
pub struct FileBytesRefWr<'a, 'vfs, Vfs: 'vfs>(&'a [u8], marker::PhantomData<&'vfs Vfs>)
where
    'vfs: 'a;

impl<'a, 'vfs, Vfs: vfs::WriteSupportingVfs<'vfs>> WriteTo<'vfs, Vfs>
    for FileBytesRefWr<'a, 'vfs, Vfs>
where
    'vfs: 'a,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<(), Vfs> {
        vfs.create_parent_dir(path)?;
        vfs.write(path, self.0)?;
        Ok(())
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs>
    for FileBytesRefWr<'a, 'a, Vfs>
{
    type Future = Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + 'a>>;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        Box::pin(async move {
            vfs.create_parent_dir(path.clone()).await?;
            vfs.write(path, self.0).await?;
            Ok(())
        })
    }
}

/// A newtype around a [`String`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
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

impl<'a, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for FileString {
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        vfs.read_string(path).map(Self)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[doc(hidden)]
pub struct FileStringReadFuture<'a, Vfs: VfsAsync + 'static>(Vfs::ReadStringFuture<'a>);

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: VfsAsync + 'static> Future for FileStringReadFuture<'a, Vfs> {
    type Output = VfsResult<FileString, Vfs>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match Pin::new(&mut self.0).poll(cx) {
            Poll::Ready(res) => Poll::Ready(res.map(FileString)),
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: VfsAsync + 'static> ReadFromAsync<'a, Vfs> for FileString {
    type Future = FileStringReadFuture<'a, Vfs>;

    fn read_from_async(
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        FileStringReadFuture(vfs.read_string(path))
    }
}

impl<'a, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for FileString {
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        Self::from_ref_for_writer(&self.0).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for FileString {
    type Future = Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + 'a>>;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        Box::pin(async move {
            vfs.create_parent_dir(path.clone()).await?;
            vfs.write(path, self.0.as_bytes()).await?;
            Ok(())
        })
    }
}

impl<'a, 'vfs, Vfs: vfs::WriteSupportingVfs<'vfs> + 'vfs> FromRefForWriter<'a, 'vfs, Vfs>
    for FileString
where
    'vfs: 'a,
{
    type Inner = str;
    type Wr = FileStrWr<'a, 'vfs, Vfs>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileStrWr(value, marker::PhantomData)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> FromRefForWriterAsync<'a, Vfs> for FileString {
    type Inner = str;
    type Wr = FileStrWr<'a, 'a, Vfs>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        FileStrWr(value, marker::PhantomData)
    }
}

/// The [`WriteTo`] wrapper around a reference to a [`str`].
pub struct FileStrWr<'a, 'vfs, Vfs: 'vfs>(&'a str, marker::PhantomData<&'vfs Vfs>)
where
    'vfs: 'a;

impl<'a, 'vfs, Vfs: vfs::WriteSupportingVfs<'vfs>> WriteTo<'vfs, Vfs> for FileStrWr<'a, 'vfs, Vfs>
where
    'vfs: 'a,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<(), Vfs> {
        FileBytes::from_ref_for_writer(self.0.as_bytes()).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for FileStrWr<'a, 'a, Vfs> {
    type Future = <FileBytesRefWr<'a, 'a, Vfs> as WriteToAsync<'a, Vfs>>::Future;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        FileBytes::from_ref_for_writer_async(self.0.as_bytes()).write_to_async(path, vfs)
    }
}

// Impls for std types.

impl<'a, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for String {
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        vfs.read_string(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: VfsAsync + 'static> ReadFromAsync<'a, Vfs> for String {
    type Future = Vfs::ReadStringFuture<'a>;

    fn read_from_async(
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        vfs.read_string(path)
    }
}

impl<'a, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for String {
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        FileString::from_ref_for_writer(self).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for String {
    type Future = <FileString as WriteToAsync<'a, Vfs>>::Future;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        FileString::new(self).write_to_async(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsyncRef<'a, Vfs> for String {
    type Future<'b>
        = <FileString as WriteToAsync<'b, Vfs>>::Future
    where
        Self: 'b,
        'a: 'b,
        Vfs: 'b;

    fn write_to_async_ref<'b>(
        &'b self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'b Vfs>,
    ) -> <Self as WriteToAsync<'b, Vfs>>::Future
    where
        'a: 'b,
    {
        FileString::new(self).write_to_async(path, vfs)
    }
}

impl<'a, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for Vec<u8> {
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        FileBytes::read_from(path, vfs).map(|v| v.0)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project]
#[doc(hidden)]
pub struct VecReadFuture<'a, Vfs: VfsAsync + 'static>(
    #[pin] <FileBytes as ReadFromAsync<'a, Vfs>>::Future,
);

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: VfsAsync + 'static> Future for VecReadFuture<'a, Vfs> {
    type Output = VfsResult<Vec<u8>, Vfs>;

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
impl<'a, Vfs: VfsAsync + 'static> ReadFromAsync<'a, Vfs> for Vec<u8> {
    type Future = VecReadFuture<'a, Vfs>;

    fn read_from_async(
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        VecReadFuture(FileBytes::read_from_async(path, vfs))
    }
}

impl<'a, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for Vec<u8> {
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        FileBytes::from_ref_for_writer(self).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for Vec<u8> {
    type Future = <FileBytesRefWr<'a, 'a, Vfs> as WriteToAsync<'a, Vfs>>::Future;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        Box::pin(async move {
            vfs.create_parent_dir(path.clone()).await?;
            vfs.write(path, &self).await
        })
    }
}

impl<'a, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for str {
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        FileStrWr(self, marker::PhantomData).write_to(path, vfs)
    }
}

impl<'a, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for &str {
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        FileStrWr(self, marker::PhantomData).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for &'a str {
    type Future = <Vfs as WriteSupportingVfsAsync>::WriteFuture<'a>;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        vfs.write(path, self.as_bytes())
    }
}

impl<'a, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for [u8] {
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        FileBytesRefWr(self, marker::PhantomData).write_to(path, vfs)
    }
}

impl<'a, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for &[u8] {
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        FileBytesRefWr(self, marker::PhantomData).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for &'a [u8] {
    type Future = <Vfs as WriteSupportingVfsAsync>::WriteFuture<'a>;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        vfs.write(path, self)
    }
}

impl<'a, T: 'a, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for marker::PhantomData<T> {
    fn read_from(_path: &Vfs::Path, _vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs>
    where
        Self: Sized,
    {
        Ok(Self)
    }
}

impl<'a, T, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for marker::PhantomData<T> {
    fn write_to(&self, _path: &Vfs::Path, _vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        Ok(())
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: VfsAsync + 'a> ReadFromAsync<'a, Vfs> for marker::PhantomData<T>
where
    T: Send + Sync + 'static,
{
    type Future = future::Ready<VfsResult<Self, Vfs>>;

    fn read_from_async(_path: <Vfs::Path as PathType>::OwnedPath, _vfs: Pin<&Vfs>) -> Self::Future {
        future::ready(Ok(Self))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for marker::PhantomData<T>
where
    T: Send + Sync + 'static,
{
    type Future = future::Ready<VfsResult<(), Vfs>>;

    fn write_to_async(
        self,
        _path: <Vfs::Path as PathType>::OwnedPath,
        _vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        future::ready(Ok(()))
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::TokioFsVfs;

//     fn assert_unpin<T: Unpin>() {}

//     #[test]
//     fn test_file_string_read_future() {
//         assert_unpin::<FileStringReadFuture<TokioFsVfs>>();
//     }
// }
