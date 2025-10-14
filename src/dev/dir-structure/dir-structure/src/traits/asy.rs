//! Asynchronous reading / writing traits.

use std::future;
use std::future::Future;
use std::pin::Pin;

use crate::error::VfsResult;
use crate::traits::async_vfs::VfsAsync;
use crate::traits::async_vfs::WriteSupportingVfsAsync;
use crate::traits::vfs::PathType;
use crate::traits::vfs::VfsCore;

/// Trait for types / structures that can be read from disk asynchronously.
///
/// `async` version of [`ReadFrom`](crate::traits::sync::ReadFrom).
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait ReadFromAsync<'a, Vfs: VfsAsync + ?Sized + 'a>: Sized + 'a {
    /// The future type returned by the async read function.
    type Future: Future<Output = VfsResult<Self, Vfs>> + Send + Unpin + 'a
    where
        Self: 'a;

    /// Asynchronously reads the structure from the specified path,
    /// which can be either a file or a directory.
    fn read_from_async(
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future;
}

/// Trait for types / structures that can be written to disk asynchronously.
///
/// The difference between this and [`WriteToAsyncRef`] is that this trait takes in
/// owned data instead of a reference.
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteToAsync<'a, Vfs: WriteSupportingVfsAsync + ?Sized + 'a> {
    /// The future type returned by the async write function.
    type Future: Future<Output = VfsResult<(), Vfs>> + Send + Unpin + 'a;

    /// Asynchronously writes the structure to the specified path.
    fn write_to_async(
        self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future;
}

/// Trait for types / structures that can be written to disk asynchronously.
///
/// The difference between this and [`WriteToAsync`] is that this trait takes in
/// a reference instead of owned data.
///
/// This is an async equivalent of [`WriteTo`](crate::traits::sync::WriteTo).
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteToAsyncRef<'r, Vfs: WriteSupportingVfsAsync + ?Sized + 'r> {
    /// The future type returned by the async write function.
    type Future<'a>: Future<Output = VfsResult<(), Vfs>> + Send + Unpin + 'a
    where
        Self: 'a,
        'r: 'a,
        Vfs: 'a;

    /// Asynchronously writes the structure to the specified path.
    fn write_to_async_ref<'a>(
        &'a self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future<'a>
    where
        'r: 'a;
}

/// Async equivalent of [`FromRefForWriter`](crate::traits::sync::FromRefForWriter).
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait FromRefForWriterAsync<'a, Vfs: WriteSupportingVfsAsync + ?Sized + 'a> {
    /// The inner type to cast.
    type Inner: ?Sized;
    /// The reference type to cast to.
    type Wr: WriteToAsync<'a, Vfs> + 'a;

    /// Casts the reference to the inner type to a [`WriteToAsync`]
    /// reference type.
    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: VfsAsync + ?Sized + 'a> ReadFromAsync<'a, Vfs> for () {
    type Future = future::Ready<VfsResult<Self, Vfs>>;

    fn read_from_async(
        _path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        _vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        future::ready(Ok(()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + ?Sized + 'a> WriteToAsync<'a, Vfs> for () {
    type Future = future::Ready<VfsResult<(), Vfs>>;

    fn write_to_async(
        self,
        _path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        _vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        future::ready(Ok(()))
    }
}
