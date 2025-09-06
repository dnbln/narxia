//! Asynchronous reading / writing traits.

use std::future;
use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;

use crate::error::Result;
#[cfg(feature = "async")]
use crate::traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::WriteSupportingVfsAsync;

/// Trait for types / structures that can be read from disk asynchronously.
///
/// `async` version of [`ReadFrom`].
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait ReadFromAsync<'a, Vfs: VfsAsync + 'a>: Sized {
    /// The future type returned by the async read function.
    type Future: Future<Output = Result<Self>> + Send + Unpin + 'a
    where
        Self: 'a;

    /// Asynchronously reads the structure from the specified path,
    /// which can be either a file or a directory.
    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future;
}

/// Trait for types / structures that can be written to disk asynchronously.
///
/// The difference between this and [`WriteToAsyncRef`] is that this trait takes in
/// owned data instead of a reference.
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteToAsync<'a, Vfs: WriteSupportingVfsAsync + 'a> {
    /// The future type returned by the async write function.
    type Future: Future<Output = Result<()>> + Send + Unpin + 'a;

    /// Asynchronously writes the structure to the specified path.
    fn write_to_async(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future;
}

/// Trait for types / structures that can be written to disk asynchronously.
///
/// The difference between this and [`WriteToAsync`] is that this trait takes in
/// a reference instead of owned data.
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteToAsyncRef<'r, Vfs: WriteSupportingVfsAsync + 'r> {
    /// The future type returned by the async write function.
    type Future<'a>: Future<Output = Result<()>> + Send + Unpin + 'a
    where
        Self: 'a,
        'r: 'a,
        Vfs: 'a;

    /// Asynchronously writes the structure to the specified path.
    fn write_to_async_ref<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a>
    where
        'r: 'a;
}

/// Async equivalent of [`FromRefForWriter`](crate::FromRefForWriter).
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait FromRefForWriterAsync<'a, Vfs: WriteSupportingVfsAsync + 'a> {
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
impl<'a, Vfs: VfsAsync + 'a> ReadFromAsync<'a, Vfs> for () {
    type Future = future::Ready<Result<Self>>;

    fn read_from_async(_path: PathBuf, _vfs: Pin<&'a Vfs>) -> Self::Future {
        future::ready(Ok(()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: WriteSupportingVfsAsync + 'a> WriteToAsync<'a, Vfs> for () {
    type Future = future::Ready<Result<()>>;

    fn write_to_async(self, _path: PathBuf, _vfs: Pin<&'a Vfs>) -> Self::Future {
        future::ready(Ok(()))
    }
}
