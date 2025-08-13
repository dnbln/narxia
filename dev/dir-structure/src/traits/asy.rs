use std::future;
use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;

use crate::error::Result;

/// Trait for types / structures that can be
/// read from disk asynchronously.
///
/// `async` version of [`ReadFrom`].
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait ReadFromAsync<'a, Vfs: crate::VfsAsync + 'a>: Sized {
    /// The future type returned by the async read function.
    type Future: Future<Output = Result<Self>> + Send + 'a
    where
        Self: 'a;

    /// Asynchronously reads the structure from the specified path,
    /// which can be either a file or a directory.
    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteToAsync<Vfs: crate::VfsAsync> {
    /// The future type returned by the async write function.
    type Future<'a>: Future<Output = Result<()>> + Send
    where
        Self: 'a,
        Vfs: 'a;

    /// Asynchronously writes the structure to the specified path.
    fn write_to_async<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a>;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteToAsyncOwned<'a, Vfs: crate::VfsAsync>: Sized {
    /// The future type returned by the async write function.
    type Future: Future<Output = Result<()>> + Send + 'a;

    /// Asynchronously writes the structure to the specified path.
    fn write_to_async_owned(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait FromRefForWriterAsync<'a, Vfs: crate::VfsAsync> {
    /// The inner type to cast.
    type Inner: ?Sized;
    /// The reference type to cast to.
    type Wr: WriteToAsyncOwned<'a, Vfs>;

    /// Casts the reference to the inner type to a [`WriteToAsync`]
    /// reference type.
    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync + 'a> ReadFromAsync<'a, Vfs> for () {
    type Future = future::Ready<Result<Self>>;

    fn read_from_async(_path: PathBuf, _vfs: Pin<&'a Vfs>) -> Self::Future {
        future::ready(Ok(()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<Vfs: crate::VfsAsync> WriteToAsync<Vfs> for () {
    type Future<'a>
        = future::Ready<Result<()>>
    where
        Self: 'a,
        Vfs: 'a;

    fn write_to_async<'a>(&'a self, _path: PathBuf, _vfs: Pin<&'a Vfs>) -> Self::Future<'a> {
        future::ready(Ok(()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: crate::VfsAsync> WriteToAsyncOwned<'a, Vfs> for () {
    type Future = future::Ready<Result<()>>;

    fn write_to_async_owned(self, _path: PathBuf, _vfs: Pin<&Vfs>) -> Self::Future {
        future::ready(Ok(()))
    }
}
