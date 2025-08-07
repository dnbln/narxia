use std::future;
use std::future::Future;
use std::path::PathBuf;

use crate::error::Result;

/// Trait for types / structures that can be
/// read from disk, either from a file or a directory.
///
/// This is the asynchronous counterpart of [`DirStructureItem`].
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait DirStructureItemAsync: ReadFromAsync + WriteToAsync {
    /// Uses the [`ReadFromAsync`] implementation to read the structure from
    /// disk, from the specified path.
    fn read_async(path: impl Into<PathBuf>) -> <Self as ReadFromAsync>::Future
    where
        Self: Sized,
    {
        Self::read_from_async(path.into())
    }

    /// Uses the [`WriteToAsync`] implementation to write the structure
    /// to disk at the specified path.
    fn write_async<'a>(&'a self, path: impl Into<PathBuf>) -> <Self as WriteToAsync>::Future<'a> {
        self.write_to_async(path.into())
    }

    /// Uses the [`WriteToAsyncOwned`] implementation to write the structure
    /// to disk at the specified path.
    fn write_owned_async(
        self,
        path: impl Into<PathBuf>,
    ) -> <Self as WriteToAsyncOwned<'static>>::Future
    where
        Self: for<'a> WriteToAsyncOwned<'a> + Sized,
    {
        self.write_to_async_owned(path.into())
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
// Blanket impl.
impl<T> DirStructureItemAsync for T where T: ReadFromAsync + WriteToAsync {}

/// Trait for types / structures that can be
/// read from disk asynchronously.
///
/// `async` version of [`ReadFrom`].
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait ReadFromAsync: Sized {
    /// The future type returned by the async read function.
    type Future: Future<Output = Result<Self>> + Send + 'static
    where
        Self: 'static;

    /// Asynchronously reads the structure from the specified path,
    /// which can be either a file or a directory.
    fn read_from_async(path: PathBuf) -> Self::Future;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteToAsync {
    /// The future type returned by the async write function.
    type Future<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;

    /// Asynchronously writes the structure to the specified path.
    fn write_to_async<'a>(&'a self, path: PathBuf) -> Self::Future<'a>;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteToAsyncOwned<'a>: Sized {
    /// The future type returned by the async write function.
    type Future: Future<Output = Result<()>> + Send + 'a;

    /// Asynchronously writes the structure to the specified path.
    fn write_to_async_owned(self, path: PathBuf) -> Self::Future;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait FromRefForWriterAsync<'a> {
    /// The inner type to cast.
    type Inner: ?Sized;
    /// The reference type to cast to.
    type Wr: WriteToAsyncOwned<'a>;

    /// Casts the reference to the inner type to a [`WriteToAsync`]
    /// reference type.
    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl ReadFromAsync for () {
    type Future = future::Ready<Result<Self>>;

    fn read_from_async(_path: PathBuf) -> Self::Future {
        future::ready(Ok(()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl WriteToAsync for () {
    type Future<'a> = future::Ready<Result<()>>;

    fn write_to_async<'a>(&'a self, _path: PathBuf) -> Self::Future<'a> {
        future::ready(Ok(()))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a> WriteToAsyncOwned<'a> for () {
    type Future = future::Ready<Result<()>>;

    fn write_to_async_owned(self, _path: PathBuf) -> Self::Future {
        future::ready(Ok(()))
    }
}
