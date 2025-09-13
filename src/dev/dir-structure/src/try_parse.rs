//! A wrapper that tries to parse a value of type `T`, keeping the original error if it fails.
//!
//! See [`TryParse`] for more details.

#[cfg(feature = "async")]
use std::future;
#[cfg(feature = "async")]
use std::future::poll_fn;
use std::path::Path;
#[cfg(feature = "async")]
use std::path::PathBuf;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Poll;
#[cfg(feature = "async")]
use std::task::ready;

use crate::error::Error;
use crate::error::Result;
use crate::prelude::*;
#[cfg(feature = "async")]
use crate::traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::WriteSupportingVfsAsync;
use crate::traits::vfs;

/// A type that tries to parse a value of type `T`, but doesn't fail if it can't.
///
/// Instead, it keeps the original [`Error`].
pub enum TryParse<T> {
    /// Successfully parsed a value of type `T`.
    Success(T),

    /// Failed to parse a value of type `T`.
    Failure(Error),
}

impl<'vfs, Vfs, T> ReadFrom<'vfs, Vfs> for TryParse<T>
where
    Vfs: vfs::Vfs<'vfs>,
    T: ReadFrom<'vfs, Vfs>,
{
    fn read_from(path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<Self> {
        match T::read_from(path, vfs) {
            Ok(value) => Ok(TryParse::Success(value)),
            Err(error) => Ok(TryParse::Failure(error)),
        }
    }
}

impl<'vfs, Vfs, T> WriteTo<'vfs, Vfs> for TryParse<T>
where
    Vfs: vfs::WriteSupportingVfs<'vfs>,
    T: WriteTo<'vfs, Vfs>,
{
    fn write_to(&self, path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<()> {
        match self {
            Self::Success(value) => value.write_to(path, vfs),
            Self::Failure(_error) => Ok(()),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs, T> ReadFromAsync<'vfs, Vfs> for TryParse<T>
where
    Vfs: VfsAsync + 'vfs,
    T: ReadFromAsync<'vfs, Vfs> + Send + 'vfs,
{
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'vfs>>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'vfs Vfs>) -> Self::Future {
        let mut read_fut = Box::pin(T::read_from_async(path, vfs));
        Box::pin(poll_fn(move |cx| {
            let result = ready!(read_fut.as_mut().poll(cx));
            match result {
                Ok(value) => Poll::Ready(Ok(TryParse::Success(value))),
                Err(error) => Poll::Ready(Ok(TryParse::Failure(error))),
            }
        }))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs, T> WriteToAsync<'vfs, Vfs> for TryParse<T>
where
    Vfs: WriteSupportingVfsAsync + 'vfs,
    T: WriteToAsync<'vfs, Vfs> + Send + 'vfs,
    <T as WriteToAsync<'vfs, Vfs>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'vfs>>;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'vfs Vfs>) -> Self::Future {
        match self {
            Self::Success(value) => Box::pin(value.write_to_async(path, vfs)),
            Self::Failure(_error) => Box::pin(future::ready(Ok(()))),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs, T> WriteToAsyncRef<'vfs, Vfs> for TryParse<T>
where
    Vfs: WriteSupportingVfsAsync + 'static,
    T: WriteToAsyncRef<'vfs, Vfs> + Send + 'vfs,
    for<'a> <T as WriteToAsyncRef<'vfs, Vfs>>::Future<'a>:
        Future<Output = Result<()>> + Send + Sync + Unpin + 'a,
{
    type Future<'a>
        = Pin<Box<dyn Future<Output = Result<()>> + Send + Sync + 'a>>
    where
        Self: 'a,
        'vfs: 'a,
        T: 'a,
        Vfs: 'a;

    fn write_to_async_ref<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a>
    where
        'vfs: 'a,
    {
        use std::future::poll_fn;

        let mut wr: Option<Pin<Box<<T as WriteToAsyncRef<'vfs, Vfs>>::Future<'a>>>> = match self {
            Self::Success(value) => Some(Box::pin(value.write_to_async_ref(path, vfs))),
            Self::Failure(_error) => None,
        };

        Box::pin(poll_fn(move |cx| match &mut wr {
            Some(wr) => wr.as_mut().poll(cx),
            None => Poll::Ready(Ok(())),
        }))
    }
}
