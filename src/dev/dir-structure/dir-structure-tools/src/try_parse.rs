//! A wrapper that tries to parse a value of type `T`, keeping the original error if it fails.
//!
//! See [`TryParse`] for more details.

#[cfg(feature = "async")]
use std::future;
#[cfg(feature = "async")]
use std::future::poll_fn;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Poll;
#[cfg(feature = "async")]
use std::task::ready;

use dir_structure::error::Error;
use dir_structure::error::VfsResult;
use dir_structure::prelude::*;
#[cfg(feature = "async")]
use dir_structure::traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
use dir_structure::traits::async_vfs::WriteSupportingVfsAsync;
use dir_structure::traits::vfs;
use dir_structure::traits::vfs::PathType;

/// A type that tries to parse a value of type `T`, but doesn't fail if it can't.
///
/// Instead, it keeps the original [`Error`].
pub enum TryParse<T, P: PathType + ?Sized> {
    /// Successfully parsed a value of type `T`.
    Success(T),

    /// Failed to parse a value of type `T`.
    Failure(Error<P::OwnedPath>),
}

impl<'vfs, Vfs, T, P> ReadFrom<'vfs, Vfs> for TryParse<T, P>
where
    Vfs: vfs::Vfs<'vfs, Path = P>,
    P: PathType + ?Sized + 'vfs,
    T: ReadFrom<'vfs, Vfs>,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<Self, Vfs> {
        match T::read_from(path, vfs) {
            Ok(value) => Ok(TryParse::Success(value)),
            Err(error) => Ok(TryParse::Failure(error)),
        }
    }
}

impl<'vfs, Vfs, T, P> WriteTo<'vfs, Vfs> for TryParse<T, P>
where
    P: PathType + ?Sized + 'vfs,
    Vfs: vfs::WriteSupportingVfs<'vfs, Path = P>,
    T: WriteTo<'vfs, Vfs>,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<(), Vfs> {
        match self {
            Self::Success(value) => value.write_to(path, vfs),
            Self::Failure(_error) => Ok(()),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs, T, P> ReadFromAsync<'vfs, Vfs> for TryParse<T, P>
where
    P: PathType + 'vfs,
    Vfs: VfsAsync<Path = P> + 'vfs,
    T: ReadFromAsync<'vfs, Vfs> + Send + 'vfs,
{
    type Future = Pin<Box<dyn Future<Output = VfsResult<Self, Vfs>> + Send + 'vfs>>;

    fn read_from_async(path: P::OwnedPath, vfs: Pin<&'vfs Vfs>) -> Self::Future {
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
impl<'vfs, Vfs, T, P> WriteToAsync<'vfs, Vfs> for TryParse<T, P>
where
    P: PathType + ?Sized + 'vfs,
    Vfs: WriteSupportingVfsAsync<Path = P> + 'vfs,
    T: WriteToAsync<'vfs, Vfs> + Send + 'vfs,
    <T as WriteToAsync<'vfs, Vfs>>::Future: Future<Output = VfsResult<(), Vfs>> + Unpin,
{
    type Future = Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + 'vfs>>;

    fn write_to_async(self, path: P::OwnedPath, vfs: Pin<&'vfs Vfs>) -> Self::Future {
        match self {
            Self::Success(value) => Box::pin(value.write_to_async(path, vfs)),
            Self::Failure(_error) => Box::pin(future::ready(Ok(()))),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs, T, P> WriteToAsyncRef<'vfs, Vfs> for TryParse<T, P>
where
    P: PathType + ?Sized + 'vfs,
    Vfs: WriteSupportingVfsAsync<Path = P> + 'static,
    T: WriteToAsyncRef<'vfs, Vfs> + Send + 'vfs,
    for<'a> <T as WriteToAsyncRef<'vfs, Vfs>>::Future<'a>:
        Future<Output = VfsResult<(), Vfs>> + Send + Sync + Unpin + 'a,
{
    type Future<'a>
        = Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + Sync + 'a>>
    where
        Self: 'a,
        'vfs: 'a,
        T: 'a,
        Vfs: 'a;

    fn write_to_async_ref<'a>(&'a self, path: P::OwnedPath, vfs: Pin<&'a Vfs>) -> Self::Future<'a>
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
