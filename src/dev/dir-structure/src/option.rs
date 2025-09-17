//! `Option<T>` implementations.

#[cfg(any(feature = "resolve-path"))]
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
use crate::traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::WriteSupportingVfsAsync;
#[cfg(feature = "resolve-path")]
use crate::traits::resolve::DynamicHasField;
#[cfg(feature = "resolve-path")]
use crate::traits::resolve::HAS_FIELD_MAX_LEN;
#[cfg(feature = "resolve-path")]
use crate::traits::resolve::HasField;
use crate::traits::vfs;
use crate::traits::vfs::PathType;

impl<'a, T, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for Option<T>
where
    T: ReadFrom<'a, Vfs>,
{
    fn read_from(
        path: &Vfs::Path,
        vfs: Pin<&'a Vfs>,
    ) -> Result<Self, <Vfs::Path as PathType>::OwnedPath>
    where
        Self: Sized,
    {
        if vfs.exists(path)? {
            T::read_from(path, vfs).map(Some)
        } else {
            Ok(None)
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = OptionReadFromAsyncFutureOwnProj)]
#[doc(hidden)]
pub enum OptionReadFromAsyncFuture<'a, T, P: PathType + ?Sized + 'a, Vfs: VfsAsync<Path = P> + 'a>
where
    T: ReadFromAsync<'a, Vfs> + 'static,
    Vfs::ExistsFuture<'a>: Future<Output = Result<bool, P::OwnedPath>>,
    T::Future: Future<Output = Result<T, P::OwnedPath>> + Unpin,
{
    Poison,
    Check {
        path: P::OwnedPath,
        check_fut: Pin<Box<Vfs::ExistsFuture<'a>>>,
        vfs: Pin<&'a Vfs>,
    },
    HasContents {
        inner: T::Future,
        vfs: Pin<&'a Vfs>,
    },
    NoContents,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: VfsAsync<Path = P> + 'a, P: PathType + ?Sized + 'a> Future
    for OptionReadFromAsyncFuture<'a, T, P, Vfs>
where
    T: ReadFromAsync<'a, Vfs> + 'static,
    T::Future: Future<Output = Result<T, P::OwnedPath>> + Unpin,
{
    type Output = Result<Option<T>, P::OwnedPath>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        use std::task::Poll;

        let this = self.as_mut().project_replace(Self::Poison);
        match this {
            OptionReadFromAsyncFutureOwnProj::Check {
                path,
                mut check_fut,
                vfs,
            } => {
                match check_fut.as_mut().poll(cx) {
                    Poll::Ready(Ok(true)) => {
                        self.project_replace(Self::HasContents {
                            inner: T::read_from_async(path, vfs),
                            vfs,
                        });
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(Ok(false)) => {
                        // If the path does not exist, we return None
                        Poll::Ready(Ok(None))
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        // If the check is still pending, we return Pending
                        self.project_replace(Self::Check {
                            path,
                            check_fut,
                            vfs,
                        });
                        Poll::Pending
                    }
                }
            }
            OptionReadFromAsyncFutureOwnProj::HasContents { mut inner, vfs } => {
                match Pin::new(&mut inner).poll(cx) {
                    Poll::Ready(v) => Poll::Ready(v.map(Some)),
                    Poll::Pending => {
                        self.project_replace(Self::HasContents { inner, vfs });
                        Poll::Pending
                    }
                }
            }
            OptionReadFromAsyncFutureOwnProj::NoContents => {
                // If there are no contents, we return None
                Poll::Ready(Ok(None))
            }
            OptionReadFromAsyncFutureOwnProj::Poison => {
                panic!("OptionReadFromAsyncFuture was polled after it was replaced with Poison");
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: VfsAsync<Path = P> + 'a, P: PathType + ?Sized + 'a> ReadFromAsync<'a, Vfs>
    for Option<T>
where
    T: ReadFromAsync<'a, Vfs> + 'static,
    T::Future: Future<Output = Result<T, P::OwnedPath>> + Unpin + 'a,
{
    type Future
        = OptionReadFromAsyncFuture<'a, T, P, Vfs>
    where
        Self: 'static;

    fn read_from_async(path: P::OwnedPath, vfs: Pin<&'a Vfs>) -> Self::Future {
        OptionReadFromAsyncFuture::Check {
            check_fut: Box::pin(vfs.exists(path.clone())),
            path,
            vfs,
        }
    }
}

impl<'vfs, T, P: PathType + 'vfs, Vfs: vfs::WriteSupportingVfs<'vfs, Path = P>> WriteTo<'vfs, Vfs>
    for Option<T>
where
    T: WriteTo<'vfs, Vfs>,
{
    fn write_to(&self, path: &P, vfs: Pin<&'vfs Vfs>) -> Result<(), P::OwnedPath> {
        if let Some(v) = self {
            v.write_to(path, vfs)
        } else {
            Ok(())
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project = OptionWriteToAsyncFutureProj)]
#[doc(hidden)]
pub enum OptionWriteToAsyncFuture<'a, T, Vfs: WriteSupportingVfsAsync + 'a>
where
    T: WriteToAsync<'a, Vfs> + 'static,
{
    HasContents {
        #[pin]
        inner: <T as WriteToAsync<'a, Vfs>>::Future,
    },
    NoContents,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync<Path = P>, P: PathType + ?Sized + 'a> Future
    for OptionWriteToAsyncFuture<'a, T, Vfs>
where
    T: WriteToAsync<'a, Vfs> + 'static,
{
    type Output = Result<(), P::OwnedPath>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        match this {
            OptionWriteToAsyncFutureProj::HasContents { inner } => inner.poll(cx),
            OptionWriteToAsyncFutureProj::NoContents => Poll::Ready(Ok(())),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for Option<T>
where
    T: WriteToAsync<'a, Vfs> + Send + 'static,
{
    type Future = OptionWriteToAsyncFuture<'a, T, Vfs>;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        if let Some(v) = self {
            OptionWriteToAsyncFuture::HasContents {
                inner: v.write_to_async(path, vfs),
            }
        } else {
            OptionWriteToAsyncFuture::NoContents
        }
    }
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
impl<const NAME: [char; HAS_FIELD_MAX_LEN], T> HasField<NAME> for Option<T>
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
impl<T> DynamicHasField for Option<T>
where
    T: DynamicHasField,
{
    type Inner = <T as DynamicHasField>::Inner;

    fn resolve_path(p: PathBuf, name: &str) -> PathBuf {
        T::resolve_path(p, name)
    }
}
