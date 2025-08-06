use std::path::Path;
#[cfg(any(feature = "async", feature = "resolve-path"))]
use std::path::PathBuf;
#[cfg(feature = "async")]
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

#[cfg(feature = "resolve-path")]
use crate::DynamicHasField;
#[cfg(feature = "resolve-path")]
use crate::HAS_FIELD_MAX_LEN;
#[cfg(feature = "resolve-path")]
use crate::HasField;
use crate::ReadFrom;
#[cfg(feature = "async")]
use crate::ReadFromAsync;
use crate::Result;
use crate::WriteTo;
#[cfg(feature = "async")]
use crate::WriteToAsync;
#[cfg(feature = "async")]
use crate::WriteToAsyncOwned;

impl<T> ReadFrom for Option<T>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        if path.exists() {
            T::read_from(path).map(Some)
        } else {
            Ok(None)
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project = OptionReadFromAsyncFutureProj)]
pub enum OptionReadFromAsyncFuture<T>
where
    T: ReadFromAsync + 'static,
{
    HasContents {
        #[pin]
        inner: T::Future,
    },
    NoContents,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T> Future for OptionReadFromAsyncFuture<T>
where
    T: ReadFromAsync + 'static,
{
    type Output = Result<Option<T>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        use std::task::Poll;

        let this = self.project();
        match this {
            OptionReadFromAsyncFutureProj::HasContents { inner } => match inner.poll(cx) {
                Poll::Ready(v) => Poll::Ready(v.map(Some)),
                Poll::Pending => Poll::Pending,
            },
            OptionReadFromAsyncFutureProj::NoContents => {
                // If there are no contents, we return None
                Poll::Ready(Ok(None))
            }
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T> ReadFromAsync for Option<T>
where
    T: ReadFromAsync + 'static,
{
    type Future
        = OptionReadFromAsyncFuture<T>
    where
        Self: 'static;

    fn read_from_async(path: PathBuf) -> Self::Future {
        if path.exists() {
            OptionReadFromAsyncFuture::HasContents {
                inner: T::read_from_async(path),
            }
        } else {
            OptionReadFromAsyncFuture::NoContents
        }
    }
}

impl<T> WriteTo for Option<T>
where
    T: WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        if let Some(v) = self {
            v.write_to(path)
        } else {
            Ok(())
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project = OptionWriteToAsyncFutureProj)]
pub enum OptionWriteToAsyncFuture<'a, T>
where
    T: WriteToAsync + 'static,
{
    HasContents {
        #[pin]
        inner: <T as WriteToAsync>::Future<'a>,
    },
    NoContents,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> Future for OptionWriteToAsyncFuture<'a, T>
where
    T: WriteToAsync + 'static,
{
    type Output = Result<()>;

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
impl<T> WriteToAsync for Option<T>
where
    T: WriteToAsync + Send + 'static,
{
    type Future<'a>
        = OptionWriteToAsyncFuture<'a, T>
    where
        Self: 'a;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        if let Some(v) = self {
            OptionWriteToAsyncFuture::HasContents {
                inner: v.write_to_async(path),
            }
        } else {
            OptionWriteToAsyncFuture::NoContents
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project = OptionWriteToAsyncOwnedFutureProj)]
pub enum OptionWriteToAsyncOwnedFuture<'a, T>
where
    T: WriteToAsyncOwned<'a> + 'static,
{
    HasContents {
        #[pin]
        inner: <T as WriteToAsyncOwned<'a>>::Future,
    },
    NoContents,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> Future for OptionWriteToAsyncOwnedFuture<'a, T>
where
    T: WriteToAsyncOwned<'a> + 'static,
{
    type Output = Result<()>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        use std::task::Poll;

        let this = self.project();
        match this {
            OptionWriteToAsyncOwnedFutureProj::HasContents { inner } => inner.poll(cx),
            OptionWriteToAsyncOwnedFutureProj::NoContents => Poll::Ready(Ok(())),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> WriteToAsyncOwned<'a> for Option<T>
where
    T: WriteToAsyncOwned<'a> + Send + 'static,
{
    type Future
        = OptionWriteToAsyncOwnedFuture<'a, T>
    where
        Self: 'a;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        if let Some(v) = self {
            OptionWriteToAsyncOwnedFuture::HasContents {
                inner: v.write_to_async_owned(path),
            }
        } else {
            OptionWriteToAsyncOwnedFuture::NoContents
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
