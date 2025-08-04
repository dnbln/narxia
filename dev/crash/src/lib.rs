use std::marker;
use std::path::PathBuf;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use pin_project::pin_project;

pub enum Error {}

pub type Result<T> = std::result::Result<T, Error>;

pub trait ReadFromAsync: Sized {
    type Future: Future<Output = Result<Self>> + Send + 'static
    where
        Self: 'static;

    fn read_from_async(path: PathBuf) -> Self::Future;
}

pub trait WriteToAsync {
    type Future<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;
    fn write_to_async<'a>(&'a self, path: PathBuf) -> Self::Future<'a>;
}

pub trait WriteToAsyncOwned<'a>: Sized {
    type Future: Future<Output = Result<()>> + Send + 'a;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future;
}

#[derive(Debug, Clone, Hash)]
pub struct DeferredRead<T>(pub PathBuf, marker::PhantomData<T>);

#[pin_project(project_replace = DeferredReadWriteFutureProj)]
pub enum DeferredReadWriteFuture<'a, T>
where
    T: ReadFromAsync + WriteToAsyncOwned<'a> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    <T as WriteToAsyncOwned<'a>>::Future: Future<Output = Result<()>> + Unpin,
{
    A {
        v: marker::PhantomData<T>,
        l: marker::PhantomData<&'a ()>,
    },
}

impl<'a, T> Future for DeferredReadWriteFuture<'a, T>
where
    T: ReadFromAsync + WriteToAsyncOwned<'a> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    <T as WriteToAsyncOwned<'a>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Output = Result<()>;

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        todo!()
    }
}

impl<T> WriteToAsync for DeferredRead<T>
where
    T: ReadFromAsync + for<'a> WriteToAsyncOwned<'a> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    for<'a> <T as WriteToAsyncOwned<'a>>::Future: Future<Output = Result<()>> + Unpin,
{
    type Future<'a>
        = DeferredReadWriteFuture<'a, T>
    where
        Self: 'a;

    fn write_to_async(&self, _path: PathBuf) -> Self::Future<'_> {
        todo!()
    }
}

#[pin_project(project_replace = DeferredReadOrOwnWriteFutureProj)]
pub enum DeferredReadOrOwnWriteFuture<'a, T>
where
    T: ReadFromAsync + WriteToAsync + for<'b> WriteToAsyncOwned<'b> + Send + 'static,
    <T as ReadFromAsync>::Future: Future<Output = Result<T>> + Unpin,
    <T as WriteToAsync>::Future<'a>: Future<Output = Result<()>> + Unpin,
    for<'b> <T as WriteToAsyncOwned<'b>>::Future: Future<Output = Result<()>> + Unpin,
{
    Deferred {
        inner: <DeferredRead<T> as WriteToAsync>::Future<'a>,
    },
}
