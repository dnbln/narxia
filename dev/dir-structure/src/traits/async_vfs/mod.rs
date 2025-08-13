use std::ffi::OsString;
use std::fs;
use std::path::PathBuf;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use futures_core::Stream;
use pin_project::pin_project;

use crate::Result;

pub trait VfsAsync: Send + Sync {
    type ReadFuture<'a>: Future<Output = Result<Vec<u8>>> + Send + 'a
    where
        Self: 'a;

    fn read<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ReadFuture<'a>;

    type ReadStringFuture<'a>: Future<Output = Result<String>> + Send + 'a
    where
        Self: 'a;
    fn read_string<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ReadStringFuture<'a>;

    type WriteFuture<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;

    fn write<'a, 'd: 'a>(
        self: Pin<&'a Self>,
        path: PathBuf,
        data: &'d [u8],
    ) -> Self::WriteFuture<'d>;

    type ExistsFuture<'a>: Future<Output = Result<bool>> + Send + 'a
    where
        Self: 'a;

    fn exists<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ExistsFuture<'a>;

    type RemoveDirAllFuture<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;
    fn remove_dir_all<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::RemoveDirAllFuture<'a>;

    type CreateDirFuture<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;
    fn create_dir<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::CreateDirFuture<'a>;

    type CreateDirAllFuture<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;
    fn create_dir_all<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::CreateDirAllFuture<'a>;

    type CreateParentDirFuture<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;
    fn create_parent_dir<'a>(self: Pin<&'a Self>, path: PathBuf)
    -> Self::CreateParentDirFuture<'a>;
    type StatFuture<'a>: Future<Output = Result<fs::Metadata>> + Send + 'a
    where
        Self: 'a;
    fn stat<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::StatFuture<'a>;

    type DirWalk<'a>: Stream<Item = Result<(OsString, PathBuf)>> + Send + 'a
    where
        Self: 'a;
    type DirWalkFuture<'a>: Future<Output = Result<Self::DirWalk<'a>>> + Send + 'a
    where
        Self: 'a;
    fn walk_dir<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::DirWalkFuture<'a>;
}

#[pin_project(project_replace = CreateParentDirDefaultFutureProjOwn)]
pub enum CreateParentDirDefaultFuture<'a, Vfs: VfsAsync + 'a>
where
    for<'f> Vfs::ExistsFuture<'f>: Future<Output = Result<bool>> + Unpin,
    for<'f> Vfs::CreateDirAllFuture<'f>: Future<Output = Result<()>> + Unpin,
{
    Poison,
    Start {
        vfs: Pin<&'a Vfs>,
        path: PathBuf,
    },
    ExistsFuture {
        vfs: Pin<&'a Vfs>,
        path: PathBuf,
        exists_future: Vfs::ExistsFuture<'a>,
    },
    CreateDirAllFuture {
        vfs: Pin<&'a Vfs>,
        create_dir_all_future: Vfs::CreateDirAllFuture<'a>,
    },
}

impl<'a, Vfs: VfsAsync + 'a> Future for CreateParentDirDefaultFuture<'a, Vfs>
where
    for<'f> Vfs::ExistsFuture<'f>: Future<Output = Result<bool>> + Unpin,
    for<'f> Vfs::CreateDirAllFuture<'f>: Future<Output = Result<()>> + Unpin,
{
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);
        match this {
            CreateParentDirDefaultFutureProjOwn::Start { vfs, path } => {
                self.project_replace(Self::ExistsFuture {
                    exists_future: vfs.exists(path.clone()),
                    vfs,
                    path,
                });
                cx.waker().wake_by_ref();
                Poll::Pending
            }
            CreateParentDirDefaultFutureProjOwn::ExistsFuture {
                vfs,
                path,
                mut exists_future,
            } => match Pin::new(&mut exists_future).poll(cx) {
                Poll::Ready(Ok(true)) => Poll::Ready(Ok(())),
                Poll::Ready(Ok(false)) => {
                    self.project_replace(Self::CreateDirAllFuture {
                        create_dir_all_future: vfs.create_dir_all(path),
                        vfs,
                    });
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                Poll::Pending => {
                    self.project_replace(Self::ExistsFuture {
                        vfs,
                        path,
                        exists_future,
                    });
                    Poll::Pending
                }
            },
            CreateParentDirDefaultFutureProjOwn::CreateDirAllFuture {
                vfs,
                mut create_dir_all_future,
            } => match Pin::new(&mut create_dir_all_future).poll(cx) {
                Poll::Ready(Ok(())) => Poll::Ready(Ok(())),
                Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                Poll::Pending => {
                    self.project_replace(Self::CreateDirAllFuture {
                        vfs,
                        create_dir_all_future,
                    });
                    Poll::Pending
                }
            },
            CreateParentDirDefaultFutureProjOwn::Poison => {
                panic!("CreateParentDirDefaultFuture polled after completion")
            }
        }
    }
}

#[pin_project(project = IoErrorWrapperFutureProj)]
pub struct IoErrorWrapperFuture<T, F: Future<Output = std::io::Result<T>>> {
    #[pin]
    future: F,
    path: PathBuf,
}

impl<T, F> IoErrorWrapperFuture<T, F>
where
    F: Future<Output = std::io::Result<T>>,
{
    pub fn new(path: PathBuf, future: F) -> Self {
        Self { future, path }
    }
}

impl<T, F> Future for IoErrorWrapperFuture<T, F>
where
    F: Future<Output = std::io::Result<T>>,
{
    type Output = Result<T>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project();
        match this.future.poll(cx) {
            Poll::Ready(Ok(value)) => Poll::Ready(Ok(value)),
            Poll::Ready(Err(e)) => {
                let path = self.path.clone();
                Poll::Ready(Err(crate::Error::Io(path, e)))
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
mod tokio_fs_vfs;

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
pub use tokio_fs_vfs::TokioFsVfs;
