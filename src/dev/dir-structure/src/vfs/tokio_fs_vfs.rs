//! Tokio file system virtual file system implementation.

// use std::fs as std_fs;
use std::io;
use std::path::PathBuf;
use std::pin::Pin;
use std::task::Poll;

use futures_core::Stream;
use tokio::fs;

use crate::error::Result;
use crate::error::WrapIoError;
use crate::traits::async_vfs::CreateParentDirDefaultFuture;
use crate::traits::async_vfs::IoErrorWrapperFuture;
use crate::traits::async_vfs::VfsAsync;
use crate::traits::async_vfs::WriteSupportingVfsAsync;

/// A [`VfsAsync`] and [`WriteSupportingVfsAsync`] implementation using [`tokio::fs`].
pub struct TokioFsVfs;

impl VfsAsync for TokioFsVfs {
    type ReadFuture<'a>
        = IoErrorWrapperFuture<
        Vec<u8>,
        Pin<Box<dyn Future<Output = io::Result<Vec<u8>>> + Send + 'a>>,
    >
    where
        Self: 'a;

    fn read<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ReadFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::read(path)))
    }

    type ReadStringFuture<'a>
        =
        IoErrorWrapperFuture<String, Pin<Box<dyn Future<Output = io::Result<String>> + Send + 'a>>>
    where
        Self: 'a;

    fn read_string<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ReadStringFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::read_to_string(path)))
    }

    type ExistsFuture<'a>
        = IoErrorWrapperFuture<bool, Pin<Box<dyn Future<Output = io::Result<bool>> + Send + 'a>>>
    where
        Self: 'a;

    fn exists<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ExistsFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::try_exists(path)))
    }

    type DirWalk<'a>
        = imp::DirWalker
    where
        Self: 'a;

    type DirWalkFuture<'a>
        = IoErrorWrapperFuture<
        Self::DirWalk<'a>,
        Pin<Box<dyn Future<Output = io::Result<Self::DirWalk<'a>>> + Send + 'a>>,
    >
    where
        Self: 'a;

    fn walk_dir<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::DirWalkFuture<'a> {
        IoErrorWrapperFuture::new(
            path.clone(),
            Box::pin(async move {
                fs::read_dir(path.clone())
                    .await
                    .map(|inner| imp::DirWalker { inner, path })
            }),
        )
    }
}

impl WriteSupportingVfsAsync for TokioFsVfs {
    type WriteFuture<'a>
        = IoErrorWrapperFuture<(), Pin<Box<dyn Future<Output = io::Result<()>> + Send + 'a>>>
    where
        Self: 'a;

    fn write<'a, 'd: 'a>(
        self: Pin<&'a Self>,
        path: PathBuf,
        data: &'d [u8],
    ) -> Self::WriteFuture<'d> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::write(path, data)))
    }

    type RemoveDirAllFuture<'a>
        = IoErrorWrapperFuture<(), Pin<Box<dyn Future<Output = io::Result<()>> + Send + 'a>>>
    where
        Self: 'a;

    fn remove_dir_all<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::RemoveDirAllFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::remove_dir_all(path)))
    }

    type CreateDirFuture<'a>
        = IoErrorWrapperFuture<(), Pin<Box<dyn Future<Output = io::Result<()>> + Send + 'a>>>
    where
        Self: 'a;

    fn create_dir<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::CreateDirFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::create_dir(path)))
    }

    type CreateDirAllFuture<'a>
        = IoErrorWrapperFuture<(), Pin<Box<dyn Future<Output = io::Result<()>> + Send + 'a>>>
    where
        Self: 'a;

    fn create_dir_all<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::CreateDirAllFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::create_dir_all(path)))
    }

    type CreateParentDirFuture<'a>
        = CreateParentDirDefaultFuture<'a, Self>
    where
        Self: 'a;

    fn create_parent_dir<'a>(
        self: Pin<&'a Self>,
        path: PathBuf,
    ) -> Self::CreateParentDirFuture<'a> {
        let parent = path
            .parent()
            .map_or_else(|| path.join(".."), |p| p.to_path_buf());
        CreateParentDirDefaultFuture::Start {
            vfs: self,
            path: parent,
        }
    }
}

mod imp {
    use std::ffi::OsString;
    use std::task::Context;

    use super::*;

    /// Directory walker for asynchronous file system operations on [`tokio::fs`].
    pub struct DirWalker {
        pub(super) inner: fs::ReadDir,
        pub(super) path: PathBuf,
    }

    impl Stream for DirWalker {
        type Item = Result<(OsString, PathBuf)>;

        fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            match self.inner.poll_next_entry(cx) {
                Poll::Ready(Ok(Some(v))) => Poll::Ready(Some(Ok((v.file_name(), v.path())))),
                Poll::Ready(Ok(None)) => Poll::Ready(None),
                Poll::Ready(Err(e)) => Poll::Ready(Some(Err(e).wrap_io_error_with(&self.path))),
                Poll::Pending => Poll::Pending,
            }
        }
    }
}
