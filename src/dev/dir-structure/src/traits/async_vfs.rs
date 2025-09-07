//! Asynchronous virtual file system traits.

use std::io;
use std::path::PathBuf;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use futures::AsyncWrite;
use futures::Stream;
use futures::io::AsyncBufRead;
use futures::io::AsyncSeek;
use pin_project::pin_project;

use crate::error::Error;
use crate::error::Result;
use crate::prelude::*;
use crate::traits::vfs::DirEntryInfo;

/// An asynchronous virtual file system. Writing operations are provided by the [`WriteSupportingVfsAsync` trait](self::WriteSupportingVfsAsync).
pub trait VfsAsync: Send + Sync + Unpin {
    /// The type of the file returned by the [`open_read` method](VfsAsync::open_read).
    type RFile: AsyncBufRead + Send + Unpin;
    /// The future returned by the [`open_read` method](VfsAsync::open_read).
    type OpenReadFuture: Future<Output = Result<Self::RFile>> + Send + Unpin;

    /// Opens a file for reading, at the specified path.
    fn open_read(self: Pin<&Self>, path: PathBuf) -> Self::OpenReadFuture;

    /// The future returned by the [`read` method](VfsAsync::read).
    type ReadFuture<'a>: Future<Output = Result<Vec<u8>>> + Send + Unpin + 'a
    where
        Self: 'a;

    /// Reads the contents of a file, at the specified path.
    fn read<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ReadFuture<'a>;

    /// The future returned by the [`read_string` method](VfsAsync::read_string).
    type ReadStringFuture<'a>: Future<Output = Result<String>> + Send + Unpin + 'a
    where
        Self: 'a;

    /// Reads the contents of a file, at the specified path, and returns it as a string.
    fn read_string<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ReadStringFuture<'a>;

    /// The future returned by the [`exists` method](VfsAsync::exists).
    type ExistsFuture<'a>: Future<Output = Result<bool>> + Send + 'a
    where
        Self: 'a;

    /// Checks if a file exists at the specified path.
    fn exists<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ExistsFuture<'a>;

    /// The stream type returned by the [`DirWalkFuture`](VfsAsync::DirWalkFuture).
    type DirWalk<'a>: Stream<Item = Result<DirEntryInfo>> + Send + 'a
    where
        Self: 'a;
    /// The future type returned by the [`walk_dir` method](VfsAsync::walk_dir).
    type DirWalkFuture<'a>: Future<Output = Result<Self::DirWalk<'a>>> + Send + 'a
    where
        Self: 'a;
    /// Walks a directory at the given path, returning a stream of directory entries.
    fn walk_dir<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::DirWalkFuture<'a>;
}

/// Marks that the [`RFile`](VfsAsync::RFile) type of this [`VfsAsync`] also implements
/// [`AsyncSeek`](futures::io::AsyncSeek), allowing it to be used in contexts that require seeking, such as image decoding.
///
/// This trait is automatically implemented for any [`VfsAsync`] whose [`RFile`](VfsAsync::RFile) implements
/// [`AsyncSeek`](futures::io::AsyncSeek).
pub trait VfsAsyncWithSeekRead: VfsAsync
where
    Self::RFile: AsyncSeek + Send + Unpin,
{
}

impl<T: VfsAsync> VfsAsyncWithSeekRead for T where T::RFile: AsyncSeek + Send + Unpin {}

/// Extension trait for [`VfsAsync`] that provides additional convenience methods.
pub trait VfsAsyncExt: VfsAsync {
    /// Reads a file / directory at the specified path, and parses it into the specified type using its
    /// [`ReadFromAsync`] implementation.
    ///
    /// This method takes `self` as a pinned reference, to ensure that the `VfsAsync` implementation
    /// is not moved while the read operation is in progress.
    fn read_typed_async_pinned<'a, T: ReadFromAsync<'a, Self>>(
        self: Pin<&'a Self>,
        path: impl Into<PathBuf>,
    ) -> T::Future {
        T::read_from_async(path.into(), self)
    }

    /// Reads a file / directory at the specified path, and parses it into the specified type using its
    /// [`ReadFromAsync`] implementation.
    ///
    /// This method takes `self` as a regular reference, and pins it internally.
    fn read_typed_async<'a, T: ReadFromAsync<'a, Self>>(
        &'a self,
        path: impl Into<PathBuf>,
    ) -> T::Future {
        Pin::new(self).read_typed_async_pinned::<T>(path)
    }
}

// Blanket impl.
impl<V: VfsAsync + ?Sized> VfsAsyncExt for V {}

/// A virtual file system that supports writing operations.
pub trait WriteSupportingVfsAsync: VfsAsync {
    /// The type of the file returned by the [`open_write` method](WriteSupportingVfsAsync::open_write).
    type WFile: AsyncWrite + Send + Unpin;

    /// The future type returned by the [`open_write` method](WriteSupportingVfsAsync::open_write).
    type OpenWriteFuture: Future<Output = Result<Self::WFile>> + Send + Unpin;

    /// Opens a file for writing, at the specified path.
    fn open_write(self: Pin<&Self>, path: PathBuf) -> Self::OpenWriteFuture;

    /// The future type returned by the [`write` method](WriteSupportingVfsAsync::write).
    type WriteFuture<'a>: Future<Output = Result<()>> + Send + Unpin + 'a
    where
        Self: 'a;

    /// Writes the contents of a file, at the specified path.
    fn write<'a, 'd: 'a>(
        self: Pin<&'a Self>,
        path: PathBuf,
        data: &'d [u8],
    ) -> Self::WriteFuture<'d>;

    /// The future type returned by the [`remove_dir_all` method](WriteSupportingVfsAsync::remove_dir_all).
    type RemoveDirAllFuture<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;
    /// Removes a directory and all its contents.
    fn remove_dir_all<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::RemoveDirAllFuture<'a>;

    /// The future type returned by the [`create_dir` method](WriteSupportingVfsAsync::create_dir).
    type CreateDirFuture<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;
    /// Creates a new directory at the specified path.
    fn create_dir<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::CreateDirFuture<'a>;

    /// The future type returned by the [`create_dir_all` method](WriteSupportingVfsAsync::create_dir_all).
    type CreateDirAllFuture<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;
    /// Creates a new directory and all its parent directories at the specified path.
    fn create_dir_all<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::CreateDirAllFuture<'a>;

    /// The future type returned by the [`create_parent_dir` method](WriteSupportingVfsAsync::create_parent_dir).
    type CreateParentDirFuture<'a>: Future<Output = Result<()>> + Send + 'a
    where
        Self: 'a;
    /// Creates a new parent directory at the specified path.
    fn create_parent_dir<'a>(self: Pin<&'a Self>, path: PathBuf)
    -> Self::CreateParentDirFuture<'a>;
}

/// Marks that the [`WFile`](WriteSupportingVfsAsync::WFile) type of this [`WriteSupportingVfsAsync`] also implements
/// [`AsyncSeek`](futures::io::AsyncSeek), allowing it to be used in
/// contexts that require seeking.
pub trait VfsAsyncWithSeekWrite: WriteSupportingVfsAsync
where
    Self::WFile: AsyncSeek + Send + Unpin,
{
}

impl<T: WriteSupportingVfsAsync> VfsAsyncWithSeekWrite for T where T::WFile: AsyncSeek + Send + Unpin
{}

/// Extension trait for [`WriteSupportingVfsAsync`] that provides additional convenience methods.
pub trait WriteSupportingVfsAsyncExt: WriteSupportingVfsAsync {
    /// Writes a file / directory at the specified path, using the specified data type's
    /// [`WriteToAsync`] implementation.
    ///
    /// This method takes `self` as a pinned reference, to ensure that the `VfsAsync` implementation
    /// is not moved while the write operation is in progress.
    fn write_typed_async_ref_pinned<'r, 'a: 'r, T: WriteToAsyncRef<'a, Self>>(
        self: Pin<&'r Self>,
        path: impl Into<PathBuf>,
        value: &'r T,
    ) -> T::Future<'r> {
        T::write_to_async_ref(value, path.into(), self)
    }

    /// Writes a file / directory at the specified path, using the specified data type's
    /// [`WriteToAsync`] implementation.
    ///
    /// This method takes `self` as a regular reference, and pins it internally.
    fn write_typed_async_ref<'r, 'a: 'r, T: WriteToAsyncRef<'a, Self>>(
        &'r self,
        path: impl Into<PathBuf>,
        data: &'r T,
    ) -> T::Future<'r>
    where
        Self: Unpin,
    {
        Pin::new(self).write_typed_async_ref_pinned(path, data)
    }

    /// Writes a file / directory at the specified path, using the specified data type's
    /// [`WriteToAsync`] implementation.
    ///
    /// This method takes `self` as a pinned reference, to ensure that the `VfsAsync` implementation
    /// is not moved while the write operation is in progress.
    fn write_typed_async_pinned<'a, T: WriteToAsync<'a, Self>>(
        self: Pin<&'a Self>,
        path: impl Into<PathBuf>,
        value: T,
    ) -> T::Future {
        value.write_to_async(path.into(), self)
    }

    /// Writes a file / directory at the specified path, using the specified data type's
    /// [`WriteToAsync`] implementation.
    ///
    /// This method takes `self` as a regular reference, and pins it internally.
    fn write_typed_async<'a, T: WriteToAsync<'a, Self>>(
        &'a self,
        path: impl Into<PathBuf>,
        value: T,
    ) -> T::Future
    where
        Self: Unpin,
    {
        Pin::new(self).write_typed_async_pinned(path, value)
    }
}

// Blanket impl.
impl<V: WriteSupportingVfsAsync + ?Sized> WriteSupportingVfsAsyncExt for V {}

#[pin_project(project_replace = CreateParentDirDefaultFutureProjOwn)]
#[doc(hidden)]
pub enum CreateParentDirDefaultFuture<'a, Vfs: WriteSupportingVfsAsync + 'a>
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

impl<'a, Vfs: WriteSupportingVfsAsync + 'a> Future for CreateParentDirDefaultFuture<'a, Vfs>
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
#[doc(hidden)]
pub struct IoErrorWrapperFuture<T, F: Future<Output = io::Result<T>>> {
    #[pin]
    future: F,
    path: PathBuf,
}

impl<T, F> IoErrorWrapperFuture<T, F>
where
    F: Future<Output = io::Result<T>>,
{
    pub fn new(path: PathBuf, future: F) -> Self {
        Self { future, path }
    }
}

impl<T, F> Future for IoErrorWrapperFuture<T, F>
where
    F: Future<Output = io::Result<T>>,
{
    type Output = Result<T>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project();
        match this.future.poll(cx) {
            Poll::Ready(Ok(value)) => Poll::Ready(Ok(value)),
            Poll::Ready(Err(e)) => {
                let path = self.path.clone();
                Poll::Ready(Err(Error::Io(path, e)))
            }
            Poll::Pending => Poll::Pending,
        }
    }
}
