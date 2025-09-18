//! Asynchronous virtual file system traits.
//!
//! These traits are similar to the ones in the [`vfs`](super::vfs) module, but they use
//! asynchronous I/O operations, and return `Future`s resolving to `Result`s instead of
//! directly returning `Result`s, in line with Rust's async programming model.
//!
//! These traits require the `async` feature to be enabled.
//!
//! They are designed to be used with async runtimes like Tokio or async-std.
//!
//! You should refer to the documentation of the [`vfs`](super::vfs) module for
//! more details on the individual methods, as the async versions have the same semantics,
//! just with async I/O.
//!
//! An important difference between the [`VfsAsync`] traits and their synchronous counterparts
//! is that the methods take _owned_ paths instead of references. This is because
//! the async methods typically need to move the path into the future, and
//! references would not be valid for the entire duration of the future.

use std::io;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use futures::AsyncWrite;
use futures::Stream;
use futures::io::AsyncRead;
use futures::io::AsyncSeek;
use pin_project::pin_project;

use crate::error::Error;
use crate::error::Result;
use crate::error::VfsResult;
use crate::prelude::*;
use crate::traits::vfs::DirEntryInfo;
use crate::traits::vfs::OwnedPathType;
use crate::traits::vfs::PathType;
use crate::traits::vfs::VfsCore;

/// An asynchronous virtual file system.
///
/// This is the asynchronous counterpart to the [`Vfs`] trait in the [`vfs`](super::vfs) module.
///
/// Writing operations are provided by the [`WriteSupportingVfsAsync` trait](self::WriteSupportingVfsAsync).
pub trait VfsAsync: VfsCore + Send + Sync + Unpin {
    /// The type of the file returned by the [`open_read` method](VfsAsync::open_read).
    type RFile: AsyncRead + Send + Unpin;
    /// The future returned by the [`open_read` method](VfsAsync::open_read).
    type OpenReadFuture: Future<Output = VfsResult<Self::RFile, Self>> + Send + Unpin;

    /// Opens a file for reading, at the specified path.
    fn open_read(
        self: Pin<&Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::OpenReadFuture;

    /// The future returned by the [`read` method](VfsAsync::read).
    type ReadFuture<'a>: Future<Output = VfsResult<Vec<u8>, Self>> + Send + Unpin + 'a
    where
        Self: 'a;

    /// Reads the contents of a file, at the specified path.
    fn read<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::ReadFuture<'a>;

    /// The future returned by the [`read_string` method](VfsAsync::read_string).
    type ReadStringFuture<'a>: Future<Output = VfsResult<String, Self>> + Send + Unpin + 'a
    where
        Self: 'a;

    /// Reads the contents of a file, at the specified path, and returns it as a string.
    fn read_string<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::ReadStringFuture<'a>;

    /// The future returned by the [`exists` method](VfsAsync::exists).
    type ExistsFuture<'a>: Future<Output = VfsResult<bool, Self>> + Send + 'a
    where
        Self: 'a;

    /// Checks if a file exists at the specified path.
    fn exists<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::ExistsFuture<'a>;

    /// The future type returned by the [`is_dir` method](VfsAsync::is_dir).
    type IsDirFuture<'a>: Future<Output = VfsResult<bool, Self>> + Send + 'a
    where
        Self: 'a;

    /// Checks if a directory exists at the specified path.
    fn is_dir<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::IsDirFuture<'a>;

    /// The stream type returned by the [`DirWalkFuture`](VfsAsync::DirWalkFuture).
    type DirWalk<'a>: Stream<Item = VfsResult<DirEntryInfo<<Self as VfsCore>::Path>, Self>>
        + Send
        + 'a
    where
        Self: 'a;

    /// The future type returned by the [`walk_dir` method](VfsAsync::walk_dir).
    type DirWalkFuture<'a>: Future<Output = VfsResult<Self::DirWalk<'a>, Self>> + Send + 'a
    where
        Self: 'a;

    /// Walks a directory at the given path, returning a stream of directory entries.
    fn walk_dir<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::DirWalkFuture<'a>;
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
    /// This method takes `self` as a pinned reference, to ensure that the [`VfsAsync`] value
    /// is not moved while the read operation is in progress.
    fn read_typed_async_pinned<'a, T: ReadFromAsync<'a, Self>>(
        self: Pin<&'a Self>,
        path: impl Into<<<Self as VfsCore>::Path as PathType>::OwnedPath>,
    ) -> T::Future {
        T::read_from_async(path.into(), self)
    }

    /// Reads a file / directory at the specified path, and parses it into the specified type using its
    /// [`ReadFromAsync`] implementation.
    ///
    /// This method takes `self` as a regular reference, and pins it internally.
    fn read_typed_async<'a, T: ReadFromAsync<'a, Self>>(
        &'a self,
        path: impl Into<<<Self as VfsCore>::Path as PathType>::OwnedPath>,
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
    type OpenWriteFuture: Future<Output = VfsResult<Self::WFile, Self>> + Send + Unpin;

    /// Opens a file for writing, at the specified path.
    fn open_write(
        self: Pin<&Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::OpenWriteFuture;

    /// The future type returned by the [`write` method](WriteSupportingVfsAsync::write).
    type WriteFuture<'a>: Future<Output = VfsResult<(), Self>> + Send + Unpin + 'a
    where
        Self: 'a;

    /// Writes the contents of a file, at the specified path.
    fn write<'d, 'a: 'd>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
        data: &'d [u8],
    ) -> Self::WriteFuture<'d>;

    /// The future type returned by the [`remove_dir_all` method](WriteSupportingVfsAsync::remove_dir_all).
    type RemoveDirAllFuture<'a>: Future<Output = VfsResult<(), Self>> + Send + 'a
    where
        Self: 'a;

    /// Removes a directory and all its contents.
    fn remove_dir_all<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::RemoveDirAllFuture<'a>;

    /// The future type returned by the [`create_dir` method](WriteSupportingVfsAsync::create_dir).
    type CreateDirFuture<'a>: Future<Output = VfsResult<(), Self>> + Send + 'a
    where
        Self: 'a;
    /// Creates a new directory at the specified path.
    fn create_dir<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::CreateDirFuture<'a>;

    /// The future type returned by the [`create_dir_all` method](WriteSupportingVfsAsync::create_dir_all).
    type CreateDirAllFuture<'a>: Future<Output = VfsResult<(), Self>> + Send + 'a
    where
        Self: 'a;
    /// Creates a new directory and all its parent directories at the specified path.
    fn create_dir_all<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::CreateDirAllFuture<'a>;

    /// The future type returned by the [`create_parent_dir` method](WriteSupportingVfsAsync::create_parent_dir).
    type CreateParentDirFuture<'a>: Future<Output = VfsResult<(), Self>> + Send + 'a
    where
        Self: 'a;
    /// Creates a new parent directory at the specified path.
    fn create_parent_dir<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::CreateParentDirFuture<'a>;
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
        path: impl Into<<<Self as VfsCore>::Path as PathType>::OwnedPath>,
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
        path: impl Into<<<Self as VfsCore>::Path as PathType>::OwnedPath>,
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
        path: impl Into<<<Self as VfsCore>::Path as PathType>::OwnedPath>,
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
        path: impl Into<<<Self as VfsCore>::Path as PathType>::OwnedPath>,
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
    for<'f> Vfs::ExistsFuture<'f>: Future<Output = VfsResult<bool, Vfs>> + Unpin,
    for<'f> Vfs::CreateDirAllFuture<'f>: Future<Output = VfsResult<(), Vfs>> + Unpin,
{
    Poison,
    Start {
        vfs: Pin<&'a Vfs>,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
    },
    ExistsFuture {
        vfs: Pin<&'a Vfs>,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        exists_future: Vfs::ExistsFuture<'a>,
    },
    CreateDirAllFuture {
        vfs: Pin<&'a Vfs>,
        create_dir_all_future: Vfs::CreateDirAllFuture<'a>,
    },
}

impl<'a, Vfs: WriteSupportingVfsAsync + 'a> Future for CreateParentDirDefaultFuture<'a, Vfs>
where
    for<'f> Vfs::ExistsFuture<'f>: Future<Output = VfsResult<bool, Vfs>> + Unpin,
    for<'f> Vfs::CreateDirAllFuture<'f>: Future<Output = VfsResult<(), Vfs>> + Unpin,
{
    type Output = VfsResult<(), Vfs>;

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
pub struct IoErrorWrapperFuture<T, F: Future<Output = io::Result<T>>, P: OwnedPathType> {
    #[pin]
    future: F,
    path: P,
}

impl<T, F, P> IoErrorWrapperFuture<T, F, P>
where
    F: Future<Output = io::Result<T>>,
    P: OwnedPathType,
{
    pub fn new(path: P, future: F) -> Self {
        Self { future, path }
    }
}

impl<T, F, P> Future for IoErrorWrapperFuture<T, F, P>
where
    F: Future<Output = io::Result<T>>,
    P: OwnedPathType + Clone,
{
    type Output = Result<T, P>;

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
