//! A module to enable atomic directory writes.
//!
//! This module provides the [`AtomicDir`] struct, which allows for safe and atomic
//! writes to directories.
//!
//! It achieves this by writing changes to a temporary directory first, and then
//! renaming the temporary directory to the target directory once all writes are successful.
//! This ensures that the target directory is never left in a partially updated state.

#[cfg(feature = "async")]
use std::future::Future;
use std::pin::Pin;
#[cfg(feature = "async")]
use std::task::Context;
#[cfg(feature = "async")]
use std::task::Poll;

#[cfg(feature = "async")]
use pin_project::pin_project;

#[cfg(feature = "async")]
use dir_structure::error::Error;
use dir_structure::error::VfsResult;
use dir_structure::prelude::*;
#[cfg(feature = "async")]
use dir_structure::traits::async_vfs::WriteSupportingVfsAsync;
use dir_structure::traits::vfs;
use dir_structure::traits::vfs::WriteSupportingVfs;

/// A wrapper type that enables atomic writes to directories.
///
/// When writing, it first writes to a temporary directory and then renames it
/// to the target directory, ensuring that the target directory is never left
/// in a partially updated state.
///
/// If the write fails, the original directory remains unchanged, and the temporary
/// directory is cleaned up, before the error is propagated.
///
/// The Vfs used for writing must support temporary directories via the
/// [`VfsSupportsTemporaryDirectories`] trait (or its async counterpart
/// [`VfsSupportsTemporaryDirectoriesAsync`] when using async writes).
pub struct AtomicDir<T>(pub T);

impl<'vfs, T, Vfs: vfs::Vfs<'vfs> + 'vfs> ReadFrom<'vfs, Vfs> for AtomicDir<T>
where
    T: ReadFrom<'vfs, Vfs>,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<Self, Vfs> {
        T::read_from(path, vfs).map(Self)
    }
}

#[cfg(feature = "async")]
impl<'vfs, T, Vfs: VfsAsync + 'vfs> ReadFromAsync<'vfs, Vfs> for AtomicDir<T>
where
    T: ReadFromAsync<'vfs, Vfs> + Send + 'static,
{
    type Future = Pin<Box<dyn Future<Output = VfsResult<Self, Vfs>> + Send + 'vfs>>;

    fn read_from_async(
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'vfs Vfs>,
    ) -> Self::Future {
        let fut = T::read_from_async(path, vfs);
        Box::pin(async move { fut.await.map(Self) })
    }
}

/// An API for temporary directories provided by a virtual file system (VFS).
#[must_use]
pub trait TempDirApi<'vfs> {
    /// The associated VFS type.
    type Vfs: WriteSupportingVfs<'vfs>;
    /// Returns the path of the temporary directory.
    fn path(&self) -> &<Self::Vfs as VfsCore>::Path;

    /// Persists the temporary directory at the specified path in the VFS.
    fn persist_at(
        self,
        vfs: Pin<&'vfs Self::Vfs>,
        path: &<Self::Vfs as VfsCore>::Path,
    ) -> VfsResult<(), Self::Vfs>;

    /// Deletes the temporary directory.
    fn delete(self, vfs: Pin<&'vfs Self::Vfs>) -> VfsResult<(), Self::Vfs>;
}

/// A trait for VFS implementations that support temporary directories.
pub trait VfsSupportsTemporaryDirectories<'vfs>: WriteSupportingVfs<'vfs> {
    /// The type representing a temporary directory.
    type TemporaryDirectory: TempDirApi<'vfs, Vfs = Self>;
    /// Creates and returns a new temporary directory.
    fn create_temporary_directory(
        self: Pin<&'vfs Self>,
    ) -> VfsResult<Self::TemporaryDirectory, Self>;
}

impl<'vfs, T, Vfs: vfs::Vfs<'vfs> + vfs::WriteSupportingVfs<'vfs> + 'vfs> WriteTo<'vfs, Vfs>
    for AtomicDir<T>
where
    T: WriteTo<'vfs, Vfs>,
    Vfs: VfsSupportsTemporaryDirectories<'vfs>,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<(), Vfs> {
        let temp_dir = vfs.create_temporary_directory()?;
        match self.0.write_to(temp_dir.path(), vfs) {
            Ok(()) => temp_dir.persist_at(vfs, path),
            Err(e) => {
                let _ = temp_dir.delete(vfs); // try to clean up the temp dir, but ignore errors from that
                Err(e)
            }
        }
    }
}

/// An async version of the [`TempDirApi`] trait.
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait TempDirApiAsync<'vfs> {
    /// The associated VFS type.
    type Vfs: VfsAsync + WriteSupportingVfsAsync;
    /// The future type for the [`persist_at` method](TempDirApiAsync::persist_at).
    type FuturePersistAt: Future<Output = VfsResult<(), Self::Vfs>> + Send + 'vfs;
    /// The future type for the [`delete` method](TempDirApiAsync::delete).
    type FutureDelete: Future<Output = VfsResult<(), Self::Vfs>> + Send + 'vfs;

    /// Returns the path of the temporary directory.
    fn path(&self) -> &<Self::Vfs as VfsCore>::Path;

    /// Persists the temporary directory at the specified path in the VFS.
    fn persist_at(
        self,
        vfs: Pin<&'vfs Self::Vfs>,
        path: <<Self::Vfs as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::FuturePersistAt;

    /// Deletes the temporary directory.
    fn delete(self, vfs: Pin<&'vfs Self::Vfs>) -> Self::FutureDelete;
}

/// A trait for VFS implementations that support temporary directories in async contexts.
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait VfsSupportsTemporaryDirectoriesAsync<'vfs>: VfsAsync + WriteSupportingVfsAsync {
    /// The type representing a temporary directory.
    type TemporaryDirectory: TempDirApiAsync<'vfs, Vfs = Self> + Send;
    /// The future type for the [`create_temporary_directory` method](VfsSupportsTemporaryDirectoriesAsync::create_temporary_directory).
    type TemporaryDirectoryFuture: Future<Output = VfsResult<Self::TemporaryDirectory, Self>>
        + Send
        + 'vfs;
    /// Creates and returns a new temporary directory.
    fn create_temporary_directory(self: Pin<&'vfs Self>) -> Self::TemporaryDirectoryFuture;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, T, Vfs: WriteSupportingVfsAsync + 'vfs> WriteToAsync<'vfs, Vfs> for AtomicDir<T>
where
    T: WriteToAsync<'vfs, Vfs> + Send + 'static,
    Vfs: VfsSupportsTemporaryDirectoriesAsync<'vfs> + VfsAsync + Send,
{
    type Future = Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + 'vfs>>;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'vfs Vfs>,
    ) -> Self::Future {
        Box::pin(async move {
            let temp_dir = vfs.create_temporary_directory().await?;
            match self.0.write_to_async(temp_dir.path().owned(), vfs).await {
                Ok(()) => temp_dir.persist_at(vfs, path).await,
                Err(e) => {
                    let fut = temp_dir.delete(vfs); // try to clean up the temp dir, but ignore errors from that
                    let _ = fut.await;
                    Err(e)
                }
            }
        })
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[pin_project(project_replace = AtomicDirWriteToAsyncRefFutureProj)]
#[doc(hidden)]
pub enum AtomicDirWriteToAsyncRefFuture<'r, 'vfs, T, Vfs>
where
    Vfs: WriteSupportingVfsAsync + 'vfs,
    T: WriteToAsyncRef<'vfs, Vfs> + Sync + 'static,
    <T as WriteToAsyncRef<'vfs, Vfs>>::Future<'r>:
        Future<Output = VfsResult<(), Vfs>> + Unpin + Send + 'r,
    Vfs: VfsSupportsTemporaryDirectoriesAsync<'r> + VfsAsync + Send + 'static,
    'vfs: 'r,
{
    CreatingTempDir(
        Pin<
            Box<
                dyn Future<
                        Output = VfsResult<
                            <Vfs as VfsSupportsTemporaryDirectoriesAsync<'r>>::TemporaryDirectory,
                            Vfs,
                        >,
                    > + Send
                    + 'r,
            >,
        >,
        &'r T,
        <Vfs::Path as PathType>::OwnedPath,
        Pin<&'r Vfs>,
    ),
    WritingToTempDir(
        <Vfs as VfsSupportsTemporaryDirectoriesAsync<'r>>::TemporaryDirectory,
        <T as WriteToAsyncRef<'vfs, Vfs>>::Future<'r>,
        <Vfs::Path as PathType>::OwnedPath,
        Pin<&'r Vfs>,
    ),
    PersistingTempDir(Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + 'r>>),
    DeletingTempDir(
        Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + 'r>>,
        Error<<Vfs::Path as PathType>::OwnedPath>,
    ),
    Done,
    Poison,
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'r, 'vfs, T, Vfs> Future for AtomicDirWriteToAsyncRefFuture<'r, 'vfs, T, Vfs>
where
    Vfs: WriteSupportingVfsAsync + 'vfs,
    T: WriteToAsyncRef<'vfs, Vfs> + Sync + 'static,
    <T as WriteToAsyncRef<'vfs, Vfs>>::Future<'r>:
        Future<Output = VfsResult<(), Vfs>> + Unpin + Send + 'r,
    Vfs: VfsSupportsTemporaryDirectoriesAsync<'r> + VfsAsync + Send + 'static,
    'vfs: 'r,
{
    type Output = VfsResult<(), Vfs>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.as_mut().project_replace(Self::Poison);
        match this {
            AtomicDirWriteToAsyncRefFutureProj::CreatingTempDir(mut fut, val, path, vfs) => {
                match fut.as_mut().poll(cx) {
                    Poll::Ready(Ok(temp_dir)) => {
                        let write_fut = val.write_to_async_ref(temp_dir.path().owned(), vfs);
                        self.as_mut().project_replace(Self::WritingToTempDir(
                            temp_dir, write_fut, path, vfs,
                        ));
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    Poll::Pending => {
                        self.as_mut()
                            .project_replace(Self::CreatingTempDir(fut, val, path, vfs));
                        Poll::Pending
                    }
                }
            }
            AtomicDirWriteToAsyncRefFutureProj::WritingToTempDir(temp_dir, mut fut, path, vfs) => {
                match Pin::new(&mut fut).poll(cx) {
                    Poll::Ready(Ok(())) => {
                        let persist_fut = temp_dir.persist_at(vfs.clone(), path.clone());
                        self.as_mut()
                            .project_replace(Self::PersistingTempDir(Box::pin(persist_fut)));
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Ready(Err(e)) => {
                        let delete_fut = temp_dir.delete(vfs.clone());
                        self.as_mut()
                            .project_replace(Self::DeletingTempDir(Box::pin(delete_fut), e));
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                    Poll::Pending => {
                        self.as_mut()
                            .project_replace(Self::WritingToTempDir(temp_dir, fut, path, vfs));
                        Poll::Pending
                    }
                }
            }
            AtomicDirWriteToAsyncRefFutureProj::PersistingTempDir(mut fut) => {
                match fut.as_mut().poll(cx) {
                    Poll::Ready(result) => {
                        *self = Self::Done;
                        Poll::Ready(result)
                    }
                    Poll::Pending => {
                        self.as_mut().project_replace(Self::PersistingTempDir(fut));
                        Poll::Pending
                    }
                }
            }
            AtomicDirWriteToAsyncRefFutureProj::DeletingTempDir(mut fut, error) => {
                match fut.as_mut().poll(cx) {
                    Poll::Ready(_) => Poll::Ready(Err(error)),
                    Poll::Pending => {
                        self.as_mut()
                            .project_replace(Self::DeletingTempDir(fut, error));
                        Poll::Pending
                    }
                }
            }
            AtomicDirWriteToAsyncRefFutureProj::Done => panic!("polled after completion"),
            AtomicDirWriteToAsyncRefFutureProj::Poison => panic!("future was poisoned"),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, T, Vfs: WriteSupportingVfsAsync + 'vfs> WriteToAsyncRef<'vfs, Vfs> for AtomicDir<T>
where
    T: WriteToAsyncRef<'vfs, Vfs> + Sync + 'static,
    for<'r> <T as WriteToAsyncRef<'vfs, Vfs>>::Future<'r>:
        Future<Output = VfsResult<(), Vfs>> + Unpin + Send + 'r,
    for<'r> Vfs: VfsSupportsTemporaryDirectoriesAsync<'r> + VfsAsync + Send + 'static,
{
    type Future<'r>
        = AtomicDirWriteToAsyncRefFuture<'r, 'vfs, T, Vfs>
    where
        'vfs: 'r,
        Self: 'r;

    fn write_to_async_ref<'r>(
        &'r self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'r Vfs>,
    ) -> Self::Future<'r>
    where
        'vfs: 'r,
    {
        AtomicDirWriteToAsyncRefFuture::CreatingTempDir(
            Box::pin(vfs.create_temporary_directory()),
            &self.0,
            path,
            vfs,
        )
    }
}

mod std_fs_impl {
    //! The [`VfsSupportsTemporaryDirectories`] implementation for the [`FsVfs`] file system.
    use super::*;

    use std::env;
    use std::fs;
    use std::mem;
    use std::path::PathBuf;
    use std::process;
    use std::sync::atomic::AtomicU64;
    use std::sync::atomic::Ordering;

    use crate::atomic_dir::TempDirApi;
    use crate::atomic_dir::VfsSupportsTemporaryDirectories;
    use dir_structure::error::WrapIoError;
    use dir_structure::vfs::fs_vfs::FsVfs;

    /// A temporary directory in the real file system.
    pub struct TempDir(PathBuf);

    impl<'vfs> TempDirApi<'vfs> for TempDir {
        type Vfs = FsVfs;

        fn path(&self) -> &<Self::Vfs as VfsCore>::Path {
            &self.0
        }

        fn persist_at(
            self,
            vfs: Pin<&'vfs Self::Vfs>,
            path: &<Self::Vfs as VfsCore>::Path,
        ) -> VfsResult<(), Self::Vfs> {
            vfs.create_parent_dir(path)?;
            fs::rename(&self.0, path).wrap_io_error_with(path)?;
            // do not run the Drop impl, as we already moved the directory
            mem::forget(self);
            Ok(())
        }

        fn delete(self, vfs: Pin<&'vfs Self::Vfs>) -> VfsResult<(), Self::Vfs> {
            vfs.remove_dir_all(&self.0)?;
            // do not run the Drop impl, as we already deleted the directory
            mem::forget(self);
            Ok(())
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    pub(crate) static FS_TEMP_DIR_ID: AtomicU64 = AtomicU64::new(0);

    impl<'vfs> VfsSupportsTemporaryDirectories<'vfs> for FsVfs {
        type TemporaryDirectory = TempDir;

        fn create_temporary_directory(
            self: Pin<&'vfs Self>,
        ) -> VfsResult<Self::TemporaryDirectory, Self> {
            let temp_dir = make_new_temp_dir_path();
            // if the temp dir already exists, remove it first
            // this is safe, because the name contains the process id,
            // so it's impossible for another process to be using it.
            //
            // unless the user manually created a directory with that name,
            // which is highly highly unlikely.
            if self.exists(&temp_dir)? {
                self.remove_dir_all(&temp_dir)?;
            }
            self.create_dir(&temp_dir)?;
            Ok(TempDir(temp_dir))
        }
    }

    pub(crate) fn make_new_temp_dir_path() -> PathBuf {
        env::temp_dir().join(format!(
            "__rust_dir_structure_temp_{}_{}",
            process::id(),
            FS_TEMP_DIR_ID.fetch_add(1, Ordering::SeqCst)
        ))
    }
}

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
mod tokio_fs_impl {
    //! The [`VfsSupportsTemporaryDirectoriesAsync`] implementation for the [`TokioFsVfs`] file system.

    use std::path::Path;
    use std::path::PathBuf;

    use dir_structure::vfs::tokio_fs_vfs::TokioFsVfs;
    use tokio::fs;

    use super::std_fs_impl;
    use super::*;
    use dir_structure::error::Error;
    use dir_structure::error::VfsResult;

    /// A temporary directory in the real file system.
    pub struct TempDir(PathBuf);

    impl<'vfs> TempDirApiAsync<'vfs> for TempDir {
        type Vfs = TokioFsVfs;

        fn path(&self) -> &Path {
            &self.0
        }

        type FuturePersistAt =
            Pin<Box<dyn Future<Output = VfsResult<(), Self::Vfs>> + Send + 'vfs>>;

        type FutureDelete = <TokioFsVfs as WriteSupportingVfsAsync>::RemoveDirAllFuture<'vfs>;

        fn persist_at(
            self,
            vfs: Pin<&'vfs Self::Vfs>,
            path: <<Self::Vfs as VfsCore>::Path as PathType>::OwnedPath,
        ) -> Self::FuturePersistAt {
            Box::pin(async move {
                vfs.create_parent_dir(path.clone()).await?;
                fs::rename(&self.0, &path)
                    .await
                    .map_err(|e| Error::Io(path.clone(), e))?;
                Ok(())
            })
        }

        fn delete(self, vfs: Pin<&'vfs Self::Vfs>) -> Self::FutureDelete {
            vfs.remove_dir_all(self.0.clone())
        }
    }

    impl<'vfs> VfsSupportsTemporaryDirectoriesAsync<'vfs> for TokioFsVfs {
        type TemporaryDirectory = TempDir;

        type TemporaryDirectoryFuture =
            Pin<Box<dyn Future<Output = VfsResult<TempDir, Self>> + Send + 'vfs>>;

        fn create_temporary_directory(self: Pin<&'vfs Self>) -> Self::TemporaryDirectoryFuture {
            let temp_dir = std_fs_impl::make_new_temp_dir_path();
            let path = temp_dir.clone();
            Box::pin(async move {
                if self.exists(path.clone()).await? {
                    self.remove_dir_all(path.clone()).await?;
                }
                self.create_dir(path.clone()).await?;
                Ok(TempDir(temp_dir))
            })
        }
    }
}
