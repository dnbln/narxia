//! A [`Vfs`] and [`WriteSupportingVfs`] implementation built upon the [`std::fs`] APIs.
//!
//! Main item is the [`FsVfs`] struct.

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::error::Result;
use crate::error::VfsResult;
use crate::error::WrapIoError;
use crate::traits::vfs::DirEntryInfo;
use crate::traits::vfs::DirEntryKind;
use crate::traits::vfs::DirWalker;
use crate::traits::vfs::PathType;
use crate::traits::vfs::Vfs;
use crate::traits::vfs::VfsCore;
use crate::traits::vfs::WriteSupportingVfs;

/// A [`Vfs`] and [`WriteSupportingVfs`] implementation built upon the [`std::fs`] APIs.
#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct FsVfs;

impl VfsCore for FsVfs {
    type Path = Path;
}

impl<'a> Vfs<'a> for FsVfs {
    type DirWalk<'b>
        = imp::FsDirWalker
    where
        'a: 'b,
        Self: 'b;

    type RFile = fs::File;

    fn open_read(self: Pin<&Self>, path: &Self::Path) -> VfsResult<Self::RFile, Self> {
        fs::File::open(path).wrap_io_error_with(path)
    }

    fn read(self: Pin<&Self>, path: &Self::Path) -> VfsResult<Vec<u8>, Self> {
        fs::read(path).wrap_io_error_with(path)
    }

    fn read_string(self: Pin<&Self>, path: &Self::Path) -> VfsResult<String, Self> {
        fs::read_to_string(path).wrap_io_error_with(path)
    }

    fn exists(self: Pin<&Self>, path: &Self::Path) -> VfsResult<bool, Self> {
        Ok(path.exists())
    }

    fn is_dir(self: Pin<&Self>, path: &Self::Path) -> VfsResult<bool, Self> {
        Ok(path.is_dir())
    }

    fn walk_dir<'b>(self: Pin<&'b Self>, path: &Self::Path) -> VfsResult<Self::DirWalk<'b>, Self>
    where
        'a: 'b,
    {
        fs::read_dir(path)
            .wrap_io_error_with(path)
            .map(|read_dir| imp::FsDirWalker(read_dir, path.to_path_buf()))
    }
}

impl<'a> WriteSupportingVfs<'a> for FsVfs {
    type WFile = fs::File;

    fn open_write(self: Pin<&Self>, path: &Self::Path) -> VfsResult<Self::WFile, Self> {
        fs::File::create(path).wrap_io_error_with(path)
    }

    fn write(self: Pin<&Self>, path: &Self::Path, data: &[u8]) -> VfsResult<(), Self> {
        fs::write(path, data).wrap_io_error_with(path)
    }

    fn create_dir(self: Pin<&Self>, path: &Self::Path) -> VfsResult<(), Self> {
        fs::create_dir(path).wrap_io_error_with(path)
    }

    fn create_dir_all(self: Pin<&Self>, path: &Self::Path) -> VfsResult<(), Self> {
        fs::create_dir_all(path).wrap_io_error_with(path)
    }

    fn remove_dir_all(self: Pin<&Self>, path: &Self::Path) -> VfsResult<(), Self> {
        fs::remove_dir_all(path).wrap_io_error_with(path)
    }

    fn create_parent_dir(self: Pin<&Self>, path: &Self::Path) -> VfsResult<(), Self> {
        if let Some(parent) = path.parent()
            && !self.exists(parent)?
        {
            self.create_dir_all(parent)?;
        }
        Ok(())
    }
}

mod imp {
    use std::io;

    use super::*;

    /// The [`DirWalker`] implementation for the file system.
    pub struct FsDirWalker(pub(super) fs::ReadDir, pub(super) PathBuf);

    impl<'a> DirWalker<'a> for FsDirWalker {
        type P = Path;

        fn next(
            &mut self,
        ) -> Option<Result<DirEntryInfo<Self::P>, <Self::P as PathType>::OwnedPath>> {
            self.0.next().map(|entry| {
                <io::Result<_> as WrapIoError<Self::P>>::wrap_io_error(
                    entry.and_then(|e| {
                        Ok(DirEntryInfo {
                            name: e.file_name(),
                            path: e.path(),
                            kind: if e.file_type()?.is_dir() {
                                DirEntryKind::Directory
                            } else {
                                DirEntryKind::File
                            },
                        })
                    }),
                    || self.1.clone(),
                )
            })
        }
    }
}

#[cfg(feature = "tools-atomic-dir")]
pub(crate) mod atomic_dir_imp {
    //! The [`VfsSupportsTemporaryDirectories`] implementation for the [`FsVfs`] file system.

    use std::sync::atomic::AtomicU64;

    use super::*;
    use crate::atomic_dir::TempDirApi;
    use crate::atomic_dir::VfsSupportsTemporaryDirectories;

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
            std::fs::rename(&self.0, path).wrap_io_error_with(path)?;
            // do not run the Drop impl, as we already moved the directory
            std::mem::forget(self);
            Ok(())
        }

        fn delete(self, vfs: Pin<&'vfs Self::Vfs>) -> VfsResult<(), Self::Vfs> {
            vfs.remove_dir_all(&self.0)?;
            // do not run the Drop impl, as we already deleted the directory
            std::mem::forget(self);
            Ok(())
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
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

    pub fn make_new_temp_dir_path() -> PathBuf {
        std::env::temp_dir().join(format!(
            "__rust_dir_structure_temp_{}_{}",
            std::process::id(),
            FS_TEMP_DIR_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
        ))
    }
}
