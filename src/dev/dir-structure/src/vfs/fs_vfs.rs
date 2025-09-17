//! A [`Vfs`] and [`WriteSupportingVfs`] implementation built upon the [`std::fs`] APIs.
//!
//! Main item is the [`FsVfs`] struct.

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::error::Result;
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

    fn open_read(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<Self::RFile, <Path as PathType>::OwnedPath> {
        fs::File::open(path).wrap_io_error_with(path)
    }

    fn read(self: Pin<&Self>, path: &Self::Path) -> Result<Vec<u8>, <Path as PathType>::OwnedPath> {
        fs::read(path).wrap_io_error_with(path)
    }

    fn read_string(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<String, <Path as PathType>::OwnedPath> {
        fs::read_to_string(path).wrap_io_error_with(path)
    }

    fn exists(self: Pin<&Self>, path: &Self::Path) -> Result<bool, <Path as PathType>::OwnedPath> {
        Ok(path.exists())
    }

    fn is_dir(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<bool, <Self::Path as PathType>::OwnedPath> {
        Ok(path.is_dir())
    }

    fn walk_dir<'b>(
        self: Pin<&'b Self>,
        path: &Self::Path,
    ) -> Result<Self::DirWalk<'b>, <Path as PathType>::OwnedPath>
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

    fn open_write(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<Self::WFile, <Self::Path as PathType>::OwnedPath> {
        fs::File::create(path).wrap_io_error_with(path)
    }

    fn write(
        self: Pin<&Self>,
        path: &Self::Path,
        data: &[u8],
    ) -> Result<(), <Self::Path as PathType>::OwnedPath> {
        fs::write(path, data).wrap_io_error_with(path)
    }

    fn create_dir(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath> {
        fs::create_dir(path).wrap_io_error_with(path)
    }

    fn create_dir_all(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath> {
        fs::create_dir_all(path).wrap_io_error_with(path)
    }

    fn remove_dir_all(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath> {
        fs::remove_dir_all(path).wrap_io_error_with(path)
    }

    fn create_parent_dir(
        self: Pin<&Self>,
        path: &Self::Path,
    ) -> Result<(), <Self::Path as PathType>::OwnedPath> {
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
