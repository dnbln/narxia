//! A [`Vfs`] and [`WriteSupportingVfs`] implementation built upon the [`std::fs`] APIs.
//!
//! Main item is the [`FsVfs`] struct.

use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::error::Result;
use crate::error::WrapIoError;
use crate::traits::vfs::DirEntryInfo;
use crate::traits::vfs::DirEntryKind;
use crate::traits::vfs::DirWalker;
use crate::traits::vfs::Vfs;
use crate::traits::vfs::WriteSupportingVfs;

/// A [`Vfs`] and [`WriteSupportingVfs`] implementation built upon the [`std::fs`] APIs.
#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct FsVfs;

impl Vfs for FsVfs {
    type DirWalk = imp::FsDirWalker;

    type RFile = io::BufReader<fs::File>;

    fn open_read(self: Pin<&Self>, path: &Path) -> Result<Self::RFile> {
        fs::File::open(path)
            .map(io::BufReader::new)
            .wrap_io_error_with(path)
    }

    fn read(self: Pin<&Self>, path: &Path) -> Result<Vec<u8>> {
        fs::read(path).wrap_io_error_with(path)
    }

    fn read_string(self: Pin<&Self>, path: &Path) -> Result<String> {
        fs::read_to_string(path).wrap_io_error_with(path)
    }

    fn exists(self: Pin<&Self>, path: &Path) -> Result<bool> {
        Ok(path.exists())
    }

    fn walk_dir(self: Pin<&Self>, path: &Path) -> Result<Self::DirWalk> {
        fs::read_dir(path)
            .wrap_io_error_with(path)
            .map(|read_dir| imp::FsDirWalker(read_dir, path.to_path_buf()))
    }
}

impl WriteSupportingVfs for FsVfs {
    type WFile = fs::File;

    fn open_write(self: Pin<&Self>, path: &Path) -> Result<Self::WFile> {
        fs::File::create(path).wrap_io_error_with(path)
    }

    fn write(self: Pin<&Self>, path: &Path, data: &[u8]) -> Result<()> {
        fs::write(path, data).wrap_io_error_with(path)
    }

    fn create_dir(self: Pin<&Self>, path: &Path) -> Result<()> {
        fs::create_dir(path).wrap_io_error_with(path)
    }

    fn create_dir_all(self: Pin<&Self>, path: &Path) -> Result<()> {
        fs::create_dir_all(path).wrap_io_error_with(path)
    }

    fn remove_dir_all(self: Pin<&Self>, path: &Path) -> Result<()> {
        fs::remove_dir_all(path).wrap_io_error_with(path)
    }
}

mod imp {
    use super::*;

    /// The [`DirWalker`] implementation for the file system.
    pub struct FsDirWalker(pub(super) fs::ReadDir, pub(super) PathBuf);

    impl DirWalker for FsDirWalker {
        fn next(&mut self) -> Option<Result<DirEntryInfo>> {
            self.0.next().map(|entry| {
                entry
                    .and_then(|e| {
                        Ok(DirEntryInfo {
                            name: e.file_name(),
                            path: e.path(),
                            kind: if e.file_type()?.is_dir() {
                                DirEntryKind::Directory
                            } else {
                                DirEntryKind::File
                            },
                        })
                    })
                    .wrap_io_error_with(&self.1)
            })
        }
    }
}
