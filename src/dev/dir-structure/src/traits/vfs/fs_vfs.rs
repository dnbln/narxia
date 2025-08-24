use std::ffi::OsString;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;

use crate::DirEntryInfo;
use crate::DirEntryKind;
use crate::DirWalker;
use crate::Result;
use crate::Vfs;
use crate::WrapIoError;

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct FsVfs;

impl Vfs for FsVfs {
    type DirWalk = FsDirWalker;

    fn read(self: Pin<&Self>, path: &Path) -> Result<Vec<u8>> {
        fs::read(path).wrap_io_error_with(path)
    }

    fn read_string(self: Pin<&Self>, path: &Path) -> Result<String> {
        fs::read_to_string(path).wrap_io_error_with(path)
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

    fn exists(self: Pin<&Self>, path: &Path) -> Result<bool> {
        Ok(path.exists())
    }

    fn walk_dir(self: Pin<&Self>, path: &Path) -> Result<Self::DirWalk> {
        fs::read_dir(path)
            .wrap_io_error_with(path)
            .map(|read_dir| FsDirWalker(read_dir, path.to_path_buf()))
    }
}

pub struct FsDirWalker(fs::ReadDir, PathBuf);

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
