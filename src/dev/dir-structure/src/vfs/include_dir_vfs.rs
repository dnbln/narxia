//! A [`Vfs`] implementation for an [`include_dir::Dir`] directory.

use core::fmt;
use std::error;
use std::io;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
use std::result::Result as StdResult;

use include_dir::Dir;
#[cfg(doc)]
use include_dir::include_dir;

use crate::error::Error;
use crate::error::Result;
use crate::traits::vfs::DirEntryInfo;
use crate::traits::vfs::DirEntryKind;
use crate::traits::vfs::DirWalker;
use crate::traits::vfs::Vfs;

/// A [`Vfs`] implementation with an [`include_dir::Dir`] directory.
pub struct IncludeDirVfs {
    dir: Dir<'static>,
}

impl IncludeDirVfs {
    /// Creates a new [`IncludeDirVfs`].
    pub fn new(dir: Dir<'static>) -> Self {
        Self { dir }
    }
}

/// Convenience macro to [`include_dir!(...)`][include_dir] and wrap it in an [`IncludeDirVfs`].
///
/// [include_dir]: include_dir
#[macro_export]
macro_rules! include_dir_vfs {
    ($path:expr) => {{
        let dir = $crate::include_dir::include_dir!($path);
        $crate::vfs::include_dir_vfs::IncludeDirVfs::new(dir)
    }};
}

#[derive(Debug, thiserror::Error)]
#[error("path normalization error")]
struct NormalizeError;

/// [`Path::normalize_lexically`] is unstable, so we implement a simplified version here.
///
/// This version also allows for paths to begin with the current directory (`.`).
fn normalize_lexically(path: &Path) -> StdResult<PathBuf, NormalizeError> {
    let mut lexical = PathBuf::new();
    let mut iter = path.components().peekable();

    // Find the root, if any, and add it to the lexical path.
    // Here we treat the Windows path "C:\" as a single "root" even though
    // `components` splits it into two: (Prefix, RootDir).
    let root = match iter.peek() {
        Some(Component::ParentDir) => return Err(NormalizeError),
        Some(p @ Component::RootDir) => {
            lexical.push(p);
            iter.next();
            lexical.as_os_str().len()
        }
        Some(Component::CurDir) => {
            iter.next();
            0
        }
        Some(Component::Prefix(prefix)) => {
            lexical.push(prefix.as_os_str());
            iter.next();
            if let Some(p @ Component::RootDir) = iter.peek() {
                lexical.push(p);
                iter.next();
            }
            lexical.as_os_str().len()
        }
        None => return Ok(PathBuf::new()),
        Some(Component::Normal(_)) => 0,
    };

    for component in iter {
        match component {
            Component::RootDir => unreachable!(),
            Component::Prefix(_) => return Err(NormalizeError),
            Component::CurDir => continue,
            Component::ParentDir => {
                // It's an error if ParentDir causes us to go above the "root".
                if lexical.as_os_str().len() == root {
                    return Err(NormalizeError);
                } else {
                    lexical.pop();
                }
            }
            Component::Normal(path) => lexical.push(path),
        }
    }
    Ok(lexical)
}

fn norm(path: &Path) -> Result<PathBuf> {
    normalize_lexically(path).map_err(|e| {
        Error::Io(
            path.to_path_buf(),
            io::Error::new(io::ErrorKind::InvalidInput, e).into(),
        )
    })
}

fn get_dir_or_root(root: Dir<'static>, path: &Path) -> Result<Dir<'static>> {
    if path.as_os_str().is_empty() || path == Path::new(".") {
        return Ok(root);
    }

    let p = norm(path)?;
    root.get_dir(&p)
        .ok_or(Error::Io(p, io::ErrorKind::NotFound.into()))
}

impl<'vfs> Vfs<'vfs> for IncludeDirVfs {
    type DirWalk<'a>
        = IncludeDirWalker
    where
        'vfs: 'a,
        Self: 'a;
    type RFile = io::Cursor<&'static [u8]>;

    fn open_read(self: Pin<&Self>, path: &Path) -> Result<Self::RFile> {
        let p = norm(path)?;
        if self.dir.get_dir(&p).is_some() {
            return Err(Error::Io(p, io::ErrorKind::IsADirectory.into()));
        }

        let file = self
            .dir
            .get_file(&p)
            .ok_or(Error::Io(p.clone(), io::ErrorKind::NotFound.into()))?;

        Ok(io::Cursor::new(file.contents()))
    }

    fn read(self: Pin<&Self>, path: &Path) -> Result<Vec<u8>> {
        let p = norm(path)?;
        self.dir
            .get_file(&p)
            .map(|it| it.contents().to_vec())
            .ok_or(Error::Io(p, io::ErrorKind::NotFound.into()))
    }

    fn read_string(self: Pin<&Self>, path: &Path) -> Result<String> {
        let p = norm(path)?;
        #[derive(Debug)]
        struct Utf8Error;
        impl fmt::Display for Utf8Error {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "invalid utf-8: corrupt contents")
            }
        }

        impl error::Error for Utf8Error {
            fn description(&self) -> &str {
                "invalid utf-8: corrupt contents"
            }
        }

        if self.dir.get_dir(&p).is_some() {
            return Err(Error::Io(p, io::ErrorKind::IsADirectory.into()));
        }

        let file = self
            .dir
            .get_file(&p)
            .ok_or(Error::Io(p.clone(), io::ErrorKind::NotFound.into()))?;

        file.contents_utf8()
            .map(|s| s.to_string())
            .ok_or(Error::Parse(p, Box::new(Utf8Error)))
    }

    fn exists(self: Pin<&Self>, path: &Path) -> Result<bool> {
        let path = norm(path)?;

        Ok(get_dir_or_root(self.dir, &path)
            .map_or_else(|_| self.dir.get_file(&path).is_some(), |_| true))
    }

    fn walk_dir<'b>(self: Pin<&'b Self>, path: &Path) -> Result<Self::DirWalk<'b>>
    where
        'vfs: 'b,
    {
        let path = norm(path)?;
        Ok(IncludeDirWalker(get_dir_or_root(self.dir, &path)?, 0))
    }
}

/// The [`DirWalker`] implementation for [`IncludeDirVfs`].
pub struct IncludeDirWalker(Dir<'static>, usize);

impl<'a> DirWalker<'a> for IncludeDirWalker {
    fn next(&mut self) -> Option<Result<DirEntryInfo>> {
        self.0
            .dirs()
            .get(self.1)
            .map(|dir| DirEntryInfo {
                name: dir.path().file_name().unwrap().to_owned(),
                path: dir.path().to_path_buf(),
                kind: DirEntryKind::Directory,
            })
            .or_else(|| {
                self.0
                    .files()
                    .get(self.1 - self.0.dirs().len())
                    .map(|file| DirEntryInfo {
                        name: file.path().file_name().unwrap().to_owned(),
                        path: file.path().to_path_buf(),
                        kind: DirEntryKind::File,
                    })
            })
            .map(|entry| {
                self.1 += 1;
                Ok(entry)
            })
    }
}
