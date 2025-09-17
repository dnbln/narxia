//! Error type, see [`Error`].

use std::error;
use std::fmt;
use std::io;
use std::result;

use crate::traits::vfs;
use crate::traits::vfs::OwnedPathType;

/// The error type for this library.
#[derive(Debug)]
pub enum Error<P> {
    /// An IO error.
    Io(P, io::Error),
    /// Parse error.
    Parse(P, Box<dyn error::Error + Send + Sync>),
    /// Write error.
    Write(P, Box<dyn error::Error + Send + Sync>),
    /// Serde error.
    Serde(P, Box<dyn error::Error + Send + Sync>),

    /// An error related to the directory structure.
    UnexpectedNumberOfChildren {
        /// The expected number of children.
        expected: &'static str,
        /// How many children were found.
        found: usize,
        /// The path to the directory where this happened.
        path: P,
    },
}

impl<P: fmt::Debug> error::Error for Error<P> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::Io(_, e) => Some(e),
            Self::Parse(_, e) => Some(e.as_ref()),
            Self::Write(_, e) => Some(e.as_ref()),
            Self::Serde(_, e) => Some(e.as_ref()),
            Self::UnexpectedNumberOfChildren { .. } => None,
        }
    }
}

impl<P: fmt::Debug> fmt::Display for Error<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(path, e) => write!(f, "IO error at {:?}: {}", path, e),
            Self::Parse(path, e) => write!(f, "Parse error at {:?}: {}", path, e),
            Self::Write(path, e) => write!(f, "Write error at {:?}: {}", path, e),
            Self::Serde(path, e) => write!(f, "Serde error at {:?}: {}", path, e),
            Self::UnexpectedNumberOfChildren {
                expected,
                found,
                path,
            } => write!(
                f,
                "Unexpected number of children: expected {}, found {} at {:?}",
                expected, found, path
            ),
        }
    }
}

mod sealed {
    use std::io;

    pub trait Sealed {}

    impl<T> Sealed for io::Result<T> {}
}

/// A trait for wrapping IO errors with the path where they happened, turning [`std::io::Result`]s into [`crate::Result`]s.
pub trait WrapIoError<PathType: vfs::PathType + ?Sized>: Sized + sealed::Sealed {
    /// The inner type.
    type Output;

    /// Wrap the IO error with the path where it happened.
    fn wrap_io_error(
        self,
        get_path: impl FnOnce() -> PathType::OwnedPath,
    ) -> Result<Self::Output, PathType::OwnedPath>;

    /// Wrap the IO error with the given path.
    fn wrap_io_error_with(self, path: &PathType) -> Result<Self::Output, PathType::OwnedPath> {
        self.wrap_io_error(|| path.owned())
    }
}

impl<T, P: vfs::PathType + ?Sized> WrapIoError<P> for io::Result<T> {
    type Output = T;

    fn wrap_io_error(self, get_path: impl FnOnce() -> P::OwnedPath) -> Result<Self::Output, P::OwnedPath> {
        self.map_err(|e| Error::Io(get_path(), e))
    }
}

/// The result type for this library.
///
/// See [the `Error` enum](Error) for the errors that can happen.
pub type Result<T, P: OwnedPathType> = result::Result<T, Error<P>>;
