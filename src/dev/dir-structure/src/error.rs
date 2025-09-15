//! Error type, see [`Error`].

use std::error;
use std::fmt;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::result;

/// The error type for this library.
#[derive(Debug)]
pub enum Error {
    /// An IO error.
    Io(PathBuf, io::Error),
    /// Parse error.
    Parse(PathBuf, Box<dyn error::Error + Send + Sync>),
    /// Write error.
    Write(PathBuf, Box<dyn error::Error + Send + Sync>),
    /// Serde error.
    Serde(PathBuf, Box<dyn error::Error + Send + Sync>),

    /// An error related to the directory structure.
    UnexpectedNumberOfChildren {
        /// The expected number of children.
        expected: &'static str,
        /// How many children were found.
        found: usize,
        /// The path to the directory where this happened.
        path: PathBuf,
    },
}

impl error::Error for Error {
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

impl fmt::Display for Error {
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
pub trait WrapIoError: Sized + sealed::Sealed {
    /// The inner type.
    type Output;

    /// Wrap the IO error with the path where it happened.
    fn wrap_io_error(self, get_path: impl FnOnce() -> PathBuf) -> Result<Self::Output>;

    /// Wrap the IO error with the given path.
    fn wrap_io_error_with(self, path: &Path) -> Result<Self::Output> {
        self.wrap_io_error(|| path.to_path_buf())
    }
}

impl<T> WrapIoError for io::Result<T> {
    type Output = T;

    fn wrap_io_error(self, get_path: impl FnOnce() -> PathBuf) -> Result<Self::Output> {
        self.map_err(|e| Error::Io(get_path(), e))
    }
}

/// The result type for this library.
///
/// See [the `Error` enum](Error) for the errors that can happen.
pub type Result<T> = result::Result<T, Error>;
