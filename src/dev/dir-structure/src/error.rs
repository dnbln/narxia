use std::error;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::result;

/// The error type for this library.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// An IO error.
    #[error("IO error at {0:?}: {1}")]
    Io(PathBuf, #[source] io::Error),
    /// Parse error.
    #[error("Parse error at {0:?}: {1}")]
    Parse(PathBuf, #[source] Box<dyn error::Error + Send + Sync>),
    /// Serde error.
    #[error("Serde error at {0:?}: {1}")]
    Serde(PathBuf, #[source] Box<dyn error::Error + Send + Sync>),

    /// An error related to the directory structure.
    #[error("Unexpected number of children: expected {expected}, found {found} at {path:?}")]
    UnexpectedNumberOfChildren {
        /// The expected number of children.
        expected: &'static str,
        /// How many children were found.
        found: usize,
        /// The path to the directory where this happened.
        path: PathBuf,
    },
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
