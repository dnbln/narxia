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
        expected: &'static str,
        found: usize,
        path: PathBuf,
    },
}

mod sealed {
    use std::io;

    pub trait Sealed {}

    impl<T> Sealed for io::Result<T> {}
}

pub trait WrapIoError: Sized + sealed::Sealed {
    type Output;

    fn wrap_io_error(self, get_path: impl FnOnce() -> PathBuf) -> Result<Self::Output>;

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

pub type Result<T> = result::Result<T, Error>;
