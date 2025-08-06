use std::path::Path;
use std::path::PathBuf;

/// The error type for this library.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// An IO error.
    #[error("IO error at {0:?}: {1}")]
    Io(PathBuf, #[source] std::io::Error),
    /// Parse error.
    #[error("Parse error at {0:?}: {1}")]
    Parse(PathBuf, #[source] Box<dyn std::error::Error + Send + Sync>),
    /// Serde error.
    #[error("Serde error at {0:?}: {1}")]
    Serde(PathBuf, #[source] Box<dyn std::error::Error + Send + Sync>),
}

mod sealed {
    pub trait Sealed {}

    impl<T> Sealed for std::io::Result<T> {}
}

pub trait WrapIoError: Sized + sealed::Sealed {
    type Output;

    fn wrap_io_error(self, get_path: impl FnOnce() -> PathBuf) -> Result<Self::Output>;

    fn wrap_io_error_with(self, path: &Path) -> Result<Self::Output> {
        self.wrap_io_error(|| path.to_path_buf())
    }
}

impl<T> WrapIoError for std::io::Result<T> {
    type Output = T;

    fn wrap_io_error(self, get_path: impl FnOnce() -> PathBuf) -> Result<Self::Output> {
        self.map_err(|e| Error::Io(get_path(), e))
    }
}

pub type Result<T> = std::result::Result<T, Error>;
