use std::fmt::Display;
use std::path::Path;
#[cfg(feature = "async")]
use std::path::PathBuf;
#[cfg(feature = "async")]
use std::pin::Pin;
use std::str::FromStr;

use crate::Error;
use crate::FileString;
use crate::FromRefForWriter;
#[cfg(feature = "async")]
use crate::FromRefForWriterAsync;
use crate::NewtypeToInner;
use crate::ReadFrom;
#[cfg(feature = "async")]
use crate::ReadFromAsync;
use crate::Result;
use crate::WrapIoError;
use crate::WriteTo;
#[cfg(feature = "async")]
use crate::WriteToAsync;
#[cfg(feature = "async")]
use crate::WriteToAsyncOwned;
use crate::utils;

/// A wrapper around a type which will use the [`Display`] and [`FromStr`] implementations
/// for serialization / deserialization.
///
/// For example: u8, i8, i16, u16, all integer types... bool etc.
///
/// # Examples
///
/// ```rust
/// use std::path::Path;
/// use dir_structure::DirStructureItem;
///
/// use dir_structure::FmtWrapper;
///
/// #[derive(dir_structure::DirStructure, PartialEq, Debug)]
/// struct Dir {
///    #[dir_structure(path = "f.txt", with_newtype = FmtWrapper<u8>)]
///    f: u8,
///    #[dir_structure(path = "b.txt", with_newtype = FmtWrapper<bool>)]
///    b: bool,
/// }
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let d = Path::new("dir");
///     std::fs::create_dir_all(&d)?;
///     std::fs::write(d.join("f.txt"), "42")?;
///     std::fs::write(d.join("b.txt"), "true")?;
///     let mut dir = Dir::read(&d)?;
///     assert_eq!(dir.f, 42);
///     assert_eq!(dir.b, true);
///     dir.f = 100;
///     dir.b = false;
///     dir.write(&d)?;
///     assert_eq!(std::fs::read_to_string(d.join("f.txt"))?, "100");
///     assert_eq!(std::fs::read_to_string(d.join("b.txt"))?, "false");
///     # std::fs::remove_dir_all(&d)?;
///     Ok(())
/// }
/// ```
pub struct FmtWrapper<T>(pub T);

impl<T> NewtypeToInner for FmtWrapper<T> {
    type Inner = T;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

impl<T> ReadFrom for FmtWrapper<T>
where
    T: FromStr,
    T::Err: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        let contents = FileString::read_from(path)?.0;
        match contents.parse::<T>() {
            Ok(v) => Ok(Self(v)),
            Err(e) => Err(Error::Parse(path.to_path_buf(), e.into())),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T> ReadFromAsync for FmtWrapper<T>
where
    T: FromStr + Send + 'static,
    T::Err: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send>>;

    fn read_from_async(path: PathBuf) -> Self::Future {
        Box::pin(async move {
            let contents = FileString::read_from_async(path.clone()).await?.0;
            match contents.parse::<T>() {
                Ok(v) => Ok(Self(v)),
                Err(e) => Err(Error::Parse(path, e.into())),
            }
        })
    }
}

impl<T> WriteTo for FmtWrapper<T>
where
    T: Display,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<T> WriteToAsync for FmtWrapper<T>
where
    T: Display + Send + Sync + 'static,
{
    type Future<'a> = <FmtWrapperRefWr<'a, T> as WriteToAsyncOwned<'a>>::Future;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        Self::from_ref_for_writer_async(&self.0).write_to_async_owned(path)
    }
}

impl<'a, T> FromRefForWriter<'a> for FmtWrapper<T>
where
    T: Display + 'a,
{
    type Inner = T;
    type Wr = FmtWrapperRefWr<'a, T>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FmtWrapperRefWr(value)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> FromRefForWriterAsync<'a> for FmtWrapper<T>
where
    T: Display + Send + 'a,
{
    type Inner = T;
    type Wr = FmtWrapperRefWr<'a, T>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        FmtWrapperRefWr(value)
    }
}

/// A [`WriteTo`] wrapper around a reference to a type which will use the [`Display`]
/// implementation to write the value.
pub struct FmtWrapperRefWr<'a, T: ?Sized>(pub &'a T);

impl<T> WriteTo for FmtWrapperRefWr<'_, T>
where
    T: Display + ?Sized,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        use std::io::Write;
        utils::create_parent_dir(path)?;
        let mut f = std::fs::File::create(path).wrap_io_error_with(path)?;
        write!(f, "{}", self.0).wrap_io_error_with(path)?;
        Ok(())
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> WriteToAsync for FmtWrapperRefWr<'a, T>
where
    T: Display + Send + 'a,
{
    type Future<'f>
        = <FileString as WriteToAsync>::Future<'f>
    where
        Self: 'f;

    fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
        let s = self.0.to_string();
        FileString::new(s).write_to_async_owned(path)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T> WriteToAsyncOwned<'a> for FmtWrapperRefWr<'a, T>
where
    T: Display + Send + 'a,
{
    type Future = <FileString as WriteToAsyncOwned<'a>>::Future;

    fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
        let s = self.0.to_string();
        FileString::new(s).write_to_async_owned(path)
    }
}
