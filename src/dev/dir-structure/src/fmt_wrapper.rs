//! [`ReadFrom`] and [`WriteTo`] implementations using [`FromStr::from_str`] and [`Display::fmt`].
//!
//! See [`FmtWrapper`] for more details.

use std::error;
use std::fmt::Display;
use std::marker;
use std::path::Path;
#[cfg(feature = "async")]
use std::path::PathBuf;
use std::pin::Pin;
use std::str::FromStr;

use crate::error::Error;
use crate::error::Result;
use crate::prelude::*;
use crate::std_types::FileString;
#[cfg(feature = "async")]
use crate::traits::asy::FromRefForWriterAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::WriteSupportingVfsAsync;
use crate::traits::sync::FromRefForWriter;
use crate::traits::sync::NewtypeToInner;
use crate::traits::vfs;

/// A wrapper around a type which will use the [`Display`] and [`FromStr`] implementations
/// for serialization / deserialization.
///
/// For example: u8, i8, i16, u16, all integer types... bool etc.
///
/// # Examples
///
/// ```rust
/// use std::path::Path;
///
/// use dir_structure::traits::sync::DirStructureItem;
/// use dir_structure::fmt_wrapper::FmtWrapper;
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
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct FmtWrapper<T>(pub T);

impl<T> NewtypeToInner for FmtWrapper<T> {
    type Inner = T;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

impl<'a, T, Vfs: vfs::Vfs> ReadFrom<'a, Vfs> for FmtWrapper<T>
where
    T: FromStr + 'a,
    T::Err: Into<Box<dyn error::Error + Send + Sync>>,
{
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>
    where
        Self: Sized,
    {
        let contents = FileString::read_from(path, vfs)?.0;
        match contents.parse::<T>() {
            Ok(v) => Ok(Self(v)),
            Err(e) => Err(Error::Parse(path.to_path_buf(), e.into())),
        }
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: VfsAsync + 'static> ReadFromAsync<'a, Vfs> for FmtWrapper<T>
where
    T: FromStr + Send + 'static,
    T::Err: Into<Box<dyn error::Error + Send + Sync>>,
{
    type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'a>>;

    fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        Box::pin(async move {
            let contents = FileString::read_from_async(path.clone(), vfs).await?.0;
            match contents.parse::<T>() {
                Ok(v) => Ok(Self(v)),
                Err(e) => Err(Error::Parse(path, e.into())),
            }
        })
    }
}

impl<T, Vfs: vfs::WriteSupportingVfs> WriteTo<Vfs> for FmtWrapper<T>
where
    T: Display,
{
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for FmtWrapper<T>
where
    T: Display + Send + Sync + 'static,
{
    type Future = <FileString as WriteToAsync<'a, Vfs>>::Future;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        let s = self.0.to_string();
        FileString::new(s).write_to_async(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsyncRef<'a, Vfs> for FmtWrapper<T>
where
    T: Display + Send + Sync + 'a,
{
    type Future<'b> = <FileString as WriteToAsync<'b, Vfs>>::Future
    where
        Self: 'b,
        'a: 'b,
        Vfs: 'b;

    fn write_to_async_ref<'b>(&'b self, path: PathBuf, vfs: Pin<&'b Vfs>) -> Self::Future<'b>
    where
        'a: 'b,
    {
        let s = self.0.to_string();
        FileString::new(s).write_to_async(path, vfs)
    }
}

impl<'a, T, Vfs: vfs::WriteSupportingVfs> FromRefForWriter<'a, Vfs> for FmtWrapper<T>
where
    T: Display + 'a,
    Vfs: 'a,
{
    type Inner = T;
    type Wr = FmtWrapperRefWr<'a, T, Vfs>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FmtWrapperRefWr(value, marker::PhantomData)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> FromRefForWriterAsync<'a, Vfs> for FmtWrapper<T>
where
    T: Display + Send + 'a,
{
    type Inner = T;
    type Wr = FmtWrapperRefWr<'a, T, Vfs>;

    fn from_ref_for_writer_async(value: &'a Self::Inner) -> Self::Wr {
        FmtWrapperRefWr(value, marker::PhantomData)
    }
}

/// A [`WriteTo`] wrapper around a reference to a type which will use the [`Display`]
/// implementation to write the value.
pub struct FmtWrapperRefWr<'a, T: ?Sized, Vfs>(pub &'a T, marker::PhantomData<Vfs>);

impl<T, Vfs: vfs::WriteSupportingVfs> WriteTo<Vfs> for FmtWrapperRefWr<'_, T, Vfs>
where
    T: Display + ?Sized,
{
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()> {
        FileString::new(self.0.to_string()).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs>
    for FmtWrapperRefWr<'a, T, Vfs>
where
    T: Display + Send + 'a,
{
    type Future = <FileString as WriteToAsync<'a, Vfs>>::Future;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
        let s = self.0.to_string();
        FileString::new(s).write_to_async(path, vfs)
    }
}
