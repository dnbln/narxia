//! Synchronous reading / writing traits.

use std::path::Path;
use std::pin::Pin;

use crate::error::Result;
use crate::traits::vfs;
use crate::vfs::fs_vfs;

/// The main trait. This is implemented for
/// all directory structures by the derive macro.
///
/// This trait doesn't have any methods, just a supertype:
/// [`DirStructureItem`].
pub trait DirStructure: DirStructureItem {}

/// Helper trait, implemented for all types that have a [`ReadFrom`]
/// and [`WriteTo`] implementation.
pub trait DirStructureItem {
    /// Uses the [`ReadFrom`] implementation to read the structure from
    /// disk, from the specified path.
    fn read(path: impl AsRef<Path>) -> Result<Self>
    where
        Self: ReadFrom<'static, fs_vfs::FsVfs> + Sized,
    {
        Self::read_from(path.as_ref(), Pin::new(&fs_vfs::FsVfs))
    }

    /// Uses the [`WriteTo`] implementation to write the structure
    /// to disk at the specified path.
    fn write(&self, path: impl AsRef<Path>) -> Result<()>
    where
        Self: WriteTo<fs_vfs::FsVfs>,
    {
        self.write_to(path.as_ref(), Pin::new(&fs_vfs::FsVfs))
    }
}

// Blanket impl.
impl<T> DirStructureItem for T {}

/// Trait for types / structures that can be
/// read from disk, either from a file or a directory.
pub trait ReadFrom<'a, Vfs: vfs::Vfs>: Sized + 'a {
    /// Reads the structure from the specified path, which
    /// can be either a file or a directory.
    fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self>;
}

/// Trait for types / structures that can be
/// written to disk. All types in the library that
/// write to files first check that the parent
/// directories exist, so implementations of
/// this that create the whole directory are
/// not necessary (unless used empty children
/// directories, in which case no directories will
/// really be created).
pub trait WriteTo<Vfs: vfs::WriteSupportingVfs> {
    /// Writes the structure to the specified path.
    fn write_to(&self, path: &Path, vfs: Pin<&Vfs>) -> Result<()>;
}

/// Trait to use when using the `with_newtype` attribute.
///
/// This is used to convert a reference to a normal type
/// (like `String`, `Vec<u8>` etc. into a type that is a
/// reference to them, like `&str`, `&[u8]` etc.), so that
/// the `WriteTo` implementation can be written only for the
/// reference types, and all the other [`WriteTo`] impls will
/// only cast what they have to write to those reference types
/// (via the function below), and then call the [`WriteTo::write_to`]
/// method on that reference.
pub trait FromRefForWriter<'a, Vfs: vfs::WriteSupportingVfs + 'a> {
    /// The inner type to cast.
    type Inner: ?Sized;
    /// The reference type to cast to.
    type Wr: WriteTo<Vfs> + 'a;

    /// Casts the reference to the inner type to a [`WriteTo`]
    /// reference type.
    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr;
}

/// Trait to use when using the `with_newtype` attribute.
///
/// This is used to convert a newtype to its inner type.
/// We are using this because we cannot make blanket impls with
/// [`From`] due to the orphan rules.
pub trait NewtypeToInner {
    /// The inner type.
    type Inner;

    /// Converts the newtype to its inner type.
    fn into_inner(self) -> Self::Inner;
}

impl<'a, Vfs: vfs::Vfs> ReadFrom<'a, Vfs> for () {
    fn read_from(_path: &Path, _vfs: Pin<&'a Vfs>) -> Result<Self> {
        Ok(())
    }
}

impl<Vfs: vfs::WriteSupportingVfs> WriteTo<Vfs> for () {
    fn write_to(&self, _path: &Path, _vfs: Pin<&Vfs>) -> Result<()> {
        Ok(())
    }
}
