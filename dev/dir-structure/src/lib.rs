//! A library for reading and writing directory structures.
//!
//! This library provides a macro for defining directory structures, and a
//! trait for reading and writing those structures to / from disk.
//!
//! # Example
//!
//! ## Writing a structure to disk
//! ```
//! use std::path::Path;
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     use dir_structure::DirStructureItem;
//!     #[derive(dir_structure::DirStructure)]
//!     struct Dir {
//!         #[dir_structure(path = "f1.txt")]
//!         f1: String,
//!         #[dir_structure(path = "subdir/f2.txt")]
//!         f2: String,
//!         // default path is just a file name from the field's name.
//!         f3: String,
//!         // also works with nested structures
//!         #[dir_structure(path = "subdir2")]
//!         subdir: Subdir,
//!     }
//!     #[derive(dir_structure::DirStructure)]
//!     struct Subdir {
//!         #[dir_structure(path = "f4.txt")]
//!         f4: String,
//!     }
//!
//!     let d = Path::new("dir");
//!     Dir {
//!         f1: "f1".to_owned(),
//!         f2: "f2".to_owned(),
//!         f3: "f3".to_owned(),
//!         subdir: Subdir {
//!             f4: "f4".to_owned(),
//!         },
//!     }.write(&d)?;
//!     assert_eq!(std::fs::read_to_string(d.join("f1.txt"))?, "f1");
//!     assert_eq!(std::fs::read_to_string(d.join("subdir/f2.txt"))?, "f2");
//!     assert_eq!(std::fs::read_to_string(d.join("f3"))?, "f3");
//!     assert_eq!(std::fs::read_to_string(d.join("subdir2/f4.txt"))?, "f4");
//!
//!     # std::fs::remove_dir_all(&d)?;
//!
//!     Ok(())
//! }
//! ```
//!
//! ## Reading a structure from disk
//!
//! ```
//! use std::path::Path;
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     use dir_structure::DirStructureItem;
//!     #[derive(dir_structure::DirStructure)]
//!     struct Dir {
//!         #[dir_structure(path = "f1.txt")]
//!         f1: String,
//!         #[dir_structure(path = "subdir/f2.txt")]
//!         f2: String,
//!         // default path is just a file name from the field's name.
//!         f3: String,
//!         // also works with nested structures
//!         #[dir_structure(path = "subdir2")]
//!         subdir: Subdir,
//!     }
//!     #[derive(dir_structure::DirStructure)]
//!     struct Subdir {
//!         #[dir_structure(path = "f4.txt")]
//!         f4: String,
//!     }
//!     let d = Path::new("dir");
//!     std::fs::create_dir_all(&d)?;
//!     std::fs::create_dir_all(d.join("subdir"))?;
//!     std::fs::create_dir_all(d.join("subdir2"))?;
//!     std::fs::write(d.join("f1.txt"), "f1")?;
//!     std::fs::write(d.join("subdir/f2.txt"), "f2")?;
//!     std::fs::write(d.join("f3"), "f3")?;
//!     std::fs::write(d.join("subdir2/f4.txt"), "f4")?;
//!     let dir = Dir::read(&d)?;
//!     assert_eq!(dir.f1, "f1");
//!     assert_eq!(dir.f2, "f2");
//!     assert_eq!(dir.f3, "f3");
//!     assert_eq!(dir.subdir.f4, "f4");
//!
//!     # std::fs::remove_dir_all(&d)?;
//!
//!     Ok(())
//! }
//! ```

use std::ffi::{OsStr, OsString};
use std::fmt::Display;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::str::FromStr;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("IO error at {0:?}: {1}")]
    Io(PathBuf, #[source] std::io::Error),
    #[error("Parse error at {0:?}: {1}")]
    Parse(PathBuf, #[source] Box<dyn std::error::Error + Send + Sync>),
}

trait WrapIoError: Sized {
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

/// The main trait. This is implemented for
/// all directory structures by the derive macro.
///
/// This trait doesn't have any methods, just a supertype:
/// [`DirStructureItem`].
pub trait DirStructure: DirStructureItem {}

/// Helper trait, implemented for all types that have a [`ReadFrom`]
/// and [`WriteTo`] implementation.
pub trait DirStructureItem: ReadFrom + WriteTo {
    /// Uses the [`ReadFrom`] implementation to read the structure from
    /// disk, from the specified path.
    fn read(path: impl AsRef<Path>) -> Result<Self>
    where
        Self: Sized,
    {
        Self::read_from(path.as_ref())
    }

    /// Uses the [`WriteTo`] implementation to write the structure
    /// to disk at the specified path.
    fn write(&self, path: impl AsRef<Path>) -> Result<()> {
        self.write_to(path.as_ref())
    }
}

// Blanket impl.
impl<T> DirStructureItem for T where T: ReadFrom + WriteTo {}

/// Trait for types / structures that can be
/// read from disk, either from a file or a directory.
pub trait ReadFrom {
    /// Reads the structure from the specified path, which
    /// can be either a file or a directory.
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized;
}

/// Trait for types / structures that can be
/// written to disk. All types in the library that
/// write to files first check that the parent
/// directories exist, so implementations of
/// this that create the whole directory are
/// not necessary (unless used empty children
/// directories, in which case no directories will
/// really be created).
pub trait WriteTo {
    /// Writes the structure to the specified path.
    fn write_to(&self, path: &Path) -> Result<()>;
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
pub trait FromRefForWriter<'a> {
    /// The inner type to cast.
    type Inner: ?Sized;
    /// The reference type to cast to.
    type Wr: WriteTo + 'a;

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

/// A directory structure where we don't know the names of the folders at compile-time,
/// and as such we cannot use the derive macro.
///
/// Instead we know that all the entries in the directory are folders,
/// and that they all have the same structure inside (defined by the [`T`] type parameter),
/// or they are all files (which can be read with [`DirChildren`]<[`String`]> for example).
///
/// In either case, [`<T as ReadFrom>::read_from`] must be able to read all the entries in
/// the directory.
///
/// The [`WriteTo`] implementation will directly write the children to the directory it
/// is passed, with no regards to the path stored in `self_path`.
pub struct DirChildren<T>
where
    T: DirStructureItem,
{
    /// The path to the root directory.
    ///
    /// This path doesn't influence writing in any way, it is only to
    /// point out the directory after it has been read and parsed.
    pub self_path: PathBuf,
    /// The children of the root directory.
    pub children: Vec<DirChild<T>>,
}

impl<T> DirChildren<T>
where
    T: DirStructureItem,
{
    /// Creates an empty [`DirChildren`], with no children.
    pub fn new() -> Self {
        Self {
            self_path: PathBuf::new(),
            children: Vec::new(),
        }
    }

    /// Creates a [`DirChildren`] with the given path and children.
    pub fn with_children_from_iter(
        self_path: impl Into<PathBuf>,
        children: impl IntoIterator<Item = DirChild<T>>,
    ) -> Self {
        Self {
            self_path: self_path.into(),
            children: children.into_iter().collect(),
        }
    }

    /// Returns the number of children.
    pub fn len(&self) -> usize {
        self.children.len()
    }

    /// Gets the child at the specified index.
    pub fn get(&self, index: usize) -> Option<&DirChild<T>> {
        self.children.get(index)
    }

    /// Gets the child with the specified "file" name (last segment of path).
    pub fn get_name(&self, name: impl AsRef<OsStr>) -> Option<&DirChild<T>> {
        self.children
            .iter()
            .find(|child| child.file_name == name.as_ref())
    }

    /// Returns an iterator over the children.
    pub fn iter(&self) -> DirChildrenIter<'_, T> {
        DirChildrenIter(self.children.iter())
    }
}

impl<T> ReadFrom for DirChildren<T>
where
    T: DirStructureItem,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        let mut children = Vec::new();
        for child in path.read_dir().wrap_io_error_with(path)? {
            let child = child.wrap_io_error_with(path)?;
            let file_name = child.file_name();
            let value = T::read_from(&child.path())?;
            children.push(DirChild { file_name, value });
        }

        Ok(DirChildren {
            self_path: path.to_path_buf(),
            children,
        })
    }
}

impl<T> WriteTo for DirChildren<T>
where
    T: DirStructureItem,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        for child in &self.children {
            let child_path = path.join(&child.file_name);
            child.value.write_to(&child_path)?;
        }

        Ok(())
    }
}

/// A single child of a [`DirChildren`] structure.
pub struct DirChild<T>
where
    T: DirStructureItem,
{
    /// The file name of the child.
    file_name: OsString,
    /// The parsed value of the child.
    value: T,
}

impl<T> DirChild<T>
where
    T: DirStructureItem,
{
    /// Creates a new [`DirChild`] with the specified file name and value.
    pub fn new(file_name: impl Into<OsString>, value: T) -> Self {
        Self {
            file_name: file_name.into(),
            value,
        }
    }

    /// Gets the file name of the child (or the name of the directory; the last segment in the path).
    pub fn file_name(&self) -> &OsString {
        &self.file_name
    }

    /// Gets the file name of the child (or the name of the directory; the last segment in the path).
    ///
    /// Mutable reference version of [`Self::file_name`].
    pub fn file_name_mut(&mut self) -> &mut OsString {
        &mut self.file_name
    }

    /// Gets the value of the child.
    ///
    /// This is the parsed value of the file / directory.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Gets the value of the child.
    ///
    /// This is the parsed value of the file / directory.
    ///
    /// Mutable reference version of [`Self::value`].
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> IntoIterator for DirChildren<T>
where
    T: DirStructureItem,
{
    type Item = DirChild<T>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

/// A [`DirChildren`] iterator. It iterates over the children of a
/// [`DirChildren`] structure.
pub struct DirChildrenIter<'a, T: DirStructureItem>(std::slice::Iter<'a, DirChild<T>>);

impl<'a, T> Iterator for DirChildrenIter<'a, T>
where
    T: DirStructureItem,
{
    type Item = &'a DirChild<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> ExactSizeIterator for DirChildrenIter<'a, T>
where
    T: DirStructureItem,
{
    fn len(&self) -> usize {
        self.0.len()
    }
}

/// A simple macro that generates a DirChildren<T> newtype, together with
/// a few impls to make it easy to use.
#[macro_export]
macro_rules! dir_children_wrapper {
    ($vis:vis $name:ident $ty:ty) => {
        $vis struct $name(pub $crate::DirChildren<$ty>);

        impl $crate::ReadFrom for $name {
            fn read_from(path: &::std::path::Path) -> $crate::Result<Self>
            where
                Self: Sized,
            {
                Ok(Self(<$crate::DirChildren<$ty>>::read_from(path)?))
            }
        }

        impl $crate::WriteTo for $name {
            fn write_to(&self, path: &::std::path::Path) -> $crate::Result<()> {
                self.0.write_to(path)
            }
        }

        impl std::ops::Deref for $name {
            type Target = $crate::DirChildren<$ty>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl std::ops::DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl std::iter::IntoIterator for $name {
            type Item = $crate::DirChild<$ty>;
            type IntoIter = std::vec::IntoIter<Self::Item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.into_iter()
            }
        }
    };
}

pub use dir_structure_macros::DirStructure;

#[cfg(feature = "json")]
pub mod json {
    //! With the `json` feature, this module provides the [`Json`] type,
    //!
    //! This allows us to read and parse json files to some `serde::Deserialize` type,
    //! and write them back to disk.
    use std::fmt;
    use std::fmt::Formatter;
    use std::path::Path;
    use std::str::FromStr;

    use crate::{FromRefForWriter, NewtypeToInner, ReadFrom, WriteTo};

    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to json when we read / write a
    /// directory structure.
    #[derive(Debug, Copy, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Hash)]
    #[serde(transparent)]
    pub struct Json<T>(#[serde(bound = "")] pub T)
    where
        T: 'static + serde::Serialize + for<'d> serde::Deserialize<'d>;

    impl<T> FromStr for Json<T>
    where
        T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
    {
        type Err = serde_json::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            serde_json::from_str(s).map(Self)
        }
    }

    struct JsonToStr<'a, T>(&'a T)
    where
        T: serde::Serialize + 'a;

    impl<'a, T> fmt::Display for JsonToStr<'a, T>
    where
        T: serde::Serialize + 'a,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            let s = serde_json::to_string(&self.0).map_err(|_| fmt::Error)?;
            write!(f, "{}", s)
        }
    }

    impl<T> fmt::Display for Json<T>
    where
        T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            JsonToStr(&self.0).fmt(f)
        }
    }

    impl<T> ReadFrom for Json<T>
    where
        T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
    {
        fn read_from(path: &Path) -> crate::Result<Self> {
            let contents = crate::FileString::read_from(path)?.0;
            let v = serde_json::from_str::<Self>(&contents)
                .map_err(|e| crate::Error::Parse(path.to_path_buf(), e.into()))?;
            Ok(v)
        }
    }

    impl<T> WriteTo for Json<T>
    where
        T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
    {
        fn write_to(&self, path: &Path) -> crate::Result<()> {
            Self::from_ref_for_writer(&self.0).write_to(path)
        }
    }

    impl<T> NewtypeToInner for Json<T>
    where
        T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
    {
        type Inner = T;

        fn into_inner(self) -> Self::Inner {
            self.0
        }
    }

    impl<'a, T> FromRefForWriter<'a> for Json<T>
    where
        T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
    {
        type Inner = T;
        type Wr = JsonWr<'a, T>;

        fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
            JsonWr(value)
        }
    }

    /// [`WriteTo`] impl for [`Json`].
    pub struct JsonWr<'a, T>(&'a T)
    where
        T: serde::Serialize + 'a;

    impl<'a, T> WriteTo for JsonWr<'a, T>
    where
        T: serde::Serialize + 'a,
    {
        fn write_to(&self, path: &Path) -> crate::Result<()> {
            crate::FileString::from_ref_for_writer(&format!("{}", JsonToStr(self.0))).write_to(path)
        }
    }
}

/// A wrapper around a type which will use the [`Display`] and [`FromStr`] implementations
/// for serialization / deserialization.
///
/// For example: u8, i8, i16, u16, all integer types... bool etc.
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

impl<T> WriteTo for FmtWrapper<T>
where
    T: Display,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
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

/// A [`WriteTo`] wrapper around a reference to a type which will use the [`Display`]
/// implementation to write the value.
pub struct FmtWrapperRefWr<'a, T: ?Sized>(pub &'a T);

impl<'a, T> WriteTo for FmtWrapperRefWr<'a, T>
where
    T: Display + ?Sized,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        use std::io::Write;
        utils::create_parent_dir(path)?;
        let mut f = File::create(path).wrap_io_error_with(path)?;
        write!(f, "{}", self.0).wrap_io_error_with(path)?;
        Ok(())
    }
}

/// A newtype around a [`Vec`]<[`u8`]>.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileBytes(pub Vec<u8>);

impl FileBytes {
    /// Creates a new [`FileBytes`] from the specified [`Vec`]<[`u8`]>.
    pub fn new(v: impl Into<Vec<u8>>) -> Self {
        Self(v.into())
    }
}

impl ReadFrom for FileBytes {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        std::fs::read(path).wrap_io_error_with(path).map(Self)
    }
}

impl WriteTo for FileBytes {
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

impl From<FileBytes> for Vec<u8> {
    fn from(value: FileBytes) -> Self {
        value.0
    }
}

impl NewtypeToInner for FileBytes {
    type Inner = Vec<u8>;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

impl<'a> FromRefForWriter<'a> for FileBytes {
    type Inner = [u8];
    type Wr = FileBytesRefWr<'a>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileBytesRefWr(value)
    }
}

/// The [`WriteTo`] wrapper around a reference to a `[u8]`.
pub struct FileBytesRefWr<'a>(&'a [u8]);

impl<'a> WriteTo for FileBytesRefWr<'a> {
    fn write_to(&self, path: &Path) -> Result<()> {
        utils::create_parent_dir(path)?;
        std::fs::write(path, self.0).wrap_io_error_with(path)?;
        Ok(())
    }
}

/// A newtype around a [`String`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileString(pub String);

impl FileString {
    /// Creates a new [`FileString`] from the specified [`String`].
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }
}

impl From<FileString> for String {
    fn from(value: FileString) -> Self {
        value.0
    }
}

impl NewtypeToInner for FileString {
    type Inner = String;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

impl ReadFrom for FileString {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        std::fs::read_to_string(path)
            .wrap_io_error_with(path)
            .map(Self)
    }
}

impl WriteTo for FileString {
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

impl<'a> FromRefForWriter<'a> for FileString {
    type Inner = str;
    type Wr = FileStrWr<'a>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        FileStrWr(value)
    }
}

/// The [`WriteTo`] wrapper around a reference to a [`str`].
pub struct FileStrWr<'a>(&'a str);

impl WriteTo for FileStrWr<'_> {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytes::from_ref_for_writer(self.0.as_bytes()).write_to(path)
    }
}

impl<T> ReadFrom for Option<T>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        if path.exists() {
            T::read_from(path).map(Some)
        } else {
            Ok(None)
        }
    }
}

impl<T> WriteTo for Option<T>
where
    T: WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        if let Some(v) = self {
            v.write_to(path)
        } else {
            Ok(())
        }
    }
}

/// A wrapper that defers the reading of a file until it is actually needed.
#[derive(Debug, Clone, Hash)]
pub struct DeferredRead<T>(pub PathBuf, std::marker::PhantomData<T>)
where
    T: ReadFrom;

impl<T> ReadFrom for DeferredRead<T>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(path.to_path_buf(), std::marker::PhantomData))
    }
}

impl<T> DeferredRead<T>
where
    T: ReadFrom,
{
    /// Performs the read and returns the value.
    pub fn perform_read(&self) -> Result<T> {
        T::read_from(&self.0)
    }
}

impl<T> WriteTo for DeferredRead<T>
where
    T: ReadFrom + WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        if path == self.0 {
            // Optimization: We were asked to write to the same path
            // we are supposed to read from. We can just ignore it, since
            // the file / directory should already be in the given state.

            // If T doesn't have non-trivial ReadFrom / WriteTo implementations,
            // this should not be a problem, but if it is, a custom DeferredRead
            // implementation should be written for it.
            return Ok(());
        }

        let r = self.perform_read()?;
        r.write_to(path)
    }
}

/// A wrapper that defers the reading of a file until it is actually needed.
///
/// It can also store the value.
#[derive(Debug, Clone, Hash)]
pub enum DeferredReadOrOwn<T>
where
    T: ReadFrom,
{
    Own(T),
    Deferred(DeferredRead<T>),
}

impl<T> DeferredReadOrOwn<T>
where
    T: ReadFrom,
{
    /// Gets the value. If it is not already read, it will read it, but without saving it.
    pub fn get(&self) -> Result<T>
    where
        T: Clone,
    {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own.clone()),
            DeferredReadOrOwn::Deferred(d) => Ok(d.perform_read()?),
        }
    }

    /// Performs the read and stores the value. If the value is already read, it will
    /// just return a reference to it.
    pub fn perform_and_store_read(&mut self) -> Result<&T> {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own),
            DeferredReadOrOwn::Deferred(d) => {
                let value = d.perform_read()?;
                *self = DeferredReadOrOwn::Own(value);
                let DeferredReadOrOwn::Own(own) = self else {
                    unreachable!()
                };
                Ok(own)
            }
        }
    }
}

impl<T> ReadFrom for DeferredReadOrOwn<T>
where
    T: ReadFrom,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        ReadFrom::read_from(path).map(Self::Deferred)
    }
}

impl<T> WriteTo for DeferredReadOrOwn<T>
where
    T: ReadFrom + WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        match self {
            DeferredReadOrOwn::Own(own) => own.write_to(path),
            DeferredReadOrOwn::Deferred(d) => d.write_to(path),
        }
    }
}

/// A newtype that will clean the directory it is written to, before writing
/// the value.
///
/// This is useful when we want to write a directory structure, but we want
/// to make sure that the directory is clean before writing it, so that there
/// are no old files / directories left in it.
pub struct CleanDir<T: DirStructureItem>(pub T);

impl<T> ReadFrom for CleanDir<T>
where
    T: DirStructureItem,
{
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        Ok(Self(T::read_from(path)?))
    }
}

impl<T> WriteTo for CleanDir<T>
where
    T: DirStructureItem,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        Self::from_ref_for_writer(&self.0).write_to(path)
    }
}

impl<'a, T> FromRefForWriter<'a> for CleanDir<T>
where
    T: DirStructureItem + 'a,
{
    type Inner = T;
    type Wr = CleanDirRefWr<'a, T>;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
        CleanDirRefWr(value)
    }
}

impl<T> NewtypeToInner for CleanDir<T>
where
    T: DirStructureItem,
{
    type Inner = T;

    fn into_inner(self) -> Self::Inner {
        self.0
    }
}

/// [`WriteTo`] impl for [`CleanDir`]
pub struct CleanDirRefWr<'a, T: ?Sized + DirStructureItem>(&'a T);

impl<'a, T> WriteTo for CleanDirRefWr<'a, T>
where
    T: ?Sized + DirStructureItem,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        if path.exists() {
            std::fs::remove_dir_all(path).wrap_io_error_with(path)?;
        } else {
            utils::create_parent_dir(path)?;
        }
        self.0.write_to(path)
    }
}

// Impls for std types.

impl ReadFrom for String {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        FileString::read_from(path).map(|v| v.0)
    }
}

impl WriteTo for String {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileString::from_ref_for_writer(self).write_to(path)
    }
}

impl ReadFrom for Vec<u8> {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized,
    {
        FileBytes::read_from(path).map(|v| v.0)
    }
}

impl WriteTo for Vec<u8> {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytes::from_ref_for_writer(self).write_to(path)
    }
}

impl WriteTo for str {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileStrWr(self).write_to(path)
    }
}

impl<'a> WriteTo for &'a str {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileStrWr(self).write_to(path)
    }
}

impl WriteTo for [u8] {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytesRefWr(self).write_to(path)
    }
}

impl<'a> WriteTo for &'a [u8] {
    fn write_to(&self, path: &Path) -> Result<()> {
        FileBytesRefWr(self).write_to(path)
    }
}

mod utils {
    use crate::WrapIoError;

    pub fn create_parent_dir(path: &std::path::Path) -> crate::Result<()> {
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                std::fs::create_dir_all(parent).wrap_io_error_with(parent)?;
            }
        }
        Ok(())
    }
}
