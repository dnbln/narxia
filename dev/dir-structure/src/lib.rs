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

pub trait WrapIoError: Sized {
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

pub trait DirStructure: DirStructureItem {}

pub trait DirStructureItem: ReadFrom + WriteTo {
    fn read(path: impl AsRef<Path>) -> Result<Self>
    where
        Self: Sized,
    {
        Self::read_from(path.as_ref())
    }
    fn write(&self, path: impl AsRef<Path>) -> Result<()> {
        self.write_to(path.as_ref())
    }
}

impl<T> DirStructureItem for T where T: ReadFrom + WriteTo {}

pub trait ReadFrom {
    fn read_from(path: &Path) -> Result<Self>
    where
        Self: Sized;
}

pub trait WriteTo {
    fn write_to(&self, path: &Path) -> Result<()>;
}

pub trait FromRefForWriter<'a> {
    type Inner: ?Sized;
    type Wr: WriteTo + 'a;

    fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr;
}

pub trait NewtypeToInner {
    type Inner;

    fn into_inner(self) -> Self::Inner;
}

pub struct DirChildren<T>
where
    T: DirStructureItem,
{
    pub self_path: PathBuf,
    pub children: Vec<DirChild<T>>,
}

impl<T> DirChildren<T>
where
    T: DirStructureItem,
{
    pub fn new(self_path: impl Into<PathBuf>) -> Self {
        Self {
            self_path: self_path.into(),
            children: Vec::new(),
        }
    }

    pub fn with_children_from_iter(
        self_path: impl Into<PathBuf>,
        children: impl IntoIterator<Item = DirChild<T>>,
    ) -> Self {
        Self {
            self_path: self_path.into(),
            children: children.into_iter().collect(),
        }
    }

    pub fn len(&self) -> usize {
        self.children.len()
    }

    pub fn get(&self, index: usize) -> Option<&DirChild<T>> {
        self.children.get(index)
    }

    pub fn get_name(&self, name: impl AsRef<OsStr>) -> Option<&DirChild<T>> {
        self.children
            .iter()
            .find(|child| child.file_name == name.as_ref())
    }

    pub fn iter(&self) -> DirStructureIter<'_, T> {
        DirStructureIter(self.children.iter())
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

pub struct DirChild<T>
where
    T: DirStructureItem,
{
    file_name: OsString,
    value: T,
}

impl<T> DirChild<T>
where
    T: DirStructureItem,
{
    pub fn new(file_name: impl Into<OsString>, value: T) -> Self {
        Self {
            file_name: file_name.into(),
            value,
        }
    }

    pub fn file_name(&self) -> &OsString {
        &self.file_name
    }

    pub fn file_name_mut(&mut self) -> &mut OsString {
        &mut self.file_name
    }

    pub fn value(&self) -> &T {
        &self.value
    }

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

pub struct DirStructureIter<'a, T: DirStructureItem>(std::slice::Iter<'a, DirChild<T>>);

impl<'a, T> Iterator for DirStructureIter<'a, T>
where
    T: DirStructureItem,
{
    type Item = (&'a OsString, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|child| (&child.file_name, &child.value))
    }
}

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
    use std::fmt;
    use std::fmt::Formatter;
    use std::str::FromStr;

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

    impl<T> fmt::Display for Json<T>
    where
        T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            let s = serde_json::to_string(&self.0).map_err(|_| fmt::Error)?;
            write!(f, "{}", s)
        }
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileBytes(pub Vec<u8>);

impl FileBytes {
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

pub struct FileBytesRefWr<'a>(&'a [u8]);

impl<'a> WriteTo for FileBytesRefWr<'a> {
    fn write_to(&self, path: &Path) -> Result<()> {
        utils::create_parent_dir(path)?;
        std::fs::write(path, self.0).wrap_io_error_with(path)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileString(pub String);

impl FileString {
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
    pub fn perform_read(&self) -> Result<T> {
        T::read_from(&self.0)
    }
}

impl<T> WriteTo for DeferredRead<T>
where
    T: ReadFrom + WriteTo,
{
    fn write_to(&self, path: &Path) -> Result<()> {
        let r = self.perform_read()?;
        r.write_to(path)
    }
}

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
    pub fn get(&self) -> Result<T>
    where
        T: Clone,
    {
        match self {
            DeferredReadOrOwn::Own(own) => Ok(own.clone()),
            DeferredReadOrOwn::Deferred(d) => Ok(d.perform_read()?),
        }
    }

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
