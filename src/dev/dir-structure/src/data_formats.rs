//! Serde [`ReadFrom`](crate::traits::sync::ReadFrom) and [`WriteTo`](crate::traits::sync::WriteTo) implementations.

macro_rules! data_format_impl {
    (
        $(#[$mod_attr:meta])*
        $mod_name:ident,
        $(#[$main_ty_attrs:meta])*
        $main_ty:ident,

        $from_str_impl:expr,
        $from_str_error:ty,

        $(#[$to_str_ty_attrs:meta])*
        $to_str_ty:ident,
        $to_str_impl:expr,
        $to_writer_impl:expr,
        $to_str_error:ty,

        $(#[$writer_ty_attrs:meta])*
        $writer_ty:ident,

        $extension:literal,
        $text:literal $(,)?
    ) => {
        $(#[$mod_attr])*
        pub mod $mod_name {
            #![doc = concat!(r##"
With the `"##, stringify!($mod_name), r##"` feature, this module provides the [`"##, stringify!($main_ty), r##"`] type.

This allows us to read and parse `"##, stringify!($mod_name), r##"` files to some `serde::Deserialize` type,
and write them back to disk."##
            )]
            //!
            //! # Examples
            //!
            #![doc = concat!(r##"## Reading a "##, stringify!($mod_name), r##" file"##)]
            //!
            #![cfg_attr(feature = "derive", doc = "```rust")]
            #![cfg_attr(not(feature = "derive"), doc = "```rust,compile_fail")]
            //! use std::path::Path;
            //!
            //! use dir_structure::traits::sync::DirStructureItem;
            #![doc = concat!(r##"use dir_structure::data_formats::"##, stringify!($mod_name), "::", stringify!($main_ty), r##";"##)]
            //!
            //! #[derive(dir_structure::DirStructure)]
            //! struct Dir {
            #![doc = concat!(r##"    #[dir_structure(path = "f"##, $extension, r##"", with_newtype = "##, stringify!($main_ty), r##"<Obj>)]"##)]
            //!     f: Obj,
            //! }
            //!
            //! #[derive(Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
            //! struct Obj {
            //!     name: String,
            //!     age: u32,
            //! }
            //!
            //! fn main() -> Result<(), Box<dyn std::error::Error>> {
            //!     let d = Path::new("dir");
            //!     std::fs::create_dir_all(&d)?;
            #![doc = concat!(r##"    std::fs::write(d.join("f"##, $extension, r##""), "##, $text, r##")?;"##)]
            //!     let dir = Dir::read(&d)?;
            //!     assert_eq!(dir.f, Obj { name: "John".to_owned(), age: 30 });
            //!     # std::fs::remove_dir_all(&d)?;
            //!     Ok(())
            //! }
            //! ```
            //!
            #![doc = concat!(r##"## Writing a "##, stringify!($mod_name), r##" file"##)]
            //!
            #![cfg_attr(feature = "derive", doc = "```rust")]
            #![cfg_attr(not(feature = "derive"), doc = "```rust,compile_fail")]
            //! use std::path::Path;
            //!
            //! use dir_structure::traits::sync::DirStructureItem;
            #![doc = concat!(r##"use dir_structure::data_formats::"##, stringify!($mod_name), "::", stringify!($main_ty), r##";"##)]
            //!
            //! #[derive(dir_structure::DirStructure)]
            //! struct Dir {
            #![doc = concat!(r##"    #[dir_structure(path = "f"##, $extension, r##"", with_newtype = "##, stringify!($main_ty), r##"<Obj>)]"##)]
            //!     f: Obj,
            //! }
            //!
            //! #[derive(Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
            //! struct Obj {
            //!     name: String,
            //!     age: u32,
            //! }
            //!
            //! fn main() -> Result<(), Box<dyn std::error::Error>> {
            //!     let d = Path::new("dir");
            //!     let dir = Dir {
            //!         f: Obj {
            //!             name: "John".to_owned(),
            //!             age: 30,
            //!         },
            //!     };
            //!     dir.write(&d)?;
            #![doc = concat!(r##"    assert_eq!(std::fs::read_to_string(d.join("f"##, $extension, r##""))?,"##)]
            #![doc = concat!(r##"        "##, $text)]
            //!     );
            //!     # std::fs::remove_dir_all(&d)?;
            //!     Ok(())
            //! }
            //! ```

            use std::fmt;
            use std::fmt::Formatter;
            use std::path::Path;
            #[cfg(feature = "async")]
            use std::path::PathBuf;
            use std::str::FromStr;

            use std::pin::Pin;
            use std::marker;
            use std::result::Result as StdResult;

            use crate::traits::sync::FromRefForWriter;
            use crate::traits::vfs;
            #[cfg(feature = "async")]
            use crate::traits::async_vfs::VfsAsync;
            #[cfg(feature = "async")]
            use crate::traits::async_vfs::WriteSupportingVfsAsync;
            #[cfg(feature = "async")]
            use crate::traits::asy::FromRefForWriterAsync;
            use crate::traits::sync::NewtypeToInner;
            use crate::prelude::*;
            use crate::std_types::FileString;
            use crate::error::Result;
            use crate::error::Error;

            $(#[$main_ty_attrs])*
            #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, Hash)]
            #[serde(transparent)]
            #[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
            pub struct $main_ty<T>(#[serde(bound = "")] pub T)
            where
                T: 'static + serde::Serialize + for<'d> serde::Deserialize<'d>;

            impl<T> FromStr for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Err = $from_str_error;

                fn from_str(s: &str) -> StdResult<Self, Self::Err> {
                    $from_str_impl(s).map(Self)
                }
            }

            $(#[$to_str_ty_attrs])*
            struct $to_str_ty<'a, T>(&'a T)
            where
                T: serde::Serialize + 'a;

            impl<'a, T> $to_str_ty<'a, T>
            where
                T: serde::Serialize + 'a
            {
                fn to_str(&self) -> StdResult<String, $to_str_error> {
                    $to_str_impl(&self.0)
                }

                fn to_writer<W>(&self, writer: &mut W) -> StdResult<(), ToWriterError>
                where
                    W: std::io::Write,
                {
                    $to_writer_impl(&self.0, writer)
                }
            }

            enum ToWriterError {
                #[allow(clippy::allow_attributes, unused)]
                Io(std::io::Error),
                Serde($to_str_error),
            }

            impl<'a, T> fmt::Display for $to_str_ty<'a, T>
            where
                T: serde::Serialize + 'a,
            {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    let s = self.to_str().map_err(|_| fmt::Error)?;
                    write!(f, "{s}")
                }
            }

            impl<T> fmt::Display for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    $to_str_ty(&self.0).fmt(f)
                }
            }

            impl<'a, T, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                fn read_from(path: &Path, vfs: Pin<&'a Vfs>) -> Result<Self> {
                    let contents = FileString::read_from(path, vfs)?.0;
                    let v = contents
                        .parse::<$main_ty<T>>()
                        .map_err(|e| Error::Parse(path.to_path_buf(), e.into()))?;
                    Ok(v)
                }
            }

            #[cfg(feature = "async")]
            #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
            impl<'a, T, Vfs: VfsAsync + 'static> ReadFromAsync<'a, Vfs> for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Future = Pin<Box<dyn Future<Output = Result<Self>> + Send + 'a>>;

                fn read_from_async(path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
                    Box::pin(async move {
                        let contents = FileString::read_from_async(path.clone(), vfs).await?.0;
                        let v = contents
                            .parse::<$main_ty<T>>()
                            .map_err(|e| Error::Parse(path, e.into()))?;
                        Ok(v)
                    })
                }
            }

            impl<'a, T, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                fn write_to(&self, path: &Path, vfs: Pin<&'a Vfs>) -> Result<()> {
                    Self::from_ref_for_writer(&self.0).write_to(path, vfs)
                }
            }

            #[cfg(feature = "async")]
            #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
            impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + Send + Sync + 'static,
            {
                type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;

                fn write_to_async(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
                    Box::pin(async move {
                        let s = $to_str_ty(&self.0).to_str()
                            .map_err(|e| Error::Serde(path.clone(), e.into()))?;
                        FileString::new(s).write_to_async(path, vfs).await
                    })
                }
            }

            impl<T> NewtypeToInner for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Inner = T;

                fn into_inner(self) -> Self::Inner {
                    self.0
                }
            }

            impl<'a, 'vfs, T, Vfs: vfs::WriteSupportingVfs<'vfs> + 'vfs> FromRefForWriter<'a, 'vfs, Vfs> for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
                'vfs: 'a,
            {
                type Inner = T;
                type Wr = $writer_ty<'a, 'vfs, T, Vfs>;

                fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
                    $writer_ty(value, marker::PhantomData)
                }
            }

            #[cfg(feature = "async")]
            #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
            impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> FromRefForWriterAsync<'a, Vfs> for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + Send + Sync + 'static,
            {
                type Inner = T;
                type Wr = $writer_ty<'a, 'a, T, Vfs>;

                fn from_ref_for_writer_async(value: &'a <Self as FromRefForWriterAsync<'a, Vfs>>::Inner) -> Self::Wr {
                    $writer_ty(value, marker::PhantomData)
                }
            }

            $(#[$writer_ty_attrs])*
            pub struct $writer_ty<'a, 'vfs, T, Vfs: 'vfs>(&'a T, marker::PhantomData<&'vfs Vfs>)
            where
                T: serde::Serialize + 'a,
                'vfs: 'a;

            impl<'a, 'vfs, T, Vfs: vfs::WriteSupportingVfs<'vfs>> WriteTo<'vfs, Vfs> for $writer_ty<'a, 'vfs, T, Vfs>
            where
                T: serde::Serialize + 'a,
                'vfs: 'a,
            {
                fn write_to(&self, path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<()> {
                    vfs.create_parent_dir(path)?;

                    $to_str_ty(self.0).to_writer(&mut vfs.open_write(path)?)
                        .map_err(|e| match e {
                            ToWriterError::Io(e) => Error::Io(path.to_path_buf(), e),
                            ToWriterError::Serde(e) => Error::Serde(path.to_path_buf(), e.into()),
                        })?;

                    Ok(())
                }
            }

            #[cfg(feature = "async")]
            #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
            impl<'a, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsync<'a, Vfs> for $writer_ty<'a, 'a, T, Vfs>
            where
                T: serde::Serialize + Send + Sync + 'a,
            {
                type Future = Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> where Self: 'a, Vfs: 'a;

                fn write_to_async(self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future {
                    Box::pin(async move {
                        let s = $to_str_ty(self.0).to_str()
                            .map_err(|e| Error::Serde(path.clone(), e.into()))?;
                        FileString::new(s).write_to_async(path, vfs).await
                    })
                }
            }

            impl<T> std::ops::Deref for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Target = T;

                fn deref(&self) -> &Self::Target {
                    &self.0
                }
            }

            impl<T> std::ops::DerefMut for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.0
                }
            }
        }
    };
}

data_format_impl!(
    #[cfg(feature = "json")]
    #[cfg_attr(docsrs, doc(cfg(feature = "json")))]
    #[allow(clippy::absolute_paths)]
    json,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to json when we read / write a
    /// directory structure.
    Json,
    |s| serde_json::from_str(s),
    serde_json::Error,
    JsonToStr,
    |v| serde_json::to_string(&v),
    |v, w| serde_json::to_writer(w, v).map_err(ToWriterError::Serde),
    serde_json::Error,
    /// [`FromRefForWriter`] implementation for [`Json`].
    JsonRefWr,
    ".json", r##"r#"{"name":"John","age":30}"#"##,
);

data_format_impl!(
    #[cfg(feature = "json")]
    #[cfg_attr(docsrs, doc(cfg(feature = "json")))]
    #[allow(clippy::absolute_paths)]
    json_pretty,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to json when we read / write a
    /// directory structure.
    ///
    /// This is a pretty-printed version of [`Json`][crate::json::Json].
    JsonPretty,
    |s| serde_json::from_str(s),
    serde_json::Error,
    JsonPrettyToStr,
    |v| serde_json::to_string_pretty(&v),
    |v, w| serde_json::to_writer_pretty(w, v).map_err(ToWriterError::Serde),
    serde_json::Error,
    /// [`FromRefForWriter`] implementation for [`JsonPretty`].
    JsonPrettyRefWr,
    ".json", r##"r#"{
  "name": "John",
  "age": 30
}"#"##,
);

data_format_impl!(
    #[cfg(feature = "toml")]
    #[cfg_attr(docsrs, doc(cfg(feature = "toml")))]
    #[allow(clippy::absolute_paths)]
    toml,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to toml when we read / write a
    /// directory structure.
    Toml,
    |s| toml::de::from_str(s),
    toml::de::Error,
    TomlToStr,
    |v| toml::ser::to_string(&v),
    |v, w: &mut dyn std::io::Write| {
        let s = toml::ser::to_string(&v).map_err(ToWriterError::Serde)?;
        w.write_all(s.as_bytes()).map_err(ToWriterError::Io)?;
        Ok(())
    },
    toml::ser::Error,
    /// [`FromRefForWriter`] implementation for [`Toml`].
    TomlRefWr,
    ".toml", r##"r#"
name = "John"
age = 30
"#.trim_start()"##,
);

data_format_impl!(
    #[cfg(feature = "yaml")]
    #[cfg_attr(docsrs, doc(cfg(feature = "yaml")))]
    #[allow(clippy::absolute_paths)]
    yaml,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to yaml when we read / write a
    /// directory structure.
    Yaml,
    |s| serde_yaml::from_str(s),
    serde_yaml::Error,
    YamlToStr,
    |v| serde_yaml::to_string(&v),
    |v, w| serde_yaml::to_writer(w, v).map_err(ToWriterError::Serde),
    serde_yaml::Error,
    /// [`FromRefForWriter`] implementation for [`Yaml`].
    YamlRefWr,
    ".yaml", r##"r#"
name: John
age: 30
"#.trim_start()"##,
);

data_format_impl!(
    #[cfg(feature = "ron")]
    #[cfg_attr(docsrs, doc(cfg(feature = "ron")))]
    #[allow(clippy::absolute_paths)]
    ron,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to ron when we read / write a
    /// directory structure.
    Ron,
    |s| ron::de::from_str(s),
    ron::error::SpannedError,
    RonToStr,
    |v| ron::ser::to_string(&v),
    |v, w| ron::options::Options::default().to_io_writer(w, v).map_err(ToWriterError::Serde),
    ron::error::Error,
    /// [`FromRefForWriter`] implementation for [`Ron`].
    RonRefWr,
    ".ron", r##"r#"(name:"John",age:30)"#"##,
);

data_format_impl!(
    #[cfg(feature = "ron")]
    #[cfg_attr(docsrs, doc(cfg(feature = "ron")))]
    #[allow(clippy::absolute_paths)]
    ron_pretty,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to ron when we read / write a
    /// directory structure.
    RonPretty,
    |s| ron::de::from_str(s),
    ron::error::SpannedError,
    RonToStr,
    |v| ron::ser::to_string_pretty(&v, ron::ser::PrettyConfig::default()),
    |v, w| ron::options::Options::default().to_io_writer_pretty(w, v, ron::ser::PrettyConfig::default()).map_err(ToWriterError::Serde),
    ron::error::Error,
    /// [`FromRefForWriter`] implementation for [`RonPretty`].
    RonPrettyRefWr,
    ".ron", r##"r#"(
    name: "John",
    age: 30,
)"#"##,
);
