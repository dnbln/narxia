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
            //! ```
            //! use std::path::Path;
            //!
            //! use dir_structure::DirStructureItem;
            #![doc = concat!(r##"use dir_structure::"##, stringify!($mod_name), "::", stringify!($main_ty), r##";"##)]
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
            //! ```
            //! use std::path::Path;
            //!
            //! use dir_structure::DirStructureItem;
            #![doc = concat!(r##"use dir_structure::"##, stringify!($mod_name), "::", stringify!($main_ty), r##";"##)]
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

            #[cfg(feature = "async")]
            use std::pin::Pin;

            use crate::FromRefForWriter;
            #[cfg(feature = "async")]
            use crate::FromRefForWriterAsync;
            use crate::NewtypeToInner;
            use crate::ReadFrom;
            #[cfg(feature = "async")]
            use crate::ReadFromAsync;
            use crate::WriteTo;
            #[cfg(feature = "async")]
            use crate::WriteToAsync;
            #[cfg(feature = "async")]
            use crate::WriteToAsyncOwned;
            #[cfg(feature = "async")]
            use crate::FileString;

            $(#[$main_ty_attrs])*
            #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, Hash)]
            #[serde(transparent)]
            pub struct $main_ty<T>(#[serde(bound = "")] pub T)
            where
                T: 'static + serde::Serialize + for<'d> serde::Deserialize<'d>;

            impl<T> FromStr for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Err = $from_str_error;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
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
                fn to_str(&self) -> Result<String, $to_str_error> {
                    $to_str_impl(&self.0)
                }

                fn to_writer<W>(&self, writer: &mut W) -> Result<(), ToWriterError>
                where
                    W: std::io::Write,
                {
                    $to_writer_impl(&self.0, writer)
                }
            }

            enum ToWriterError {
                #[allow(unused)]
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

            impl<T> ReadFrom for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                fn read_from(path: &Path) -> crate::Result<Self> {
                    let contents = crate::FileString::read_from(path)?.0;
                    let v = contents
                        .parse::<$main_ty<T>>()
                        .map_err(|e| crate::Error::Parse(path.to_path_buf(), e.into()))?;
                    Ok(v)
                }
            }

            #[cfg(feature = "async")]
            #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
            impl<T> ReadFromAsync for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Future = Pin<Box<dyn Future<Output = crate::Result<Self>> + Send>>;

                fn read_from_async(path: PathBuf) -> Self::Future {
                    Box::pin(async move {
                        let contents = crate::FileString::read_from_async(path.clone()).await?.0;
                        let v = contents
                            .parse::<$main_ty<T>>()
                            .map_err(|e| crate::Error::Parse(path, e.into()))?;
                        Ok(v)
                    })
                }
            }

            impl<T> WriteTo for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                fn write_to(&self, path: &Path) -> crate::Result<()> {
                    Self::from_ref_for_writer(&self.0).write_to(path)
                }
            }

            #[cfg(feature = "async")]
            #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
            impl<T> WriteToAsync for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + Send + Sync + 'static,
            {
                type Future<'a> = Pin<Box<dyn Future<Output = crate::Result<()>> + Send + 'a>> where Self: 'a;

                fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
                    Self::from_ref_for_writer_async(&self.0).write_to_async_owned(path)
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

            impl<'a, T> FromRefForWriter<'a> for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + 'static,
            {
                type Inner = T;
                type Wr = $writer_ty<'a, T>;

                fn from_ref_for_writer(value: &'a Self::Inner) -> Self::Wr {
                    $writer_ty(value)
                }
            }

            #[cfg(feature = "async")]
            #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
            impl<'a, T> FromRefForWriterAsync<'a> for $main_ty<T>
            where
                T: serde::Serialize + for<'d> serde::Deserialize<'d> + Send + Sync + 'static,
            {
                type Inner = T;
                type Wr = $writer_ty<'a, T>;

                fn from_ref_for_writer_async(value: &'a <Self as FromRefForWriterAsync<'a>>::Inner) -> Self::Wr {
                    $writer_ty(value)
                }
            }

            $(#[$writer_ty_attrs])*
            pub struct $writer_ty<'a, T>(&'a T)
            where
                T: serde::Serialize + 'a;

            impl<'a, T> WriteTo for $writer_ty<'a, T>
            where
                T: serde::Serialize + 'a,
            {
                fn write_to(&self, path: &Path) -> crate::Result<()> {
                    let mut f = crate::sfw::StreamingFileWriter::new(path)?;
                    $to_str_ty(self.0).to_writer(&mut f)
                        .map_err(|e| match e {
                            ToWriterError::Io(e) => crate::Error::Io(path.to_path_buf(), e),
                            ToWriterError::Serde(e) => crate::Error::Serde(path.to_path_buf(), e.into()),
                        })?;

                    Ok(())
                }
            }

            #[cfg(feature = "async")]
            #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
            impl<'a, T> WriteToAsync for $writer_ty<'a, T>
            where
                T: serde::Serialize + Send + Sync + 'a,
            {
                type Future<'b> = Pin<Box<dyn Future<Output = crate::Result<()>> + Send + 'b>> where Self: 'b;

                fn write_to_async(&self, path: PathBuf) -> Self::Future<'_> {
                    Box::pin(async move {
                        let s = $to_str_ty(self.0).to_str()
                            .map_err(|e| crate::Error::Serde(path.clone(), e.into()))?;
                        FileString::new(s).write_to_async_owned(path).await
                    })
                }
            }

            #[cfg(feature = "async")]
            #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
            impl<'a, T> WriteToAsyncOwned<'a> for $writer_ty<'a, T>
            where
                T: serde::Serialize + Send + Sync + 'a,
            {
                type Future = Pin<Box<dyn Future<Output = crate::Result<()>> + Send + 'a>>;

                fn write_to_async_owned(self, path: PathBuf) -> Self::Future {
                    Box::pin(async move {
                        let s = $to_str_ty(self.0).to_str()
                            .map_err(|e| crate::Error::Serde(path.clone(), e.into()))?;
                        FileString::new(s).write_to_async_owned(path).await
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
    /// [`FromRefForWriter`] implementation for [`Json`].
    JsonPrettyRefWr,
    ".json", r##"r#"{
  "name": "John",
  "age": 30
}"#"##,
);

data_format_impl!(
    #[cfg(feature = "toml")]
    #[cfg_attr(docsrs, doc(cfg(feature = "toml")))]
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
    ron,
    /// A wrapper around a type that implements [`serde::Serialize`] and [`serde::Deserialize`],
    /// thus allowing us to parse and serialize it from / to ron when we read / write a
    /// directory structure.
    Ron,
    |s| ron::de::from_str(s),
    ron::error::SpannedError,
    RonToStr,
    |v| ron::ser::to_string(&v),
    |v, w| ron::ser::to_writer(w, v).map_err(ToWriterError::Serde),
    ron::error::Error,
    /// [`FromRefForWriter`] implementation for [`Ron`].
    RonRefWr,
    ".ron", r##"r#"(name:"John",age:30)"#"##,
);
