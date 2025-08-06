//! A library for reading and writing directory structures.
//!
//! This library provides a macro for defining directory structures, and a
//! trait for reading and writing those structures to / from disk.
//!
//! [An intro guide.](https://nrx.dnbln.dev/docs/dx/dir-structure/guide)
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

#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(feature = "resolve-path", feature(adt_const_params))]

#[cfg(doctest)]
mod __doc_check {
    #[doc = include_str!("../../../doc/docs/content/docs/dx/dir-structure/.guide.mdx.doctests")]
    struct Guide;

    #[doc = include_str!("../../../doc/docs/content/docs/dx/dir-structure/.custom-impl.mdx.doctests")]
    struct PlumbingGuide;

    #[doc = include_str!("../README.md")]
    struct Readme;
}

// TODO: other async runtimes
#[cfg(all(feature = "async", not(any(feature = "tokio"))))]
compile_error!(
    "The `async` feature requires the `tokio` feature to be enabled. \
     Please enable the `tokio` feature in your Cargo.toml."
);

use std::fs::File;
use std::path::Path;

pub use dir_structure_macros::DirStructure;

pub mod prelude {
    pub use super::DirStructure;
    pub use super::ReadFrom;
    #[cfg(feature = "async")]
    pub use super::ReadFromAsync;
    pub use super::WriteTo;
    #[cfg(feature = "async")]
    pub use super::WriteToAsync;
    #[cfg(feature = "async")]
    pub use super::WriteToAsyncOwned;
}

mod clean_dir;
mod data_formats;
mod deferred_read;
mod deferred_read_or_own;
mod dir_children;
mod error;
mod fmt_wrapper;
mod option;
mod std_types;
mod traits;
mod versioned;

mod utils;

#[cfg(any(feature = "json", feature = "toml", feature = "yaml", feature = "ron"))]
mod sfw;

pub use clean_dir::*;
pub use data_formats::*;
pub use deferred_read::*;
pub use deferred_read_or_own::*;
pub use dir_children::*;
pub use error::*;
pub use fmt_wrapper::*;
pub use std_types::*;
#[cfg(feature = "async")]
pub use traits::asy::*;
#[cfg(feature = "resolve-path")]
pub use traits::resolve::*;
pub use traits::sync::*;
pub use versioned::*;
