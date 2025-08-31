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
#![cfg_attr(feature = "include_dir", feature(normalize_lexically))]
#![deny(missing_docs)]

#[cfg(feature = "async")]
pub extern crate pin_project;

#[cfg(feature = "include_dir")]
pub extern crate include_dir;

#[cfg(doctest)]
mod __doc_check {
    mod guide {
        include!("../../../../doc/docs/content/docs/dx/dir-structure/.guide.mdx.doctests");
    }

    mod plumbing_guide {
        include!("../../../../doc/docs/content/docs/dx/dir-structure/.custom-impl.mdx.doctests");
    }

    #[doc = include_str!("../README.md")]
    struct Readme;
}

pub use dir_structure_macros::DirStructure;
#[cfg(feature = "async")]
pub use dir_structure_macros::DirStructureAsync;

pub mod prelude {
    //! A prelude for the most commonly used items in this crate.
    pub use super::DirStructure;
    pub use super::ReadFrom;
    #[cfg(feature = "async")]
    pub use super::ReadFromAsync;
    pub use super::WriteTo;
    #[cfg(feature = "async")]
    pub use super::WriteToAsync;
    #[cfg(feature = "async")]
    pub use super::WriteToAsyncRef;
}

pub mod clean_dir;
pub mod data_formats;
pub mod deferred_read;
pub mod deferred_read_or_own;
pub mod dir_children;
pub mod dir_descendants;
pub mod error;
pub mod fmt_wrapper;
pub mod option;
pub mod std_types;
pub mod traits;
pub mod try_parse;
pub mod versioned;

#[cfg(any(feature = "json", feature = "toml", feature = "yaml", feature = "ron"))]
mod sfw;

pub use clean_dir::*;
#[cfg(any(feature = "json", feature = "toml", feature = "yaml", feature = "ron"))]
pub use data_formats::*;
pub use deferred_read::DeferredRead;
pub use deferred_read_or_own::DeferredReadOrOwn;
pub use dir_children::DirChild;
pub use dir_children::DirChildren;
pub use dir_children::Filter;
pub use dir_descendants::DirDescendant;
pub use dir_descendants::DirDescendants;
pub use dir_descendants::FileFilter;
pub use dir_descendants::FolderFilter;
pub use dir_descendants::FolderRecurseFilter;
pub use error::Error;
pub use error::Result;
pub use fmt_wrapper::FmtWrapper;
pub use std_types::*;
#[cfg(feature = "async")]
pub use traits::asy::*;
#[cfg(feature = "async")]
pub use traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
pub use traits::async_vfs::WriteSupportingVfsAsync;
#[cfg(feature = "resolve-path")]
pub use traits::resolve::DynamicHasField;
#[cfg(feature = "resolve-path")]
pub use traits::resolve::HAS_FIELD_MAX_LEN;
#[cfg(feature = "resolve-path")]
pub use traits::resolve::HasField;
#[cfg(feature = "resolve-path")]
pub use traits::resolve::load_path;
#[cfg(feature = "resolve-path")]
pub use traits::resolve::resolve_path;
pub use traits::sync::DirStructure;
pub use traits::sync::DirStructureItem;
pub use traits::sync::FromRefForWriter;
pub use traits::sync::NewtypeToInner;
pub use traits::sync::ReadFrom;
pub use traits::sync::WriteTo;
pub use traits::vfs::Vfs;
pub use traits::vfs::WriteSupportingVfs;
pub use traits::vfs::fs_vfs::FsVfs;
pub use try_parse::TryParse;
pub use versioned::Versioned;
pub use versioned::VersionedBytes;
pub use versioned::VersionedString;

/// A [`Filter`], [`FileFilter`], [`FolderFilter`], and [`FolderRecurseFilter`] that allows all paths.
///
/// This can be passed as a filter to [`DirChildren`] and [`DirDescendants`] to read all paths; custom
/// filtering will require a new filter type.
///
/// ```rust
/// # use std::path::Path;
/// # use dir_structure::{Filter, NoFilter};
/// #
/// assert!(NoFilter::allows(Path::new("foo.txt")));
/// assert!(NoFilter::allows(Path::new("foo/bar.txt")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz.txt")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz/")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz/.")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz/..")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz/../..")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz/../../..")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz/../../../..")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz/../../../../..")));
/// assert!(NoFilter::allows(Path::new("foo/bar/baz/../../../../../..")));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NoFilter;
