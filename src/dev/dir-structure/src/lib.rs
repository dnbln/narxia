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
//!     use dir_structure::traits::sync::DirStructureItem;
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
//!     use dir_structure::traits::sync::DirStructureItem;
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
    pub use crate::DirStructure;
    #[cfg(feature = "async")]
    pub use crate::traits::asy::ReadFromAsync;
    #[cfg(feature = "async")]
    pub use crate::traits::asy::WriteToAsync;
    #[cfg(feature = "async")]
    pub use crate::traits::asy::WriteToAsyncRef;
    pub use crate::traits::sync::ReadFrom;
    pub use crate::traits::sync::WriteTo;
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
pub mod versioned_hash;
pub mod vfs;

#[cfg(any(feature = "json", feature = "toml", feature = "yaml", feature = "ron"))]
mod sfw;

/// A [`Filter`], [`FileFilter`], [`FolderFilter`], and [`FolderRecurseFilter`] that allows all paths.
///
/// This can be passed as a filter to [`DirChildren`] and [`DirDescendants`] to read all paths; custom
/// filtering will require a new filter type.
///
/// ```rust
/// # use std::path::Path;
/// # use dir_structure::{NoFilter, dir_children::Filter};
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
