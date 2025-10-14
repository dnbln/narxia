//! Tools for [`dir-structure`].

#![cfg_attr(feature = "resolve-path", feature(adt_const_params))]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![deny(missing_docs)]

pub extern crate dir_structure;

#[cfg(feature = "atomic-dir")]
#[cfg_attr(docsrs, doc(cfg(feature = "atomic-dir")))]
pub mod atomic_dir;
#[cfg(feature = "clean-dir")]
#[cfg_attr(docsrs, doc(cfg(feature = "clean-dir")))]
pub mod clean_dir;
#[cfg(feature = "data-formats")]
#[cfg_attr(docsrs, doc(cfg(feature = "data-formats")))]
pub mod data_formats;
#[cfg(feature = "deferred-read")]
#[cfg_attr(docsrs, doc(cfg(feature = "deferred-read")))]
pub mod deferred_read;
#[cfg(feature = "deferred-read-or-own")]
#[cfg_attr(docsrs, doc(cfg(feature = "deferred-read-or-own")))]
pub mod deferred_read_or_own;
#[cfg(feature = "dir-children")]
#[cfg_attr(docsrs, doc(cfg(feature = "dir-children")))]
pub mod dir_children;
#[cfg(feature = "dir-descendants")]
#[cfg_attr(docsrs, doc(cfg(feature = "dir-descendants")))]
pub mod dir_descendants;
#[cfg(feature = "fmt-wrapper")]
#[cfg_attr(docsrs, doc(cfg(feature = "fmt-wrapper")))]
pub mod fmt_wrapper;
#[cfg(feature = "image")]
#[cfg_attr(docsrs, doc(cfg(feature = "image")))]
pub mod image;
#[cfg(feature = "try-parse")]
#[cfg_attr(docsrs, doc(cfg(feature = "try-parse")))]
pub mod try_parse;
#[cfg(feature = "versioned")]
#[cfg_attr(docsrs, doc(cfg(feature = "versioned")))]
pub mod versioned;
#[cfg(feature = "versioned-hash")]
#[cfg_attr(docsrs, doc(cfg(feature = "versioned-hash")))]
pub mod versioned_hash;

/// A [`Filter`](dir_children::Filter), [`FileFilter`](dir_descendants::FileFilter),
/// [`FolderFilter`](dir_descendants::FolderFilter), and [`FolderRecurseFilter`](dir_descendants::FolderRecurseFilter) that allows all paths.
///
/// This can be passed as a filter to [`DirChildren`](dir_children::DirChildren) and [`DirDescendants`](dir_descendants::DirDescendants) to read all paths; custom
/// filtering will require a new filter type.
///
/// ```rust
/// # use std::path::Path;
/// # use dir_structure_tools::{NoFilter, dir_children::Filter};
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
#[cfg(any(feature = "dir-children", feature = "dir-descendants",))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct NoFilter;

#[cfg(test)]
mod test_utils {
    use dir_structure::traits::vfs;
    pub(crate) fn assert_is_read_from<
        'vfs,
        Vfs: vfs::Vfs<'vfs>,
        T: dir_structure::traits::sync::ReadFrom<'vfs, Vfs>,
    >() {
    }
    pub(crate) fn assert_is_write_to<
        'vfs,
        Vfs: vfs::WriteSupportingVfs<'vfs>,
        T: dir_structure::traits::sync::WriteTo<'vfs, Vfs>,
    >() {
    }
}
