//! Virtual file system implementations.
//!

pub mod fs_vfs;

#[cfg(feature = "tokio")]
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
pub mod tokio_fs_vfs;
