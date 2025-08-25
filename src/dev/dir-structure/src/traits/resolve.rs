use std::path::PathBuf;

pub const HAS_FIELD_MAX_LEN: usize = dir_structure_macros::__resolve_max_len!();

/// A trait to declare that a type has a field with a specific name,
/// and the type of the field is [`HasField::Inner`].
///
/// This is used to resolve paths with [`resolve_path`].
#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
pub trait HasField<const NAME: [char; HAS_FIELD_MAX_LEN]> {
    type Inner;

    fn resolve_path(p: PathBuf) -> PathBuf;
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
pub trait DynamicHasField {
    type Inner;
    fn resolve_path(p: PathBuf, name: &str) -> PathBuf;
}

#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
pub use dir_structure_macros::load_path;
/// A macro to resolve a path to a specific field in a directory structure.
///
/// # Examples
///
/// ```rust
/// use std::path::PathBuf;
/// use dir_structure::{DirStructure, resolve_path};
///
/// #[derive(DirStructure)]
/// struct MyStruct {
///     #[dir_structure(path = "my_field.txt")]
///     my_field: String,
///     #[dir_structure(path = "my_field2.d")]
///     my_field2: MyStruct2,
/// }
///
/// #[derive(DirStructure)]
/// struct MyStruct2 {
///     #[dir_structure(path = "my_field3.txt")]
///     my_field3: String,
/// }
///
/// assert_eq!(
///     resolve_path!([MyStruct @ "/path/to/dir"].my_field),
///     PathBuf::from("/path/to/dir/my_field.txt")
/// );
/// assert_eq!(
///     resolve_path!(["/path/to/dir" as MyStruct].my_field2.my_field3),
///     PathBuf::from("/path/to/dir/my_field2.d/my_field3.txt")
/// );
/// ```
#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
pub use dir_structure_macros::resolve_path;
