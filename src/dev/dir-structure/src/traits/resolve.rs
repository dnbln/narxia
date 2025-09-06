//! Traits for resolving paths in a directory structure.
//!
//! [`HasField`] is automatically derived by the `#[derive(DirStructure)]` macro.

use std::path::PathBuf;

#[doc(hidden)]
pub const HAS_FIELD_MAX_LEN: usize = dir_structure_macros::__resolve_max_len!();

/// A trait to declare that a type has a field with a specific name,
/// and the type of the field is [`HasField::Inner`].
///
/// This is used to resolve paths with [`resolve_path`].
#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
pub trait HasField<const NAME: [char; HAS_FIELD_MAX_LEN]> {
    /// The type of the field.
    type Inner;

    /// How to resolve the path for the field, from the path of `Self`.
    fn resolve_path(p: PathBuf) -> PathBuf;
}

/// A trait for types that may or may not have a newtype wrapper around their field type for reading / writing.
pub trait HasFieldMaybeNewtype<const NAME: [char; HAS_FIELD_MAX_LEN]>: HasField<NAME> {
    /// The reader type, which may be a newtype wrapper around [`HasField::Inner`], or the [`HasField::Inner`] itself if no newtype is
    /// supplied to the `#[derive(DirStructure)]` macro.
    type ReaderType;
    /// Parses the read type into the inner type.
    fn parse(read: Self::ReaderType) -> Self::Inner;
}

/// A trait to declare that a type has a field with a specific name, and it's not wrapped in a newtype.
///
/// This allows it to be used within the `first..last-1` segments of the `load_path!` macro, where newtypes are not supported.
pub trait HasFieldNoNewtype<const NAME: [char; HAS_FIELD_MAX_LEN]>: HasField<NAME> {}

impl<const NAME: [char; HAS_FIELD_MAX_LEN], S, T> HasFieldMaybeNewtype<NAME> for S
where
    S: HasFieldNoNewtype<NAME, Inner = T>,
{
    type ReaderType = T;

    fn parse(read: Self::ReaderType) -> Self::Inner {
        read
    }
}

/// A trait to declare that a type has fields with dynamic names,
/// such as [`DirChildren`](crate::DirChildren), [`DirDescendants`](crate::DirDescendants), etc.
///
/// This is used to resolve paths with [`resolve_path`], particularly with the `"name"` and
/// `${expr}` syntaxes.
#[cfg(feature = "resolve-path")]
#[cfg_attr(docsrs, doc(cfg(feature = "resolve-path")))]
pub trait DynamicHasField {
    /// The type of the field.
    type Inner;
    /// How to resolve the path for the field, from the path of `Self`, given the name
    /// passed into [the `resolve_path!` macro](resolve_path).
    fn resolve_path(p: PathBuf, name: &str) -> PathBuf;
}

/// [`DynamicHasField`] for types that do not have a newtype wrapper around their field type.
pub trait DynamicHasFieldNoNewtype: DynamicHasField {}

/// [`DynamicHasField`] for types that may or may not have a newtype wrapper around their field type.
pub trait DynamicHasFieldMaybeNewtype: DynamicHasField {
    /// The reader type, which may be a newtype wrapper around [`DynamicHasField::Inner`], or the [`DynamicHasField::Inner`] itself if no newtype is
    /// supplied to the `#[derive(DirStructure)]` macro.
    type ReaderType;
    /// Parses the read type into the inner type.
    fn parse(read: Self::ReaderType) -> Self::Inner;
}

impl<S, T> DynamicHasFieldMaybeNewtype for S
where
    S: DynamicHasFieldNoNewtype<Inner = T>,
{
    type ReaderType = T;

    fn parse(read: Self::ReaderType) -> Self::Inner {
        read
    }
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
/// use dir_structure::{DirStructure, traits::resolve::resolve_path};
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
