//! Location-aware equality assertions.
//!
//! This library provides the [`AssertEq`] trait and the [`assert_eq!`] and [`debug_assert_eq!`] macros.
//!
//! The main feature is that when an assertion fails, the error message includes the path to the
//! field that failed, making it much easier to debug complex nested structures.
//!
//! # Example
//!
//! ```should_panic
//! # #[path = "check_panic_message.rs"]
//! # mod check_panic_message;
//! # check_panic_message::check_panic_message(|| {
//! use assert_eq::AssertEq;
//!
//! #[derive(AssertEq, Debug)]
//! struct Inner {
//!     a: i32,
//!     b: String,
//! }
//!
//! #[derive(AssertEq, Debug)]
//! struct Outer {
//!     x: Inner,
//!     y: Vec<i32>,
//! }
//!
//! let a = Outer {
//!     x: Inner { a: 1, b: "hello".to_owned() },
//!     y: vec![1, 2, 3],
//! };
//! let b = Outer {
//!     x: Inner { a: 1, b: "world".to_owned() },
//!     y: vec![1, 2, 3],
//! };
//! assert_eq::assert_eq!(a, b);
//! # }, "at .x → .b\n  left: \"hello\"\n right: \"world\"");
//! ```

#![cfg_attr(feature = "nightly", feature(ascii_char, try_reserve_kind))]
#![deny(missing_docs)]

use std::borrow::Cow;
use std::fmt;
use std::fmt::Debug;
use std::ops::Deref;
use std::ops::DerefMut;

/// A path to a field in a nested structure, used for error reporting.
#[derive(Default)]
pub struct AssertPath(Vec<Cow<'static, str>>);

impl AssertPath {
    /// Creates a new, empty path.
    ///
    /// This is usually only called by the `assert_eq!` macro.
    /// The only way to read the path is through the `Debug` implementation.
    ///
    /// # Example
    ///
    /// ```
    /// let p = assert_eq::AssertPath::new();
    /// assert_eq!(format!("{p:?}"), "<root>");
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Internal method to push a new segment to the path, returning a guard that will pop it when dropped.
    ///
    /// This is used by the derive macro to track the current path during comparisons.
    ///
    /// # Example
    ///
    /// ```
    /// let mut p = assert_eq::AssertPath::new();
    /// let mut _g1 = p.__guard(".x");
    /// assert_eq!(format!("{:?}", &*_g1), ".x");
    /// {
    ///     let mut _g2 = _g1.__guard(".y");
    ///     assert_eq!(format!("{:?}", &*_g2), ".x → .y");
    /// }
    /// drop(_g1);
    /// assert_eq!(format!("{p:?}"), "<root>");
    /// ```
    pub fn __guard(&mut self, segment: impl Into<Cow<'static, str>>) -> AssertPathGuard<'_> {
        self.0.push(segment.into());
        AssertPathGuard { path: self }
    }
}

impl Drop for AssertPath {
    fn drop(&mut self) {
        assert!(self.0.is_empty(), "AssertPath dropped but not empty");
    }
}

impl Debug for AssertPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            return write!(f, "<root>");
        }

        for (i, p) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, " → ")?;
            }
            write!(f, "{p}")?;
        }
        Ok(())
    }
}

/// The guard returned by [`AssertPath::__guard`], which pops the last segment when dropped.
pub struct AssertPathGuard<'a> {
    path: &'a mut AssertPath,
}

impl Deref for AssertPathGuard<'_> {
    type Target = AssertPath;

    fn deref(&self) -> &Self::Target {
        self.path
    }
}

impl DerefMut for AssertPathGuard<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.path
    }
}

impl Drop for AssertPathGuard<'_> {
    fn drop(&mut self) {
        self.path.0.pop();
    }
}

/// Trait for types that can be compared for equality with location-aware error reporting.
///
/// This trait is automatically implemented for types that derive `AssertEq` using the
/// `#[derive(AssertEq)]` macro.
///
/// The main feature of this trait is that when an assertion fails, the error message includes
/// the path to the field that failed, making it much easier to debug complex nested structures.
///
/// # Example
///
/// ```should_panic
/// # #[path = "check_panic_message.rs"]
/// # mod check_panic_message;
/// # check_panic_message::check_panic_message(|| {
/// use assert_eq::AssertEq;
///
/// #[derive(AssertEq, Debug)]
/// struct Inner {
///     a: i32,
///     b: String,
/// }
///
/// #[derive(AssertEq, Debug)]
/// struct Outer {
///     x: Inner,
///     y: Vec<i32>,
/// }
///
/// let a = Outer {
///     x: Inner { a: 1, b: "hello".to_owned() },
///     y: vec![1, 2, 3],
/// };
/// let b = Outer {
///     x: Inner { a: 1, b: "world".to_owned() },
///     y: vec![1, 2, 3],
/// };
/// assert_eq::assert_eq!(a, b);
/// # }, "at .x → .b\n  left: \"hello\"\n right: \"world\"");
/// ```
pub trait AssertEq<T = Self>
where
    T: ?Sized,
{
    /// Asserts that `self` is equal to `other`, panicking if they are not equal.
    ///
    /// The panic message includes the path to the field that failed, making it easier to debug
    /// complex nested structures.
    #[track_caller]
    fn assert_eq(&self, other: &T, path: &mut AssertPath);
}

mod __impls;

/// A macro to assert that two values are equal, with location-aware error reporting.
///
/// This macro uses the [`AssertEq`] trait to perform the comparison.
///
/// # Example
///
/// ```should_panic
/// # #[path = "check_panic_message.rs"]
/// # mod check_panic_message;
/// # check_panic_message::check_panic_message(|| {
/// use assert_eq::AssertEq;
///
/// #[derive(AssertEq, Debug)]
/// struct Inner {
///    a: i32,
///    b: String,
/// }
///
/// #[derive(AssertEq, Debug)]
/// struct Outer {
///   x: Inner,
///   y: Vec<i32>,
/// }
///
/// let a = Outer {
///  x: Inner { a: 1, b: "hello".to_owned() },
///  y: vec![1, 2, 3],
/// };
///
/// let b = Outer {
/// x: Inner { a: 1, b: "world".to_owned() },
/// y: vec![1, 2, 3],
/// };
///
/// assert_eq::assert_eq!(a, b);
/// # }, "at .x → .b\n  left: \"hello\"\n right: \"world\"");
/// ```
#[macro_export]
macro_rules! assert_eq {
    ($a:expr, $b:expr) => {
        $crate::AssertEq::assert_eq(&$a, &$b, &mut $crate::AssertPath::new());
    };
}

/// A macro to assert that two values are equal in debug builds, with location-aware error reporting.
#[macro_export]
macro_rules! debug_assert_eq {
    ($a:expr, $b:expr) => {
        #[cfg(debug_assertions)]
        $crate::AssertEq::assert_eq(&$a, &$b, &mut $crate::AssertPath::new());
    };
}

/// Derive macro for the [`AssertEq`] trait.
pub use assert_eq_macros::AssertEq;

#[cfg(doctest)]
mod __doc_check {
    #[doc = include_str!("../README.md")]
    struct Readme;
}
