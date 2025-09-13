#![allow(clippy::absolute_paths)]

use super::*;

macro_rules! panik {
    ($left:expr, $right:expr, $init_left:expr, $init_right:expr, $path:expr) => {
        panic!(
            "assertion `left == right` failed: at {:?}\n  left: {:?}\n right: {:?};\nassert_eq! called initially on:\n  left: {}\n right: {}", $path, $left, $right, $init_left, $init_right
        )
    };

    ($left:expr, $right:expr, $init_left:expr, $init_right:expr, $path:expr, $($arg:tt)+) => {
        panic!(
            "assertion `left == right` failed: at {:?}: {}\n  left: {:?}\n right: {:?};\nassert_eq! called initially on:\n  left: {}\n right: {}",
            $path, format_args!($($arg)+), $left, $right, $init_left, $init_right
        )
    };
}

macro_rules! panik_if_unequal {
    ($a:expr, $b:expr, $init_left:expr, $init_right:expr, $path:expr) => {
        if !($a == $b) {
            panik!($a, $b, $init_left, $init_right, $path);
        }
    };
    ($a:expr, $b:expr, $init_left:expr, $init_right:expr, $path:expr, $($arg:tt)+) => {
        if !($a == $b) {
            panik!($a, $b, $init_left, $init_right, $path, $($arg)+);
        }
    };
}

macro_rules! impls {
    ($(#[cfg($cfg:meta)])? $a:ty, $b:ty) => {
        $(#[cfg($cfg)])?
        impl AssertEq<$a> for $b {
            #[track_caller]
            fn assert_eq(&self, other: &$a, path: &mut AssertPath, init_left: &impl fmt::Display, init_right: &impl fmt::Display) {
                panik_if_unequal!(self, other, init_left, init_right, path);
            }
        }
    };

    ($(#[cfg($cfg:meta)])? $a:ty) => {
        $(#[cfg($cfg)])?
        impl AssertEq<$a> for $a {
            #[track_caller]
            fn assert_eq(&self, other: &$a, path: &mut AssertPath, init_left: &impl fmt::Display, init_right: &impl fmt::Display) {
                panik_if_unequal!(self, other, init_left, init_right, path);
            }
        }
    };
}

impls!(u8);
impls!(u16);
impls!(u32);
impls!(u64);
impls!(u128);
impls!(i8);
impls!(i16);
impls!(i32);
impls!(i64);
impls!(i128);
impls!(usize);
impls!(isize);
impls!(bool);
impls!(alloc::string::String);
impls!(str);
impls!(&'_ str);
impls!(f32);
impls!(f64);
impls!(
    #[cfg(feature = "std")]
    std::ffi::OsString
);
impls!(
    #[cfg(feature = "std")]
    std::ffi::OsStr
);
impls!(
    #[cfg(feature = "std")]
    std::path::PathBuf
);
impls!(
    #[cfg(feature = "std")]
    std::path::Path
);
impls!(
    #[cfg(feature = "std")]
    std::net::Ipv4Addr
);
impls!(
    #[cfg(feature = "std")]
    std::net::Ipv6Addr
);
impls!(
    #[cfg(feature = "std")]
    std::net::IpAddr
);
impls!(
    #[cfg(feature = "std")]
    std::net::SocketAddrV4
);
impls!(
    #[cfg(feature = "std")]
    std::net::SocketAddrV6
);
impls!(
    #[cfg(feature = "std")]
    std::net::SocketAddr
);
impls!(core::time::Duration);
impls!(
    #[cfg(feature = "std")]
    std::time::SystemTime
);
impls!(
    #[cfg(feature = "std")]
    std::time::Instant
);
impls!(
    #[cfg(all(feature = "nightly", feature = "std"))]
    std::ascii::Char
);
impls!(
    #[cfg(feature = "std")]
    std::backtrace::BacktraceStatus
);
impls!(
    #[cfg(feature = "nightly")]
    std::collections::TryReserveErrorKind
);
impls!(alloc::collections::TryReserveError);
impls!(core::convert::Infallible);
impls!(
    #[cfg(feature = "std")]
    std::env::VarError
);
impls!(
    #[cfg(feature = "std")]
    std::ffi::FromBytesWithNulError
);

impl<T, U> AssertEq<[U]> for [T]
where
    T: AssertEq<U> + Debug,
    U: Debug,
{
    fn assert_eq(
        &self,
        other: &[U],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        panik_if_unequal!(
            self.len(),
            other.len(),
            init_left,
            init_right,
            path,
            "lengths differ between\n  left: {:?}\n right: {:?}:\n",
            self,
            other
        );
        for (i, (a, b)) in self.iter().zip(other.iter()).enumerate() {
            a.assert_eq(
                b,
                &mut *path.__guard(alloc::format!("[{i}]")),
                &init_left,
                &init_right,
            );
        }
    }
}

impl<T, U> AssertEq<&[U]> for &[T]
where
    T: AssertEq<U> + Debug,
    U: Debug,
{
    fn assert_eq(
        &self,
        other: &&[U],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        (*self).assert_eq(*other, path, init_left, init_right);
    }
}

impl<T, U, const N: usize> AssertEq<[U; N]> for [T; N]
where
    T: AssertEq<U> + Debug,
    U: Debug,
{
    fn assert_eq(
        &self,
        other: &[U; N],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        self.as_slice()
            .assert_eq(other.as_slice(), path, init_left, init_right);
    }
}

impl<T> AssertEq<Option<T>> for Option<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Option<T>,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        match (self, other) {
            (Some(a), Some(b)) => {
                a.assert_eq(b, &mut *path.__guard("[Some]"), init_left, init_right)
            }
            (None, None) => {}
            _ => {
                panik!(
                    self,
                    other,
                    init_left,
                    init_right,
                    path,
                    "left and right are different kinds of Option:"
                );
            }
        }
    }
}

impl<T, E> AssertEq<Result<T, E>> for Result<T, E>
where
    T: AssertEq + Debug,
    E: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Result<T, E>,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        match (self, other) {
            (Ok(a), Ok(b)) => a.assert_eq(b, &mut *path.__guard("[Ok]"), init_left, init_right),
            (Err(a), Err(b)) => a.assert_eq(b, &mut *path.__guard("[Err]"), init_left, init_right),
            _ => panik!(
                self,
                other,
                init_left,
                init_right,
                path,
                "left and right are different kinds of Result:"
            ),
        }
    }
}

impl<T> AssertEq<Vec<T>> for Vec<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Vec<T>,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(
            self.as_slice(),
            other.as_slice(),
            path,
            init_left,
            init_right,
        );
    }
}

impl<T> AssertEq<[T]> for Vec<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &[T],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(self.as_slice(), other, path, init_left, init_right);
    }
}

impl<T> AssertEq<&[T]> for Vec<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &&[T],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(self.as_slice(), *other, path, init_left, init_right);
    }
}

impl<T> AssertEq<Vec<T>> for [T]
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Vec<T>,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(self, other.as_slice(), path, init_left, init_right);
    }
}

impl<T, const N: usize> AssertEq<Vec<T>> for [T; N]
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Vec<T>,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(
            self.as_slice(),
            other.as_slice(),
            path,
            init_left,
            init_right,
        );
    }
}

impl<T, const N: usize> AssertEq<[T; N]> for Vec<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &[T; N],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(
            self.as_slice(),
            other.as_slice(),
            path,
            init_left,
            init_right,
        );
    }
}

impl<T, const N: usize> AssertEq<&[T; N]> for Vec<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &&[T; N],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(self.as_slice(), *other, path, init_left, init_right);
    }
}

impl<T, const N: usize> AssertEq<[T]> for [T; N]
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &[T],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(self.as_slice(), other, path, init_left, init_right);
    }
}

impl<T, const N: usize> AssertEq<[T; N]> for [T]
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &[T; N],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(self, other.as_slice(), path, init_left, init_right);
    }
}

impl<T, const N: usize> AssertEq<&[T; N]> for [T; N]
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &&[T; N],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(self, *other, path, init_left, init_right);
    }
}

impl<T, const N: usize> AssertEq<[T; N]> for &[T; N]
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &[T; N],
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        AssertEq::assert_eq(*self, other, path, init_left, init_right);
    }
}

impl<T> AssertEq for core::ops::Bound<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Self,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        match (self, other) {
            (Self::Included(a), Self::Included(b)) => {
                a.assert_eq(b, &mut *path.__guard("[Included]"), init_left, init_right)
            }
            (Self::Excluded(a), Self::Excluded(b)) => {
                a.assert_eq(b, &mut *path.__guard("[Excluded]"), init_left, init_right)
            }
            (Self::Unbounded, Self::Unbounded) => {}
            _ => panik!(
                self,
                other,
                init_left,
                init_right,
                path,
                "left and right are different kinds of Bound:"
            ),
        }
    }
}

impl<T> AssertEq for core::ops::Range<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Self,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        self.start.assert_eq(
            &other.start,
            &mut *path.__guard(".start"),
            init_left,
            init_right,
        );
        self.end.assert_eq(
            &other.end,
            &mut *path.__guard(".end"),
            init_left,
            init_right,
        );
    }
}

impl<T> AssertEq for core::ops::RangeInclusive<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Self,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        self.start().assert_eq(
            other.start(),
            &mut *path.__guard(".start"),
            init_left,
            init_right,
        );
        self.end().assert_eq(
            other.end(),
            &mut *path.__guard(".end"),
            init_left,
            init_right,
        );
    }
}

impl<T> AssertEq for core::ops::RangeFrom<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Self,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        self.start.assert_eq(
            &other.start,
            &mut *path.__guard(".start"),
            init_left,
            init_right,
        );
    }
}

impl<T> AssertEq for core::ops::RangeTo<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Self,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        self.end.assert_eq(
            &other.end,
            &mut *path.__guard(".end"),
            init_left,
            init_right,
        );
    }
}

impl<T> AssertEq for core::ops::RangeToInclusive<T>
where
    T: AssertEq + Debug,
{
    fn assert_eq(
        &self,
        other: &Self,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        self.end.assert_eq(
            &other.end,
            &mut *path.__guard(".end"),
            init_left,
            init_right,
        );
    }
}

impl<T> AssertEq for Cow<'_, T>
where
    T: AssertEq + Debug + Clone,
{
    fn assert_eq(
        &self,
        other: &Self,
        path: &mut AssertPath,
        init_left: &impl fmt::Display,
        init_right: &impl fmt::Display,
    ) {
        match (self, other) {
            (Cow::Borrowed(a), Cow::Borrowed(b)) => {
                (**a).assert_eq(b, &mut *path.__guard("[Borrowed]"), init_left, init_right)
            }
            (Cow::Owned(a), Cow::Owned(b)) => {
                a.assert_eq(b, &mut *path.__guard("[Owned]"), init_left, init_right)
            }
            (Cow::Borrowed(a), Cow::Owned(b)) => (**a).assert_eq(
                b,
                &mut *path.__guard("[Borrowed->Owned]"),
                init_left,
                init_right,
            ),
            (Cow::Owned(a), Cow::Borrowed(b)) => (*a).assert_eq(
                b,
                &mut *path.__guard("[Owned->Borrowed]"),
                init_left,
                init_right,
            ),
        }
    }
}

impl<T> AssertEq for core::marker::PhantomData<T> {
    fn assert_eq(
        &self,
        _other: &Self,
        _path: &mut AssertPath,
        _init_left: &impl fmt::Display,
        _init_right: &impl fmt::Display,
    ) {
    }
}

impl AssertEq<()> for () {
    fn assert_eq(
        &self,
        _other: &(),
        _path: &mut AssertPath,
        _init_left: &impl fmt::Display,
        _init_right: &impl fmt::Display,
    ) {
    }
}
