use super::*;

macro_rules! impls {
    ($(#[cfg($cfg:meta)])? $a:ty, $b:ty) => {
        $(#[cfg($cfg)])?
        impl AssertEq<$a> for $b {
            #[track_caller]
            fn assert_eq(&self, other: &$a, path: &mut AssertPath) {
                std::assert_eq!(self, other, "at {path:?}");
            }
        }
    };

    ($(#[cfg($cfg:meta)])? $a:ty) => {
        $(#[cfg($cfg)])?
        impl AssertEq<$a> for $a {
            #[track_caller]
            fn assert_eq(&self, other: &$a, path: &mut AssertPath) {
                std::assert_eq!(self, other, "at {path:?}");
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
impls!(String);
impls!(str);
impls!(&'_ str);
impls!(f32);
impls!(f64);
impls!(std::ffi::OsString);
impls!(std::ffi::OsStr);
impls!(std::path::PathBuf);
impls!(std::path::Path);
impls!(std::net::Ipv4Addr);
impls!(std::net::Ipv6Addr);
impls!(std::net::IpAddr);
impls!(std::net::SocketAddrV4);
impls!(std::net::SocketAddrV6);
impls!(std::net::SocketAddr);
impls!(std::time::Duration);
impls!(std::time::SystemTime);
impls!(std::time::Instant);
impls!(
    #[cfg(feature = "nightly")]
    std::ascii::Char
);
impls!(std::backtrace::BacktraceStatus);
impls!(
    #[cfg(feature = "nightly")]
    std::collections::TryReserveErrorKind
);
impls!(std::collections::TryReserveError);
impls!(std::convert::Infallible);
impls!(std::env::VarError);
impls!(std::ffi::FromBytesWithNulError);

impl<T, U> AssertEq<[U]> for [T]
where
    T: AssertEq<U> + Debug,
    U: Debug,
{
    fn assert_eq(&self, other: &[U], path: &mut AssertPath) {
        std::assert_eq!(
            self.len(),
            other.len(),
            "at {path:?}, lengths differ between\n  left: {:?}\n right: {:?}",
            self,
            other
        );
        for (i, (a, b)) in self.iter().zip(other.iter()).enumerate() {
            a.assert_eq(b, &mut *path.__guard(format!("[{i}]")));
        }
    }
}

impl<T, const N: usize> AssertEq<[T; N]> for [T; N]
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &[T; N], path: &mut AssertPath) {
        for (i, (a, b)) in self.iter().zip(other.iter()).enumerate() {
            a.assert_eq(b, &mut *path.__guard(format!("[{i}]")));
        }
    }
}

impl<T> AssertEq<Option<T>> for Option<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &Option<T>, path: &mut AssertPath) {
        match (self, other) {
            (Some(a), Some(b)) => a.assert_eq(b, &mut *path.__guard("[Some]")),
            (None, None) => {}
            _ => {
                panic!(
                    "at {path:?}, left and right are different kinds of Option:\n  left: {self:?}\n right: {other:?}"
                );
            }
        }
    }
}

impl<T, E> AssertEq<Result<T, E>> for Result<T, E>
where
    T: AssertEq<T> + Debug,
    E: AssertEq<E> + Debug,
{
    fn assert_eq(&self, other: &Result<T, E>, path: &mut AssertPath) {
        match (self, other) {
            (Ok(a), Ok(b)) => a.assert_eq(b, &mut *path.__guard("[Ok]")),
            (Err(a), Err(b)) => a.assert_eq(b, &mut *path.__guard("[Err]")),
            _ => panic!(
                "at {path:?}, left and right are different kinds of Result:\n  left: {self:?}\n right: {other:?}"
            ),
        }
    }
}

impl<T> AssertEq<Vec<T>> for Vec<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &Vec<T>, path: &mut AssertPath) {
        AssertEq::assert_eq(self.as_slice(), other.as_slice(), path);
    }
}

impl<T> AssertEq<[T]> for Vec<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &[T], path: &mut AssertPath) {
        AssertEq::assert_eq(self.as_slice(), other, path);
    }
}

impl<T> AssertEq<&[T]> for Vec<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &&[T], path: &mut AssertPath) {
        AssertEq::assert_eq(self.as_slice(), *other, path);
    }
}

impl<T> AssertEq<Vec<T>> for [T]
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &Vec<T>, path: &mut AssertPath) {
        AssertEq::assert_eq(self, other.as_slice(), path);
    }
}

impl<T, const N: usize> AssertEq<Vec<T>> for [T; N]
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &Vec<T>, path: &mut AssertPath) {
        AssertEq::assert_eq(self.as_slice(), other.as_slice(), path);
    }
}

impl<T, const N: usize> AssertEq<[T; N]> for Vec<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &[T; N], path: &mut AssertPath) {
        AssertEq::assert_eq(self.as_slice(), other.as_slice(), path);
    }
}

impl<T, const N: usize> AssertEq<&[T; N]> for Vec<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &&[T; N], path: &mut AssertPath) {
        AssertEq::assert_eq(self.as_slice(), *other, path);
    }
}

impl<T, const N: usize> AssertEq<[T]> for [T; N]
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &[T], path: &mut AssertPath) {
        AssertEq::assert_eq(self.as_slice(), other, path);
    }
}

impl<T, const N: usize> AssertEq<[T; N]> for [T]
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &[T; N], path: &mut AssertPath) {
        AssertEq::assert_eq(self, other.as_slice(), path);
    }
}

impl<T, const N: usize> AssertEq<&[T; N]> for [T; N]
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &&[T; N], path: &mut AssertPath) {
        AssertEq::assert_eq(self, *other, path);
    }
}

impl<T, const N: usize> AssertEq<[T; N]> for &[T; N]
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &[T; N], path: &mut AssertPath) {
        AssertEq::assert_eq(*self, other, path);
    }
}

impl<T> AssertEq<std::collections::Bound<T>> for std::collections::Bound<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &std::collections::Bound<T>, path: &mut AssertPath) {
        match (self, other) {
            (std::collections::Bound::Included(a), std::collections::Bound::Included(b)) => {
                a.assert_eq(b, &mut *path.__guard("[Included]"))
            }
            (std::collections::Bound::Excluded(a), std::collections::Bound::Excluded(b)) => {
                a.assert_eq(b, &mut *path.__guard("[Excluded]"))
            }
            (std::collections::Bound::Unbounded, std::collections::Bound::Unbounded) => {}
            _ => panic!(
                "at {path:?}, left and right are different kinds of Bound:\n  left: {self:?}\n right: {other:?}"
            ),
        }
    }
}

impl<T> AssertEq<std::ops::Range<T>> for std::ops::Range<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &std::ops::Range<T>, path: &mut AssertPath) {
        self.start
            .assert_eq(&other.start, &mut *path.__guard(".start"));
        self.end.assert_eq(&other.end, &mut *path.__guard(".end"));
    }
}

impl<T> AssertEq<std::ops::RangeInclusive<T>> for std::ops::RangeInclusive<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &std::ops::RangeInclusive<T>, path: &mut AssertPath) {
        self.start()
            .assert_eq(other.start(), &mut *path.__guard(".start"));
        self.end()
            .assert_eq(other.end(), &mut *path.__guard(".end"));
    }
}

impl<T> AssertEq<std::ops::RangeFrom<T>> for std::ops::RangeFrom<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &std::ops::RangeFrom<T>, path: &mut AssertPath) {
        self.start
            .assert_eq(&other.start, &mut *path.__guard(".start"));
    }
}

impl<T> AssertEq<std::ops::RangeTo<T>> for std::ops::RangeTo<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &std::ops::RangeTo<T>, path: &mut AssertPath) {
        self.end.assert_eq(&other.end, &mut *path.__guard(".end"));
    }
}

impl<T> AssertEq<std::ops::RangeToInclusive<T>> for std::ops::RangeToInclusive<T>
where
    T: AssertEq<T> + Debug,
{
    fn assert_eq(&self, other: &std::ops::RangeToInclusive<T>, path: &mut AssertPath) {
        self.end.assert_eq(&other.end, &mut *path.__guard(".end"));
    }
}

impl<T> AssertEq<Cow<'_, T>> for Cow<'_, T>
where
    T: AssertEq<T> + Debug + Clone + ?Sized,
{
    fn assert_eq(&self, other: &Cow<'_, T>, path: &mut AssertPath) {
        match (self, other) {
            (Cow::Borrowed(a), Cow::Borrowed(b)) => {
                (**a).assert_eq(b, &mut *path.__guard("[Borrowed]"))
            }
            (Cow::Owned(a), Cow::Owned(b)) => a.assert_eq(b, &mut *path.__guard("[Owned]")),
            (Cow::Borrowed(a), Cow::Owned(b)) => {
                (**a).assert_eq(b, &mut *path.__guard("[Borrowed->Owned]"))
            }
            (Cow::Owned(a), Cow::Borrowed(b)) => {
                (*a).assert_eq(b, &mut *path.__guard("[Owned->Borrowed]"))
            }
        }
    }
}

impl<T> AssertEq<std::marker::PhantomData<T>> for std::marker::PhantomData<T> {
    fn assert_eq(&self, _other: &std::marker::PhantomData<T>, _path: &mut AssertPath) {}
}

impl AssertEq<()> for () {
    fn assert_eq(&self, _other: &(), _path: &mut AssertPath) {}
}
