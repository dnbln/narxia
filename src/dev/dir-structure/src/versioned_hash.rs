//! A module for working with versioned values.
//!
//! Similar to [`versioned`](crate::versioned), but uses hashing to determine cleanliness of a value.
//!
//! See the [`VersionedHash`] struct for more details.

use std::collections::hash_map::DefaultHasher;
#[cfg(feature = "async")]
use std::future::Future;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker;
use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::pin::Pin;

use crate::error::VfsResult;
use crate::prelude::*;
#[cfg(feature = "async")]
use crate::traits::async_vfs::VfsAsync;
#[cfg(feature = "async")]
use crate::traits::async_vfs::WriteSupportingVfsAsync;
use crate::traits::vfs;
use crate::traits::vfs::PathType;
#[cfg(feature = "async")]
use crate::traits::vfs::VfsCore;

/// A value with a hash to determine if it has changed.
///
/// You can use any hasher that implements [`std::hash::Hasher`] and [`std::default::Default`].
/// By default, it uses [`std::collections::hash_map::DefaultHasher`].
///
/// To access the inner value, you can use [`Deref`](std::ops::Deref) / [`DerefMut`](std::ops::DerefMut)
/// or the [`into_inner`](VersionedHash::into_inner) method.
///
/// # Examples
///
/// ```
/// use std::path::PathBuf;
/// use dir_structure::versioned_hash::VersionedHash;
///
/// let mut vh = VersionedHash::<String>::new_clean(PathBuf::from("test.txt"), "Hello, world!".to_owned());
/// assert!(vh.is_clean());
///
/// vh.push_str(" Modified.");
/// assert!(vh.is_dirty());
/// assert!(!vh.is_clean());
/// ```
#[cfg_attr(feature = "assert_eq", derive(assert_eq::AssertEq))]
pub struct VersionedHash<T: Hash, P: PathType + ?Sized = Path, H: Hasher + Default = DefaultHasher>
{
    value: T,
    hash: u64,
    path: P::OwnedPath,
    _hasher: marker::PhantomData<H>,
}

impl<T: Hash, P: PathType + ?Sized, H: Hasher + Default> VersionedHash<T, P, H> {
    /// Get the inner value. You can also use [`Deref`](std::ops::Deref) / [`DerefMut`](std::ops::DerefMut)
    /// to get references to the inner value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::versioned_hash::VersionedHash;
    ///
    /// let vh = VersionedHash::<String>::new_clean(PathBuf::from("test.txt"), "Hello, world!".to_owned());
    /// assert_eq!(vh.into_inner(), "Hello, world!".to_owned());
    ///
    /// let vh = VersionedHash::<String>::new_dirty(PathBuf::from("test.txt"), "Hello, world!".to_owned());
    /// assert_eq!(vh.into_inner(), "Hello, world!".to_owned());
    /// ```
    pub fn into_inner(self) -> T {
        self.value
    }

    fn new_with_hash(path: P::OwnedPath, value: T, hash: u64) -> Self {
        Self {
            value,
            hash,
            path,
            _hasher: marker::PhantomData,
        }
    }

    fn hash_value(value: &T) -> u64 {
        let mut hasher = H::default();
        T::hash(value, &mut hasher);
        hasher.finish()
    }

    /// Create a new clean value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::versioned_hash::VersionedHash;
    ///
    /// let vh = VersionedHash::<String>::new_clean(PathBuf::from("test.txt"), "Hello, world!".to_owned());
    /// assert!(vh.is_clean());
    /// assert!(!vh.is_dirty());
    /// ```
    pub fn new_clean(path: P::OwnedPath, value: T) -> Self {
        let hash = Self::hash_value(&value);

        Self::new_with_hash(path, value, hash)
    }

    /// Create a new dirty value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::versioned_hash::VersionedHash;
    ///
    /// let vh = VersionedHash::<String>::new_dirty(PathBuf::from("test.txt"), "Hello, world!".to_owned());
    /// assert!(!vh.is_clean());
    /// assert!(vh.is_dirty());
    /// ```
    pub fn new_dirty(path: P::OwnedPath, value: T) -> Self {
        let hash = Self::hash_value(&value);
        Self::new_with_hash(path, value, hash.wrapping_add(1))
    }

    /// Checks if the value has been modified since being read.
    /// Returns true if the value is clean (i.e., has not been modified).
    /// Returns false if the value is dirty (i.e., has been modified).
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::versioned_hash::VersionedHash;
    ///
    /// let vh = VersionedHash::<String>::new_clean(PathBuf::from("test.txt"), "Hello, world!".to_owned());
    /// assert!(vh.is_clean());
    ///
    /// let vh = VersionedHash::<String>::new_dirty(PathBuf::from("test.txt"), "Hello, world!".to_owned());
    /// assert!(!vh.is_clean());
    /// ```
    pub fn is_clean(&self) -> bool {
        self.hash == Self::hash_value(&self.value)
    }

    /// Checks if the value has been modified since being read.
    /// Returns true if the value is dirty (i.e., has been modified).
    /// Returns false if the value is clean (i.e., has not been modified).
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::versioned_hash::VersionedHash;
    ///
    /// let vh = VersionedHash::<String>::new_clean(PathBuf::from("test.txt"), "Hello, world!".to_owned());
    /// assert!(!vh.is_dirty());
    ///
    /// let vh = VersionedHash::<String>::new_dirty(PathBuf::from("test.txt"), "Hello, world!".to_owned());
    /// assert!(vh.is_dirty());
    /// ```
    pub fn is_dirty(&self) -> bool {
        !self.is_clean()
    }

    /// Resets the hash to the current value's hash.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it can lead to changes not being written if the value has changed.
    /// Ideally this function should only be called after writing the value to disk.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::PathBuf;
    /// use dir_structure::versioned_hash::VersionedHash;
    ///
    /// let mut vh = VersionedHash::<String>::new_clean(PathBuf::from("test.txt"), "Hello, world!".to_owned());
    /// assert!(vh.is_clean());
    ///
    /// vh.push_str(" Modified.");
    /// assert!(vh.is_dirty());
    /// assert!(!vh.is_clean());
    ///
    /// unsafe {
    ///   vh.reset();
    /// }
    /// assert!(vh.is_clean());
    /// assert!(!vh.is_dirty());
    /// ```
    #[expect(unsafe_code, reason = "Inherently unsafe function, see documentation")]
    pub unsafe fn reset(&mut self) {
        self.hash = Self::hash_value(&self.value);
    }
}

impl<T: Hash, P: PathType + ?Sized, H: Hasher + Default> Deref for VersionedHash<T, P, H> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: Hash, P: PathType + ?Sized, H: Hasher + Default> DerefMut for VersionedHash<T, P, H> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<'a, T, H, Vfs: vfs::Vfs<'a>> ReadFrom<'a, Vfs> for VersionedHash<T, Vfs::Path, H>
where
    T: ReadFrom<'a, Vfs> + Hash + 'a,
    H: Hasher + Default + 'a,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<Self, Vfs> {
        let value = T::read_from(path, vfs)?;
        let mut hasher = H::default();
        T::hash(&value, &mut hasher);
        let hash = hasher.finish();
        Ok(VersionedHash {
            value,
            hash,
            path: path.owned(),
            _hasher: marker::PhantomData,
        })
    }
}

impl<'a, T, H, Vfs: vfs::WriteSupportingVfs<'a>> WriteTo<'a, Vfs> for VersionedHash<T, Vfs::Path, H>
where
    T: WriteTo<'a, Vfs> + Hash,
    Vfs::Path: PartialEq,
    H: Hasher + Default,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'a Vfs>) -> VfsResult<(), Vfs> {
        if self.path.as_ref() == path && self.is_clean() {
            return Ok(());
        }

        T::write_to(&self.value, path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, H, Vfs: VfsAsync + 'a> ReadFromAsync<'a, Vfs> for VersionedHash<T, Vfs::Path, H>
where
    T: ReadFromAsync<'a, Vfs> + Hash + 'a,
    H: Hasher + Default + 'a,
{
    type Future
        = Pin<Box<dyn Future<Output = VfsResult<Self, Vfs>> + Send + 'a>>
    where
        Self: 'a;
    fn read_from_async(
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        use std::future::poll_fn;

        let mut fut = Box::pin(T::read_from_async(path.clone(), vfs));

        Box::pin(poll_fn(move |cx| {
            fut.as_mut().poll(cx).map_ok(|value| {
                let path = path.clone();
                let mut hasher = H::default();
                T::hash(&value, &mut hasher);
                let hash = hasher.finish();
                VersionedHash {
                    value,
                    hash,
                    path,
                    _hasher: marker::PhantomData,
                }
            })
        }))
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, P, H, Vfs: WriteSupportingVfsAsync<Path = P> + 'a> WriteToAsync<'a, Vfs>
    for VersionedHash<T, P, H>
where
    T: WriteToAsync<'a, Vfs> + Hash + 'a,
    P: PathType + ?Sized + PartialEq + 'a,
    H: Hasher + Default + 'a,
{
    type Future
        = Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + 'a>>
    where
        Self: 'a;

    fn write_to_async(
        self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        if self.path.as_ref() == path.as_ref() && self.is_clean() {
            return Box::pin(async { Ok(()) });
        }

        let fut = Box::pin(T::write_to_async(self.value, path, vfs));

        Box::pin(fut)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, T, H, Vfs: WriteSupportingVfsAsync + 'a> WriteToAsyncRef<'a, Vfs>
    for VersionedHash<T, Vfs::Path, H>
where
    T: WriteToAsyncRef<'a, Vfs> + Hash + 'a,
    H: Hasher + Default + 'a,
{
    type Future<'f>
        = Pin<Box<dyn Future<Output = VfsResult<(), Vfs>> + Send + 'f>>
    where
        Self: 'f,
        'a: 'f,
        Vfs: 'f;

    fn write_to_async_ref<'f>(
        &'f self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'f Vfs>,
    ) -> Self::Future<'f>
    where
        'a: 'f,
    {
        if self.path == path && self.is_clean() {
            return Box::pin(async { Ok(()) });
        }

        let fut = Box::pin(T::write_to_async_ref(&self.value, path, vfs));

        Box::pin(fut)
    }
}
