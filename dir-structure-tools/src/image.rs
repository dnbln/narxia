//! Implementations of [`ReadFrom`] and [`WriteTo`] for image files.
//! 
//! The [`image`] crate is used for image decoding and encoding.
//! 
//! Main wrapper type is the [`Img`] struct, which is generic over an image format.
//! Specific image formats are provided as zero-sized types implementing the [`ImgFormat`] trait.
//! 
//! The following image formats are supported, behind feature flags:
//!
//! - PNG (`image-format-png`)
//! - JPEG (`image-format-jpeg`)
//! - GIF (`image-format-gif`)
//! - WebP (`image-format-webp`)
//! - PNM (`image-format-pnm`)
//! - TIFF (`image-format-tiff`)
//! - TGA (`image-format-tga`)
//! - BMP (`image-format-bmp`)
//! - ICO (`image-format-ico`)
//! - HDR (`image-format-hdr`)
//! - OpenEXR (`image-format-exr`)
//! - Farbfeld (`image-format-ff`)
//! - AVIF (`image-format-avif`)
//! - QOI (`image-format-qoi`)
//! 
//! For an async VFS implementation to support reading and writing images using these types,
//! the following impls are required for the VFS type:
//! 
//! - `impl<T: ImgFormat> ReadImageFromAsync<T> for NewVfsType`
//! - `impl<'a> WriteImageToAsync<'a> for NewVfsType`
//! - `impl<'a> WriteImageToAsyncRef<'a> for NewVfsType`

#[cfg(all(
    feature = "assert_eq",
    any(
        feature = "image-format-png",
        feature = "image-format-jpeg",
        feature = "image-format-gif",
        feature = "image-format-webp",
        feature = "image-format-pnm",
        feature = "image-format-tiff",
        feature = "image-format-tga",
        feature = "image-format-bmp",
        feature = "image-format-ico",
        feature = "image-format-hdr",
        feature = "image-format-exr",
        feature = "image-format-ff",
        feature = "image-format-avif",
        feature = "image-format-qoi",
    )
))]
use std::fmt;
use std::io;
use std::io::Seek;
use std::marker;
use std::pin::Pin;

#[cfg(feature = "async")]
use dir_structure::traits::async_vfs::WriteSupportingVfsAsync;
#[cfg(feature = "async")]
use futures::AsyncSeek;

use dir_structure::error::Error;
use dir_structure::error::VfsResult;
use dir_structure::error::WrapIoError;
use dir_structure::prelude::*;
#[cfg(feature = "async")]
use dir_structure::traits::async_vfs::VfsAsyncWithSeekWrite;
use dir_structure::traits::sync::FromRefForWriter;
use dir_structure::traits::sync::NewtypeToInner;
use dir_structure::traits::vfs;
use dir_structure::traits::vfs::PathType;

/// A wrapper around `image::DynamicImage` to implement [`ReadFrom`].
pub struct DynImage(image::DynamicImage);
/// A wrapper around `image::DynamicImage` and its format to implement [`WriteTo`].
pub struct DynImageWithFormat(image::DynamicImage, image::ImageFormat);
/// A wrapper around a reference to [`image::DynamicImage`] and its format to implement [`WriteTo`].
pub struct DynImageRefWithFormat<'a>(&'a image::DynamicImage, image::ImageFormat);

impl<'vfs, Vfs: vfs::VfsWithSeekRead<'vfs>> ReadFrom<'vfs, Vfs> for DynImage
where
    Vfs::RFile: Seek,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<Self, Vfs> {
        image::ImageReader::new(&mut io::BufReader::new(vfs.open_read(path)?))
            .with_guessed_format()
            .wrap_io_error_with(path)?
            .decode()
            .map(Self)
            .map_err(|e| Error::Parse(path.owned(), Box::new(e)))
    }
}

impl<'vfs, Vfs: vfs::VfsWithSeekWrite<'vfs>> WriteTo<'vfs, Vfs> for DynImageWithFormat
where
    Vfs::WFile: Seek,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<(), Vfs> {
        vfs.create_parent_dir(path)?;
        let mut f = vfs.open_write(path)?;

        let DynImageWithFormat(img, format) = self;
        img.write_to(&mut f, *format)
            .map_err(|e| Error::Write(path.owned(), Box::new(e)))
    }
}

impl<'vfs, Vfs: vfs::VfsWithSeekWrite<'vfs>> WriteTo<'vfs, Vfs> for DynImageRefWithFormat<'_>
where
    Vfs::WFile: Seek,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<(), Vfs> {
        vfs.create_parent_dir(path)?;
        let mut f = vfs.open_write(path)?;

        let DynImageRefWithFormat(img, format) = self;
        img.write_to(&mut f, *format)
            .map_err(|e| Error::Write(path.owned(), Box::new(e)))
    }
}

/// An image format. This is used to implement [`ReadFrom`] and [`WriteTo`] for specific image formats.
pub trait ImgFormat: Clone {
    /// The image format.
    const FORMAT: image::ImageFormat;
    /// The writer type for this image format.
    type WriterType<'a, Vfs>: From<&'a image::DynamicImage>
    where
        Vfs: 'a;
}

/// A wrapper around an image of a specific format.
pub struct Img<F: ImgFormat>(DynImage, marker::PhantomData<F>);

impl<T: ImgFormat> NewtypeToInner for Img<T> {
    type Inner = image::DynamicImage;

    fn into_inner(self) -> Self::Inner {
        self.0.0
    }
}

impl<'a, 'vfs, T: ImgFormat + 'a, Vfs: vfs::VfsWithSeekWrite<'vfs>> FromRefForWriter<'a, 'vfs, Vfs>
    for Img<T>
where
    Vfs: 'a,
    Vfs::WFile: Seek,
    T::WriterType<'a, Vfs>: WriteTo<'vfs, Vfs> + 'a,
    'vfs: 'a,
{
    type Inner = image::DynamicImage;
    type Wr = T::WriterType<'a, Vfs>;

    fn from_ref_for_writer(inner: &'a Self::Inner) -> Self::Wr {
        T::WriterType::<'a, Vfs>::from(inner)
    }
}

impl<'vfs, Vfs: vfs::VfsWithSeekRead<'vfs>, T> ReadFrom<'vfs, Vfs> for Img<T>
where
    T: ImgFormat + 'vfs,
    Vfs::RFile: Seek,
{
    fn read_from(path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<Self, Vfs> {
        debug_assert!(
            T::FORMAT.reading_enabled(),
            "Image format {:?} does not support reading; enable the corresponding feature",
            T::FORMAT
        );
        let mut img_reader = image::ImageReader::new(io::BufReader::new(vfs.open_read(path)?));
        img_reader.set_format(T::FORMAT);
        let img = img_reader
            .decode()
            .map_err(|e| Error::Parse(path.owned(), Box::new(e)))?;
        Ok(Img::<T>(DynImage(img), marker::PhantomData))
    }
}

impl<'vfs, Vfs: vfs::VfsWithSeekWrite<'vfs>, T> WriteTo<'vfs, Vfs> for Img<T>
where
    T: ImgFormat,
    Vfs::WFile: Seek,
{
    fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<(), Vfs> {
        debug_assert!(
            T::FORMAT.writing_enabled(),
            "Image format {:?} does not support writing; enable the corresponding feature",
            T::FORMAT
        );
        DynImageRefWithFormat(&self.0.0, T::FORMAT).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs: VfsAsyncWithSeekWrite<Path = P>, P: PathType + ?Sized + 'vfs, T: ImgFormat>
    WriteToAsync<'vfs, Vfs> for Img<T>
where
    Vfs: 'vfs,
    Vfs::WFile: AsyncSeek,
    DynImageWithFormat: WriteToAsync<'vfs, Vfs>,
{
    type Future = <DynImageWithFormat as WriteToAsync<'vfs, Vfs>>::Future;

    fn write_to_async(self, path: P::OwnedPath, vfs: Pin<&'vfs Vfs>) -> Self::Future {
        debug_assert!(
            T::FORMAT.writing_enabled(),
            "Image format {:?} does not support writing; enable the corresponding feature",
            T::FORMAT
        );
        DynImageWithFormat(self.0.0, T::FORMAT).write_to_async(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs: VfsAsyncWithSeekWrite<Path = P>, P: PathType + ?Sized + 'vfs, T: ImgFormat>
    WriteToAsyncRef<'vfs, Vfs> for Img<T>
where
    Vfs: 'vfs,
    Vfs::WFile: AsyncSeek,
    for<'a> DynImageRefWithFormat<'a>: WriteToAsync<'a, Vfs>,
{
    type Future<'a>
        = <DynImageRefWithFormat<'a> as WriteToAsync<'a, Vfs>>::Future
    where
        Self: 'a,
        'vfs: 'a,
        Vfs: 'a;

    fn write_to_async_ref<'a>(&'a self, path: P::OwnedPath, vfs: Pin<&'a Vfs>) -> Self::Future<'a>
    where
        'vfs: 'a,
    {
        debug_assert!(
            T::FORMAT.writing_enabled(),
            "Image format {:?} does not support writing; enable the corresponding feature",
            T::FORMAT
        );
        DynImageRefWithFormat(&self.0.0, T::FORMAT).write_to_async(path, vfs)
    }
}

// async implementations are available only for specific VFS implementations,
// as they require specific async traits that are not part of the standard library (e.g. a la tokio::task::spawn_blocking,
// or similar, for other runtimes, given that image encoding / decoding with the image crate is CPU-bound and blocking).
//
// as such, they are implemented in the respective VFS modules.
// for new async VFS implementations, the following impls are required for Image formats to work with it:
// - `impl<T: ImgFormat> ReadFromAsync<'vfs, NewVfsType> for T`                                 to satisfy the bound `T: ReadFromAsync<'vfs, NewVfsType>`
// - `impl<'a> WriteToAsync<'a, NewVfsType> for (image::DynamicImage, image::ImageFormat)`      to satisfy the bound `T: WriteToAsync<'a, NewVfsType>`
// - `impl<'a> WriteToAsync<'a, NewVfsType> for (&'a image::DynamicImage, image::ImageFormat)`  to satisfy the bound `T: WriteToAsyncRef<'a, NewVfsType>`
//
// These impls are automatically generated for Vfs types that implement the following traits:
// - `ReadImageFromAsync<T>`
// - `WriteImageToAsync<'a>`
// - `WriteImageToAsyncRef<'a>`

macro_rules! img_format {
    ($(#[$meta:meta])* cfg $(#[$cfg_meta:meta])* $struct_name:ident, $format:expr, $(#[$writer_meta:meta])* $writer_type:ident) => {
        $(#[$meta])*
        $(#[$cfg_meta])*
        #[derive(Debug, Clone, PartialEq)]
        pub struct $struct_name(image::DynamicImage);

        $(#[$cfg_meta])*
        #[cfg(feature = "assert_eq")]
        impl assert_eq::AssertEq for $struct_name {
            fn assert_eq(&self, other: &Self, path: &mut assert_eq::AssertPath, init_left: &impl fmt::Display, init_right: &impl fmt::Display) {
                if self.0 != other.0 {
                    panic!("Images differ (at {:?})\nassert_eq! initially called with:\n  left: {}\n right: {}", &*path.__guard("image"), init_left, init_right);
                }
            }
        }

        $(#[$cfg_meta])*
        impl ImgFormat for $struct_name {
            const FORMAT: image::ImageFormat = $format;

            type WriterType<'a, Vfs> = $writer_type<'a, Vfs> where Vfs: 'a;
        }

        $(#[$writer_meta])*
        $(#[$cfg_meta])*
        pub struct $writer_type<'a, Vfs: 'a>(&'a image::DynamicImage, marker::PhantomData<Vfs>);

        $(#[$cfg_meta])*
        impl<'a, Vfs> From<&'a image::DynamicImage> for $writer_type<'a, Vfs> {
            fn from(img: &'a image::DynamicImage) -> Self {
                $writer_type(img, marker::PhantomData)
            }
        }

        $(#[$cfg_meta])*
        impl<'a, 'vfs, Vfs: vfs::VfsWithSeekWrite<'vfs>> WriteTo<'vfs, Vfs> for $writer_type<'a, Vfs>
        where
            Vfs::WFile: Seek,
            'vfs: 'a,
        {
            fn write_to(&self, path: &Vfs::Path, vfs: Pin<&'vfs Vfs>) -> VfsResult<(), Vfs> {
                debug_assert!(
                    $struct_name::FORMAT.writing_enabled(),
                    "Image format {:?} does not support writing; enable the corresponding feature",
                    $struct_name::FORMAT
                );
                DynImageRefWithFormat(self.0, $struct_name::FORMAT).write_to(path, vfs)
            }
        }
    };
}

img_format!(
    /// An image in PNG format.
    cfg
    #[cfg(feature = "image-format-png")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-png")))]
    Png,
    image::ImageFormat::Png,
    /// A writer for PNG images.
    PngWriter
);

img_format!(
    /// An image in JPEG format.
    cfg
    #[cfg(feature = "image-format-jpeg")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-jpeg")))]
    Jpeg,
    image::ImageFormat::Jpeg,
    /// A writer for JPEG images.
    JpegWriter
);

img_format!(
    /// An image in GIF format.
    cfg
    #[cfg(feature = "image-format-gif")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-gif")))]
    Gif,
    image::ImageFormat::Gif,
    /// A writer for GIF images.
    GifWriter
);

img_format!(
    /// An image in WebP format.
    cfg
    #[cfg(feature = "image-format-webp")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-webp")))]
    WebP,
    image::ImageFormat::WebP,
    /// A writer for WebP images.
    WebPWriter
);

img_format!(
    /// An image in PNM format.
    cfg
    #[cfg(feature = "image-format-pnm")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-pnm")))]
    Pnm,
    image::ImageFormat::Pnm,
    /// A writer for PNM images.
    PnmWriter
);

img_format!(
    /// An image in TIFF format.
    cfg
    #[cfg(feature = "image-format-tiff")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-tiff")))]
    Tiff,
    image::ImageFormat::Tiff,
    /// A writer for TIFF images.
    TiffWriter
);

img_format!(
    /// An image in TGA format.
    cfg
    #[cfg(feature = "image-format-tga")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-tga")))]
    Tga,
    image::ImageFormat::Tga,
    /// A writer for TGA images.
    TgaWriter
);

// DDS support is not there as `image` doesn't support reading / writing DDS files.
// img_format!(
//     /// An image in DDS format.
//     cfg
//     #[cfg(feature = "image-format-dds")]
//     #[cfg_attr(docsrs, doc(cfg(feature = "image-format-dds")))]
//     Dds,
//     image::ImageFormat::Dds
// );

img_format!(
    /// An image in BMP format.
    cfg
    #[cfg(feature = "image-format-bmp")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-bmp")))]
    Bmp,
    image::ImageFormat::Bmp,
    /// A writer for BMP images.
    BmpWriter
);

img_format!(
    /// An image in ICO format.
    cfg
    #[cfg(feature = "image-format-ico")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-ico")))]
    Ico,
    image::ImageFormat::Ico,
    /// A writer for ICO images.
    IcoWriter
);

img_format!(
    /// An image in HDR format.
    cfg
    #[cfg(feature = "image-format-hdr")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-hdr")))]
    Hdr,
    image::ImageFormat::Hdr,
    /// A writer for HDR images.
    HdrWriter
);

img_format!(
    /// An image in OpenEXR format.
    cfg
    #[cfg(feature = "image-format-exr")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-exr")))]
    OpenExr,
    image::ImageFormat::OpenExr,
    /// A writer for OpenEXR images.
    OpenExrWriter
);

img_format!(
    /// An image in Farbfeld format.
    cfg
    #[cfg(feature = "image-format-ff")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-ff")))]
    Farbfeld,
    image::ImageFormat::Farbfeld,
    /// A writer for Farbfeld images.
    FarbfeldWriter
);

img_format!(
    /// An image in AVIF format.
    cfg
    #[cfg(feature = "image-format-avif")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image-format-avif")))]
    Avif,
    image::ImageFormat::Avif,
    /// A writer for AVIF images.
    AvifWriter
);

img_format!(
    /// An image in QOI format.
    cfg
    #[cfg(feature = "image-format-qoi")]
    Qoi,
    image::ImageFormat::Qoi,
    /// A writer for QOI images.
    QoiWriter
);

/// A trait implemented by async vfs implementations that support reading images.
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait ReadImageFromAsync<T>: VfsAsync
where
    T: ImgFormat,
{
    /// The future type returned by the [`read_image_async` method](ReadImageFromAsync::read_image_async).
    type ReadImageFuture<'a>: Future<Output = VfsResult<Img<T>, Self>> + Send + Unpin + 'a
    where
        Self: 'a;

    /// Reads an image file at the specified path, returning the decoded image and its format.
    fn read_image_async<'a>(
        self: Pin<&'a Self>,
        path: <Self::Path as PathType>::OwnedPath,
    ) -> Self::ReadImageFuture<'a>;
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs, T> ReadFromAsync<'vfs, Vfs> for Img<T>
where
    Vfs: VfsAsync + ReadImageFromAsync<T> + 'vfs,
    T: ImgFormat + Send + 'vfs,
{
    type Future = <Vfs as ReadImageFromAsync<T>>::ReadImageFuture<'vfs>;

    fn read_from_async(
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'vfs Vfs>,
    ) -> Self::Future {
        Vfs::read_image_async(vfs, path)
    }
}

/// A trait implemented by async vfs implementations that support writing images.
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteImageToAsync<'a>: WriteSupportingVfsAsync {
    /// The future type returned by the [`write_image_async` method](WriteImageToAsync::write_image_async).
    type WriteImageFuture: Future<Output = VfsResult<(), Self>> + Send + Unpin + 'a;

    /// Writes an image file at the specified path, using the specified image and format.
    fn write_image_async(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
        image: image::DynamicImage,
        format: image::ImageFormat,
    ) -> Self::WriteImageFuture;
}

// impl for owned images
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs> WriteToAsync<'a, Vfs> for DynImageWithFormat
where
    Vfs: WriteSupportingVfsAsync + WriteImageToAsync<'a> + 'a,
{
    type Future = Vfs::WriteImageFuture;

    fn write_to_async(
        self,
        path: <<Vfs as VfsCore>::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        let DynImageWithFormat(image, format) = self;
        vfs.write_image_async(path, image, format)
    }
}

/// A trait implemented by async vfs implementations that support writing images from references.
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
pub trait WriteImageToAsyncRef<'a>: WriteSupportingVfsAsync {
    /// The future type returned by the [`write_image_async_ref` method](WriteImageToAsyncRef::write_image_async_ref).
    type WriteImageRefFuture: Future<Output = VfsResult<(), Self>> + Send + Unpin + 'a;

    /// Writes an image file at the specified path, using the specified image reference and format.
    fn write_image_async_ref(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
        image: &'a image::DynamicImage,
        format: image::ImageFormat,
    ) -> Self::WriteImageRefFuture;
}

// impl for image references
#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'a, Vfs: 'a> WriteToAsync<'a, Vfs> for DynImageRefWithFormat<'a>
where
    Vfs: WriteSupportingVfsAsync + WriteImageToAsyncRef<'a>,
{
    type Future = Vfs::WriteImageRefFuture;

    fn write_to_async(
        self,
        path: <Vfs::Path as PathType>::OwnedPath,
        vfs: Pin<&'a Vfs>,
    ) -> Self::Future {
        let DynImageRefWithFormat(image, format) = self;
        vfs.write_image_async_ref(path, image, format)
    }
}

#[cfg(feature = "tokio")]
mod tokio_fs_impl {
    use std::path::PathBuf;

    use super::*;

    use dir_structure::traits::vfs::WriteSupportingVfs as _;
    use dir_structure::vfs::tokio_fs_vfs::TokioFsVfs;
    use tokio::task;

    use dir_structure::error::Error;
    use dir_structure::error::Result;
    use dir_structure::vfs::fs_vfs::FsVfs;

    impl<'vfs, T: ImgFormat> ReadImageFromAsync<T> for TokioFsVfs
    where
        T: Send + 'static,
    {
        type ReadImageFuture<'a>
            = Pin<Box<dyn Future<Output = Result<Img<T>, PathBuf>> + Send + 'a>>
        where
            Self: 'a;

        fn read_image_async<'a>(
            self: Pin<&'a Self>,
            path: <Self::Path as PathType>::OwnedPath,
        ) -> Self::ReadImageFuture<'a> {
            debug_assert!(
                T::FORMAT.reading_enabled(),
                "Image format {:?} does not support reading; enable the corresponding feature",
                T::FORMAT
            );
            let std_vfs = Pin::new(&FsVfs);
            Box::pin(async move {
                let p_clone = path.clone();
                match task::spawn_blocking(move || {
                    let mut img_reader =
                        image::ImageReader::new(io::BufReader::new(std_vfs.open_read(&p_clone)?));
                    img_reader.set_format(T::FORMAT);
                    let img = img_reader
                        .decode()
                        .map_err(|e| Error::Parse(p_clone.clone(), Box::new(e)))?;
                    Ok(Img::<T>(DynImage(img), marker::PhantomData))
                })
                .await
                {
                    Ok(res) => res,
                    Err(e) => Err(Error::Parse(path, Box::new(e))),
                }
            })
        }
    }

    impl<'vfs> WriteImageToAsync<'vfs> for TokioFsVfs {
        type WriteImageFuture = Pin<Box<dyn Future<Output = Result<(), PathBuf>> + Send + 'vfs>>;

        fn write_image_async(
            self: Pin<&'vfs Self>,
            path: <Self::Path as PathType>::OwnedPath,
            image: image::DynamicImage,
            format: image::ImageFormat,
        ) -> Self::WriteImageFuture {
            Box::pin(async move {
                self.create_parent_dir(path.clone()).await?;

                let p_clone = path.clone();

                let std_vfs = Pin::new(&FsVfs);
                match task::spawn_blocking(move || {
                    let mut f = std_vfs.open_write(&p_clone)?;
                    image
                        .write_to(&mut f, format)
                        .map_err(|e| Error::Write(p_clone, Box::new(e)))
                })
                .await
                {
                    Ok(res) => res,
                    Err(e) => Err(Error::Write(path, Box::new(e))),
                }
            })
        }
    }

    impl<'vfs> WriteImageToAsyncRef<'vfs> for TokioFsVfs
    where
        'vfs: 'vfs,
    {
        type WriteImageRefFuture = Pin<Box<dyn Future<Output = Result<(), PathBuf>> + Send + 'vfs>>;

        fn write_image_async_ref(
            self: Pin<&'vfs Self>,
            path: <Self::Path as PathType>::OwnedPath,
            image: &'vfs image::DynamicImage,
            format: image::ImageFormat,
        ) -> Self::WriteImageRefFuture {
            let img = image.clone();
            DynImageWithFormat(img, format).write_to_async(path, self)
        }
    }
}

#[cfg(test)]
#[allow(dead_code)]
mod tests {
    use std::io::Seek;

    #[cfg(feature = "async")]
    use futures::AsyncSeek;

    #[cfg(any(
        feature = "image-format-png",
        feature = "image-format-jpeg",
        feature = "image-format-gif",
        feature = "image-format-webp",
        feature = "image-format-pnm",
        feature = "image-format-tiff",
        feature = "image-format-tga",
        feature = "image-format-bmp",
        feature = "image-format-ico",
        feature = "image-format-hdr",
        feature = "image-format-exr",
        feature = "image-format-ff",
        feature = "image-format-avif",
        feature = "image-format-qoi",
    ))]
    use super::Img;
    use dir_structure::prelude::*;
    #[cfg(feature = "async")]
    use dir_structure::traits::async_vfs::VfsAsyncWithSeekRead;
    #[cfg(feature = "async")]
    use dir_structure::traits::async_vfs::VfsAsyncWithSeekWrite;
    use dir_structure::traits::vfs;

    fn assert_is_read_sync<'vfs, Vfs: vfs::VfsWithSeekRead<'vfs> + 'vfs, T: ReadFrom<'vfs, Vfs>>()
    where
        Vfs::RFile: Seek,
    {
    }

    fn assert_is_write_sync<'vfs, Vfs: vfs::VfsWithSeekWrite<'vfs> + 'vfs, T: WriteTo<'vfs, Vfs>>()
    where
        Vfs::WFile: Seek,
    {
    }

    #[cfg(feature = "async")]
    fn assert_is_read_async<'vfs, Vfs: VfsAsyncWithSeekRead + 'vfs, T: ReadFromAsync<'vfs, Vfs>>()
    where
        Vfs::RFile: AsyncSeek,
    {
    }
    #[cfg(feature = "async")]
    fn assert_is_write_async<'vfs, Vfs: VfsAsyncWithSeekWrite + 'vfs, T: WriteToAsync<'vfs, Vfs>>()
    where
        Vfs::WFile: AsyncSeek,
    {
    }
    #[cfg(feature = "async")]
    fn assert_is_write_async_ref<
        'vfs,
        Vfs: VfsAsyncWithSeekWrite + 'vfs,
        T: WriteToAsyncRef<'vfs, Vfs>,
    >()
    where
        Vfs::WFile: AsyncSeek,
    {
    }

    macro_rules! test_sync_traits {
        ($(#[$attr:meta])* fn $test_name:ident (), $vfs:ty, $image:ty) => {
            #[test]
            $(#[$attr])*
            fn $test_name() {
                assert_is_read_sync::<$vfs, $image>();
                assert_is_write_sync::<$vfs, $image>();
            }
        };
    }

    macro_rules! test_async_traits {
        ($(#[$attr:meta])* fn $test_name:ident (), $vfs:ty, $image:ty) => {
            #[test]
            $(#[$attr])*
            fn $test_name() {
                assert_is_read_async::<$vfs, $image>();
                assert_is_write_async::<$vfs, $image>();
                assert_is_write_async_ref::<$vfs, $image>();
            }
        };
    }

    test_sync_traits!(
        #[cfg(feature = "image-format-png")]
        fn test_png_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Png>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-png", feature = "tokio"))]
        fn test_png_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Png>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-jpeg")]
        fn test_jpeg_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Jpeg>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-jpeg", feature = "tokio"))]
        fn test_jpeg_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Jpeg>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-gif")]
        fn test_gif_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Gif>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-gif", feature = "tokio"))]
        fn test_gif_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Gif>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-webp")]
        fn test_webp_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::WebP>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-webp", feature = "tokio"))]
        fn test_webp_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::WebP>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-pnm")]
        fn test_pnm_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Pnm>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-pnm", feature = "tokio"))]
        fn test_pnm_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Pnm>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-tiff")]
        fn test_tiff_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Tiff>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-tiff", feature = "tokio"))]
        fn test_tiff_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Tiff>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-tga")]
        fn test_tga_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Tga>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-tga", feature = "tokio"))]
        fn test_tga_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Tga>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-bmp")]
        fn test_bmp_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Bmp>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-bmp", feature = "tokio"))]
        fn test_bmp_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Bmp>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-ico")]
        fn test_ico_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Ico>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-ico", feature = "tokio"))]
        fn test_ico_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Ico>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-hdr")]
        fn test_hdr_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Hdr>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-hdr", feature = "tokio"))]
        fn test_hdr_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Hdr>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-exr")]
        fn test_openexr_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::OpenExr>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-exr", feature = "tokio"))]
        fn test_openexr_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::OpenExr>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-ff")]
        fn test_farbfeld_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Farbfeld>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-ff", feature = "tokio"))]
        fn test_farbfeld_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Farbfeld>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-avif")]
        fn test_avif_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Avif>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-avif", feature = "tokio"))]
        fn test_avif_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Avif>
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-qoi")]
        fn test_qoi_sync_traits(),
        dir_structure::vfs::fs_vfs::FsVfs,
        Img<crate::image::Qoi>
    );

    test_async_traits!(
        #[cfg(all(feature = "image-format-qoi", feature = "tokio"))]
        fn test_qoi_async_traits(),
        dir_structure::vfs::tokio_fs_vfs::TokioFsVfs,
        Img<crate::image::Qoi>
    );
}
