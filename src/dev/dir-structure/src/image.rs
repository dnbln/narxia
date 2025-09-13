//! Implementations of [`ReadFrom`] and [`WriteTo`] for image files.

use std::io::Seek;
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
use std::marker;
use std::path::Path;
#[cfg(feature = "async")]
use std::path::PathBuf;
use std::pin::Pin;

#[cfg(feature = "async")]
use futures::AsyncSeek;

use crate::error::Error;
use crate::error::Result;
use crate::error::WrapIoError;
use crate::prelude::*;
#[cfg(feature = "async")]
use crate::traits::async_vfs::VfsAsyncWithSeekWrite;
use crate::traits::sync::FromRefForWriter;
use crate::traits::sync::NewtypeToInner;
use crate::traits::vfs;

impl<'vfs, Vfs: vfs::VfsWithSeekRead<'vfs>> ReadFrom<'vfs, Vfs> for image::DynamicImage
where
    Vfs::RFile: Seek,
{
    fn read_from(path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<Self> {
        image::ImageReader::new(&mut vfs.open_read(path)?)
            .with_guessed_format()
            .wrap_io_error_with(path)?
            .decode()
            .map_err(|e| Error::Parse(path.to_path_buf(), Box::new(e)))
    }
}

impl<'vfs, Vfs: vfs::VfsWithSeekWrite<'vfs>> WriteTo<'vfs, Vfs>
    for (image::DynamicImage, image::ImageFormat)
where
    Vfs::WFile: Seek,
{
    fn write_to(&self, path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<()> {
        vfs.create_parent_dir(path)?;
        let mut f = vfs.open_write(path)?;

        let (img, format) = self;
        img.write_to(&mut f, *format)
            .map_err(|e| Error::Write(path.to_path_buf(), Box::new(e)))
    }
}

impl<'vfs, Vfs: vfs::VfsWithSeekWrite<'vfs>> WriteTo<'vfs, Vfs>
    for (&image::DynamicImage, image::ImageFormat)
where
    Vfs::WFile: Seek,
{
    fn write_to(&self, path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<()> {
        vfs.create_parent_dir(path)?;
        let mut f = vfs.open_write(path)?;

        let (img, format) = self;
        img.write_to(&mut f, *format)
            .map_err(|e| Error::Write(path.to_path_buf(), Box::new(e)))
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

    /// Create an instance of this type from a [`image::DynamicImage`].
    fn from_image(img: image::DynamicImage) -> Self;
    /// Get a reference to the inner [`image::DynamicImage`].
    fn as_image(&self) -> &image::DynamicImage;
    /// Consumes this instance and returns the inner [`image::DynamicImage`].
    fn into_image(self) -> image::DynamicImage;
}

impl<T: ImgFormat> NewtypeToInner for T {
    type Inner = image::DynamicImage;

    fn into_inner(self) -> Self::Inner {
        self.into_image()
    }
}

impl<'a, 'vfs, T: ImgFormat + 'a, Vfs: vfs::VfsWithSeekWrite<'vfs>> FromRefForWriter<'a, 'vfs, Vfs>
    for T
where
    Vfs: 'a,
    Vfs::WFile: Seek,
    T::WriterType<'a, Vfs>: WriteTo<'vfs, Vfs> + 'a,
{
    type Inner = image::DynamicImage;
    type Wr = T::WriterType<'a, Vfs>;

    fn from_ref_for_writer(inner: &'a Self::Inner) -> Self::Wr {
        T::WriterType::<'a, Vfs>::from(inner)
    }
}

impl<'vfs, Vfs: vfs::VfsWithSeekRead<'vfs>, T> ReadFrom<'vfs, Vfs> for T
where
    T: ImgFormat + 'vfs,
    Vfs::RFile: Seek,
{
    fn read_from(path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<Self> {
        debug_assert!(
            T::FORMAT.reading_enabled(),
            "Image format {:?} does not support reading; enable the corresponding feature",
            T::FORMAT
        );
        let mut img_reader = image::ImageReader::new(vfs.open_read(path)?);
        img_reader.set_format(T::FORMAT);
        let img = img_reader
            .decode()
            .map_err(|e| Error::Parse(path.to_path_buf(), Box::new(e)))?;
        Ok(T::from_image(img))
    }
}

impl<'vfs, Vfs: vfs::VfsWithSeekWrite<'vfs>, T> WriteTo<'vfs, Vfs> for T
where
    T: ImgFormat,
    Vfs::WFile: Seek,
{
    fn write_to(&self, path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<()> {
        debug_assert!(
            T::FORMAT.writing_enabled(),
            "Image format {:?} does not support writing; enable the corresponding feature",
            T::FORMAT
        );
        (self.as_image(), T::FORMAT).write_to(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs: VfsAsyncWithSeekWrite, T: ImgFormat> WriteToAsync<'vfs, Vfs> for T
where
    Vfs: 'vfs,
    Vfs::WFile: AsyncSeek,
    (image::DynamicImage, image::ImageFormat): WriteToAsync<'vfs, Vfs>,
{
    type Future = <(image::DynamicImage, image::ImageFormat) as WriteToAsync<'vfs, Vfs>>::Future;

    fn write_to_async(self, path: PathBuf, vfs: Pin<&'vfs Vfs>) -> Self::Future {
        debug_assert!(
            T::FORMAT.writing_enabled(),
            "Image format {:?} does not support writing; enable the corresponding feature",
            T::FORMAT
        );
        (self.into_image(), T::FORMAT).write_to_async(path, vfs)
    }
}

#[cfg(feature = "async")]
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
impl<'vfs, Vfs: VfsAsyncWithSeekWrite, T: ImgFormat> WriteToAsyncRef<'vfs, Vfs> for T
where
    Vfs: 'vfs,
    Vfs::WFile: AsyncSeek,
    for<'a> (&'a image::DynamicImage, image::ImageFormat): WriteToAsync<'a, Vfs>,
{
    type Future<'a>
        = <(&'a image::DynamicImage, image::ImageFormat) as WriteToAsync<'a, Vfs>>::Future
    where
        Self: 'a,
        'vfs: 'a,
        Vfs: 'a;

    fn write_to_async_ref<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a>
    where
        'vfs: 'a,
    {
        debug_assert!(
            T::FORMAT.writing_enabled(),
            "Image format {:?} does not support writing; enable the corresponding feature",
            T::FORMAT
        );
        let img = self.as_image();
        (img, T::FORMAT).write_to_async(path, vfs)
    }
}

// async implementations are available only for specific VFS implementations,
// as they require specific async traits that are not part of the standard library.
//
// as such, they are implemented in the respective VFS modules.
// only the following impls are required:
// - `impl<T: ImgFormat> ReadFromAsync<'vfs, ...> for T`
// - `impl<'a> WriteToAsync<'a, ...> for (image::DynamicImage, image::ImageFormat)` for `WriteToAsync`
// - `impl<'a> WriteToAsync<'a, ...> for (&'a image::DynamicImage, image::ImageFormat)` for `WriteToAsyncRef`

macro_rules! img_format {
    ($(#[$meta:meta])* cfg $(#[$cfg_meta:meta])* $struct_name:ident, $format:expr, $(#[$writer_meta:meta])* $writer_type:ident) => {
        $(#[$meta])*
        $(#[$cfg_meta])*
        #[derive(Debug, Clone, PartialEq)]
        pub struct $struct_name(image::DynamicImage);

        $(#[$cfg_meta])*
        #[cfg(feature = "assert_eq")]
        impl assert_eq::AssertEq for $struct_name {
            fn assert_eq(&self, other: &Self, path: &mut assert_eq::AssertPath) {
                if self.0 != other.0 {
                    panic!("Images differ (at {:?})", &*path.__guard("image"));
                }
            }
        }

        $(#[$cfg_meta])*
        impl ImgFormat for $struct_name {
            const FORMAT: image::ImageFormat = $format;

            type WriterType<'a, Vfs> = $writer_type<'a, Vfs> where Vfs: 'a;

            fn from_image(img: image::DynamicImage) -> Self {
                $struct_name(img)
            }

            fn as_image(&self) -> &image::DynamicImage {
                &self.0
            }

            fn into_image(self) -> image::DynamicImage {
                self.0
            }
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
        {
            fn write_to(&self, path: &Path, vfs: Pin<&'vfs Vfs>) -> Result<()> {
                debug_assert!(
                    $struct_name::FORMAT.writing_enabled(),
                    "Image format {:?} does not support writing; enable the corresponding feature",
                    $struct_name::FORMAT
                );
                (self.0, $struct_name::FORMAT).write_to(path, vfs)
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

#[cfg(test)]
#[allow(dead_code)]
mod tests {
    use std::io::Seek;

    use futures::AsyncSeek;

    use crate::prelude::*;
    use crate::traits::async_vfs::VfsAsyncWithSeekRead;
    use crate::traits::async_vfs::VfsAsyncWithSeekWrite;
    use crate::traits::vfs;

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

    fn assert_is_read_async<'vfs, Vfs: VfsAsyncWithSeekRead + 'vfs, T: ReadFromAsync<'vfs, Vfs>>()
    where
        Vfs::RFile: AsyncSeek,
    {
    }
    fn assert_is_write_async<'vfs, Vfs: VfsAsyncWithSeekWrite + 'vfs, T: WriteToAsync<'vfs, Vfs>>()
    where
        Vfs::WFile: AsyncSeek,
    {
    }
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
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Png
    );

    test_async_traits!(
        #[cfg(feature = "image-format-png")]
        fn test_png_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Png
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-jpeg")]
        fn test_jpeg_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Jpeg
    );

    test_async_traits!(
        #[cfg(feature = "image-format-jpeg")]
        fn test_jpeg_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Jpeg
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-gif")]
        fn test_gif_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Gif
    );

    test_async_traits!(
        #[cfg(feature = "image-format-gif")]
        fn test_gif_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Gif
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-webp")]
        fn test_webp_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::WebP
    );

    test_async_traits!(
        #[cfg(feature = "image-format-webp")]
        fn test_webp_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::WebP
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-pnm")]
        fn test_pnm_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Pnm
    );

    test_async_traits!(
        #[cfg(feature = "image-format-pnm")]
        fn test_pnm_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Pnm
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-tiff")]
        fn test_tiff_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Tiff
    );

    test_async_traits!(
        #[cfg(feature = "image-format-tiff")]
        fn test_tiff_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Tiff
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-tga")]
        fn test_tga_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Tga
    );

    test_async_traits!(
        #[cfg(feature = "image-format-tga")]
        fn test_tga_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Tga
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-bmp")]
        fn test_bmp_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Bmp
    );

    test_async_traits!(
        #[cfg(feature = "image-format-bmp")]
        fn test_bmp_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Bmp
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-ico")]
        fn test_ico_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Ico
    );

    test_async_traits!(
        #[cfg(feature = "image-format-ico")]
        fn test_ico_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Ico
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-hdr")]
        fn test_hdr_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Hdr
    );

    test_async_traits!(
        #[cfg(feature = "image-format-hdr")]
        fn test_hdr_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Hdr
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-exr")]
        fn test_openexr_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::OpenExr
    );

    test_async_traits!(
        #[cfg(feature = "image-format-exr")]
        fn test_openexr_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::OpenExr
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-ff")]
        fn test_farbfeld_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Farbfeld
    );

    test_async_traits!(
        #[cfg(feature = "image-format-ff")]
        fn test_farbfeld_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Farbfeld
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-avif")]
        fn test_avif_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Avif
    );

    test_async_traits!(
        #[cfg(feature = "image-format-avif")]
        fn test_avif_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Avif
    );

    test_sync_traits!(
        #[cfg(feature = "image-format-qoi")]
        fn test_qoi_sync_traits(),
        crate::vfs::fs_vfs::FsVfs,
        crate::image::Qoi
    );

    test_async_traits!(
        #[cfg(feature = "image-format-qoi")]
        fn test_qoi_async_traits(),
        crate::vfs::tokio_fs_vfs::TokioFsVfs,
        crate::image::Qoi
    );
}
