//! Tokio file system virtual file system implementation.

use std::fs as std_fs;
use std::io;
use std::io::SeekFrom;
use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use futures::AsyncBufRead as FuturesBufRead;
use futures::Stream;
use futures::io::AsyncRead as FuturesAsyncRead;
use futures::io::AsyncSeek as FuturesAsyncSeek;
use futures::io::AsyncWrite as FuturesAsyncWrite;
use pin_project::pin_project;
use tokio::fs;
use tokio::io::AsyncBufRead;
use tokio::io::AsyncRead;
use tokio::io::AsyncSeek;
use tokio::io::AsyncWrite;
use tokio::io::BufReader;
use tokio::io::BufWriter;
use tokio::io::ReadBuf;

use crate::traits::async_vfs::CreateParentDirDefaultFuture;
use crate::traits::async_vfs::IoErrorWrapperFuture;
use crate::traits::async_vfs::VfsAsync;
use crate::traits::async_vfs::WriteSupportingVfsAsync;
use crate::traits::vfs::PathType;
use crate::traits::vfs::VfsCore;

/// A [`VfsAsync`] and [`WriteSupportingVfsAsync`] implementation using [`tokio::fs`].
pub struct TokioFsVfs;

/// Adapter to convert a type implementing [`tokio::io`] traits to one implementing
/// [`futures::io`] traits.
#[pin_project]
pub struct TokioAsyncAdapter<T>(#[pin] T, Option<SeekFrom>);

impl<R: AsyncRead> FuturesAsyncRead for TokioAsyncAdapter<R> {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut [u8],
    ) -> Poll<io::Result<usize>> {
        let mut rbuf = ReadBuf::new(buf);
        let this = self.project();
        *this.1 = None;
        match this.0.poll_read(cx, &mut rbuf) {
            Poll::Ready(Ok(())) => Poll::Ready(Ok(rbuf.filled().len())),
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
            Poll::Pending => Poll::Pending,
        }
    }
}

impl<R: AsyncBufRead> FuturesBufRead for TokioAsyncAdapter<R> {
    fn poll_fill_buf(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<&[u8]>> {
        let this = self.project();
        *this.1 = None;
        this.0.poll_fill_buf(cx)
    }

    fn consume(self: Pin<&mut Self>, amt: usize) {
        let this = self.project();
        *this.1 = None;
        this.0.consume(amt)
    }
}

impl<T: AsyncSeek> FuturesAsyncSeek for TokioAsyncAdapter<T> {
    fn poll_seek(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        pos: io::SeekFrom,
    ) -> Poll<io::Result<u64>> {
        let mut this = self.project();
        if *this.1 != Some(pos) {
            *this.1 = Some(pos);
            match this.0.as_mut().start_seek(pos) {
                Ok(()) => {}
                Err(e) => {
                    *this.1 = None;
                    return Poll::Ready(Err(e));
                }
            }
        }

        match this.0.poll_complete(cx) {
            Poll::Ready(Ok(v)) => {
                *this.1 = None;
                Poll::Ready(Ok(v))
            }
            Poll::Ready(Err(e)) => {
                *this.1 = None;
                Poll::Ready(Err(e))
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

impl<T: AsyncWrite> FuturesAsyncWrite for TokioAsyncAdapter<T> {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        let this = self.project();
        *this.1 = None;
        this.0.poll_write(cx, buf)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        let this = self.project();
        *this.1 = None;
        this.0.poll_flush(cx)
    }

    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        let this = self.project();
        *this.1 = None;
        this.0.poll_shutdown(cx)
    }
}

impl VfsCore for TokioFsVfs {
    type Path = Path;
}

impl VfsAsync for TokioFsVfs {
    type RFile = TokioAsyncAdapter<BufReader<fs::File>>;
    type OpenReadFuture = IoErrorWrapperFuture<
        Self::RFile,
        Pin<Box<dyn Future<Output = io::Result<Self::RFile>> + Send>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >;

    fn open_read(self: Pin<&Self>, path: PathBuf) -> Self::OpenReadFuture {
        IoErrorWrapperFuture::new(
            path.clone(),
            Box::pin(async move {
                fs::File::open(path)
                    .await
                    .map(|f| TokioAsyncAdapter(BufReader::new(f), None))
            }),
        )
    }

    type ReadFuture<'a>
        = IoErrorWrapperFuture<
        Vec<u8>,
        Pin<Box<dyn Future<Output = io::Result<Vec<u8>>> + Send + 'a>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >
    where
        Self: 'a;

    fn read<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::ReadFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::read(path)))
    }

    type ReadStringFuture<'a>
        = IoErrorWrapperFuture<
        String,
        Pin<Box<dyn Future<Output = io::Result<String>> + Send + 'a>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >
    where
        Self: 'a;

    fn read_string<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::ReadStringFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::read_to_string(path)))
    }

    type ExistsFuture<'a>
        = IoErrorWrapperFuture<
        bool,
        Pin<Box<dyn Future<Output = io::Result<bool>> + Send + 'a>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >
    where
        Self: 'a;

    fn exists<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::ExistsFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::try_exists(path)))
    }

    type IsDirFuture<'a>
        = IoErrorWrapperFuture<
        bool,
        Pin<Box<dyn Future<Output = io::Result<bool>> + Send + 'a>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >
    where
        Self: 'a;

    fn is_dir<'a>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::IsDirFuture<'a> {
        IoErrorWrapperFuture::new(
            path.clone(),
            Box::pin(async move { fs::metadata(path).await.map(|m| m.is_dir()) }),
        )
    }

    type DirWalk<'a>
        = imp::DirWalker
    where
        Self: 'a;

    type DirWalkFuture<'a>
        = IoErrorWrapperFuture<
        Self::DirWalk<'a>,
        Pin<Box<dyn Future<Output = io::Result<Self::DirWalk<'a>>> + Send + 'a>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >
    where
        Self: 'a;

    fn walk_dir<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::DirWalkFuture<'a> {
        IoErrorWrapperFuture::new(
            path.clone(),
            Box::pin(async move {
                fs::read_dir(path.clone())
                    .await
                    .map(|inner| imp::DirWalker {
                        inner,
                        path,
                        current_kind_future: None,
                    })
            }),
        )
    }
}

impl WriteSupportingVfsAsync for TokioFsVfs {
    type WFile = TokioAsyncAdapter<BufWriter<fs::File>>;
    type OpenWriteFuture = IoErrorWrapperFuture<
        Self::WFile,
        Pin<Box<dyn Future<Output = io::Result<Self::WFile>> + Send>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >;
    fn open_write(
        self: Pin<&Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
    ) -> Self::OpenWriteFuture {
        IoErrorWrapperFuture::new(
            path.clone(),
            Box::pin(async move {
                fs::File::create(path)
                    .await
                    .map(|f| TokioAsyncAdapter(BufWriter::new(f), None))
            }),
        )
    }

    type WriteFuture<'a>
        = IoErrorWrapperFuture<
        (),
        Pin<Box<dyn Future<Output = io::Result<()>> + Send + 'a>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >
    where
        Self: 'a;

    fn write<'d, 'a: 'd>(
        self: Pin<&'a Self>,
        path: <<Self as VfsCore>::Path as PathType>::OwnedPath,
        data: &'d [u8],
    ) -> Self::WriteFuture<'d> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::write(path, data)))
    }

    type RemoveDirAllFuture<'a>
        = IoErrorWrapperFuture<
        (),
        Pin<Box<dyn Future<Output = io::Result<()>> + Send + 'a>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >
    where
        Self: 'a;

    fn remove_dir_all<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::RemoveDirAllFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::remove_dir_all(path)))
    }

    type CreateDirFuture<'a>
        = IoErrorWrapperFuture<
        (),
        Pin<Box<dyn Future<Output = io::Result<()>> + Send + 'a>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >
    where
        Self: 'a;

    fn create_dir<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::CreateDirFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::create_dir(path)))
    }

    type CreateDirAllFuture<'a>
        = IoErrorWrapperFuture<
        (),
        Pin<Box<dyn Future<Output = io::Result<()>> + Send + 'a>>,
        <<Self as VfsCore>::Path as PathType>::OwnedPath,
    >
    where
        Self: 'a;

    fn create_dir_all<'a>(self: Pin<&'a Self>, path: PathBuf) -> Self::CreateDirAllFuture<'a> {
        IoErrorWrapperFuture::new(path.clone(), Box::pin(fs::create_dir_all(path)))
    }

    type CreateParentDirFuture<'a>
        = CreateParentDirDefaultFuture<'a, Self>
    where
        Self: 'a;

    fn create_parent_dir<'a>(
        self: Pin<&'a Self>,
        path: PathBuf,
    ) -> Self::CreateParentDirFuture<'a> {
        let parent = path
            .parent()
            .map_or_else(|| path.join(".."), |p| p.to_path_buf());
        CreateParentDirDefaultFuture::Start {
            vfs: self,
            path: parent,
        }
    }
}

mod imp {
    use std::ffi::OsString;
    use std::path::Path;
    use std::task::Context;

    use futures::FutureExt;
    #[cfg(feature = "image")]
    use tokio::task;

    use super::*;
    #[cfg(feature = "image")]
    use crate::error::Error;
    use crate::error::Result;
    #[cfg(feature = "image")]
    use crate::image::ImgFormat;
    #[cfg(feature = "image")]
    use crate::prelude::*;
    use crate::traits::vfs::DirEntryInfo;
    use crate::traits::vfs::DirEntryKind;
    #[cfg(feature = "image")]
    use crate::traits::vfs::WriteSupportingVfs as _;
    #[cfg(feature = "image")]
    use crate::vfs::fs_vfs::FsVfs;

    /// Directory walker for asynchronous file system operations on [`tokio::fs`].
    pub struct DirWalker {
        pub(super) inner: fs::ReadDir,
        pub(super) path: PathBuf,

        pub(super) current_kind_future: Option<(
            OsString,
            PathBuf,
            Pin<Box<dyn Future<Output = io::Result<std_fs::FileType>> + Send>>,
        )>,
    }

    impl Stream for DirWalker {
        type Item = Result<DirEntryInfo<Path>, PathBuf>;

        fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
            if let Some((name, path, fut)) = self.current_kind_future.as_mut() {
                match fut.poll_unpin(cx) {
                    Poll::Ready(Ok(kind)) => {
                        let name = name.clone();
                        let path = path.clone();
                        self.current_kind_future = None;
                        let kind = if kind.is_dir() {
                            DirEntryKind::Directory
                        } else {
                            DirEntryKind::File
                        };
                        return Poll::Ready(Some(Ok(DirEntryInfo { name, path, kind })));
                    }
                    Poll::Ready(Err(e)) => {
                        let path = self.path.clone();
                        self.current_kind_future = None;
                        return Poll::Ready(Some(Err(Error::Io(path.clone(), e))));
                    }
                    Poll::Pending => return Poll::Pending,
                }
            }
            match self.inner.poll_next_entry(cx) {
                Poll::Ready(Ok(Some(v))) => {
                    let name = v.file_name();
                    let path = v.path();
                    let fut = Box::pin(async move { v.file_type().await });
                    self.current_kind_future = Some((name, path, fut));
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                Poll::Ready(Ok(None)) => Poll::Ready(None),
                Poll::Ready(Err(e)) => Poll::Ready(Some(Err(Error::Io(self.path.clone(), e)))),
                Poll::Pending => Poll::Pending,
            }
        }
    }

    #[cfg(feature = "image")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image")))]
    impl<'vfs, T: ImgFormat> ReadFromAsync<'vfs, TokioFsVfs> for T
    where
        T: Send + 'static,
    {
        type Future = Pin<Box<dyn Future<Output = Result<Self, PathBuf>> + Send + 'vfs>>;

        fn read_from_async(
            path: PathBuf,
            #[expect(unused, reason = "Uses FsVfs under the hood")] vfs: Pin<&'vfs TokioFsVfs>,
        ) -> Self::Future {
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
                    Ok(T::from_image(img))
                })
                .await
                {
                    Ok(res) => res,
                    Err(e) => Err(Error::Parse(path, Box::new(e))),
                }
            })
        }
    }

    #[cfg(feature = "image")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image")))]
    impl<'vfs> WriteToAsync<'vfs, TokioFsVfs> for (image::DynamicImage, image::ImageFormat) {
        type Future = Pin<Box<dyn Future<Output = Result<(), PathBuf>> + Send + 'vfs>>;

        fn write_to_async(self, path: PathBuf, vfs: Pin<&'vfs TokioFsVfs>) -> Self::Future {
            Box::pin(async move {
                use crate::traits::async_vfs::WriteSupportingVfsAsync;

                vfs.create_parent_dir(path.clone()).await?;

                let p_clone = path.clone();

                let std_vfs = Pin::new(&FsVfs);
                match task::spawn_blocking(move || {
                    let (image, format) = self;
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

    #[cfg(feature = "image")]
    #[cfg_attr(docsrs, doc(cfg(feature = "image")))]
    /// Warning! This implementation clones `self` and calls `write_to_async`.
    impl<'vfs> WriteToAsync<'vfs, TokioFsVfs> for (&'vfs image::DynamicImage, image::ImageFormat)
    where
        'vfs: 'vfs,
    {
        type Future = Pin<Box<dyn Future<Output = Result<(), PathBuf>> + Send + 'vfs>>;

        fn write_to_async(self, path: PathBuf, vfs: Pin<&'vfs TokioFsVfs>) -> Self::Future {
            let img = self.0.clone();
            (img, self.1).write_to_async(path, vfs)
        }
    }
}
