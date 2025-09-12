use std::path::Path;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use dir_structure::clean_dir::CleanDir;
use dir_structure::dir_children::DirChild;
use dir_structure::dir_children::DirChildren;
use dir_structure::prelude::*;
use dir_structure::traits::async_vfs::VfsAsync;
use dir_structure::traits::async_vfs::WriteSupportingVfsAsync;
use dir_structure::versioned::Versioned;
use dir_structure::versioned::VersionedString;
use dir_structure::vfs::tokio_fs_vfs::TokioFsVfs;

fn test_dir(name: &str) -> PathBuf {
    let p = Path::new(env!("CARGO_TARGET_TMPDIR"))
        .join("dir-structure-tests")
        .join(name);

    if p.exists() {
        std::fs::remove_dir_all(&p).unwrap();
    }
    std::fs::create_dir_all(&p).unwrap();

    p
}

macro_rules! read_test {
    ($name:ident, {$($it:item)*}, $(($setup_path:expr => $setup_expr:expr)),*; t: $read_ty:ty, $end_expr:expr) => {
        #[tokio::test]
        async fn $name() {
            $($it)*

            let p = test_dir(stringify!($name));
            let d = p.join("dir");
            std::fs::create_dir_all(d.clone()).unwrap();
            $(
                let setup_path = d.join($setup_path);
                if let Some(parent) = setup_path.parent() {
                    std::fs::create_dir_all(parent).unwrap();
                }
                std::fs::write(setup_path, $setup_expr).unwrap();
            )*
            let dir = TokioFsVfs.read_typed_async::<$read_ty>(d.clone()).await.unwrap();
            assert_eq!(dir, $end_expr);
        }
    };
}

macro_rules! write_test {
    ($name:ident, {$($it:item)*}, $(($check_path:expr => $check_expr:expr)),*; t: $write_ty:ty, $start_expr:expr) => {
        #[tokio::test]
        async fn $name() {
            $($it)*

            let p = test_dir(stringify!($name));
            let d = p.join("dir");
            TokioFsVfs.write_typed_async_ref(d.clone(), &$start_expr).await.unwrap();
            $(
                let check_path = d.join($check_path);
                let content = std::fs::read(check_path).unwrap();
                assert_eq!(content, $check_expr);
            )*
        }
    };
}

mod fmt_wrapper;
mod simple;

#[tokio::test]
async fn deferred_read() {
    #[derive(dir_structure::DirStructureAsync)]
    struct Dir<'vfs, Vfs> {
        #[dir_structure(path = "f1.txt")]
        f: dir_structure::deferred_read::DeferredRead<'vfs, String, Vfs>,
    }

    let p = test_dir("deferred_read");
    let d = p.join("dir");
    std::fs::create_dir_all(d.clone()).unwrap();
    let r = TokioFsVfs.read_typed_async::<Dir<_>>(d.clone()).await;
    assert!(r.is_ok());
    let dir = r.unwrap();
    assert!(dir.f.perform_read_async().await.is_err());
    std::fs::write(d.join("f1.txt"), "f1").unwrap();
    assert_eq!(dir.f.perform_read_async().await.unwrap(), "f1");
}

#[tokio::test]
async fn read_all_directory_files() {
    #[derive(dir_structure::DirStructureAsync)]
    struct Dir {
        subdir: DirChildren<String>,
    }

    let p = test_dir("read_all_directory_files");
    let d = p.join("dir");
    let subdir = d.join("subdir");
    std::fs::create_dir_all(&subdir).unwrap();
    std::fs::write(subdir.join("f1.txt"), "f1").unwrap();
    std::fs::write(subdir.join("f2.txt"), "f2").unwrap();
    std::fs::write(subdir.join("f3"), "f3").unwrap();
    let dir = TokioFsVfs.read_typed_async::<Dir>(d.clone()).await.unwrap();
    assert_eq!(dir.subdir.len(), 3);
    assert_eq!(dir.subdir.get_name("f1.txt").unwrap().value(), "f1");
    assert_eq!(dir.subdir.get_name("f2.txt").unwrap().value(), "f2");
    assert_eq!(dir.subdir.get_name("f3").unwrap().value(), "f3");
}

#[tokio::test]
async fn write_subdirectory_children() {
    #[derive(dir_structure::DirStructureAsync)]
    struct Dir {
        subdir: DirChildren<String>,
    }

    let p = test_dir("write_subdirectory_children");
    let d = p.join("dir");
    let subdir = d.join("subdir");
    TokioFsVfs
        .write_typed_async_ref(
            d.clone(),
            &Dir {
                subdir: DirChildren::with_children_from_iter(
                    subdir.clone(),
                    [
                        DirChild::new("f1.txt", "f1".to_owned()),
                        DirChild::new("f2.txt", "f2".to_owned()),
                        DirChild::new("f3", "f3".to_owned()),
                    ],
                ),
            },
        )
        .await
        .unwrap();
    let mut len = 0;
    for file in subdir.read_dir().unwrap() {
        let file = file.unwrap();
        let name = file.file_name();
        let name_str = name.to_str().unwrap();
        let content = std::fs::read_to_string(file.path()).unwrap();
        let expected_content = match name_str {
            "f1.txt" => "f1",
            "f2.txt" => "f2",
            "f3" => "f3",
            name => panic!("Unexpected file {}", name),
        };
        assert_eq!(
            content, expected_content,
            "Unexpected content of {name_str}"
        );
        len += 1;
    }

    assert_eq!(len, 3, "Subdirectory should have 3 files");
}

#[tokio::test]
async fn parse_dirs_inner_with_self_path() {
    #[derive(dir_structure::DirStructureAsync)]
    struct Dir {
        #[dir_structure(path = self)]
        subdirs: DirChildren<InnerDir>,
    }

    #[derive(dir_structure::DirStructureAsync)]
    struct InnerDir {
        #[dir_structure(path = "f.txt")]
        f: String,
    }

    let p = test_dir("parse_dirs_inner_with_self_path");
    let d = p.join("dir");
    let subdir = d.join("subdir");
    std::fs::create_dir_all(&subdir).unwrap();
    std::fs::write(subdir.join("f.txt"), "f").unwrap();
    let dir = TokioFsVfs.read_typed_async::<Dir>(d.clone()).await.unwrap();
    assert_eq!(dir.subdirs.len(), 1);
    assert_eq!(dir.subdirs.get_name("subdir").unwrap().value().f, "f");
}

#[tokio::test]
async fn clean_dir_writer() {
    #[derive(dir_structure::DirStructureAsync)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f1: String,
        #[dir_structure(path = "f2.txt")]
        f2: String,
        f3: String,
    }

    let p = test_dir("clean_dir_writer");
    let d = p.join("dir");
    TokioFsVfs
        .write_typed_async_ref(
            d.clone(),
            &CleanDir(Dir {
                f1: "f1".to_owned(),
                f2: "f2".to_owned(),
                f3: "f3".to_owned(),
            }),
        )
        .await
        .unwrap();

    assert_eq!(std::fs::read_to_string(d.join("f1.txt")).unwrap(), "f1");
    assert_eq!(std::fs::read_to_string(d.join("f2.txt")).unwrap(), "f2");
    assert_eq!(std::fs::read_to_string(d.join("f3")).unwrap(), "f3");
    std::fs::write(d.join("f4"), "f4").unwrap();

    TokioFsVfs
        .write_typed_async_ref(
            d.clone(),
            &CleanDir(Dir {
                f1: "f1".to_owned(),
                f2: "f2".to_owned(),
                f3: "f3".to_owned(),
            }),
        )
        .await
        .unwrap();

    assert_eq!(std::fs::read_to_string(d.join("f1.txt")).unwrap(), "f1");
    assert_eq!(std::fs::read_to_string(d.join("f2.txt")).unwrap(), "f2");
    assert_eq!(std::fs::read_to_string(d.join("f3")).unwrap(), "f3");
    assert!(!d.join("f4").exists());
}

// #[tokio::test]
// async fn clean_dir_writer_newtype() {
//     #[derive(dir_structure::DirStructureAsync)]
//     struct Dir {
//         #[dir_structure(with_newtype = CleanDir<Subdir>)]
//         subdir: Subdir,
//     }

//     #[derive(dir_structure::DirStructureAsync)]
//     struct Subdir {
//         #[dir_structure(path = "f1.txt")]
//         f1: String,
//         #[dir_structure(path = "f2.txt")]
//         f2: String,
//         f3: String,
//     }

//     let p = test_dir("clean_dir_writer_newtype");
//     let d = p.join("dir");
//     TokioFsVfs
//         .write_typed_async_ref(
//             d.clone(),
//             &Dir {
//                 subdir: Subdir {
//                     f1: "f1".to_owned(),
//                     f2: "f2".to_owned(),
//                     f3: "f3".to_owned(),
//                 },
//             },
//         )
//         .await
//         .unwrap();

//     assert_eq!(
//         std::fs::read_to_string(d.join("subdir/f1.txt")).unwrap(),
//         "f1"
//     );
//     assert_eq!(
//         std::fs::read_to_string(d.join("subdir/f2.txt")).unwrap(),
//         "f2"
//     );
//     assert_eq!(std::fs::read_to_string(d.join("subdir/f3")).unwrap(), "f3");
//     std::fs::write(d.join("subdir/f4"), "f4").unwrap();

//     let dir = CleanDir(Dir {
//         subdir: Subdir {
//             f1: "f1".to_owned(),
//             f2: "f2".to_owned(),
//             f3: "f3".to_owned(),
//         },
//     });
//     dir.write_to_async_ref(d.clone(), Pin::new(&TokioFsVfs))
//         .await
//         .unwrap();

//     assert_eq!(
//         std::fs::read_to_string(d.join("subdir/f1.txt")).unwrap(),
//         "f1"
//     );
//     assert_eq!(
//         std::fs::read_to_string(d.join("subdir/f2.txt")).unwrap(),
//         "f2"
//     );
//     assert_eq!(std::fs::read_to_string(d.join("subdir/f3")).unwrap(), "f3");
//     assert!(!d.join("subdir/f4").exists());
// }

#[tokio::test]
async fn versioned_works() {
    #[derive(dir_structure::DirStructureAsync)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f1: VersionedString,
    }

    let p = test_dir("versioned_works");

    let d = p.join("dir");
    std::fs::create_dir_all(d.clone()).unwrap();
    std::fs::write(d.join("f1.txt"), "f1").unwrap();

    let dir = TokioFsVfs.read_typed_async::<Dir>(d.clone()).await.unwrap();
    assert_eq!(*dir.f1, "f1");

    TokioFsVfs
        .write_typed_async_ref(d.clone(), &dir)
        .await
        .unwrap();

    let mut dir = TokioFsVfs.read_typed_async::<Dir>(d.clone()).await.unwrap();

    assert_eq!(*dir.f1, "f1");

    *dir.f1 = "f2".to_owned();

    TokioFsVfs
        .write_typed_async_ref(d.clone(), &dir)
        .await
        .unwrap();

    assert_eq!(std::fs::read_to_string(d.join("f1.txt")).unwrap(), "f2");
}

#[tokio::test]
async fn versioned_doesnt_call_write_if_not_changed() {
    struct WriteCounter<T> {
        count: AtomicUsize,
        inner: T,
    }

    impl<'vfs, T, Vfs: VfsAsync + 'vfs> ReadFromAsync<'vfs, Vfs> for WriteCounter<T>
    where
        T: ReadFromAsync<'vfs, Vfs> + Send + Sync + 'static,
    {
        type Future =
            Pin<Box<dyn Future<Output = dir_structure::error::Result<Self>> + Send + 'vfs>>;

        fn read_from_async(path: PathBuf, vfs: Pin<&'vfs Vfs>) -> Self::Future {
            Box::pin(async move {
                Ok(Self {
                    count: AtomicUsize::new(0),
                    inner: vfs.read_typed_async_pinned::<T>(path).await?,
                })
            })
        }
    }

    impl<'r, T, Vfs: WriteSupportingVfsAsync + 'static> WriteToAsyncRef<'r, Vfs> for WriteCounter<T>
    where
        T: WriteToAsyncRef<'r, Vfs> + Send + Sync + 'static,
    {
        type Future<'a>
            = Pin<Box<dyn Future<Output = dir_structure::error::Result<()>> + Send + 'a>>
        where
            Self: 'a,
            'r: 'a,
            Vfs: 'a;

        fn write_to_async_ref<'a>(&'a self, path: PathBuf, vfs: Pin<&'a Vfs>) -> Self::Future<'a>
        where
            'r: 'a,
        {
            self.count.fetch_add(1, Ordering::SeqCst);
            Box::pin(self.inner.write_to_async_ref(path, vfs))
        }
    }

    #[derive(dir_structure::DirStructureAsync)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f1: Versioned<WriteCounter<String>>,
    }

    let p = test_dir("versioned_doesnt_call_write_if_not_changed");
    let d = p.join("dir");

    let dir = Dir {
        f1: Versioned::new_dirty(
            WriteCounter {
                count: AtomicUsize::new(0),
                inner: "f1".to_owned(),
            },
            d.join("f1.txt"),
        ),
    };

    TokioFsVfs
        .write_typed_async_ref(d.clone(), &dir)
        .await
        .unwrap();

    let mut dir = TokioFsVfs.read_typed_async::<Dir>(d.clone()).await.unwrap();

    assert_eq!(dir.f1.count.load(Ordering::SeqCst), 0);

    TokioFsVfs
        .write_typed_async_ref(d.clone(), &dir)
        .await
        .unwrap();

    assert_eq!(dir.f1.count.load(Ordering::SeqCst), 0);

    dir.f1.inner = "f2".to_owned();

    TokioFsVfs
        .write_typed_async_ref(d.clone(), &dir)
        .await
        .unwrap();

    assert_eq!(std::fs::read_to_string(d.join("f1.txt")).unwrap(), "f2");
    assert_eq!(dir.f1.count.load(Ordering::SeqCst), 1);
}
