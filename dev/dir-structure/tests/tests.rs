use std::path::{Path, PathBuf};

use dir_structure::{DirChild, DirChildren, ReadFrom, WriteTo};

fn test_dir(name: &str) -> PathBuf {
    let p = Path::new(env!("CARGO_TARGET_TMPDIR"))
        .join("dir-structure-tests")
        .join(name);

    if p.exists() {
        std::fs::remove_dir_all(&p).unwrap();
    }

    p
}

#[test]
fn write_simple() {
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f1: String,
        #[dir_structure(path = "f2.txt")]
        f2: String,
        f3: String,
    }

    let p = test_dir("write_simple");
    let d = p.join("dir");
    Dir {
        f1: "f1".to_owned(),
        f2: "f2".to_owned(),
        f3: "f3".to_owned(),
    }
    .write_to(&d)
    .unwrap();

    assert_eq!(std::fs::read_to_string(d.join("f1.txt")).unwrap(), "f1");
    assert_eq!(std::fs::read_to_string(d.join("f2.txt")).unwrap(), "f2");
    assert_eq!(std::fs::read_to_string(d.join("f3")).unwrap(), "f3");
}

#[test]
fn write_simple_with_subdir() {
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f1: String,
        #[dir_structure(path = "subdir/f2.txt")]
        f2: String,
        f3: String,
    }

    let p = test_dir("write_simple_with_subdir");
    let d = p.join("dir");
    Dir {
        f1: "f1".to_owned(),
        f2: "f2".to_owned(),
        f3: "f3".to_owned(),
    }
    .write_to(&d)
    .unwrap();

    assert_eq!(std::fs::read_to_string(d.join("f1.txt")).unwrap(), "f1");
    assert_eq!(
        std::fs::read_to_string(d.join("subdir/f2.txt")).unwrap(),
        "f2"
    );
    assert_eq!(std::fs::read_to_string(d.join("f3")).unwrap(), "f3");
}

#[test]
fn write_simple_nested() {
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f1: String,
        subdir: Subdir,
        f3: String,
    }

    #[derive(dir_structure::DirStructure)]
    struct Subdir {
        #[dir_structure(path = "f2.txt")]
        f2: String,
    }

    let p = test_dir("write_simple_nested");
    let d = p.join("dir");
    Dir {
        f1: "f1".to_owned(),
        subdir: Subdir {
            f2: "f2".to_owned(),
        },
        f3: "f3".to_owned(),
    }
    .write_to(&d)
    .unwrap();

    assert_eq!(std::fs::read_to_string(d.join("f1.txt")).unwrap(), "f1");
    assert_eq!(
        std::fs::read_to_string(d.join("subdir/f2.txt")).unwrap(),
        "f2"
    );
    assert_eq!(std::fs::read_to_string(d.join("f3")).unwrap(), "f3");
}

#[test]
fn read_simple() {
    let p = test_dir("read_simple");
    let d = p.join("dir");
    std::fs::create_dir_all(&d).unwrap();
    std::fs::write(d.join("f1.txt"), "f1").unwrap();
    std::fs::write(d.join("f2.txt"), "f2").unwrap();
    std::fs::write(d.join("f3"), "f3").unwrap();
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f1: String,
        #[dir_structure(path = "f2.txt")]
        f2: String,
        f3: String,
    }

    let dir = Dir::read_from(&d).unwrap();
    assert_eq!(dir.f1, "f1");
    assert_eq!(dir.f2, "f2");
    assert_eq!(dir.f3, "f3");
}

#[test]
fn read_simple_with_subdir() {
    let p = test_dir("read_simple_with_subdir");
    let d = p.join("dir");
    std::fs::create_dir_all(&d).unwrap();
    std::fs::write(d.join("f1.txt"), "f1").unwrap();
    std::fs::create_dir_all(d.join("subdir")).unwrap();
    std::fs::write(d.join("subdir/f2.txt"), "f2").unwrap();
    std::fs::write(d.join("f3"), "f3").unwrap();
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f1: String,
        #[dir_structure(path = "subdir/f2.txt")]
        f2: String,
        f3: String,
    }

    let dir = Dir::read_from(&d).unwrap();
    assert_eq!(dir.f1, "f1");
    assert_eq!(dir.f2, "f2");
    assert_eq!(dir.f3, "f3");
}

#[test]
fn read_simple_nested() {
    let p = test_dir("read_simple_nested");
    let d = p.join("dir");
    std::fs::create_dir_all(&d).unwrap();
    std::fs::write(d.join("f1.txt"), "f1").unwrap();
    std::fs::create_dir_all(d.join("subdir")).unwrap();
    std::fs::write(d.join("subdir/f2.txt"), "f2").unwrap();
    std::fs::write(d.join("f3"), "f3").unwrap();
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f1: String,
        subdir: Subdir,
        f3: String,
    }

    #[derive(dir_structure::DirStructure)]
    struct Subdir {
        #[dir_structure(path = "f2.txt")]
        f2: String,
    }

    let dir = Dir::read_from(&d).unwrap();
    assert_eq!(dir.f1, "f1");
    assert_eq!(dir.subdir.f2, "f2");
    assert_eq!(dir.f3, "f3");
}

#[test]
fn read_numbers() {
    let p = test_dir("read_numbers");
    let d = p.join("dir");
    std::fs::create_dir_all(&d).unwrap();
    std::fs::write(d.join("f1.txt"), "1").unwrap();
    std::fs::write(d.join("f2.txt"), "2").unwrap();
    std::fs::write(d.join("f3"), "3").unwrap();
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        #[dir_structure(path = "f1.txt", with_newtype = dir_structure::FmtWrapper<u32>)]
        f1: u32,
        #[dir_structure(path = "f2.txt", with_newtype = dir_structure::FmtWrapper<u32>)]
        f2: u32,
        #[dir_structure(with_newtype = dir_structure::FmtWrapper<u32>)]
        f3: u32,
    }

    let dir = Dir::read_from(&d).unwrap();
    assert_eq!(dir.f1, 1);
    assert_eq!(dir.f2, 2);
    assert_eq!(dir.f3, 3);
}

#[test]
fn write_numbers() {
    let p = test_dir("write_numbers");
    let d = p.join("dir");
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        #[dir_structure(path = "f1.txt", with_newtype = dir_structure::FmtWrapper<u32>)]
        f1: u32,
        #[dir_structure(path = "f2.txt", with_newtype = dir_structure::FmtWrapper<u32>)]
        f2: u32,
        #[dir_structure(with_newtype = dir_structure::FmtWrapper<u32>)]
        f3: u32,
    }

    Dir {
        f1: 1,
        f2: 2,
        f3: 3,
    }
    .write_to(&d)
    .unwrap();

    assert_eq!(std::fs::read_to_string(d.join("f1.txt")).unwrap(), "1");
    assert_eq!(std::fs::read_to_string(d.join("f2.txt")).unwrap(), "2");
    assert_eq!(std::fs::read_to_string(d.join("f3")).unwrap(), "3");
}

#[test]
fn deferred_read() {
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        #[dir_structure(path = "f1.txt")]
        f: dir_structure::DeferredRead<String>,
    }

    let p = test_dir("deferred_read");
    let d = p.join("dir");
    std::fs::create_dir_all(&d).unwrap();
    let r = Dir::read_from(&d);
    assert!(r.is_ok());
    let dir = r.unwrap();
    assert!(dir.f.perform_read().is_err());
    std::fs::write(d.join("f1.txt"), "f1").unwrap();
    assert_eq!(dir.f.perform_read().unwrap(), "f1");
}

#[test]
fn read_all_directory_files() {
    #[derive(dir_structure::DirStructure)]
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
    let dir = Dir::read_from(&d).unwrap();
    assert_eq!(dir.subdir.len(), 3);
    assert_eq!(dir.subdir.get_name("f1.txt").unwrap().value(), "f1");
    assert_eq!(dir.subdir.get_name("f2.txt").unwrap().value(), "f2");
    assert_eq!(dir.subdir.get_name("f3").unwrap().value(), "f3");
}

#[test]
fn write_subdirectory_children() {
    #[derive(dir_structure::DirStructure)]
    struct Dir {
        subdir: DirChildren<String>,
    }

    let p = test_dir("write_subdirectory_children");
    let d = p.join("dir");
    let subdir = d.join("subdir");
    Dir {
        subdir: DirChildren::with_children_from_iter(
            subdir.clone(),
            [
                DirChild::new("f1.txt", "f1".to_owned()),
                DirChild::new("f2.txt", "f2".to_owned()),
                DirChild::new("f3", "f3".to_owned()),
            ],
        ),
    }
    .write_to(&d)
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
        assert_eq!(content, expected_content, "Unexpected content of {name_str}");
        len += 1;
    }

    assert_eq!(len, 3, "Subdirectory should have 3 files");
}
