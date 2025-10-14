use super::*;

write_test!(
    write_simple,
    {
        #[derive(dir_structure::DirStructure, assert_eq::AssertEq)]
        #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Dir {
            #[dir_structure(path = "f1.txt")]
            f1: String,
            #[dir_structure(path = "f2.txt")]
            f2: String,
            f3: String,
        }
    },
    ("f1.txt" => b"f1"),
    ("f2.txt" => b"f2"),
    ("f3" => b"f3");
    t: Dir, Dir {
        f1: "f1".to_owned(),
        f2: "f2".to_owned(),
        f3: "f3".to_owned(),
    }
);

write_test!(
    write_simple_with_subdir,
    {
        #[derive(dir_structure::DirStructure, assert_eq::AssertEq)]
        #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Dir {
            #[dir_structure(path = "f1.txt")]
            f1: String,
            #[dir_structure(path = "subdir/f2.txt")]
            f2: String,
            f3: String,
        }
    },
    ("f1.txt" => b"f1"),
    ("subdir/f2.txt" => b"f2"),
    ("f3" => b"f3");
    t: Dir, Dir {
        f1: "f1".to_owned(),
        f2: "f2".to_owned(),
        f3: "f3".to_owned(),
    }
);

write_test!(
    write_simple_nested,
    {
        #[derive(dir_structure::DirStructure, assert_eq::AssertEq)]
        #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Dir {
            #[dir_structure(path = "f1.txt")]
            f1: String,
            subdir: Subdir,
            f3: String,
        }

        #[derive(dir_structure::DirStructure, assert_eq::AssertEq)]
        #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Subdir {
            #[dir_structure(path = "f2.txt")]
            f2: String,
        }
    },
    ("f1.txt" => b"f1"),
    ("subdir/f2.txt" => b"f2"),
    ("f3" => b"f3");
    t: Dir, Dir {
        f1: "f1".to_owned(),
        subdir: Subdir {
            f2: "f2".to_owned(),
        },
        f3: "f3".to_owned(),
    }
);

read_test!(
    read_simple,
    {
        #[derive(dir_structure::DirStructure, Debug, PartialEq, assert_eq::AssertEq)]
        #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Dir {
            #[dir_structure(path = "f1.txt")]
            f1: String,
            #[dir_structure(path = "f2.txt")]
            f2: String,
            f3: String,
        }
    },
    ("f1.txt" => "f1"),
    ("f2.txt" => "f2"),
    ("f3" => "f3");
    t: Dir, Dir {
        f1: "f1".to_owned(),
        f2: "f2".to_owned(),
        f3: "f3".to_owned(),
    }
);

read_test!(
    read_simple_with_subdir,
    {
        #[derive(dir_structure::DirStructure, Debug, PartialEq, assert_eq::AssertEq)]
        #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Dir {
            #[dir_structure(path = "f1.txt")]
            f1: String,
            #[dir_structure(path = "subdir/f2.txt")]
            f2: String,
            f3: String,
        }
    },
    ("f1.txt" => "f1"),
    ("subdir/f2.txt" => "f2"),
    ("f3" => "f3");
    t: Dir, Dir {
        f1: "f1".to_owned(),
        f2: "f2".to_owned(),
        f3: "f3".to_owned(),
    }
);

read_test!(
    read_simple_nested,
    {
        #[derive(dir_structure::DirStructure, Debug, PartialEq, assert_eq::AssertEq)]
        #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Dir {
            #[dir_structure(path = "f1.txt")]
            f1: String,
            subdir: Subdir,
            f3: String,
        }

        #[derive(dir_structure::DirStructure, Debug, PartialEq, assert_eq::AssertEq)]
        #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Subdir {
            #[dir_structure(path = "f2.txt")]
            f2: String,
        }
    },
    ("f1.txt" => "f1"),
    ("subdir/f2.txt" => "f2"),
    ("f3" => "f3");
    t: Dir, Dir {
        f1: "f1".to_owned(),
        subdir: Subdir {
            f2: "f2".to_owned(),
        },
        f3: "f3".to_owned(),
    }
);
