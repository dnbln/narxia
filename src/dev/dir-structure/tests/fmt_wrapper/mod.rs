use dir_structure::fmt_wrapper::FmtWrapper;

use super::*;

read_test!(
    read_numbers,
    {
        #[derive(dir_structure::DirStructure, Debug, PartialEq, assert_eq::AssertEq)]
        // #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Dir {
            #[dir_structure(path = "f1.txt", with_newtype = FmtWrapper<u32>)]
            f1: u32,
            #[dir_structure(path = "f2.txt", with_newtype = FmtWrapper<u32>)]
            f2: u32,
            #[dir_structure(with_newtype = FmtWrapper<u32>)]
            f3: u32,
        }
    },
    ("f1.txt" => "1"),
    ("f2.txt" => "2"),
    ("f3" => "3");
    t: Dir, Dir {
        f1: 1,
        f2: 2,
        f3: 3,
    }
);

write_test!(
    write_numbers,
    {
        #[derive(dir_structure::DirStructure, assert_eq::AssertEq)]
        // #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Dir {
            #[dir_structure(path = "f1.txt", with_newtype = FmtWrapper<u32>)]
            f1: u32,
            #[dir_structure(path = "f2.txt", with_newtype = FmtWrapper<u32>)]
            f2: u32,
            #[dir_structure(with_newtype = FmtWrapper<u32>)]
            f3: u32,
        }
    },
    ("f1.txt" => b"1"),
    ("f2.txt" => b"2"),
    ("f3" => b"3");
    t: Dir, Dir {
        f1: 1,
        f2: 2,
        f3: 3,
    }
);

write_test!(
    write_numbers_newtyped,
    {
        #[derive(dir_structure::DirStructure, assert_eq::AssertEq)]
        // #[cfg_attr(feature = "async", derive(dir_structure::DirStructureAsync))]
        struct Dir {
            #[dir_structure(path = "f1.txt")]
            f1: FmtWrapper<u32>,
            #[dir_structure(path = "f2.txt")]
            f2: FmtWrapper<u32>,
            f3: FmtWrapper<u32>,
        }
    },
    ("f1.txt" => b"1"),
    ("f2.txt" => b"2"),
    ("f3" => b"3");
    t: Dir, Dir {
        f1: FmtWrapper(1),
        f2: FmtWrapper(2),
        f3: FmtWrapper(3),
    }
);
