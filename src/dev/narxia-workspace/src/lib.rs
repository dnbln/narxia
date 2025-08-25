pub extern crate dir_structure;

use std::path::Path;
use std::path::PathBuf;

use dir_structure::DeferredReadOrOwn;
use dir_structure::DirDescendants;
use dir_structure::DirStructure;
use dir_structure::FileFilter;
use dir_structure::FolderFilter;
use dir_structure::Versioned;

pub fn ws_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
}

#[derive(DirStructure, Clone)]
pub struct Workspace<'vfs, Vfs> {
    pub src: SrcDir<'vfs, Vfs>,
    pub doc: DocDir<'vfs, Vfs>,
}

impl<'vfs, Vfs> Workspace<'vfs, Vfs> {
    pub fn compiler_crate(&self, name: &str) -> &Crate<'vfs, Vfs> {
        self.src.compiler.get_value_by_name(name).unwrap()
    }

    pub fn compiler_crate_mut(&mut self, name: &str) -> &mut Crate<'vfs, Vfs> {
        self.src.compiler.get_value_by_name_mut(name).unwrap()
    }
}

#[derive(DirStructure, Clone)]
pub struct SrcDir<'vfs, Vfs> {
    pub compiler: dir_structure::DirChildren<Crate<'vfs, Vfs>>,
    pub lib: dir_structure::DirChildren<Crate<'vfs, Vfs>>,
    pub dev: dir_structure::DirChildren<Crate<'vfs, Vfs>>,
}

#[derive(DirStructure, Clone)]
pub struct Crate<'vfs, Vfs> {
    pub src:
        DeferredReadOrOwn<'vfs, DirDescendants<RustSourceFile<'vfs, Vfs>, RustFileFilter>, Vfs>,
}

#[derive(DirStructure, Clone)]
pub struct RustSourceFile<'vfs, Vfs> {
    #[dir_structure(path = self)]
    pub file: DeferredReadOrOwn<'vfs, dir_structure::Versioned<String>, Vfs>,

    pub self_path: PathBuf,
}

pub struct RustFileFilter;

impl FolderFilter for RustFileFilter {
    fn allows(_folder: &Path) -> bool {
        true
    }
}

impl FileFilter for RustFileFilter {
    fn allows(file: &Path) -> bool {
        file.extension().is_some_and(|ext| ext == "rs")
    }
}

#[derive(DirStructure, Clone)]
pub struct DocDir<'vfs, Vfs> {
    pub docs: DocDirDocsSite<'vfs, Vfs>,
    pub guides: dir_structure::DirChildren<()>,
}

#[derive(DirStructure, Clone)]
pub struct DocDirDocsSite<'vfs, Vfs> {
    pub content: DocContent<'vfs, Vfs>,
}

#[derive(DirStructure, Clone)]
pub struct DocContent<'vfs, Vfs> {
    pub docs: DirDescendants<DocSourceFile<'vfs, Vfs>, MdxFileFilter>,
}

pub struct MdxFileFilter;

impl FileFilter for MdxFileFilter {
    fn allows(file: &Path) -> bool {
        file.extension().is_some_and(|ext| ext == "mdx")
    }
}

impl FolderFilter for MdxFileFilter {
    fn allows(_folder: &Path) -> bool {
        true
    }
}

#[derive(DirStructure, Clone)]
pub struct DocSourceFile<'vfs, Vfs> {
    #[dir_structure(path = self)]
    pub file: DeferredReadOrOwn<'vfs, Versioned<String>, Vfs>,
}

#[macro_export]
macro_rules! resolve_ws_path {
    ($($id:tt)*) => {
        $crate::dir_structure::resolve_path!([$crate::ws_root() as $crate::Workspace<'_, $crate::dir_structure::FsVfs>].$($id)*)
    };
}

#[macro_export]
macro_rules! display_ws_path {
    ($($id:tt)*) => {
        $crate::dir_structure::resolve_path!([::std::path::PathBuf::new() as $crate::Workspace<'_, $crate::dir_structure::FsVfs>].$($id)*).display()
    };
}

pub mod parser_tests {
    use std::path::PathBuf;

    use dir_structure::DeferredReadOrOwn;
    use dir_structure::DirStructure;
    use dir_structure::FileString;

    use crate::ws_root;

    pub fn parser_tests_dir() -> PathBuf {
        ws_root().join("src/compiler/tests/testData/parser-tests")
    }

    pub const INPUT_FILE_NAME: &str = "input.nrx";
    pub const OUTPUT_FILE_NAME: &str = "output.txt";

    #[derive(DirStructure, Clone)]
    pub struct ParserTestSingleFolder<'vfs, Vfs> {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredReadOrOwn<'vfs, FileString, Vfs>,
        #[dir_structure(path = "output.txt")]
        pub output: Option<DeferredReadOrOwn<'vfs, FileString, Vfs>>,
        pub self_path: PathBuf,
    }

    impl<'vfs, Vfs> ParserTestSingleFolder<'vfs, Vfs> {
        pub fn input_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([ParserTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].input)
        }

        pub fn output_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([ParserTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].output)
        }
    }
}

pub mod name_resolution_tests {
    use std::path::PathBuf;

    use dir_structure::DeferredReadOrOwn;
    use dir_structure::DirStructure;
    use dir_structure::FileString;

    use crate::ws_root;

    pub fn name_resolution_tests_dir() -> PathBuf {
        ws_root().join("src/compiler/tests/testData/name-resolution-tests")
    }

    #[derive(DirStructure, Clone)]
    pub struct NameResolutionTestSingleFolder<'vfs, Vfs> {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredReadOrOwn<'vfs, FileString, Vfs>,
        #[dir_structure(path = "output.txt")]
        pub output: Option<DeferredReadOrOwn<'vfs, FileString, Vfs>>,
        pub self_path: PathBuf,
    }

    impl<'vfs, Vfs> NameResolutionTestSingleFolder<'vfs, Vfs> {
        pub fn input_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([NameResolutionTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].input)
        }

        pub fn output_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([NameResolutionTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].output)
        }
    }
}

pub mod ssa_tests {
    use std::path::PathBuf;

    use dir_structure::DeferredReadOrOwn;
    use dir_structure::DirStructure;
    use dir_structure::FileString;

    use crate::ws_root;

    pub fn ssa_tests_dir() -> PathBuf {
        ws_root().join("src/compiler/tests/testData/ssa-tests")
    }

    #[derive(DirStructure, Clone)]
    pub struct SsaTestSingleFolder<'vfs, Vfs> {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredReadOrOwn<'vfs, FileString, Vfs>,
        #[dir_structure(path = "output.nrxssa")]
        pub output: Option<DeferredReadOrOwn<'vfs, FileString, Vfs>>,
        pub self_path: PathBuf,
    }

    impl<'vfs, Vfs> SsaTestSingleFolder<'vfs, Vfs> {
        pub fn input_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([SsaTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].input)
        }

        pub fn output_file_path(&self) -> PathBuf {
            dir_structure::resolve_path!([SsaTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].output)
        }
    }
}
