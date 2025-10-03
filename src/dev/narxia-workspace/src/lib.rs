pub extern crate dir_structure;

use std::path::Path;
use std::path::PathBuf;

use dir_structure::DirStructure;
use dir_structure::HasField;
use dir_structure::NoFilter;
use dir_structure::deferred_read_or_own::DeferredReadOrOwn;
use dir_structure::dir_children::DirChildren;
use dir_structure::dir_descendants::DirDescendants;
use dir_structure::dir_descendants::FileFilter;
use dir_structure::dir_descendants::FolderFilter;
use dir_structure::dir_descendants::FolderRecurseFilter;
use dir_structure::prelude::*;
use dir_structure::versioned::Versioned;

pub fn ws_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
}

#[derive(DirStructure, HasField)]
pub struct Workspace<'vfs, Vfs: VfsCore<Path = Path>> {
    pub src: SrcDir<'vfs, Vfs>,
    pub doc: DocDir<'vfs, Vfs>,
    #[dir_structure(path = ".cargo")]
    pub cargo_config: CargoConfig<'vfs, Vfs>,
}

impl<'vfs, Vfs: VfsCore<Path = Path>> Workspace<'vfs, Vfs> {
    pub fn compiler_crate(
        &self,
        name: impl AsRef<<Vfs::Path as PathType>::PathSegmentRef>,
    ) -> &Crate<'vfs, Vfs> {
        self.src.compiler.get_value_by_name(name).unwrap()
    }

    pub fn compiler_crate_mut(
        &mut self,
        name: impl AsRef<<Vfs::Path as PathType>::PathSegmentRef>,
    ) -> &mut Crate<'vfs, Vfs> {
        self.src.compiler.get_value_by_name_mut(name).unwrap()
    }

    pub fn nextest_config_file(&self) -> &Vfs::Path {
        self.cargo_config.nextest.self_path.as_ref()
    }
}

#[derive(DirStructure, HasField)]
pub struct CargoConfig<'vfs, Vfs: VfsCore<Path = Path>> {
    #[dir_structure(path = "nextest.toml")]
    pub nextest: NextestConfig<'vfs, Vfs>,
}

#[derive(DirStructure, HasField)]
pub struct NextestConfig<'vfs, Vfs: VfsCore<Path = Path>> {
    #[dir_structure(path = self)]
    pub config: DeferredReadOrOwn<'vfs, Versioned<String, Vfs::Path>, Vfs, true>,

    pub self_path: <Vfs::Path as PathType>::OwnedPath,
}

#[derive(DirStructure, HasField)]
pub struct SrcDir<'vfs, Vfs: VfsCore<Path = Path>> {
    pub config: SrcConfigDir<'vfs, Vfs>,
    pub compiler: DirChildren<Crate<'vfs, Vfs>, NoFilter, Vfs::Path>,
    // pub lib: dir_structure::DirChildren<Crate<'vfs, Vfs>>,
    pub dev: DirChildren<Crate<'vfs, Vfs>, NoFilter, Vfs::Path>,
}

#[derive(DirStructure, HasField)]
pub struct SrcConfigDir<'vfs, Vfs: VfsCore<Path = Path>> {
    #[dir_structure(path = "rustfmt.toml")]
    pub rustfmt: DeferredReadOrOwn<'vfs, Versioned<String, Vfs::Path>, Vfs, true>,
}

#[derive(DirStructure, HasField)]
pub struct Crate<'vfs, Vfs: VfsCore<Path = Path>> {
    pub src: DeferredReadOrOwn<
        'vfs,
        DirDescendants<RustSourceFile<'vfs, Vfs>, RustFileFilter, Vfs::Path>,
        Vfs,
        true,
    >,
}

#[derive(DirStructure, HasField)]
pub struct RustSourceFile<'vfs, Vfs: VfsCore<Path = Path>> {
    #[dir_structure(path = self)]
    pub file: DeferredReadOrOwn<'vfs, Versioned<String, Vfs::Path>, Vfs, true>,

    pub self_path: PathBuf,
}

pub struct RustFileFilter;

impl<P: PathType + ?Sized> FolderFilter<P> for RustFileFilter {
    fn allows(_folder: &P) -> bool {
        false
    }
}

impl<P: PathType + ?Sized> FolderRecurseFilter<P> for RustFileFilter {
    fn allows(_folder: &P) -> bool {
        true
    }
}

impl FileFilter<Path> for RustFileFilter {
    fn allows(file: &Path) -> bool {
        file.extension().is_some_and(|ext| ext == "rs")
    }
}

#[derive(DirStructure, HasField)]
pub struct DocDir<'vfs, Vfs: VfsCore<Path = Path>> {
    pub docs: DocDirDocsSite<'vfs, Vfs>,
    pub guides: DirChildren<(), NoFilter, Vfs::Path>,
}

#[derive(DirStructure, HasField)]
pub struct DocDirDocsSite<'vfs, Vfs: VfsCore<Path = Path>> {
    pub content: DocContent<'vfs, Vfs>,
}

#[derive(DirStructure, HasField)]
pub struct DocContent<'vfs, Vfs: VfsCore<Path = Path>> {
    pub docs: DirDescendants<DocSourceFile<'vfs, Vfs>, MdxFileFilter, Vfs::Path>,
}

pub struct MdxFileFilter;

impl FileFilter for MdxFileFilter {
    fn allows(file: &Path) -> bool {
        file.extension().is_some_and(|ext| ext == "mdx")
    }
}

impl FolderFilter for MdxFileFilter {
    fn allows(_folder: &Path) -> bool {
        false
    }
}

impl FolderRecurseFilter for MdxFileFilter {
    fn allows(_folder: &Path) -> bool {
        true
    }
}

#[derive(DirStructure, HasField)]
pub struct DocSourceFile<'vfs, Vfs: VfsCore<Path = Path>> {
    #[dir_structure(path = self)]
    pub file: DeferredReadOrOwn<'vfs, Versioned<String, Vfs::Path>, Vfs, true>,
}

#[macro_export]
macro_rules! resolve_ws_path {
    ($($id:tt)*) => {
        $crate::dir_structure::traits::resolve::resolve_path!([$crate::ws_root() as $crate::Workspace<'_, $crate::dir_structure::vfs::fs_vfs::FsVfs>].$($id)*)
    };
}

#[macro_export]
macro_rules! display_ws_path {
    ($($id:tt)*) => {
        $crate::dir_structure::traits::resolve::resolve_path!([::std::path::PathBuf::new() as $crate::Workspace<'_, $crate::dir_structure::vfs::fs_vfs::FsVfs>].$($id)*).display()
    };
}

pub mod parser_tests {
    use std::path::Path;
    use std::path::PathBuf;

    use dir_structure::DirStructure;
    use dir_structure::HasField;
    use dir_structure::deferred_read_or_own::DeferredReadOrOwn;
    use dir_structure::error::VfsResult;
    use dir_structure::prelude::*;
    use dir_structure::std_types::FileString;
    use dir_structure::traits::resolve::load_path;
    use dir_structure::traits::resolve::resolve_path;
    use dir_structure::traits::sync::DirStructureItem;
    use dir_structure::vfs::fs_vfs::FsVfs;

    use crate::ws_root;

    pub fn parser_tests_dir() -> PathBuf {
        ws_root().join("src/compiler/tests/testData/parser-tests")
    }

    pub const INPUT_FILE_NAME: &str = "input.nrx";
    pub const OUTPUT_FILE_NAME: &str = "output.txt";

    #[derive(DirStructure, HasField)]
    pub struct ParserTestSingleFolder<'vfs, Vfs: VfsCore<Path = Path>> {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredReadOrOwn<'vfs, FileString, Vfs, true>,
        #[dir_structure(path = "output.txt")]
        pub output: Option<DeferredReadOrOwn<'vfs, FileString, Vfs>>,
        pub self_path: <Vfs::Path as PathType>::OwnedPath,
    }

    impl<'vfs, Vfs: VfsCore<Path = Path>> ParserTestSingleFolder<'vfs, Vfs> {
        pub fn input_file_path(&self) -> PathBuf {
            resolve_path!([ParserTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].input)
        }

        pub fn output_file_path(&self) -> PathBuf {
            resolve_path!([ParserTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].output)
        }
    }

    dir_structure::dir_children_wrapper_with_vfs!(pub ParserTestsFolder ParserTestSingleFolder <Path = Path>);

    pub fn collect_parser_tests() -> VfsResult<ParserTestsFolder<'static, FsVfs>, FsVfs> {
        ParserTestsFolder::<FsVfs>::read(parser_tests_dir())
    }

    pub fn load_parser_test(
        test: &str,
    ) -> VfsResult<ParserTestSingleFolder<'static, FsVfs>, FsVfs> {
        load_path!([parser_tests_dir() as ParserTestsFolder<'static, FsVfs>].${test})
    }
}

pub mod name_resolution_tests {
    use std::path::Path;
    use std::path::PathBuf;

    use dir_structure::DirStructure;
    use dir_structure::HasField;
    use dir_structure::deferred_read_or_own::DeferredReadOrOwn;
    use dir_structure::error::VfsResult;
    use dir_structure::prelude::*;
    use dir_structure::std_types::FileString;
    use dir_structure::traits::resolve::load_path;
    use dir_structure::traits::resolve::resolve_path;
    use dir_structure::traits::sync::DirStructureItem;
    use dir_structure::vfs::fs_vfs::FsVfs;

    use crate::ws_root;

    pub fn name_resolution_tests_dir() -> PathBuf {
        ws_root().join("src/compiler/tests/testData/name-resolution-tests")
    }

    #[derive(DirStructure, HasField)]
    pub struct NameResolutionTestSingleFolder<'vfs, Vfs: VfsCore<Path = Path>> {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredReadOrOwn<'vfs, FileString, Vfs, true>,
        #[dir_structure(path = "output.txt")]
        pub output: Option<DeferredReadOrOwn<'vfs, FileString, Vfs>>,
        pub self_path: <Vfs::Path as PathType>::OwnedPath,
    }

    impl<'vfs, Vfs: VfsCore<Path = Path>> NameResolutionTestSingleFolder<'vfs, Vfs> {
        pub fn input_file_path(&self) -> PathBuf {
            resolve_path!([NameResolutionTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].input)
        }

        pub fn output_file_path(&self) -> PathBuf {
            resolve_path!([NameResolutionTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].output)
        }
    }

    dir_structure::dir_children_wrapper_with_vfs!(pub NameResolutionTestsFolder NameResolutionTestSingleFolder <Path = Path>);

    pub fn collect_name_resolution_tests()
    -> VfsResult<NameResolutionTestsFolder<'static, FsVfs>, FsVfs> {
        NameResolutionTestsFolder::<FsVfs>::read(name_resolution_tests_dir())
    }

    pub fn load_name_resolution_test(
        test: &str,
    ) -> VfsResult<NameResolutionTestSingleFolder<'static, FsVfs>, FsVfs> {
        load_path!([name_resolution_tests_dir() as NameResolutionTestsFolder<'static, FsVfs>].${test})
    }
}

pub mod ssa_tests {
    use std::path::Path;
    use std::path::PathBuf;

    use dir_structure::DirStructure;
    use dir_structure::HasField;
    use dir_structure::deferred_read_or_own::DeferredReadOrOwn;
    use dir_structure::error::VfsResult;
    use dir_structure::prelude::VfsCore;
    use dir_structure::std_types::FileString;
    use dir_structure::traits::resolve::load_path;
    use dir_structure::traits::resolve::resolve_path;
    use dir_structure::traits::sync::DirStructureItem;
    use dir_structure::vfs::fs_vfs::FsVfs;

    use crate::ws_root;

    pub fn ssa_tests_dir() -> PathBuf {
        ws_root().join("src/compiler/tests/testData/ssa-tests")
    }

    #[derive(DirStructure, HasField)]
    pub struct SsaTestSingleFolder<'vfs, Vfs: VfsCore<Path = Path>> {
        #[dir_structure(path = "input.nrx")]
        pub input: DeferredReadOrOwn<'vfs, FileString, Vfs, true>,
        #[dir_structure(path = "output.nrxssa")]
        pub output: Option<DeferredReadOrOwn<'vfs, FileString, Vfs>>,
        pub self_path: PathBuf,
    }

    impl<'vfs, Vfs: VfsCore<Path = Path>> SsaTestSingleFolder<'vfs, Vfs> {
        pub fn input_file_path(&self) -> PathBuf {
            resolve_path!([SsaTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].input)
        }

        pub fn output_file_path(&self) -> PathBuf {
            resolve_path!([SsaTestSingleFolder<'vfs, Vfs> @ self.self_path.clone()].output)
        }
    }

    dir_structure::dir_children_wrapper_with_vfs!(pub SsaTestsFolder SsaTestSingleFolder <Path = Path>);

    pub fn collect_ssa_tests() -> VfsResult<SsaTestsFolder<'static, FsVfs>, FsVfs> {
        SsaTestsFolder::<FsVfs>::read(ssa_tests_dir())
    }

    pub fn load_ssa_test(test: &str) -> VfsResult<SsaTestSingleFolder<'static, FsVfs>, FsVfs> {
        load_path!([ssa_tests_dir() as SsaTestsFolder<'static, FsVfs>].${test})
    }
}
