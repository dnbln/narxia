#![feature(string_from_utf8_lossy_owned)]
#![feature(decl_macro)]

use std::fmt;
use std::fmt::Write as _;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Read;
use std::io::Seek;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process;
use std::thread;
use std::time::Instant;

use bin_context::NexusContext;
use cargo_interface::BuildCmdBuildingProgress;
use cargo_interface::BuildTarget;
use cargo_interface::LintConfig;
use cargo_interface::RunCompilerCommand;
use cargo_interface::SysTarget;
use clap::Parser;
use clap::Subcommand;
use clap::ValueEnum;
use git_journey::git2::Repository;
use liblzma::read;
use miette::Context;
use miette::IntoDiagnostic;
use miette::bail;
use narxia_dir_structures::dir_structure::DeferredReadOrOwn;
use narxia_dir_structures::dir_structure::DirStructure;
use narxia_dir_structures::dir_structure::FileString;
use narxia_dir_structures::name_resolution_tests;
use narxia_dir_structures::name_resolution_tests::NameResolutionTestSingleFolder;
use narxia_dir_structures::parser_tests;
use narxia_dir_structures::parser_tests::ParserTestSingleFolder;
use narxia_dir_structures::ssa_tests;
use narxia_dir_structures::ssa_tests::SsaTestSingleFolder;
use narxia_dir_structures::ws_root;
use prodash::tree::Item;
use prodash::unit;
use reqwest::blocking;
use zip::ZipWriter;
use zip::write::SimpleFileOptions;

pub mod bin_context;
pub mod cargo_interface;
pub mod duration;

pub type NexusR<T = ()> = miette::Result<T>;

#[derive(Debug, Subcommand)]
pub enum BuildSysCmd {
    /// Build the project.
    #[clap(name = "build")]
    #[clap(alias = "b")]
    Build(BuildCmd),
    /// Collect parser tests.
    #[clap(name = "collect-parser-tests")]
    #[clap(alias = "cpt")]
    #[clap(alias = "ct-p")]
    CollectParserTests,

    /// Collect name resolution tests.
    #[clap(name = "collect-name-resolution-tests")]
    #[clap(alias = "ct-nr")]
    CollectNameResolutionTests,

    /// Collect SSA tests.
    #[clap(name = "collect-ssa-tests")]
    #[clap(alias = "ct-ssa")]
    CollectSSATests,

    #[clap(name = "lint")]
    Lint {
        #[clap(long)]
        fix: bool,
    },

    #[clap(name = "format")]
    #[clap(alias = "fmt")]
    Format {
        #[clap(long)]
        check: bool,
    },

    #[clap(name = "build-docs")]
    BuildDocs {
        #[clap(long, default_value = "nrx.dnbln.dev")]
        cname: String,
    },

    #[clap(name = "doc-patchup-rustdocs")]
    DocPatchupRustdocs {
        #[clap(long)]
        check: bool,
    },

    #[clap(name = "patch-guide")]
    PatchGuide { guide: PathBuf, output: PathBuf },
}

impl BuildSysCmd {
    pub fn run(self, cx: &mut NexusContext) -> NexusR {
        match self {
            Self::Build(cmd) => {
                let mut item = cx.new_child("Build");
                let bp =
                    BuildCmdBuildingProgress::new(item.add_child("Build progress"), Instant::now());
                cmd.run(&cx.llvm_manager, &mut item, Some(bp))?;
            }
            Self::CollectParserTests => {
                let mut item = cx.new_child("collect parser tests");
                collect_tests_from_source::<ParserTestSingleFolder>(
                    "crates/narxia-syn/src/**/*.rs",
                    "// parser-test:",
                    &mut item,
                )?;
            }
            Self::CollectNameResolutionTests => {
                let mut item = cx.new_child("collect name resolution tests");
                collect_tests_from_source::<NameResolutionTestSingleFolder>(
                    "crates/narxia-hir-typechk/src/**/*.rs",
                    "// name-resolution-test:",
                    &mut item,
                )?;
            }
            Self::CollectSSATests => {
                let mut item = cx.new_child("collect ssa tests");
                collect_tests_from_source::<SsaTestSingleFolder>(
                    "crates/narxia-ssa-lower/src/**/*.rs",
                    "// ssa-test:",
                    &mut item,
                )?;
            }
            Self::Lint { fix } => {
                let bins = {
                    let mut item = cx.new_child("Build");
                    let bp = BuildCmdBuildingProgress::new(
                        item.add_child("Build progress"),
                        Instant::now(),
                    );
                    BuildI {
                        targets: vec![Target::LLVM],
                        profile: Profile::Dev,
                        sys: SysTarget::Host,
                        llvm_link_behavior: LLVMLinkBehavior::PreferDynamic,
                    }
                    .run(&cx.llvm_manager, &mut item, Some(bp))?
                };
                let (llvm_k, llvm_v) = bins.llvm.as_ref().unwrap().to_env();
                let mut item = cx.new_child("Lint");
                cargo_interface::Lint::new(LintConfig { use_ansi: true })
                    .fix(fix)
                    .env(llvm_k, llvm_v)
                    .run(&mut item)?;
            }
            Self::Format { check } => {
                let mut item = cx.new_child("Format");
                cargo_interface::Format::new().check(check).run(&mut item)?;
            }

            Self::BuildDocs { cname } => {
                let mut item = cx.new_child("Build docs");
                build_docs(&mut item, &cname)?;
            }

            Self::DocPatchupRustdocs { check } => {
                let mut item = cx.new_child("Doc patchup");
                doc_patchup(check, &mut item)?;
            }

            Self::PatchGuide { guide, output } => {
                let mut item = cx.new_child("Patch guide");
                patch_guide(&guide, &output, &mut item)?;
            }
        }

        Ok(())
    }
}

fn patch_guide(guide: &Path, output: &Path, item: &mut Item) -> NexusR {
    item.init(None, None);

    let repo = Repository::open(guide)
        .into_diagnostic()
        .wrap_err("Failed to open git repository")?;
    let docs = git_journey::collect(&repo)
        .into_diagnostic()
        .wrap_err("Failed to collect git journey docs")?;
    let r = git_journey::render(&docs);

    std::fs::write(output, r)
        .into_diagnostic()
        .wrap_err("Failed to write patched guide")?;

    Ok(())
}

trait GenericTestDirType: DirStructure {
    fn path_to_write_to(&self) -> &Path;
    fn from_name_and_code(name: &str, code: String) -> Self;
}

impl GenericTestDirType for ParserTestSingleFolder {
    fn path_to_write_to(&self) -> &Path {
        &self.self_path
    }

    fn from_name_and_code(name: &str, code: String) -> Self {
        Self {
            input: DeferredReadOrOwn::Own(FileString(code)),
            output: None,
            self_path: parser_tests::parser_tests_dir().join(name),
        }
    }
}

impl GenericTestDirType for NameResolutionTestSingleFolder {
    fn path_to_write_to(&self) -> &Path {
        &self.self_path
    }

    fn from_name_and_code(name: &str, code: String) -> Self {
        Self {
            input: DeferredReadOrOwn::Own(FileString(code)),
            output: None,
            self_path: name_resolution_tests::name_resolution_tests_dir().join(name),
        }
    }
}

impl GenericTestDirType for SsaTestSingleFolder {
    fn path_to_write_to(&self) -> &Path {
        &self.self_path
    }

    fn from_name_and_code(name: &str, code: String) -> Self {
        Self {
            input: DeferredReadOrOwn::Own(FileString(code)),
            output: None,
            self_path: ssa_tests::ssa_tests_dir().join(name),
        }
    }
}

fn collect_tests_from_source<T: GenericTestDirType>(
    glob: &str,
    comment_header: &str,
    item: &mut Item,
) -> NexusR {
    let paths = glob::glob(ws_root().join(glob).to_str().unwrap())
        .into_diagnostic()?
        .collect::<Vec<_>>();
    item.init(Some(paths.len()), Some(unit::label("files")));
    for entry in paths {
        let entry = entry.into_diagnostic()?;
        let file_repo = entry.strip_prefix(ws_root()).unwrap();
        let mut file_item = item.add_child("collect tests from file");

        collect_tests_from_source_file::<T>(&entry, file_repo, comment_header, &mut file_item)?;

        item.inc();
    }

    Ok(())
}

fn collect_tests_from_source_file<T: GenericTestDirType>(
    file: &Path,
    file_repo: &Path,
    prefix: &str,
    item: &mut Item,
) -> NexusR {
    let file_contents = fs::read(file).into_diagnostic()?;
    let file_contents = String::from_utf8_lossy(&file_contents);

    item.init(None, Some(unit::label("tests")));

    let mut tests = Vec::new();
    let mut iter = file_contents.lines().enumerate().peekable();
    while let Some((line_number, line)) = iter.next() {
        let line_without_whitespace = line.trim();

        if let Some(test_prefix_position) = line_without_whitespace.find(prefix) {
            let test_name = &line_without_whitespace[test_prefix_position + prefix.len()..];

            let mut test_code = format!(
                "// test {test_name} at {file_repo}\n",
                test_name = test_name,
                file_repo = file_repo.display()
            );

            while let Some((_, next_line)) = iter.peek() {
                let next_line_without_whitespace = next_line[test_prefix_position..].trim();

                if next_line_without_whitespace == "//" {
                    test_code.push('\n');
                    iter.next();
                    continue;
                }
                if !next_line_without_whitespace.starts_with("// ") {
                    break;
                }

                let test_line = &next_line_without_whitespace[3..];

                test_code.push_str(test_line);
                test_code.push('\n');

                iter.next();
            }

            tests.push((line_number, test_name, test_code));
            item.inc();
        }
    }

    if tests.is_empty() {
        item.done(format!(
            "{:<24} in {}",
            "No tests found",
            file_repo.display()
        ));
        return Ok(());
    }
    item.done(format!(
        "Found {:>5} tests in {}",
        tests.len(),
        file_repo.display()
    ));

    for (_line_number, name, code) in tests {
        let folder = T::from_name_and_code(name, code);

        folder
            .write(T::path_to_write_to(&folder))
            .into_diagnostic()?;
    }

    Ok(())
}

// order is important. they are always built in this order.
#[derive(Debug, ValueEnum, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Target {
    LLVM,
    Compiler,
    Tests,
}

impl Target {
    pub fn dependencies(&self) -> &'static [Self] {
        match self {
            Self::Compiler => &[Self::LLVM],
            Self::Tests => &[Self::Compiler],
            _ => &[],
        }
    }
}

#[derive(Debug, ValueEnum, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Profile {
    Dev,
    Release,
}

impl Profile {
    pub fn default_llvm_link_behavior(&self) -> LLVMLinkBehavior {
        match self {
            Self::Dev => LLVMLinkBehavior::PreferDynamic,
            Self::Release => LLVMLinkBehavior::ForceStatic,
        }
    }

    pub fn cargo_name(&self) -> &str {
        match self {
            Self::Dev => "dev",
            Self::Release => "release",
        }
    }
}

#[derive(Debug, Parser)]
pub struct BuildCmd {
    /// The targets to build.
    #[clap(long, value_delimiter = ',', default_value = "compiler")]
    pub targets: Vec<Target>,

    #[clap(flatten)]
    pub profile: ProfileDeterminer,

    #[clap(long)]
    pub llvm_link_behavior: Option<LLVMLinkBehavior>,
}

#[derive(Debug, clap::Args, Clone, Copy)]
pub struct ProfileDeterminer {
    /// Build in release mode.
    #[clap(long, conflicts_with = "profile")]
    pub release: bool,

    /// The profile to build in.
    #[clap(long, conflicts_with = "release")]
    pub profile: Option<Profile>,
}

impl ProfileDeterminer {
    pub fn get_profile(self) -> Profile {
        if self.release {
            Profile::Release
        } else {
            self.profile.unwrap_or(Profile::Dev)
        }
    }
}

impl BuildCmd {
    pub fn run(
        self,
        llvm_manager: &LLVMManager,
        item: &mut Item,
        build_progress: Option<BuildCmdBuildingProgress>,
    ) -> NexusR {
        let profile = self.profile.get_profile();
        let bins = BuildI {
            targets: self.targets,
            profile,
            sys: SysTarget::Host,
            llvm_link_behavior: self
                .llvm_link_behavior
                .unwrap_or_else(|| profile.default_llvm_link_behavior()),
        }
        .run(llvm_manager, item, build_progress)?;

        Ok(())
    }
}

struct DecompressTarXz {
    item: Item,
    tar_xz: PathBuf,
    destination_path: PathBuf,
}

impl DecompressTarXz {
    fn run(&mut self) -> NexusR {
        let mut item = self.item.add_child("Decompress");
        item.init(None, Some(unit::label("kb")));

        struct Reader<'i> {
            item: &'i Item,
            total: usize,
            r: read::XzDecoder<fs::File>,
        }

        impl Read for Reader<'_> {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                let r = self.r.read(buf)?;
                self.total += r;
                self.item.set(self.total / 1024);
                Ok(r)
            }
        }

        let mut a = tar::Archive::new(Reader {
            item: &item,
            total: 0,
            r: read::XzDecoder::new(fs::File::open(&self.tar_xz).into_diagnostic()?),
        });

        match a.unpack(&self.destination_path) {
            Ok(()) => {}
            Err(e) => {
                item.fail("Failed to unpack");
                return Err(e).into_diagnostic();
            }
        }

        Ok(())
    }
}

struct DownloadAndDecompressTarXz {
    url: String,
    item: Item,
    download_to: Option<PathBuf>,
    destination_path: PathBuf,
}

impl DownloadAndDecompressTarXz {
    fn run(&mut self) -> NexusR {
        if self.destination_path.exists() {
            self.item.done("Already downloaded and unpacked");
            return Ok(());
        }

        if let Some(p) = self.download_to.as_ref()
            && p.exists()
        {
            self.item.done("Already downloaded");
            DecompressTarXz {
                item: self.item.add_child("Decompress"),
                tar_xz: p.clone(),
                destination_path: self.destination_path.clone(),
            }
            .run()?;
            return Ok(());
        }

        let mut download_item = self.item.add_child("Download");
        let mut decompress_item = self.item.add_child("Decompress");
        let resp = blocking::get(&self.url)
            .into_diagnostic()?
            .error_for_status()
            .into_diagnostic()?;

        let cl = resp.content_length().map(|cl| cl as usize / 1024);

        download_item.init(cl, Some(unit::label("kb")));
        decompress_item.init(None, Some(unit::label("kb")));

        let mut download_file = self
            .download_to
            .as_ref()
            .map(fs::File::create)
            .transpose()
            .into_diagnostic()?;

        struct DecompressReader<'i> {
            decompress_item: &'i Item,
            total: usize,
            r: read::XzDecoder<Reader<'i>>,
        }

        impl Read for DecompressReader<'_> {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                let r = self.r.read(buf)?;
                self.total += r;
                self.decompress_item.set(self.total / 1024);
                Ok(r)
            }
        }

        struct Reader<'i> {
            download_item: &'i Item,
            download_file: Option<&'i mut fs::File>,
            total: usize,
            resp: blocking::Response,
        }

        impl Read for Reader<'_> {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                let r = self.resp.read(buf)?;
                self.total += r;

                if let Some(f) = self.download_file.as_mut() {
                    f.write_all(&buf[..r])?;
                }

                self.download_item.set(self.total / 1024);

                Ok(r)
            }
        }

        let mut a = tar::Archive::new(DecompressReader {
            decompress_item: &decompress_item,
            total: 0,
            r: read::XzDecoder::new(Reader {
                download_item: &download_item,
                download_file: download_file.as_mut(),
                total: 0,
                resp,
            }),
        });

        match a.unpack(&self.destination_path) {
            Ok(()) => {}
            Err(e) => {
                download_item.fail("Failed to unpack");
                decompress_item.fail("Failed to unpack");
                self.item.fail("Failed to download and unpack");
                return Err(e).into_diagnostic();
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
struct LLVMVersion {
    major: usize,
    minor: usize,
    patch: usize,
    extra: Option<String>,
}

impl fmt::Display for LLVMVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{major}.{minor}.{patch}{extra}",
            major = self.major,
            minor = self.minor,
            patch = self.patch,
            extra = self.extra.as_deref().unwrap_or("")
        )
    }
}

pub struct LLVMManager {
    llvm_manager_path: PathBuf,
}

impl LLVMManager {
    pub fn make_from_target() -> Self {
        Self {
            llvm_manager_path: ws_root().join("nexusbuild/llvm"),
        }
    }

    fn download_path(&self, version: &LLVMVersion) -> PathBuf {
        self.llvm_manager_path
            .join(format!("llvm-{version}.src.tar.xz"))
    }

    fn src_path(&self, version: &LLVMVersion) -> PathBuf {
        self.llvm_manager_path.join(format!("llvm-{version}.src"))
    }

    fn perform_renames(&self, version: &LLVMVersion) -> NexusR {
        let mut dirs = vec![];
        let suffix = format!("-{version}.src");

        for dir in fs::read_dir(self.src_path(version)).into_diagnostic()? {
            let dir = dir.into_diagnostic()?;
            let path = dir.path();
            let p = path.to_str().unwrap();

            if let Some(new_path) = p.strip_suffix(&suffix) {
                let new_path = new_path.to_owned();
                dirs.push((path, new_path));
            }
        }

        for (old, new) in dirs {
            fs::rename(&old, new).into_diagnostic()?;
        }

        Ok(())
    }

    fn llvm_prefix_info(&self, version: &LLVMVersion) -> LLVMPrefixInfo {
        let prefix = self.install_path(version);

        LLVMPrefixInfo {
            version: version.clone(),
            prefix,
        }
    }

    fn build_path(&self, version: &LLVMVersion) -> PathBuf {
        self.llvm_manager_path.join(format!("llvm-{version}.build"))
    }

    fn install_path(&self, version: &LLVMVersion) -> PathBuf {
        self.llvm_manager_path
            .join("install")
            .join(format!("llvm-{version}.install"))
    }

    fn download_llvm_src(&self, item: &mut Item, version: &LLVMVersion) -> NexusR<PathBuf> {
        let item = item.add_child("Download::LLVM");
        let download_path = self.download_path(version);
        let src_path = self.src_path(version);

        if let Some(src_parent) = src_path.parent()
            && !src_parent.exists()
        {
            fs::create_dir_all(src_parent).into_diagnostic()?;
        }

        if let Some(download_parent) = download_path.parent()
            && !download_parent.exists()
        {
            fs::create_dir_all(download_parent).into_diagnostic()?;
        }

        let llvm_src_tar = format!(
            "https://github.com/llvm/llvm-project/releases/download/llvmorg-{version}/llvm-project-{version}.src.tar.xz",
        );

        DownloadAndDecompressTarXz {
            url: llvm_src_tar,
            item,
            download_to: Some(download_path.clone()),
            destination_path: src_path.clone(),
        }
        .run()?;

        self.perform_renames(version)?;

        let extracted = src_path.join("llvm-project");

        if !extracted.exists() {
            bail!("LLVM source not extracted (or we couldn't guess the root directory)");
        }

        Ok(extracted)
    }

    fn check_install(&self, version: &LLVMVersion) -> bool {
        let install_path = self.install_path(version);
        let llvm_config = install_path.join("bin/llvm-config");

        install_path.exists() && llvm_config.exists()
    }

    fn compile_llvm(
        &self,
        item: &mut Item,
        version: &LLVMVersion,
        src_path: &Path,
    ) -> NexusR<PathBuf> {
        let build_path = self.build_path(version);
        let install_path = self.install_path(version);
        let llvm_config = install_path.join("bin/llvm-config");

        if install_path.exists() && llvm_config.exists() {
            item.done("LLVM already built");
            return Ok(llvm_config);
        }

        'configure: {
            if build_path.exists() {
                if !install_path.exists() {
                    item.info(
                        "LLVM build path exists, but install path does not; continuing build",
                    );
                    break 'configure;
                }

                if !llvm_config.exists() {
                    item.info("LLVM build path exists, but llvm-config does not; build probably failed midway");
                    break 'configure;
                }

                item.done("LLVM already built");
                return Ok(llvm_config);
            } else {
                if !src_path.exists() {
                    item.fail("LLVM source not found");
                    bail!("LLVM source not found");
                }

                fs::create_dir(&build_path).into_diagnostic()?;

                let mut cmake = process::Command::new("cmake");
                cmake
                    .args([
                        src_path.join("llvm").to_str().unwrap(),
                        "-DCMAKE_BUILD_TYPE=Release",
                        &format!("-DCMAKE_INSTALL_PREFIX={}", install_path.to_str().unwrap()),
                        "-DLLVM_ENABLE_ASSERTIONS=ON",
                        "-G",
                        "Ninja",
                    ])
                    .current_dir(&build_path);

                let mut cmake = cmake.spawn().into_diagnostic()?;

                let status = cmake.wait().into_diagnostic()?;

                if !status.success() {
                    item.fail("CMake configuration failed");
                    bail!("CMake configuration failed");
                }
            }
        }

        let mut build = process::Command::new("cmake");
        build
            .args(["--build", ".", "--target", "install"])
            .current_dir(&build_path);

        build
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::piped());

        build.env("NINJA_STATUS", "$$NEXUS_STATUS:::%f::%t$$");

        item.info("Building LLVM");

        let mut build = build.spawn().into_diagnostic()?;

        let stdout = build.stdout.take().unwrap();
        let mut stderr = build.stderr.take().unwrap();

        let stderr = thread::spawn(move || {
            let mut v = Vec::new();
            stderr.read_to_end(&mut v).into_diagnostic()?;
            Ok::<_, miette::Report>(v)
        });
        let stdout = {
            let item = item.add_child("Build::LLVM");
            item.init(None, Some(unit::label("edges")));
            thread::spawn(move || {
                let mut out = String::new();

                let mut br = io::BufReader::new(stdout);
                loop {
                    let mut line = String::new();
                    let o = br.read_line(&mut line).into_diagnostic()?;

                    if o == 0 {
                        break;
                    }

                    if let Some(p) = line.find("$$NEXUS_STATUS:::") {
                        let p = p + "$$NEXUS_STATUS:::".len();
                        let Some(c) = line[p..].find("$$") else {
                            bail!("Invalid ninja status line");
                        };
                        let e = line[p..p + c].find("::").unwrap();
                        let f = &line[p..p + e];
                        let t = &line[p + e + "::".len()..p + c];

                        let f = f.parse::<usize>().unwrap();
                        let t = t.parse::<usize>().unwrap();

                        item.set(f);
                        item.set_max(Some(t));

                        out.push_str(&line[..p]);
                        write!(&mut out, "[{f}/{t}] ").into_diagnostic()?;
                        out.push_str(&line[p + c + "$$".len()..]);
                    } else {
                        out.push_str(&line);
                    }
                }
                Ok(out)
            })
        };

        let status = build.wait().into_diagnostic()?;

        if !status.success() {
            io::stderr()
                .write_all(&stderr.join().unwrap()?)
                .into_diagnostic()?;
            io::stdout()
                .write_all(stdout.join().unwrap()?.as_bytes())
                .into_diagnostic()?;

            item.fail("Build failed");
            bail!("Build failed");
        }

        if !llvm_config.exists() {
            item.fail("llvm-config not found");
            bail!("llvm-config not found");
        }

        Ok(install_path)
    }
}

fn build_llvm(
    llvm_manager: &LLVMManager,
    item: &mut Item,
    build_progress: Option<BuildCmdBuildingProgress>,
) -> NexusR<LLVMPrefixInfo> {
    let mut item = item.add_child("Build::LLVM");
    let progress_lock = build_progress
        .as_ref()
        .map(|bp| bp.make_progress_lock("Build::LLVM".to_owned(), Instant::now()))
        .transpose()?;
    let version = LLVMVersion {
        major: 20,
        minor: 1,
        patch: 5,
        extra: None,
    };
    if llvm_manager.check_install(&version) {
        item.done("LLVM already built");
        return Ok(llvm_manager.llvm_prefix_info(&version));
    }

    let src_path = llvm_manager.download_llvm_src(&mut item, &version)?;
    let _ = llvm_manager.compile_llvm(&mut item, &version, &src_path)?;
    let llvm_prefix = llvm_manager.llvm_prefix_info(&version);
    drop(progress_lock);

    Ok(llvm_prefix)
}

fn build_compiler(
    profile: Profile,
    sys: &SysTarget,
    llvm_prefix_info: &LLVMPrefixInfo,
    item: &mut Item,
    build_progress: Option<BuildCmdBuildingProgress>,
    llvm_link_behavior: LLVMLinkBehavior,
) -> NexusR<PathBuf> {
    let mut item = item.add_child("Build::Compiler");
    item.init(None, Some(unit::label("artifacts")));

    let (llvm_k, llvm_v) = llvm_prefix_info.to_env();

    let output = cargo_interface::build()
        .packages(pkg_spec!("narxia-driver"))
        .build_targets([BuildTarget::Bin("nrx".to_owned())])
        .profile(profile.cargo_name())
        .sys_target(sys.clone())
        .env(llvm_k, llvm_v)
        .feature(match llvm_link_behavior {
            LLVMLinkBehavior::PreferDynamic => "llvm-prefer-dynamic",
            LLVMLinkBehavior::ForceStatic => "llvm-force-static",
            LLVMLinkBehavior::ForceDynamic => "llvm-force-dynamic",
        })
        .run(Some(&mut item), build_progress)?;

    let executable = if output.status.success() {
        let last_artifact = output.target_artifact.unwrap();
        if last_artifact.was_fresh {
            item.done("Compiler was fresh");
        } else {
            item.done("Compiler built");
        }

        last_artifact.executable
    } else {
        eprintln!("{}", output.stderr);

        item.fail("Compiler build failed");
        bail!("Compiler build failed");
    };

    Ok(executable)
}

fn build_tests(
    profile: Profile,
    llvm_prefix_info: &LLVMPrefixInfo,
    item: &mut Item,
    build_progress: Option<BuildCmdBuildingProgress>,
) -> NexusR {
    let mut item = item.add_child("Build::Tests");
    item.init(None, Some(unit::label("artifacts")));
    let (llvm_k, llvm_v) = llvm_prefix_info.to_env();
    cargo_interface::build()
        .profile(profile.cargo_name())
        .build_targets(vec![BuildTarget::Tests])
        .env(llvm_k, llvm_v)
        .run(Some(&mut item), build_progress)?;

    item.done("Tests built");

    Ok(())
}

#[derive(Debug, Clone)]
pub struct LLVMPrefixInfo {
    version: LLVMVersion,
    prefix: PathBuf,
}

impl LLVMPrefixInfo {
    pub fn to_env(&self) -> (String, PathBuf) {
        (
            format!(
                "LLVM_SYS_{}{}_PREFIX",
                self.version.major, self.version.minor
            ),
            self.prefix.clone(),
        )
    }
}

pub struct NarxiaBinaries {
    pub llvm: Option<LLVMPrefixInfo>,
    pub compiler: Option<PathBuf>,
}

pub struct BuildI {
    pub targets: Vec<Target>,
    pub profile: Profile,
    pub sys: SysTarget,
    pub llvm_link_behavior: LLVMLinkBehavior,
}

impl BuildI {
    pub fn run(
        mut self,
        llvm_manager: &LLVMManager,
        item: &mut Item,
        build_progress: Option<BuildCmdBuildingProgress>,
    ) -> NexusR<NarxiaBinaries> {
        loop {
            let mut added = false;
            for t in self.targets.clone() {
                for dep in t.dependencies() {
                    if !self.targets.contains(dep) {
                        self.targets.push(*dep);
                        added = true;
                    }
                }
            }

            if !added {
                break;
            }
        }

        self.targets.sort();
        self.targets.dedup();

        let mut bins = NarxiaBinaries {
            llvm: None,
            compiler: None,
        };

        for target in self.targets {
            match target {
                Target::LLVM => {
                    bins.llvm = Some(build_llvm(llvm_manager, item, build_progress.clone())?);
                }
                Target::Compiler => {
                    bins.compiler = Some(build_compiler(
                        self.profile,
                        &self.sys,
                        bins.llvm.as_ref().unwrap(),
                        item,
                        build_progress.clone(),
                        self.llvm_link_behavior,
                    )?);
                }
                Target::Tests => {
                    build_tests(
                        self.profile,
                        bins.llvm.as_ref().unwrap(),
                        item,
                        build_progress.clone(),
                    )?;
                }
            }
        }

        Ok(bins)
    }
}

pub trait NarxiaBinProvider<'bins> {
    fn get(&'bins self, target: Target) -> Option<&'bins PathBuf>;

    fn get_llvm(&'bins self) -> Option<&'bins LLVMPrefixInfo>;
}

impl<'bins> NarxiaBinProvider<'bins> for NarxiaBinaries {
    fn get(&'bins self, target: Target) -> Option<&'bins PathBuf> {
        match target {
            Target::LLVM => self.llvm.as_ref().map(|info| &info.prefix),
            Target::Compiler => self.compiler.as_ref(),
            Target::Tests => None,
        }
    }

    fn get_llvm(&'bins self) -> Option<&'bins LLVMPrefixInfo> {
        self.llvm.as_ref()
    }
}

pub trait NarxiaNeededBins<'bins> {
    type Command;
    fn needed_bins(command: &Self::Command, buffer: &mut Vec<Target>);
    fn compile_from(bins: &'bins impl NarxiaBinProvider<'bins>) -> Self;
}

pub trait ExtractSubcommand<Subcommand> {
    fn extract_subcommand(&self) -> Option<&Subcommand>;
}

macro bin_type {
    (@field_ty_for_target: LLVM) => {
        $crate::LLVMPrefixInfo
    },
    (@field_ty_for_target: $target:ident) => {
        ::std::path::PathBuf
    },
    (@getter_for_target: LLVM, $base:expr) => {
        $base.get_llvm()
    },
    (@getter_for_target: $target:ident, $base:expr) => {
        $base.get($crate::Target::$target)
    },
    (@getter_in_self_for_target: LLVM, $v:expr) => {
        $v.prefix
    },
    (@getter_in_self_for_target: $target:ident, $v:expr) => {
        $v
    },
    (@return_if_target_is_llvm: LLVM, $v:expr) => {
        return $v;
    },
    (@return_if_target_is_llvm: $target:ident, $v:expr) => {},
    ($name:ident,
        $command:ty,
        :bins $($field_bins:ident($field_target:ident))*
        $(
            :opt_bins($compute_opt_bins:path) $($field_opt_bins:ident($field_opt_target:ident))*
        )?
        $(
            :sub_needed_bins $($field_name:ident($field_ty:ty))*
        )?
    ) => {
        #[derive(Debug, Clone, Copy)]
        pub struct $name<'bins> {
            $(
                pub $field_bins: &'bins bin_type!(@field_ty_for_target: $field_target),
            )*
            $(
                $(
                    pub $field_opt_bins: Option<&'bins bin_type!(@field_ty_for_target: $field_opt_target)>,
                )*
            )?
            $(
                $(
                    pub $field_name: $field_ty,
                )*
            )?
        }

        impl<'bins> NarxiaNeededBins<'bins> for $name<'bins> {
            type Command = $command;

            fn needed_bins(command: &Self::Command, buffer: &mut Vec<$crate::Target>) {
                $(
                    if !buffer.contains(&$crate::Target::$field_target) {
                        buffer.push($crate::Target::$field_target);
                    }
                )*

                $(
                    let f: fn(&Self::Command, &mut Vec<$crate::Target>) = $compute_opt_bins;
                    f(command, buffer);
                )?

                $(
                    $(
                        if let Some(subcmd) = <Self::Command as $crate::ExtractSubcommand<<$field_ty as $crate::NarxiaNeededBins>::Command>>::extract_subcommand(command) {
                            $field_name::needed_bins(subcmd, buffer);
                        }
                    )*
                )?
            }

            fn compile_from(bins: &'bins impl NarxiaBinProvider<'bins>) -> Self {
                Self {
                    $($field_bins: bin_type!(@getter_for_target: $field_target, bins).unwrap(),)*
                    $($($field_opt_bins: bin_type!(@getter_for_target: $field_opt_target, bins),)*)?
                    $($field_name: <$field_ty as NarxiaNeededBins<'bins>>::compile_from(bins),)*
                }
            }
        }

        impl<'bins> NarxiaBinProvider<'bins> for $name<'bins> {
            fn get(&'bins self, target: Target) -> Option<&'bins ::std::path::PathBuf> {
                match target {
                    $(
                        $crate::Target::$field_target => Some(& bin_type!(@getter_in_self_for_target: $field_target, self.$field_bins)),
                    )*
                    _ => None,
                }
            }

            fn get_llvm(&'bins self) -> Option<&'bins LLVMPrefixInfo> {
                $(
                    bin_type!(@return_if_target_is_llvm: $field_target, Some(self.$field_bins));
                )*

                $(
                    bin_type!(@return_if_target_is_llvm: $field_opt_target, self.$field_opt_bins);
                )*

                None
            }
        }
    }
}

bin_type! {
    RunCompilerBins,
    RunCompilerCommand,
    :bins
        compiler(Compiler)
        llvm(LLVM)
}

bin_type! {
    BuildDistribsBins,
    BuildDistribCommand,
    :bins
        compiler(Compiler)
}

pub struct BuildDistribCommand {
    pub pkg: PathBuf,
}

impl BuildDistribCommand {
    pub fn run(&self, item: &mut Item, bins: &BuildDistribsBins) -> NexusR {
        let mut wr = ZipWriter::new(fs::File::create(&self.pkg).into_diagnostic()?);
        write_bin_file_to_zip(
            &mut wr,
            Path::new(bins.compiler.file_name().unwrap()),
            bins.compiler,
            SimpleFileOptions::default(),
            Some(&mut item.add_child("Compiler")),
        )?;
        wr.finish().into_diagnostic()?;
        Ok(())
    }
}

fn write_bin_file_to_zip<W: Write + Seek>(
    wr: &mut ZipWriter<W>,
    zip_path: impl AsRef<Path>,
    path: impl AsRef<Path>,
    options: SimpleFileOptions,
    mut item: Option<&mut Item>,
) -> NexusR<()> {
    wr.start_file(
        zip_path
            .as_ref()
            .to_str()
            .expect("Cannot convert to string"),
        options,
    )
    .into_diagnostic()?;

    let path = path.as_ref();

    let file_size = path.metadata().into_diagnostic()?.len();

    if let Some(item) = &mut item {
        item.init(Some(file_size as usize / 1024), Some(unit::label("kb")));
    }

    let mut buf = [0u8; 0x0001_0000];
    let mut f = fs::File::open(path).into_diagnostic()?;

    let mut total_read = 0;

    loop {
        let read = f.read(&mut buf).into_diagnostic()?;

        if read == 0 {
            break;
        }

        total_read += read;

        if let Some(item) = &mut item {
            item.set(total_read / 1024);
        }

        wr.write_all(&buf[..read]).into_diagnostic()?;
    }

    Ok(())
}

pub enum ColorConfig {
    Always,
    Never,
    Auto,
}

#[derive(Debug, Clone)]
pub struct NexusOutputGroups {
    begin: String,
    end: String,
}

pub struct NexusOutputGroupRAII<'a> {
    groups: &'a NexusOutputGroups,
    group: &'a str,
}

impl Drop for NexusOutputGroupRAII<'_> {
    fn drop(&mut self) {
        println!("{}", self.groups.end(self.group));
    }
}

impl NexusOutputGroups {
    pub fn new(begin: String, end: String) -> Self {
        Self { begin, end }
    }

    pub fn begin<'a>(&'a self, group: &'a str) -> NexusOutputGroupRAII<'a> {
        println!("{}", self.do_begin(group));

        NexusOutputGroupRAII {
            groups: self,
            group,
        }
    }

    pub fn do_begin<'a>(&'a self, group: &'a str) -> NextestBeginGroup<'a> {
        NextestBeginGroup {
            groups: self,
            group,
        }
    }

    pub fn end<'a>(&'a self, group: &'a str) -> NextestEndGroup<'a> {
        NextestEndGroup {
            groups: self,
            group,
        }
    }
}

pub struct NextestBeginGroup<'a> {
    groups: &'a NexusOutputGroups,
    group: &'a str,
}

impl fmt::Display for NextestBeginGroup<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.groups.begin.replace("{group}", self.group))
    }
}

pub struct NextestEndGroup<'a> {
    groups: &'a NexusOutputGroups,
    group: &'a str,
}

impl fmt::Display for NextestEndGroup<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.groups.end.replace("{group}", self.group))
    }
}

#[derive(Debug, Copy, Clone, ValueEnum)]
pub enum LLVMLinkBehavior {
    ForceStatic,
    PreferDynamic,
    ForceDynamic,
}

impl fmt::Display for LLVMLinkBehavior {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ForceStatic => write!(f, "force-static"),
            Self::PreferDynamic => write!(f, "prefer-dynamic"),
            Self::ForceDynamic => write!(f, "force-dynamic"),
        }
    }
}

fn docs_dir() -> PathBuf {
    ws_root().join("doc/docs")
}

fn install_docs_dependencies(item: &mut Item) -> NexusR {
    item.init(None, None);
    let mut cmd = process::Command::new("npm");
    cmd.arg("install")
        .current_dir(docs_dir())
        .stdin(process::Stdio::null())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped());

    let out = cmd.output().into_diagnostic()?;
    if !out.status.success() {
        item.fail("Failed to install docs dependencies");
        io::stderr().write_all(&out.stderr).into_diagnostic()?;

        io::stdout().write_all(&out.stdout).into_diagnostic()?;

        bail!("Failed to install docs dependencies");
    }

    item.done("Installed docs dependencies");

    Ok(())
}

fn build_docs(item: &mut Item, cname: &str) -> NexusR {
    install_docs_dependencies(&mut item.add_child("Install deps"))?;

    item.init(None, None);
    let mut cmd = process::Command::new("npm");
    cmd.arg("run")
        .arg("build")
        .current_dir(docs_dir())
        .stdin(process::Stdio::null())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped());

    let out = cmd.output().into_diagnostic()?;
    if !out.status.success() {
        item.fail("Failed to build docs");
        io::stderr().write_all(&out.stderr).into_diagnostic()?;

        io::stdout().write_all(&out.stdout).into_diagnostic()?;

        bail!("Failed to build docs");
    }

    fs::write(docs_dir().join("out/CNAME"), cname).into_diagnostic()?;

    item.done("Built docs");

    Ok(())
}

fn doc_patchup(check: bool, item: &mut Item) -> NexusR {
    item.init(None, None);

    let mut cmd = process::Command::new("cargo");
    cmd.arg("run").arg("-p").arg("doc-patchup");
    if check {
        cmd.arg("--").arg("--check");
    }
    let cmd = cmd
        .current_dir(ws_root())
        .stdin(process::Stdio::null())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped())
        .output()
        .into_diagnostic()?;

    if !cmd.status.success() {
        item.fail("Failed to patch up docs");
        io::stderr().write_all(&cmd.stderr).into_diagnostic()?;
        io::stdout().write_all(&cmd.stdout).into_diagnostic()?;
        bail!("Failed to patch up docs");
    }

    item.done("Patched up docs with rustdocs");

    Ok(())
}
