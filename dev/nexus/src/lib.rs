#![feature(string_from_utf8_lossy_owned)]
#![feature(decl_macro)]

use core::fmt;
use std::fmt::Write as _;
use std::io::{BufRead, Read, Seek, Write};
use std::path::{Path, PathBuf};
use std::time::Instant;

use bin_context::NexusContext;
use cargo_interface::{
    BuildCmdBuildingProgress, BuildTarget, PkgSpec, RunCompilerCommand, SysTarget,
};
use clap::{Parser, Subcommand, ValueEnum};
use miette::{bail, IntoDiagnostic};
use narxia_dir_structures::dir_structure::DirStructureItem;
use narxia_dir_structures::ws_root;
use prodash::tree::Item;
use prodash::unit;

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
    CollectParserTests,
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
                let paths = glob::glob(
                    ws_root()
                        .join("crates/narxia-syn/src/**/*.rs")
                        .to_str()
                        .unwrap(),
                )
                .into_diagnostic()?
                .collect::<Vec<_>>();
                item.init(Some(paths.len()), Some(unit::label("files")));
                for entry in paths {
                    let entry = entry.into_diagnostic()?;
                    let file_repo = entry.strip_prefix(ws_root()).unwrap();
                    let mut file_item = item.add_child("collect parser tests from file");

                    collect_parser_tests_from_file(&entry, file_repo, &mut file_item)?;

                    item.inc();
                }
            }
        }

        Ok(())
    }
}

fn collect_parser_tests_from_file(file: &Path, file_repo: &Path, item: &mut Item) -> NexusR {
    let file_contents = std::fs::read(file).into_diagnostic()?;
    let file_contents = String::from_utf8_lossy(&file_contents);

    item.init(None, Some(unit::label("tests")));

    let mut parser_tests = Vec::new();
    let mut iter = file_contents.lines().enumerate().peekable();
    while let Some((line_number, line)) = iter.next() {
        let line_without_whitespace = line.trim();

        const PARSER_TEST_PREFIX: &str = "// parser-test:";

        if let Some(test_prefix_position) = line_without_whitespace.find(PARSER_TEST_PREFIX) {
            let test_name =
                &line_without_whitespace[test_prefix_position + PARSER_TEST_PREFIX.len()..];

            let mut test_code = format!(
                "// test {test_name} at {file_repo}\n",
                test_name = test_name,
                file_repo = file_repo.display()
            );

            while let Some((_, next_line)) = iter.peek() {
                let next_line_without_whitespace = next_line[test_prefix_position..].trim();

                if !next_line_without_whitespace.starts_with("// ") {
                    break;
                }

                let test_line = &next_line_without_whitespace[3..];

                test_code.push_str(test_line);
                test_code.push('\n');

                iter.next();
            }

            parser_tests.push((line_number, test_name, test_code));
            item.inc();
        }
    }

    if parser_tests.is_empty() {
        item.done(format!(
            "{:<24} in {}",
            "No parser tests found",
            file_repo.display()
        ));
        return Ok(());
    }
    item.done(format!(
        "Found {:>5} parser tests in {}",
        parser_tests.len(),
        file_repo.display()
    ));

    for (_line_number, name, code) in parser_tests {
        let folder = narxia_dir_structures::ParserTestSingleFolder {
            input: narxia_dir_structures::dir_structure::DeferredReadOrOwn::Own(code.into()),
            output: None,
            self_path: PathBuf::new(),
        };

        folder
            .write(narxia_dir_structures::parser_tests_dir().join(name))
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
        let bins = BuildI {
            targets: self.targets,
            profile: self.profile.get_profile(),
            sys: SysTarget::Host,
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
            r: xz::read::XzDecoder<std::fs::File>,
        }

        impl Read for Reader<'_> {
            fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
                let r = self.r.read(buf)?;
                self.total += r;
                self.item.set(self.total / 1024);
                Ok(r)
            }
        }

        let mut a = tar::Archive::new(Reader {
            item: &item,
            total: 0,
            r: xz::read::XzDecoder::new(std::fs::File::open(&self.tar_xz).into_diagnostic()?),
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

        if let Some(p) = self.download_to.as_ref() {
            if p.exists() {
                self.item.done("Already downloaded");
                DecompressTarXz {
                    item: self.item.add_child("Decompress"),
                    tar_xz: p.clone(),
                    destination_path: self.destination_path.clone(),
                }
                .run()?;
                return Ok(());
            }
        }

        let mut download_item = self.item.add_child("Download");
        let mut decompress_item = self.item.add_child("Decompress");
        let resp = reqwest::blocking::get(&self.url)
            .into_diagnostic()?
            .error_for_status()
            .into_diagnostic()?;

        let cl = resp.content_length().map(|cl| cl as usize / 1024);

        download_item.init(cl, Some(unit::label("kb")));
        decompress_item.init(None, Some(unit::label("kb")));

        let mut download_file = self
            .download_to
            .as_ref()
            .map(std::fs::File::create)
            .transpose()
            .into_diagnostic()?;

        struct DecompressReader<'i> {
            decompress_item: &'i Item,
            total: usize,
            r: xz::read::XzDecoder<Reader<'i>>,
        }

        impl Read for DecompressReader<'_> {
            fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
                let r = self.r.read(buf)?;
                self.total += r;
                self.decompress_item.set(self.total / 1024);
                Ok(r)
            }
        }

        struct Reader<'i> {
            download_item: &'i Item,
            download_file: Option<&'i mut std::fs::File>,
            total: usize,
            resp: reqwest::blocking::Response,
        }

        impl<'i> Read for Reader<'i> {
            fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
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
            r: xz::read::XzDecoder::new(Reader {
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

        for dir in std::fs::read_dir(&self.src_path(version)).into_diagnostic()? {
            let dir = dir.into_diagnostic()?;
            let path = dir.path();
            let p = path.to_str().unwrap();

            if let Some(new_path) = p.strip_suffix(&suffix) {
                let new_path = new_path.to_owned();
                dirs.push((path, new_path));
            }
        }

        for (old, new) in dirs {
            std::fs::rename(&old, new).into_diagnostic()?;
        }

        Ok(())
    }

    fn llvm_sys_env(&self, version: &LLVMVersion) -> (String, PathBuf) {
        let llvm_prefix_root = self.install_path(version);

        (
            format!("LLVM_SYS_{}{}_PREFIX", version.major, version.minor),
            llvm_prefix_root,
        )
    }

    fn build_path(&self, version: &LLVMVersion) -> PathBuf {
        self.llvm_manager_path.join(format!("llvm-{version}.build"))
    }

    fn install_path(&self, version: &LLVMVersion) -> PathBuf {
        self.llvm_manager_path
            .join(format!("llvm-{version}.install"))
    }

    fn download_llvm_src(&self, item: &mut Item, version: &LLVMVersion) -> NexusR<PathBuf> {
        let item = item.add_child("Download::LLVM");
        let download_path = self.download_path(version);
        let src_path = self.src_path(version);

        if let Some(src_parent) = src_path.parent() {
            if !src_parent.exists() {
                std::fs::create_dir_all(src_parent).into_diagnostic()?;
            }
        }

        if let Some(download_parent) = download_path.parent() {
            if !download_parent.exists() {
                std::fs::create_dir_all(download_parent).into_diagnostic()?;
            }
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

    fn compile_llvm(
        &self,
        item: &mut Item,
        version: &LLVMVersion,
        src_path: &Path,
    ) -> NexusR<PathBuf> {
        let build_path = self.build_path(version);
        let install_path = self.install_path(version);
        let llvm_config = install_path.join("bin/llvm-config");

        'configure: {
            if build_path.exists() {
                if !install_path.exists() {
                    item.info(
                        "LLVM build path exists, but install path does not; continuing build",
                    );
                    break 'configure;
                }

                if !llvm_config.exists() {
                    item.fail("LLVM build path exists, but llvm-config does not");
                    bail!("LLVM build path exists, but llvm-config does not");
                }

                item.done("LLVM already built");
                return Ok(llvm_config);
            } else {
                if !src_path.exists() {
                    item.fail("LLVM source not found");
                    bail!("LLVM source not found");
                }

                std::fs::create_dir(&build_path).into_diagnostic()?;

                let mut cmake = std::process::Command::new("cmake");
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

        let mut build = std::process::Command::new("cmake");
        build
            .args(["--build", ".", "--target", "install"])
            .current_dir(&build_path);

        build
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        build.env("NINJA_STATUS", "$$NEXUS_STATUS:::%f::%t$$");

        item.info("Building LLVM");

        let mut build = build.spawn().into_diagnostic()?;

        let mut stdout = build.stdout.take().unwrap();
        let mut stderr = build.stderr.take().unwrap();

        let stderr = std::thread::spawn(move || {
            let mut v = Vec::new();
            stderr.read_to_end(&mut v).into_diagnostic()?;
            Ok::<_, miette::Report>(v)
        });
        let stdout = {
            let item = item.add_child("Build::LLVM");
            item.init(None, Some(unit::label("edges")));
            std::thread::spawn(move || {
                let mut out = String::new();

                let mut br = std::io::BufReader::new(stdout);
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
                        write!(&mut out, "[{}/{}] ", f, t).into_diagnostic()?;
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
            std::io::stderr()
                .write_all(&stderr.join().unwrap()?)
                .into_diagnostic()?;
            std::io::stdout()
                .write_all(&stdout.join().unwrap()?.as_bytes())
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
) -> NexusR<(String, PathBuf)> {
    let mut item = item.add_child("Build::LLVM");
    let progress_lock = build_progress
        .as_ref()
        .map(|bp| bp.make_progress_lock("Build::LLVM".to_owned(), Instant::now()))
        .transpose()?;
    let version = LLVMVersion {
        major: 19,
        minor: 1,
        patch: 7,
        extra: None,
    };
    let src_path = llvm_manager.download_llvm_src(&mut item, &version)?;
    let o = llvm_manager.compile_llvm(&mut item, &version, &src_path)?;
    let llvm_prefix = llvm_manager.llvm_sys_env(&version);
    drop(progress_lock);

    Ok(llvm_prefix)
}

fn build_compiler(
    profile: Profile,
    sys: &SysTarget,
    llvm_prefix_root: &(String, PathBuf),
    item: &mut Item,
    build_progress: Option<BuildCmdBuildingProgress>,
) -> NexusR<PathBuf> {
    let mut item = item.add_child("Build::Compiler");
    item.init(None, Some(unit::label("artifacts")));
    let output = cargo_interface::build()
        .packages(pkg_spec!("narxia-driver"))
        .build_targets([BuildTarget::Bin("narxia-driver".to_owned())])
        .profile(profile.cargo_name())
        .sys_target(sys.clone())
        .env(llvm_prefix_root.0.clone(), llvm_prefix_root.1.clone())
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
    item: &mut Item,
    build_progress: Option<BuildCmdBuildingProgress>,
) -> NexusR {
    let mut item = item.add_child("Build::Tests");
    item.init(None, Some(unit::label("artifacts")));
    cargo_interface::build()
        .profile(profile.cargo_name())
        .build_targets(vec![BuildTarget::Tests])
        .run(Some(&mut item), build_progress)?;

    item.done("Tests built");

    Ok(())
}

pub struct NarxiaBinaries {
    pub llvm_prefix_root: Option<(String, PathBuf)>,
    pub compiler: Option<PathBuf>,
}

pub struct BuildI {
    pub targets: Vec<Target>,
    pub profile: Profile,
    pub sys: SysTarget,
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
            llvm_prefix_root: None,
            compiler: None,
        };

        for target in self.targets {
            match target {
                Target::LLVM => {
                    bins.llvm_prefix_root =
                        Some(build_llvm(llvm_manager, item, build_progress.clone())?);
                }
                Target::Compiler => {
                    bins.compiler = Some(build_compiler(
                        self.profile,
                        &self.sys,
                        bins.llvm_prefix_root.as_ref().unwrap(),
                        item,
                        build_progress.clone(),
                    )?);
                }
                Target::Tests => {
                    build_tests(self.profile, item, build_progress.clone())?;
                }
            }
        }

        Ok(bins)
    }
}

pub trait NarxiaBinProvider<'bins> {
    fn get(&'bins self, target: Target) -> Option<&'bins PathBuf>;
}

impl<'bins> NarxiaBinProvider<'bins> for NarxiaBinaries {
    fn get(&'bins self, target: Target) -> Option<&'bins PathBuf> {
        match target {
            Target::LLVM => self.llvm_prefix_root.as_ref().map(|(_, p)| p),
            Target::Compiler => self.compiler.as_ref(),
            Target::Tests => None,
        }
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
                pub $field_bins: &'bins ::std::path::PathBuf,
            )*
            $(
                $(
                    pub $field_opt_bins: Option<&'bins ::std::path::PathBuf>,
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
                    $($field_bins: bins.get($crate::Target::$field_target).unwrap(),)*
                    $($($field_opt_bins: bins.get($crate::Target::$field_opt_target),)*)?
                    $($field_name: <$field_ty as NarxiaNeededBins<'bins>>::compile_from(bins),)*
                }
            }
        }

        impl<'bins> NarxiaBinProvider<'bins> for $name<'bins> {
            fn get(&'bins self, target: Target) -> Option<&'bins ::std::path::PathBuf> {
                match target {
                    $(
                        $crate::Target::$field_target => Some(self.$field_bins),
                    )*
                    _ => None,
                }
            }
        }
    }
}

use zip::write::FileOptions;
use zip::ZipWriter;

bin_type! {
    RunCompilerBins,
    RunCompilerCommand,
    :bins
        compiler(Compiler)
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
        let mut wr = ZipWriter::new(std::fs::File::create(&self.pkg).into_diagnostic()?);
        write_bin_file_to_zip(
            &mut wr,
            Path::new(bins.compiler.file_name().unwrap()),
            &bins.compiler,
            FileOptions::default(),
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
    options: FileOptions,
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
    let mut f = std::fs::File::open(path).into_diagnostic()?;

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
