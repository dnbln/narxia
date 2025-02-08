#![feature(string_from_utf8_lossy_owned)]
#![feature(decl_macro)]

use std::io::{Read, Seek, Write};
use std::path::{Path, PathBuf};
use std::time::Instant;

use bin_context::NexusContext;
use cargo_interface::{BuildCmdBuildingProgress, BuildTarget, RunCompilerCommand, SysTarget};
use clap::{Parser, Subcommand, ValueEnum};
use miette::{bail, IntoDiagnostic};
use narxia_dir_structures::dir_structure::DirStructureItem;
use narxia_dir_structures::ws_root;
use prodash::tree::Item;
use prodash::unit;

pub mod bin_context;
pub mod cargo_interface;

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
    pub fn run(self, cx: &NexusContext) -> NexusR {
        match self {
            Self::Build(cmd) => {
                let mut item = cx.new_child("Build");
                let bp =
                    BuildCmdBuildingProgress::new(item.add_child("Build progress"), Instant::now());
                cmd.run(&mut item, Some(bp))?;
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
    Compiler,
    Tests,
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
    pub fn run(self, item: &mut Item, build_progress: Option<BuildCmdBuildingProgress>) -> NexusR {
        let bins = BuildI {
            targets: self.targets,
            profile: self.profile.get_profile(),
            sys: SysTarget::Host,
        }
        .run(item, build_progress)?;

        Ok(())
    }
}

fn build_compiler(
    profile: Profile,
    sys: &SysTarget,
    item: &mut Item,
    build_progress: Option<BuildCmdBuildingProgress>,
) -> NexusR<PathBuf> {
    let mut item = item.add_child("Build::Compiler");
    item.init(None, Some(unit::label("artifacts")));
    let output = cargo_interface::build()
        .package("narxia-driver")
        .build_targets([BuildTarget::Bin("narxia-driver".to_owned())])
        .profile(profile.cargo_name())
        .sys_target(sys.clone())
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
        .package("narxia-test-runner")
        .profile(profile.cargo_name())
        .build_targets(vec![BuildTarget::Tests])
        .run(Some(&mut item), build_progress)?;

    item.done("Tests built");

    Ok(())
}

pub struct NarxiaBinaries {
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
        item: &mut Item,
        build_progress: Option<BuildCmdBuildingProgress>,
    ) -> NexusR<NarxiaBinaries> {
        self.targets.sort();
        self.targets.dedup();

        let mut bins = NarxiaBinaries { compiler: None };

        for target in self.targets {
            match target {
                Target::Compiler => {
                    bins.compiler = Some(build_compiler(
                        self.profile,
                        &self.sys,
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
