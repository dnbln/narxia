use std::ffi::OsString;
use std::fmt::Write;
use std::io;
use std::io::BufRead;
use std::io::Read;
use std::mem;
use std::path::PathBuf;
use std::process;
use std::sync::mpsc;
use std::sync::mpsc::TryRecvError;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::MutexGuard;
use std::thread;
use std::thread::JoinHandle;
use std::time;
use std::time::Instant;

use cargo_metadata::diagnostic;
use cargo_metadata::TargetKind;
use miette::bail;
use miette::IntoDiagnostic;
use owo_colors::OwoColorize;
use owo_colors::Stream::*;
use prodash::tree::Item;
use prodash::unit;

use crate::duration::NexusDuration;
use crate::LLVMPrefixInfo;
use crate::NexusR;

#[derive(Debug, Clone)]
pub enum PkgSpec {
    Packages(Vec<String>),
    WorkspaceExcluding(Vec<String>),
}

#[macro_export]
macro_rules! pkg_spec {
    ($($name:expr),* $(,)?) => {
        $crate::cargo_interface::PkgSpec::Packages(vec![$($name.to_string()),*])
    };
    (workspace excluding $($name:expr),* $(,)?) => {
        $crate::cargo_interface::PkgSpec::WorkspaceExcluding(vec![$($name.to_string()),*])
    };
}

impl Default for PkgSpec {
    fn default() -> Self {
        PkgSpec::WorkspaceExcluding(Vec::new())
    }
}

#[derive(Default, Clone, Debug)]
pub struct BuildCmd {
    packages: PkgSpec,
    profile: Option<String>,
    sys_target: SysTarget,
    envs: Vec<(OsString, OsString)>,
    targets: Vec<BuildTarget>,
    features: Vec<OsString>,
    config: BuildCmdConfig,
}

#[derive(Clone, Debug)]
pub struct BuildCmdConfig {
    pub print_dependency_artifacts: bool,
    pub print_fresh: bool,
    pub print_low_level_diagnostics: bool,
    pub use_ansi: bool,
}

impl Default for BuildCmdConfig {
    fn default() -> Self {
        Self {
            print_dependency_artifacts: false,
            print_fresh: false,
            print_low_level_diagnostics: false,
            use_ansi: true,
        }
    }
}

fn cargo_command() -> process::Command {
    process::Command::new("cargo")
}

fn async_read(mut r: impl Read + Send + 'static) -> thread::JoinHandle<String> {
    thread::spawn(move || {
        let mut v = Vec::new();
        io::copy(&mut r, &mut io::Cursor::new(&mut v)).unwrap();
        String::from_utf8(v).unwrap()
    })
}

#[derive(Debug, Clone)]
pub enum BuildTarget {
    Lib,
    Bin(String),
    Bins,
    Example(String),
    Examples,
    Test(String),
    Tests,
    Benchmark(String),
    Benchmarks,
    AllTargets,
}

#[derive(Debug, Clone, Default)]
pub enum SysTarget {
    #[default]
    Host,
    Target {
        name: String,
    },
}

impl BuildCmd {
    pub fn packages(mut self, packages: impl Into<PkgSpec>) -> Self {
        self.packages = packages.into();
        self
    }

    pub fn profile(mut self, profile: impl Into<String>) -> Self {
        self.profile = Some(profile.into());
        self
    }

    pub fn sys_target(mut self, sys_target: impl Into<SysTarget>) -> Self {
        self.sys_target = sys_target.into();
        self
    }

    pub fn env(mut self, k: impl Into<OsString>, v: impl Into<OsString>) -> Self {
        self.envs.push((k.into(), v.into()));
        self
    }

    pub fn feature(self, feature: impl Into<OsString>) -> Self {
        self.features([feature])
    }

    pub fn features<T: Into<OsString>>(mut self, features: impl IntoIterator<Item = T>) -> Self {
        self.features.extend(features.into_iter().map(Into::into));
        self
    }

    pub fn build_targets(mut self, targets: impl IntoIterator<Item = BuildTarget>) -> Self {
        self.targets = targets.into_iter().collect();
        self
    }

    pub fn config(mut self, config: BuildCmdConfig) -> Self {
        self.config = config;
        self
    }

    pub fn run(
        self,
        mut item: Option<&mut Item>,
        build_progress: Option<BuildCmdBuildingProgress>,
    ) -> NexusR<BuildCmdOutput> {
        let Self {
            packages,
            targets,
            profile,
            sys_target,
            envs,
            features,
            config,
        } = self;

        let ws_members = if !config.print_dependency_artifacts {
            cargo_metadata::MetadataCommand::new()
                .exec()
                .into_diagnostic()?
                .workspace_members
        } else {
            vec![]
        };

        let mut cmd = cargo_command();
        cmd.arg("build");

        match packages {
            PkgSpec::Packages(packages) => {
                for package in packages {
                    cmd.arg("--package").arg(package);
                }
            }
            PkgSpec::WorkspaceExcluding(excluded) => {
                cmd.arg("--workspace");
                for exclude in excluded {
                    cmd.arg("--exclude").arg(exclude);
                }
            }
        }

        for target in targets {
            match target {
                BuildTarget::Lib => {
                    cmd.arg("--lib");
                }
                BuildTarget::Bin(bin) => {
                    cmd.arg("--bin").arg(bin);
                }
                BuildTarget::Bins => {
                    cmd.arg("--bins");
                }
                BuildTarget::Example(example) => {
                    cmd.arg("--example").arg(example);
                }
                BuildTarget::Examples => {
                    cmd.arg("--examples");
                }
                BuildTarget::Test(test) => {
                    cmd.arg("--test").arg(test);
                }
                BuildTarget::Tests => {
                    cmd.arg("--tests");
                }
                BuildTarget::Benchmark(bench) => {
                    cmd.arg("--bench").arg(bench);
                }
                BuildTarget::Benchmarks => {
                    cmd.arg("--benchmarks");
                }
                BuildTarget::AllTargets => {
                    cmd.arg("--all-targets");
                }
            }
        }

        if let Some(profile) = profile {
            cmd.arg("--profile").arg(profile);
        }

        match sys_target {
            SysTarget::Host => {}

            SysTarget::Target { name } => {
                cmd.arg("--target").arg(name);
            }
        }

        for feature in features {
            cmd.arg("--features").arg(feature);
        }

        if config.use_ansi {
            cmd.arg("--message-format=json-diagnostic-rendered-ansi");
        } else {
            cmd.arg("--message-format=json");
        }

        for (k, v) in envs {
            cmd.env(k, v);
        }

        cmd.stdout(process::Stdio::piped())
            .stderr(process::Stdio::piped());

        let mut child = cmd.spawn().into_diagnostic()?;

        let stdout = mem::take(&mut child.stdout).unwrap();
        let stderr = mem::take(&mut child.stderr).unwrap();

        let stderr_handle = async_read(stderr);

        let mut target_artifact = None;

        let progress_lock = if let Some(build_progress) = &build_progress {
            let name = match &item {
                Some(item) => item.name().unwrap().to_string(),
                None => "Building".to_string(),
            };

            Some(build_progress.make_progress_lock(name, Instant::now())?)
        } else {
            None
        };

        let mut critical_diagnostics = String::new();
        let mut low_level_diagnostics = String::new();

        for message in cargo_metadata::Message::parse_stream(io::BufReader::new(stdout)) {
            let message = message.into_diagnostic()?;

            match message {
                cargo_metadata::Message::CompilerArtifact(artifact) => {
                    if let Some(item) = &mut item {
                        let extra = match artifact.target.kind.as_slice() {
                            [TargetKind::Bin] => " (bin)",
                            [TargetKind::Lib
                            | TargetKind::CDyLib
                            | TargetKind::DyLib
                            | TargetKind::StaticLib
                            | TargetKind::RLib] => "",
                            [TargetKind::Test] => " (test)",
                            [TargetKind::Example] => " (example)",
                            [TargetKind::Bench] => " (bench)",
                            [TargetKind::CustomBuild] => " (custom-build)",
                            [TargetKind::ProcMacro] => " (proc-macro)",
                            _ => " unknown",
                        };

                        if config.print_dependency_artifacts
                            || ws_members.contains(&artifact.package_id)
                        {
                            if config.print_fresh && artifact.fresh {
                                item.info(format!(
                                    "Artifact fresh: {}{extra}",
                                    artifact.target.name
                                ));
                            } else if !artifact.fresh {
                                item.info(format!(
                                    "Artifact built: {}{extra}",
                                    artifact.target.name
                                ));
                            }
                        }

                        item.inc();
                    }

                    if let Some(executable) =
                        artifact.executable.clone().map(|it| it.into_std_path_buf())
                    {
                        target_artifact = Some(TargetArtifactInfo {
                            executable,
                            was_fresh: artifact.fresh,
                        });
                    }
                }
                cargo_metadata::Message::CompilerMessage(compiler_message) => {
                    match compiler_message.message.level {
                        // always render ICE's and Errors
                        diagnostic::DiagnosticLevel::Ice => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            critical_diagnostics.push_str(rendered);
                            critical_diagnostics.push('\n');
                        }
                        diagnostic::DiagnosticLevel::Error => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            critical_diagnostics.push_str(rendered);
                            critical_diagnostics.push('\n');
                        }
                        diagnostic::DiagnosticLevel::Warning => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            if ws_members.contains(&compiler_message.package_id) {
                                low_level_diagnostics.push_str(rendered);
                                low_level_diagnostics.push('\n');
                            }
                        }
                        diagnostic::DiagnosticLevel::FailureNote => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            if ws_members.contains(&compiler_message.package_id) {
                                low_level_diagnostics.push_str(rendered);
                                low_level_diagnostics.push('\n');
                            }
                        }
                        diagnostic::DiagnosticLevel::Note => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            if ws_members.contains(&compiler_message.package_id) {
                                low_level_diagnostics.push_str(rendered);
                                low_level_diagnostics.push('\n');
                            }
                        }
                        diagnostic::DiagnosticLevel::Help => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            if ws_members.contains(&compiler_message.package_id) {
                                low_level_diagnostics.push_str(rendered);
                                low_level_diagnostics.push('\n');
                            }
                        }
                        _ => todo!(),
                    }
                }
                cargo_metadata::Message::BuildScriptExecuted(build_script) => {}
                cargo_metadata::Message::BuildFinished(build_finished) => {}
                cargo_metadata::Message::TextLine(_) => {}
                _ => todo!(),
            }
        }

        if config.print_low_level_diagnostics {
            eprintln!("{low_level_diagnostics}");
        }
        eprintln!("{critical_diagnostics}");

        let status = child.wait().into_diagnostic()?;

        if let Some(mut lock) = progress_lock {
            lock.finish()?;
        }

        let stderr = stderr_handle.join().unwrap();

        Ok(BuildCmdOutput {
            stderr,
            status,
            target_artifact,
        })
    }
}

#[derive(Debug, Clone)]
pub struct BuildCmdBuildingProgress {
    item: Arc<Mutex<ItemWrapper>>,
    start: Instant,
}

enum BuildEvent {
    Start { name: String, start: Instant },
    Finish,
    FinishAll,
}

impl BuildCmdBuildingProgress {
    pub fn new(item: Item, start: Instant) -> Self {
        Self {
            item: Arc::new(Mutex::new(ItemWrapper::new(item, start))),
            start,
        }
    }

    pub(crate) fn make_progress_lock(
        &self,
        name: String,
        start: Instant,
    ) -> NexusR<ItemWrapperFinishGuard> {
        ItemWrapper::start(self.item.lock().unwrap(), name, start)
    }
}

#[derive(Debug)]
struct ItemWrapper(
    mpsc::Sender<BuildEvent>,
    mem::ManuallyDrop<JoinHandle<()>>,
    bool,
);

impl ItemWrapper {
    fn new(item: Item, start: Instant) -> Self {
        let (tx, rx) = mpsc::channel();
        ItemWrapper(
            tx,
            mem::ManuallyDrop::new(thread::spawn(move || {
                let mut item = item;
                let old_name = item.name().unwrap();
                item.init(None, Some(unit::label("ms")));

                let building_start = start;
                let mut current_item_start = Instant::now();

                loop {
                    match rx.try_recv() {
                        Ok(BuildEvent::Start { name, start }) => {
                            item.set_name(name);
                            current_item_start = start;
                        }
                        Ok(BuildEvent::Finish) => {
                            item.done(format!(
                                "Building done in {}",
                                NexusDuration::since(current_item_start)
                            ));
                            item.set_name(old_name.clone());
                        }
                        Ok(BuildEvent::FinishAll) | Err(TryRecvError::Disconnected) => {
                            item.done(format!(
                                "Building done in {}",
                                NexusDuration::since(building_start)
                            ));
                            break;
                        }
                        Err(TryRecvError::Empty) => {}
                    }

                    let duration_since_start = Instant::now().duration_since(building_start);
                    let duration_millis: usize =
                        duration_since_start.as_millis().try_into().unwrap();

                    item.set(duration_millis);
                    thread::sleep(time::Duration::from_millis(73));
                }
            })),
            false,
        )
    }

    fn start(
        guard: MutexGuard<'_, Self>,
        name: String,
        start: Instant,
    ) -> NexusR<ItemWrapperFinishGuard<'_>> {
        guard
            .0
            .send(BuildEvent::Start { name, start })
            .into_diagnostic()?;
        Ok(ItemWrapperFinishGuard {
            item_wrapper: guard,
            finished: false,
        })
    }

    pub fn finish_all(&mut self) -> NexusR {
        if self.2 {
            return Ok(());
        }
        self.2 = true;
        self.0.send(BuildEvent::FinishAll).into_diagnostic()?;

        #[expect(unsafe_code)]
        unsafe {
            mem::ManuallyDrop::take(&mut self.1).join().unwrap();
        }

        Ok(())
    }
}

pub(crate) struct ItemWrapperFinishGuard<'a> {
    item_wrapper: MutexGuard<'a, ItemWrapper>,
    finished: bool,
}

impl ItemWrapperFinishGuard<'_> {
    fn finish(&mut self) -> NexusR {
        if self.finished {
            return Ok(());
        }

        self.finished = true;
        self.item_wrapper
            .0
            .send(BuildEvent::Finish)
            .into_diagnostic()?;

        Ok(())
    }
}

impl Drop for ItemWrapperFinishGuard<'_> {
    fn drop(&mut self) {
        self.finish().unwrap();
    }
}

impl Drop for ItemWrapper {
    fn drop(&mut self) {
        self.finish_all().unwrap();
    }
}

pub struct BuildCmdOutput {
    pub stderr: String,
    pub status: process::ExitStatus,
    pub target_artifact: Option<TargetArtifactInfo>,
}

pub struct TargetArtifactInfo {
    pub executable: PathBuf,
    pub was_fresh: bool,
}

pub fn build() -> BuildCmd {
    BuildCmd::default()
}

#[derive(Debug, Default)]
pub struct RunCompilerCommand {
    compiler: Option<PathBuf>,
    llvm: Option<LLVMPrefixInfo>,
    args: Vec<String>,
}

impl RunCompilerCommand {
    pub fn arg(&mut self, arg: impl Into<String>) -> &mut Self {
        self.args.push(arg.into());
        self
    }

    pub fn args(&mut self, args: impl IntoIterator<Item = impl Into<String>>) -> &mut Self {
        self.args.extend(args.into_iter().map(Into::into));
        self
    }

    pub fn compiler(&mut self, bin: impl Into<PathBuf>) -> &mut Self {
        self.compiler = Some(bin.into());
        self
    }

    pub fn llvm(&mut self, llvm: impl Into<LLVMPrefixInfo>) -> &mut Self {
        self.llvm = Some(llvm.into());
        self
    }

    pub fn run(&self, mut item: Item) -> NexusR {
        let mut cmd = process::Command::new(self.compiler.as_ref().unwrap());

        let (k, v) = self.llvm.as_ref().unwrap().to_env();
        cmd.env(k, v);

        cmd.args(&self.args);

        cmd.stdout(process::Stdio::inherit())
            .stderr(process::Stdio::inherit());

        let mut child = cmd.spawn().into_diagnostic()?;
        let status = child.wait().into_diagnostic()?;

        if !status.success() {
            item.fail("running failed");
            bail!("running failed");
        }

        Ok(())
    }
}

pub mod tests {
    use core::fmt;
    use std::io;
    use std::process;
    use std::str;
    use std::thread;

    use owo_colors::Style;

    use super::*;
    use crate::NexusOutputGroups;

    pub fn list_tests(
        filter: Option<&String>,
        env: impl IntoIterator<Item = (OsString, OsString)>,
    ) -> NexusR<nextest_metadata::TestListSummary> {
        let mut cmd = cargo_command();
        cmd.arg("nextest")
            .args(["list", "--message-format", "json", "--workspace"]);

        if let Some(filter) = filter {
            cmd.arg("-E").arg(filter);
        }

        cmd.stdout(process::Stdio::piped())
            .stderr(process::Stdio::piped());

        cmd.envs(env);

        let mut proc = cmd.spawn().into_diagnostic()?;

        let stdout = proc.stdout.take().unwrap();
        let stderr = proc.stderr.take().unwrap();

        let stdout = async_read(stdout);
        let stderr = async_read(stderr);

        let result = proc.wait().into_diagnostic()?;
        let stdout = stdout.join().unwrap();

        if !result.success() {
            let stderr = stderr.join().unwrap();
            bail!("cargo nextest list failed:\n{stderr}");
        }

        let summary = nextest_metadata::TestListSummary::parse_json(&stdout).into_diagnostic()?;

        Ok(summary)
    }

    pub struct RunTests {
        filter: Option<String>,
        capture_nextest_stderr: bool,
        fail_fast: bool,
        profile: String,
        parser_tests_mode: ParserTestsMode,
        envs: Vec<(OsString, OsString)>,
        debug_nextest_messages: bool,
        miri: bool,
    }

    #[derive(Default)]
    pub enum ParserTestsMode {
        #[default]
        Check,
        Overwrite,
    }

    impl Default for RunTests {
        fn default() -> Self {
            Self::new()
        }
    }

    impl RunTests {
        pub fn new() -> Self {
            Self {
                filter: None,
                capture_nextest_stderr: true,
                fail_fast: true,
                profile: "dev".to_string(),
                parser_tests_mode: ParserTestsMode::default(),
                envs: Vec::new(),
                debug_nextest_messages: false,
                miri: false,
            }
        }

        pub fn filter(mut self, filter: impl Into<Option<String>>) -> Self {
            self.filter = filter.into();
            self
        }

        pub fn capture_nextest_output(mut self, capture: bool) -> Self {
            self.capture_nextest_stderr = capture;
            self
        }

        pub fn fail_fast(mut self, fail_fast: bool) -> Self {
            self.fail_fast = fail_fast;
            self
        }

        pub fn profile(mut self, profile: impl Into<String>) -> Self {
            self.profile = profile.into();
            self
        }

        pub fn env(mut self, k: impl Into<OsString>, v: impl Into<OsString>) -> Self {
            self.envs.push((k.into(), v.into()));
            self
        }

        pub fn parser_tests(mut self, mode: ParserTestsMode) -> Self {
            self.parser_tests_mode = mode;
            self
        }

        pub fn debug_nextest_messages(mut self, debug: bool) -> Self {
            self.debug_nextest_messages = debug;
            self
        }

        pub fn miri(mut self, miri: bool) -> Self {
            self.miri = miri;
            self
        }

        pub fn run(self, item: Option<&mut Item>, groups: Option<&NexusOutputGroups>) -> NexusR {
            let mut cmd = cargo_command();
            if self.miri {
                cmd.arg("miri");
            }
            cmd.arg("nextest")
                .args([
                    "run",
                    "--message-format",
                    "libtest-json-plus",
                    "--workspace",
                ])
                .env("NEXTEST_EXPERIMENTAL_LIBTEST_JSON", "1")
                .env(
                    "NARXIA_PARSER_SNAPSHOTS_TEST_MODE",
                    match self.parser_tests_mode {
                        ParserTestsMode::Check => "check",
                        ParserTestsMode::Overwrite => "overwrite",
                    },
                )
                .env("NARXIA_TEST_GUARD", "1")
                .envs(self.envs);

            if let Some(filter) = &self.filter {
                cmd.arg("-E").arg(filter);
            }

            if !self.fail_fast {
                cmd.arg("--no-fail-fast");
            }

            cmd.stdout(process::Stdio::piped());

            if self.capture_nextest_stderr {
                cmd.stderr(process::Stdio::piped());
            }

            let mut child = cmd.spawn().into_diagnostic()?;

            let start_time = Instant::now();

            let stdout = child.stdout.take().unwrap();

            let stderr_join = if self.capture_nextest_stderr {
                let stderr = child.stderr.take().unwrap();
                Some(thread::spawn(move || {
                    let mut stderr = stderr;
                    loop {
                        let num = stderr.read(&mut [0; 1024]);
                        match num {
                            Ok(0) => break,
                            Ok(_) => {}
                            Err(e) if e.kind() == io::ErrorKind::BrokenPipe => {
                                break;
                            }
                            Err(e) => {
                                eprintln!("Error reading stderr: {e}");
                                break;
                            }
                        }
                    }
                }))
            } else {
                None
            };

            struct TestResult {
                suite: String,
                test: String,
                result: TestResultKind,
            }

            enum TestResultKind {
                Passed {
                    exec_time: f64,
                },
                Failed {
                    exec_time: f64,
                    info: TestFailedInfo,
                },
                Ignored,
                Measured,
                FilteredOut,
            }

            let mut tests = Vec::new();
            let enable_tree = item.is_some();

            struct Summary {
                passed: usize,
                failed: usize,
                ignored: usize,
                measured: usize,
                filtered_out: usize,

                total_time: f64,
            }

            let mut summary = Summary {
                passed: 0,
                failed: 0,
                ignored: 0,
                measured: 0,
                filtered_out: 0,
                total_time: 0.0,
            };

            let _tests = groups.map(|g| g.begin("Test results"));

            for message in io::BufReader::new(stdout).lines() {
                let message = message.into_diagnostic()?;
                if self.debug_nextest_messages {
                    println!("{}", message);
                }
                let line: OutputLine = serde_json::from_str(&message).into_diagnostic()?;

                match line {
                    OutputLine::Test(test_event) => match test_event {
                        TestEvent::Started { name } => {}
                        TestEvent::Ok { name, exec_time } => {
                            let (suite, test) = name.split_once('$').unwrap();
                            tests.push(TestResult {
                                suite: suite.to_owned(),
                                test: test.to_owned(),
                                result: TestResultKind::Passed { exec_time },
                            });
                            item.as_deref().map(Item::inc);
                        }
                        TestEvent::Failed {
                            name,
                            exec_time,
                            info,
                        } => {
                            let (suite, test) = name.split_once('$').unwrap();

                            tests.push(TestResult {
                                suite: suite.to_owned(),
                                test: test.to_owned(),
                                result: TestResultKind::Failed { exec_time, info },
                            });
                            item.as_deref().map(Item::inc);
                        }
                    },
                    OutputLine::Suite(suite_event) => match suite_event {
                        SuiteEvent::Started {
                            test_count,
                            nextest,
                        } => {}
                        SuiteEvent::Ok {
                            exec_time,
                            passed,
                            failed,
                            ignored,
                            measured,
                            filtered_out,
                            nextest,
                        } => {
                            summary.passed += passed;
                            summary.failed += failed;
                            summary.ignored += ignored;
                            summary.measured += measured;
                            summary.filtered_out += filtered_out;
                            summary.total_time += exec_time;
                        }
                        SuiteEvent::Failed {
                            exec_time,
                            passed,
                            failed,
                            ignored,
                            measured,
                            filtered_out,
                            nextest,
                        } => {
                            summary.passed += passed;
                            summary.failed += failed;
                            summary.ignored += ignored;
                            summary.measured += measured;
                            summary.filtered_out += filtered_out;
                            summary.total_time += exec_time;
                        }
                    },
                }
            }

            if let Some(stderr_join) = stderr_join {
                stderr_join.join().unwrap();
            }

            let out_stream = Stdout;

            struct TestStatusHeader<'a>(&'a str, &'a str, owo_colors::Stream);
            impl fmt::Display for TestStatusHeader<'_> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(
                        f,
                        "{} {}{}{}",
                        "test".if_supports_color(self.2, |text| text.blue()),
                        self.0.if_supports_color(self.2, |text| text.yellow()),
                        "::".if_supports_color(self.2, |text| text.purple()),
                        self.1.if_supports_color(self.2, |text| text.cyan()),
                    )
                }
            }

            let mut out = String::new();

            for TestResult {
                suite,
                test,
                result,
            } in tests
                .iter()
                .filter(|f| !matches!(f.result, TestResultKind::Failed { .. }))
            {
                match result {
                    TestResultKind::Passed { exec_time } => {
                        writeln!(
                            &mut out,
                            "{} ... {}",
                            TestStatusHeader(suite, test, out_stream),
                            format_args!("ok ({exec_time:.3}s)")
                                .if_supports_color(out_stream, |t| {
                                    Style::new().bright_green().bold().style(t)
                                }),
                        )
                        .unwrap();
                    }
                    TestResultKind::Failed { exec_time, info } => unreachable!(),
                    TestResultKind::Ignored => {
                        writeln!(
                            &mut out,
                            "{} ... {}",
                            TestStatusHeader(suite, test, out_stream),
                            "ignored".if_supports_color(out_stream, |t| Style::new()
                                .bright_white()
                                .bold()
                                .style(t)),
                        )
                        .unwrap();
                    }
                    TestResultKind::Measured => {
                        writeln!(
                            &mut out,
                            "{} ... {}",
                            TestStatusHeader(suite, test, out_stream),
                            "measured".if_supports_color(out_stream, |t| Style::new()
                                .bright_blue()
                                .bold()
                                .style(t)),
                        )
                        .unwrap();
                    }
                    TestResultKind::FilteredOut => {
                        writeln!(
                            &mut out,
                            "{} ... {}",
                            TestStatusHeader(suite, test, out_stream),
                            "filtered out".if_supports_color(out_stream, |t| Style::new()
                                .yellow()
                                .bold()
                                .style(t)),
                        )
                        .unwrap();
                    }
                }
            }

            for TestResult {
                suite,
                test,
                result,
            } in tests
                .iter()
                .filter(|f| matches!(f.result, TestResultKind::Failed { .. }))
            {
                let TestResultKind::Failed { exec_time, info } = result else {
                    unreachable!()
                };

                writeln!(
                    &mut out,
                    "{} ... {}",
                    TestStatusHeader(suite, test, out_stream),
                    format_args!("FAILED ({exec_time:.3}s)").if_supports_color(out_stream, |t| {
                        Style::new().bright_red().bold().style(t)
                    }),
                )
                .unwrap();
                match info {
                    TestFailedInfo::Fail { stdout } => {
                        writeln!(&mut out, "stdout:").unwrap();
                        writeln!(&mut out, "{stdout}").unwrap();
                    }
                    TestFailedInfo::Reason { reason } => {
                        writeln!(&mut out, "reason: {reason}").unwrap();
                    }
                }
            }

            println!("{out}\n");
            drop(_tests);

            if let Some(item) = item {
                let initial = match (summary.passed, summary.failed) {
                    (0, 0) => "no tests run".to_string(),
                    (0, f) => format!("failed {f} tests"),
                    (p, 0) => format!("passed {p} tests"),
                    (p, f) => format!("passed {p} tests, failed {f} tests"),
                };

                let end_time = Instant::now();
                let duration = end_time.duration_since(start_time);
                let secs = duration.as_secs_f64();

                let msg = format!(
                    "{initial}{ignored}{measured}{filtered_out} in {secs:.3}s",
                    ignored = if summary.ignored > 0 {
                        format!(", ignored {}", summary.ignored)
                    } else {
                        String::new()
                    },
                    measured = if summary.measured > 0 {
                        format!(", measured {}", summary.measured)
                    } else {
                        String::new()
                    },
                    filtered_out = if summary.filtered_out > 0 {
                        format!(", filtered out {}", summary.filtered_out)
                    } else {
                        String::new()
                    },
                );
                if summary.failed == 0 {
                    item.done(msg);
                } else {
                    item.fail(msg);
                }
            }

            if summary.failed != 0 {
                bail!("tests failed");
            }

            Ok(())
        }
    }

    #[derive(Debug, serde::Deserialize)]
    #[serde(tag = "type", rename_all = "kebab-case")]
    enum OutputLine {
        Test(TestEvent),
        Suite(SuiteEvent),
    }

    #[derive(Debug, serde::Deserialize)]
    #[serde(tag = "event", rename_all = "kebab-case")]
    enum TestEvent {
        Started {
            name: String,
        },
        Ok {
            name: String,
            exec_time: f64,
        },
        Failed {
            name: String,
            exec_time: f64,
            #[serde(flatten)]
            info: TestFailedInfo,
        },
    }

    #[derive(Debug, serde::Deserialize)]
    #[serde(untagged)]
    enum TestFailedInfo {
        Fail { stdout: String },
        Reason { reason: String },
    }

    #[derive(Debug, serde::Deserialize)]
    #[serde(tag = "event", rename_all = "kebab-case")]
    enum SuiteEvent {
        Started {
            test_count: usize,
            nextest: SuiteNextest,
        },
        Ok {
            exec_time: f64,
            passed: usize,
            failed: usize,
            ignored: usize,
            measured: usize,
            filtered_out: usize,
            nextest: SuiteNextest,
        },
        Failed {
            exec_time: f64,
            passed: usize,
            failed: usize,
            ignored: usize,
            measured: usize,
            filtered_out: usize,
            nextest: SuiteNextest,
        },
    }

    #[derive(Debug, serde::Deserialize)]
    struct SuiteNextest {
        #[serde(rename = "crate")]
        krate: String,
        test_binary: String,
        kind: String,
    }
}

pub struct LintConfig {
    pub use_ansi: bool,
}

pub struct Lint {
    config: LintConfig,
    env: Vec<(OsString, OsString)>,
    fix: bool,
}

impl Lint {
    pub fn new(config: LintConfig) -> Self {
        Self {
            config,
            env: Vec::new(),
            fix: false,
        }
    }

    pub fn fix(mut self, fix: bool) -> Self {
        self.fix = fix;
        self
    }

    pub fn env(mut self, k: impl Into<OsString>, v: impl Into<OsString>) -> Self {
        self.env.push((k.into(), v.into()));
        self
    }

    pub fn run(&self, item: &mut Item) -> NexusR {
        let Self { config, env, fix } = self;
        let mut cmd = cargo_command();
        cmd.arg("clippy")
            .arg("--workspace")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::piped());

        for (k, v) in env {
            cmd.env(k, v);
        }

        if *fix {
            cmd.arg("--fix");
        }

        if config.use_ansi {
            cmd.arg("--message-format=json-diagnostic-rendered-ansi");
        } else {
            cmd.arg("--message-format=json");
        }

        let mut child = cmd.spawn().into_diagnostic()?;

        let stdout = mem::take(&mut child.stdout).unwrap();
        let stderr = mem::take(&mut child.stderr).unwrap();

        let stderr_handle = async_read(stderr);

        let mut diagnostics = String::new();

        for message in cargo_metadata::Message::parse_stream(io::BufReader::new(stdout)) {
            let message = message.into_diagnostic()?;

            if let cargo_metadata::Message::CompilerMessage(compiler_message) = message {
                match compiler_message.message.level {
                    diagnostic::DiagnosticLevel::Error => {
                        let rendered = compiler_message.message.rendered.as_ref().unwrap();
                        diagnostics.push_str(rendered);
                        diagnostics.push_str("\n\n\n");
                    }
                    diagnostic::DiagnosticLevel::Warning => {
                        let rendered = compiler_message.message.rendered.as_ref().unwrap();
                        eprintln!("{rendered}\n\n");
                    }
                    _ => {}
                }
            }
        }

        eprintln!("{diagnostics}\n\n");

        let status = child.wait().into_diagnostic()?;

        let stderr = stderr_handle.join().unwrap();

        if !status.success() {
            item.fail("clippy failed");
            eprintln!("{stderr}");
            bail!("clippy failed");
        }

        Ok(())
    }
}

pub struct Format {
    check: bool,
}

impl Default for Format {
    fn default() -> Self {
        Self::new()
    }
}

impl Format {
    pub fn new() -> Self {
        Self { check: false }
    }

    pub fn check(mut self, check: bool) -> Self {
        self.check = check;
        self
    }

    pub fn run(&self, item: &mut Item) -> NexusR {
        let mut cmd = cargo_command();
        cmd.arg("fmt").arg("--all");

        if self.check {
            cmd.arg("--check");
        }

        let mut child = cmd.spawn().into_diagnostic()?;
        let status = child.wait().into_diagnostic()?;

        if !status.success() {
            eprintln!("Formatting failed");
            bail!("format failed");
        }

        Ok(())
    }
}
