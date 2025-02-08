use std::fmt::Write;
use std::io::{BufRead, Read};
use std::path::PathBuf;
use std::sync::mpsc::TryRecvError;
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use cargo_metadata::TargetKind;
use miette::{bail, IntoDiagnostic};
use owo_colors::OwoColorize;
use owo_colors::Stream::*;
use prodash::tree::Item;
use prodash::unit;

use crate::duration::NexusDuration;
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
    targets: Vec<BuildTarget>,
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

fn cargo_command() -> std::process::Command {
    std::process::Command::new("cargo")
}

fn async_read(mut r: impl Read + Send + 'static) -> std::thread::JoinHandle<String> {
    std::thread::spawn(move || {
        let mut v = Vec::new();
        std::io::copy(&mut r, &mut std::io::Cursor::new(&mut v)).unwrap();
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

        if config.use_ansi {
            cmd.arg("--message-format=json-diagnostic-rendered-ansi");
        } else {
            cmd.arg("--message-format=json");
        }

        cmd.stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        let mut child = cmd.spawn().into_diagnostic()?;

        let stdout = std::mem::take(&mut child.stdout).unwrap();
        let stderr = std::mem::take(&mut child.stderr).unwrap();

        let stderr_handle = async_read(stderr);

        let mut target_artifact = None;

        let progress_lock = if let Some(build_progress) = &build_progress {
            let name = match &item {
                Some(item) => item.name().unwrap().to_string(),
                None => "Building".to_string(),
            };
            let lock = build_progress.item.lock().unwrap();

            Some(ItemWrapper::start(lock, name, Instant::now())?)
        } else {
            None
        };

        let mut critical_diagnostics = String::new();
        let mut low_level_diagnostics = String::new();

        for message in cargo_metadata::Message::parse_stream(std::io::BufReader::new(stdout)) {
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
                            _ => "unknown",
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
                        cargo_metadata::diagnostic::DiagnosticLevel::Ice => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            critical_diagnostics.push_str(rendered);
                            critical_diagnostics.push('\n');
                        }
                        cargo_metadata::diagnostic::DiagnosticLevel::Error => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            critical_diagnostics.push_str(rendered);
                            critical_diagnostics.push('\n');
                        }
                        cargo_metadata::diagnostic::DiagnosticLevel::Warning => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            if ws_members.contains(&compiler_message.package_id) {
                                low_level_diagnostics.push_str(rendered);
                                low_level_diagnostics.push('\n');
                            }
                        }
                        cargo_metadata::diagnostic::DiagnosticLevel::FailureNote => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            if ws_members.contains(&compiler_message.package_id) {
                                low_level_diagnostics.push_str(rendered);
                                low_level_diagnostics.push('\n');
                            }
                        }
                        cargo_metadata::diagnostic::DiagnosticLevel::Note => {
                            let rendered = compiler_message.message.rendered.as_ref().unwrap();
                            if ws_members.contains(&compiler_message.package_id) {
                                low_level_diagnostics.push_str(rendered);
                                low_level_diagnostics.push('\n');
                            }
                        }
                        cargo_metadata::diagnostic::DiagnosticLevel::Help => {
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
}

#[derive(Debug)]
struct ItemWrapper(
    std::sync::mpsc::Sender<BuildEvent>,
    std::mem::ManuallyDrop<JoinHandle<()>>,
    bool,
);

impl ItemWrapper {
    fn new(item: Item, start: Instant) -> Self {
        let (tx, rx) = std::sync::mpsc::channel();
        ItemWrapper(
            tx,
            std::mem::ManuallyDrop::new(std::thread::spawn(move || {
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
                    std::thread::sleep(std::time::Duration::from_millis(73));
                }
            })),
            false,
        )
    }

    fn start<'a>(
        guard: MutexGuard<'a, Self>,
        name: String,
        start: Instant,
    ) -> NexusR<ItemWrapperFinishGuard<'a>> {
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

        unsafe {
            std::mem::ManuallyDrop::take(&mut self.1).join().unwrap();
        }

        Ok(())
    }
}

struct ItemWrapperFinishGuard<'a> {
    item_wrapper: MutexGuard<'a, ItemWrapper>,
    finished: bool,
}

impl<'a> ItemWrapperFinishGuard<'a> {
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

impl<'a> Drop for ItemWrapperFinishGuard<'a> {
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
    pub status: std::process::ExitStatus,
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
    bin: Option<PathBuf>,
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

    pub fn bin(&mut self, bin: impl Into<PathBuf>) -> &mut Self {
        self.bin = Some(bin.into());
        self
    }

    pub fn run(&self, mut item: Item) -> NexusR {
        let mut cmd = match &self.bin {
            Some(bin) => std::process::Command::new(bin),
            None => {
                let mut cmd = cargo_command();
                cmd.arg("run")
                    .args(["-p", "narxia-driver", "--bin", "narxia-driver", "--"]);
                cmd
            }
        };

        cmd.args(&self.args);

        cmd.stdout(std::process::Stdio::inherit())
            .stderr(std::process::Stdio::inherit());

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

    use owo_colors::Style;

    use super::*;

    pub fn list_tests(filter: Option<&String>) -> NexusR<nextest_metadata::TestListSummary> {
        let mut cmd = cargo_command();
        cmd.arg("nextest")
            .args(["list", "--message-format", "json", "--all"]);

        if let Some(filter) = filter {
            cmd.arg("-E").arg(filter);
        }

        cmd.stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        let output = cmd.output().into_diagnostic()?;

        if !output.status.success() {
            bail!(
                "cargo nextest list failed:\n{}",
                String::from_utf8_lossy_owned(output.stderr)
            );
        }

        let stdout = std::str::from_utf8(&output.stdout).into_diagnostic()?;
        let summary = nextest_metadata::TestListSummary::parse_json(stdout).into_diagnostic()?;

        Ok(summary)
    }

    pub struct RunTests {
        filter: Option<String>,
        capture_nextest_stderr: bool,
        fail_fast: bool,
        profile: String,
        parser_tests_mode: ParserTestsMode,
    }

    pub enum ParserTestsMode {
        Check,
        Overwrite,
    }

    impl Default for ParserTestsMode {
        fn default() -> Self {
            ParserTestsMode::Check
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

        pub fn parser_tests(mut self, mode: ParserTestsMode) -> Self {
            self.parser_tests_mode = mode;
            self
        }

        pub fn run(self, mut item: Option<&mut Item>) -> NexusR {
            let mut cmd = cargo_command();
            cmd.arg("nextest")
                .args(["run", "--message-format", "libtest-json-plus", "--all"])
                .env("NEXTEST_EXPERIMENTAL_LIBTEST_JSON", "1")
                .env(
                    "NARXIA_PARSER_SNAPSHOTS_TEST_MODE",
                    match self.parser_tests_mode {
                        ParserTestsMode::Check => "check",
                        ParserTestsMode::Overwrite => "overwrite",
                    },
                )
                .env("NARXIA_TEST_GUARD", "1");

            if let Some(filter) = &self.filter {
                cmd.arg("-E").arg(filter);
            }

            if !self.fail_fast {
                cmd.arg("--no-fail-fast");
            }

            cmd.stdout(std::process::Stdio::piped());
            cmd.stderr(std::process::Stdio::piped());

            let mut child = cmd.spawn().into_diagnostic()?;

            let start_time = Instant::now();

            let stdout = std::mem::take(&mut child.stdout).unwrap();
            let stderr = std::mem::take(&mut child.stderr).unwrap();

            {
                let capture = self.capture_nextest_stderr;
                std::thread::spawn(move || {
                    let mut stderr = stderr;
                    if capture {
                        let _ = std::io::copy(&mut stderr, &mut std::io::sink());
                    } else {
                        let _ = std::io::copy(&mut stderr, &mut std::io::stderr());
                    }
                });
            }

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

            for message in std::io::BufReader::new(stdout).lines() {
                let message = message.into_diagnostic()?;
                // println!("{}", message);
                let line: OutputLine = serde_json::from_str(&message).into_diagnostic()?;

                match line {
                    OutputLine::Test(test_event) => match test_event {
                        TestEvent::Started { name } => {}
                        TestEvent::Ok { name, exec_time } => {
                            if let Some(item) = &mut item {
                                let (suite, test) = name.split_once('$').unwrap();
                                tests.push(TestResult {
                                    suite: suite.to_owned(),
                                    test: test.to_owned(),
                                    result: TestResultKind::Passed { exec_time },
                                });
                                item.inc();
                            }
                        }
                        TestEvent::Failed {
                            name,
                            exec_time,
                            info,
                        } => {
                            if let Some(item) = &mut item {
                                let (suite, test) = name.split_once('$').unwrap();

                                tests.push(TestResult {
                                    suite: suite.to_owned(),
                                    test: test.to_owned(),
                                    result: TestResultKind::Failed { exec_time, info },
                                });

                                item.inc();
                            }
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

            struct TestStatusHeader<'a>(&'a str, &'a str);
            impl fmt::Display for TestStatusHeader<'_> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(
                        f,
                        "{} {}{}{}",
                        "test".if_supports_color(Stderr, |text| text.blue()),
                        self.0.if_supports_color(Stderr, |text| text.yellow()),
                        "::".if_supports_color(Stderr, |text| text.purple()),
                        self.1.if_supports_color(Stderr, |text| text.cyan()),
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
                            TestStatusHeader(suite, test),
                            format_args!("ok ({exec_time:.3}s)").if_supports_color(Stderr, |t| {
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
                            TestStatusHeader(suite, test),
                            "ignored".if_supports_color(Stderr, |t| Style::new()
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
                            TestStatusHeader(suite, test),
                            "measured".if_supports_color(Stderr, |t| Style::new()
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
                            TestStatusHeader(suite, test),
                            "filtered out".if_supports_color(Stderr, |t| Style::new()
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
                    TestStatusHeader(suite, test),
                    format_args!("FAILED ({exec_time:.3}s)")
                        .if_supports_color(Stderr, |t| Style::new().bright_red().bold().style(t)),
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

            eprintln!("{out}\n");

            if let Some(item) = item {
                let initial = match (summary.passed, summary.failed) {
                    (0, 0) => format!("no tests run"),
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
