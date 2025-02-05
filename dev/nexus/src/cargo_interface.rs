use std::io::{BufRead, Read, Write};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime};

use cargo_metadata::TargetKind;
use miette::{bail, IntoDiagnostic};
use prodash::tree::Item;
use prodash::unit;

use crate::NexusR;

#[derive(Default, Clone, Debug)]
pub struct BuildCmd {
    package: Option<String>,
    binary: Option<String>,
    profile: Option<String>,
    config: BuildCmdConfig,
}

#[derive(Clone, Debug)]
pub struct BuildCmdConfig {
    pub print_dependency_artifacts: bool,
    pub print_fresh: bool,
}

impl Default for BuildCmdConfig {
    fn default() -> Self {
        Self {
            print_dependency_artifacts: false,
            print_fresh: false,
        }
    }
}

fn cargo_command() -> std::process::Command {
    std::process::Command::new("cargo")
}

impl BuildCmd {
    pub fn package(mut self, package: impl Into<String>) -> Self {
        self.package = Some(package.into());
        self
    }

    pub fn binary(mut self, binary: impl Into<String>) -> Self {
        self.binary = Some(binary.into());
        self
    }

    pub fn profile(mut self, profile: impl Into<String>) -> Self {
        self.profile = Some(profile.into());
        self
    }

    pub fn run(
        self,
        mut item: Option<&mut Item>,
        build_progress: Option<BuildCmdBuildingProgress>,
    ) -> NexusR<BuildCmdOutput> {
        let Self {
            package,
            binary,
            profile,
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

        if let Some(package) = package {
            cmd.arg("--package").arg(package);
        }

        if let Some(binary) = binary {
            cmd.arg("--bin").arg(binary);
        }

        if let Some(profile) = profile {
            cmd.arg("--profile").arg(profile);
        }

        cmd.arg("--message-format=json-render-diagnostics");

        cmd.stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        let mut child = cmd.spawn().into_diagnostic()?;

        let stdout = std::mem::take(&mut child.stdout).unwrap();
        let mut stderr = std::mem::take(&mut child.stderr).unwrap();

        let mut target_artifact = None;

        let progress_item = if let Some(bp) = build_progress {
            item.as_mut().map(|item| {
                let name = item.name().unwrap();

                let (tx, rx) = std::sync::mpsc::channel();

                (
                    tx,
                    std::thread::spawn(move || {
                        let mut progress_item = bp.item.lock().unwrap();
                        let old_name = progress_item.0.name().unwrap();
                        progress_item.0.init(None, Some(unit::label("ms")));
                        progress_item.0.set_name(name);

                        let building_start = bp.start;
                        let self_building_start = Instant::now();
                        loop {
                            let duration = Instant::now().duration_since(building_start);
                            let duration_millis: usize = duration.as_millis().try_into().unwrap();
                            match rx.try_recv() {
                                Ok(()) => {
                                    let duration_since_start =
                                        Instant::now().duration_since(self_building_start);
                                    let duration_millis: usize =
                                        duration_since_start.as_millis().try_into().unwrap();
                                    let duration = Duration::from_millis(duration_millis as u64);
                                    progress_item.0.done(format!(
                                        "Building done in {}",
                                        humantime::Duration::from(duration)
                                    ));
                                    progress_item.0.set_name(old_name);
                                    break;
                                }
                                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                                    progress_item.0.set_name(old_name);
                                    break;
                                }
                            }
                            progress_item.0.set(duration_millis);
                            std::thread::sleep(std::time::Duration::from_millis(73));
                        }
                    }),
                )
            })
        } else {
            None
        };

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
                cargo_metadata::Message::CompilerMessage(compiler_message) => {}
                cargo_metadata::Message::BuildScriptExecuted(build_script) => {}
                cargo_metadata::Message::BuildFinished(build_finished) => {}
                cargo_metadata::Message::TextLine(_) => {}
                _ => todo!(),
            }
        }

        let status = child.wait().into_diagnostic()?;

        if let Some((tx, handle)) = progress_item {
            tx.send(()).into_diagnostic()?;
            let _ = handle.join();
        }

        let mut stderr_string = String::new();
        stderr
            .read_to_string(&mut stderr_string)
            .into_diagnostic()?;

        Ok(BuildCmdOutput {
            stderr: stderr_string,
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

impl BuildCmdBuildingProgress {
    pub fn new(item: Item, start: Instant) -> Self {
        Self {
            item: Arc::new(Mutex::new(ItemWrapper(item, Instant::now()))),
            start,
        }
    }
}

#[derive(Debug)]
struct ItemWrapper(Item, Instant);

impl Drop for ItemWrapper {
    fn drop(&mut self) {
        let duration = Instant::now().duration_since(self.1);
        let duration_millis: usize = duration.as_millis().try_into().unwrap();
        let duration = Duration::from_millis(duration_millis as u64);
        self.0.done(format!(
            "Building done in {}",
            humantime::Duration::from(duration)
        ));
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
                );

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

            let mut current_suite = None::<(Item, SuiteNextest)>;
            let mut current_running_tests = Vec::new();
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
                        TestEvent::Started { name } => {
                            if enable_tree {
                                let (suite, test) = name.split_once('$').unwrap();
                                let test_item =
                                    current_suite.as_mut().unwrap().0.add_child(format!("Test"));
                                test_item.init(Some(1), None);
                                current_running_tests.push((name, test_item));
                            }
                        }
                        TestEvent::Ok { name, exec_time } => {
                            if let Some(item) = &mut item {
                                let (suite, test) = name.split_once('$').unwrap();
                                let pos = current_running_tests
                                    .iter()
                                    .position(|(n, _)| *n == name)
                                    .unwrap();
                                let (_, mut test_item) = current_running_tests.remove(pos);
                                test_item.inc();
                                test_item
                                    .done(format!("[OK]   in {exec_time:.3}s: {suite}::{test}"));
                                current_suite.as_mut().unwrap().0.inc();

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
                                let pos = current_running_tests
                                    .iter()
                                    .position(|(n, _)| *n == name)
                                    .unwrap();
                                let (_, mut test_item) = current_running_tests.remove(pos);

                                let reference = match &info {
                                    TestFailedInfo::Fail { stdout } => {
                                        println!("{}", stdout);

                                        "see above"
                                    }
                                    TestFailedInfo::Reason { reason } => reason,
                                };
                                test_item.inc();
                                test_item.fail(format!(
                                    "[FAIL] ({reference}) in {exec_time:.3}s: {suite}::{test}"
                                ));
                                let bin_id = &current_suite.as_ref().unwrap().1.test_binary;
                                test_item.fail(format!("Run `cargo nexus test -t 'binary(={bin_id}) & test(={test})'` to see the output"));
                                current_suite.as_mut().unwrap().0.inc();

                                item.inc();
                            }
                        }
                    },
                    OutputLine::Suite(suite_event) => match suite_event {
                        SuiteEvent::Started {
                            test_count,
                            nextest,
                        } => {
                            if let Some(item) = &mut item {
                                let suite_item = item.add_child("Suite");
                                suite_item.init(Some(test_count), Some(unit::label("tests")));
                                current_suite = Some((suite_item, nextest));
                            }
                        }
                        SuiteEvent::Ok {
                            exec_time,
                            passed,
                            failed,
                            ignored,
                            measured,
                            filtered_out,
                            nextest,
                        } => {
                            if enable_tree {
                                let mut suite = current_suite.take().unwrap();
                            }
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
                            if enable_tree {
                                let mut suite = current_suite.take().unwrap();
                            }
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
