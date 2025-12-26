use core::fmt;
use std::env;
use std::io;
use std::path::PathBuf;
use std::process;
use std::thread;
use std::time;

use clap::ArgAction;
use clap::Parser;
use miette::IntoDiagnostic;
use miette::bail;
use nexus::BuildDistribCommand;
use nexus::BuildDistribsBins;
use nexus::BuildSysCmd;
use nexus::ColorConfig;
use nexus::LLVMLinkBehavior;
use nexus::NarxiaNeededBins;
use nexus::NexusOutputGroups;
use nexus::NexusR;
use nexus::ProfileDeterminer;
use nexus::RunCompilerBins;
use nexus::Target;
use nexus::bin_context::NexusContext;
use nexus::cargo_interface;
use nexus::cargo_interface::SysTarget;
use nexus::cargo_interface::tests;
use nexus::duration::NexusDuration;
use prodash::render::line;
use prodash::unit;

#[derive(Debug, Parser)]
enum App {
    /// Build system command.
    #[clap(name = "build-sys")]
    #[clap(alias = "b")]
    #[clap(subcommand)]
    Build(BuildSysCmd),
    /// Test command.
    ///
    /// This command will run the tests.
    #[clap(name = "test")]
    #[clap(alias = "t")]
    Test {
        /// An optional filter to run only tests that match the filter.
        ///
        /// The filter format is described on [Nextest's Filter Set DSL reference].
        ///
        /// [Nextest's Filter Set DSL reference]: https://nexte.st/docs/filtersets/reference/
        #[clap(short, long)]
        test_filter: Option<String>,

        /// Whether to count the tests.
        ///
        /// This is useful if you want to see the progress of the tests.
        ///
        /// If this is off, the tests will run immediately and the progress
        /// will be shown per test suite, rather than overall.
        ///
        /// This does however mean that the progress will be a bit less accurate,
        /// as the total number of tests is not known in advance.
        ///
        /// It does give a performance boost though.
        #[clap(long = "no-count-tests", default_value_t = true, action = ArgAction::SetFalse)]
        count_tests: bool,

        /// Whether to capture the output of the nextest stderr.
        ///
        /// If this flag is used, the output of nextest's stderr will not be captured,
        /// and will be printed to the console instead.
        ///
        /// Every test will be mentioned twice. Once by nextest, and once by nexus.
        ///
        /// As such, it is only useful while debugging.
        #[clap(long, default_value_t = true, action = ArgAction::SetFalse)]
        #[cfg(debug_assertions)]
        capture_nextest: bool,

        #[clap(long)]
        #[cfg(debug_assertions)]
        dump_nextest_stderr_to: Option<PathBuf>,

        /// Whether to fail fast.
        ///
        /// If this flag is used, the tests will stop running after the first failure.
        #[clap(long = "no-fail-fast", default_value_t = true, action = ArgAction::SetFalse)]
        fail_fast: bool,

        #[clap(flatten)]
        profile: ProfileDeterminer,

        #[clap(long)]
        llvm_link_behavior: Option<LLVMLinkBehavior>,

        /// Parser test mode.
        ///
        /// This mode will specify whether the parser tests are checked or overwritten.
        #[clap(long, default_value_t = SnapshotsTestMode::Check)]
        parser_tests: SnapshotsTestMode,

        /// SSA test mode.
        ///
        /// This mode will specify whether the SSA tests are checked or overwritten.
        #[clap(long, default_value_t = SnapshotsTestMode::Check)]
        ssa_tests: SnapshotsTestMode,

        /// Name resolution test mode.
        ///
        /// This mode will specify whether the name resolution tests are checked or overwritten.
        #[clap(long, default_value_t = SnapshotsTestMode::Check)]
        nr_tests: SnapshotsTestMode,

        /// Whether to run the tests with Miri.
        #[clap(long)]
        miri: bool,
    },
    #[clap(name = "doctest")]
    DocTest {
        /// Whether to fail fast.
        ///
        /// If this flag is used, the tests will stop running after the first failure.
        #[clap(long = "no-fail-fast", default_value_t = true, action = ArgAction::SetFalse)]
        fail_fast: bool,

        #[clap(flatten)]
        profile: ProfileDeterminer,

        #[clap(long)]
        llvm_link_behavior: Option<LLVMLinkBehavior>,
    },
    /// Runs the narxia compiler driver.
    ///
    /// Call this command with -- --help to see the available options in the driver.
    #[clap(name = "run")]
    #[clap(alias = "r")]
    Run {
        #[clap(long)]
        llvm_link_behavior: Option<LLVMLinkBehavior>,
        #[clap(flatten)]
        profile: ProfileDeterminer,
        #[clap(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<String>,
    },
    /// Build a distributalbe package.
    #[clap(name = "dist")]
    Dist {
        #[clap(long, default_value_t = LLVMLinkBehavior::ForceStatic)]
        llvm_link_behavior: LLVMLinkBehavior,
        #[clap(long, default_value = "dist.zip")]
        pkg: PathBuf,
        #[clap(long)]
        sys: Option<String>,
    },
}

#[derive(Debug, clap::ValueEnum, Copy, Clone, Default)]
enum SnapshotsTestMode {
    #[default]
    Check,
    Overwrite,
}

impl SnapshotsTestMode {
    fn into_lib_ty(self) -> tests::SnapshotsTestMode {
        match self {
            SnapshotsTestMode::Check => tests::SnapshotsTestMode::Check,
            SnapshotsTestMode::Overwrite => tests::SnapshotsTestMode::Overwrite,
        }
    }
}

impl fmt::Display for SnapshotsTestMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SnapshotsTestMode::Check => write!(f, "check"),
            SnapshotsTestMode::Overwrite => write!(f, "overwrite"),
        }
    }
}

async fn run_app(app: App, cx: &mut NexusContext<'_>) -> NexusR {
    match app {
        App::Build(cmd) => {
            cmd.run(cx).await?;
        }
        App::Test {
            test_filter,
            #[cfg(debug_assertions)]
            capture_nextest,
            #[cfg(debug_assertions)]
            dump_nextest_stderr_to,
            count_tests,
            fail_fast,
            profile,
            llvm_link_behavior,
            parser_tests,
            ssa_tests,
            nr_tests,
            miri,
        } => {
            let profile = profile.get_profile();
            let llvm_link_behavior =
                llvm_link_behavior.unwrap_or_else(|| profile.default_llvm_link_behavior());
            let bins = {
                let mut item = cx.new_child("Building");
                item.init(None, None);

                let bp = cargo_interface::BuildCmdBuildingProgress::new(
                    item.add_child("Building progress"),
                    time::Instant::now(),
                );

                nexus::BuildI {
                    targets: vec![Target::Compiler, Target::Tests],
                    profile,
                    sys: SysTarget::Host,
                    llvm_link_behavior,
                }
                .run(&cx.llvm_manager, &mut item, Some(bp))
                .await?
            };
            let mut item = cx.new_child("Test");
            let test_count = if count_tests {
                let test_count = tests::list_tests(
                    test_filter.as_ref(),
                    [bins
                        .llvm
                        .as_ref()
                        .cloned()
                        .map(|p| p.to_env())
                        .map(|(a, b)| (a.into(), b.into()))
                        .unwrap()],
                    cx,
                )?
                .test_count;

                item.info(format!("Running {test_count} tests"));

                Some(test_count)
            } else {
                None
            };
            item.init(test_count, Some(unit::label("tests")));
            let llvm_prefix = bins.llvm.as_ref().cloned().unwrap();
            let (llvm_k, llvm_v) = llvm_prefix.to_env();
            let run_tests = tests::RunTests::new()
                .filter(test_filter.clone())
                .fail_fast(fail_fast)
                .profile(profile.cargo_name())
                .env(llvm_k, llvm_v)
                .parser_tests(parser_tests.into_lib_ty())
                .ssa_tests(ssa_tests.into_lib_ty())
                .nr_tests(nr_tests.into_lib_ty())
                .debug_nextest_messages(
                    env::var("NEXUS_DEBUG_NEXTEST_OUTPUT").is_ok_and(|it| it == "1"),
                )
                .miri(miri);

            #[cfg(debug_assertions)]
            let run_tests = run_tests.capture_nextest_output(capture_nextest);

            #[cfg(debug_assertions)]
            let run_tests = if let Some(dump_nextest_stderr_to) = dump_nextest_stderr_to {
                run_tests.dump_nextest_stderr_to(dump_nextest_stderr_to)
            } else {
                run_tests
            };

            run_tests.run(Some(&mut item), cx.groups(), cx)?;
        }
        App::DocTest {
            fail_fast,
            profile,
            llvm_link_behavior,
        } => {
            let profile = profile.get_profile();
            let llvm_link_behavior =
                llvm_link_behavior.unwrap_or_else(|| profile.default_llvm_link_behavior());
            let mut item = cx.new_child("DocTest");
            item.init(None, None);

            let bins = {
                let mut item = item.add_child("Building");
                item.init(None, None);

                let bp = cargo_interface::BuildCmdBuildingProgress::new(
                    item.add_child("Building progress"),
                    time::Instant::now(),
                );

                nexus::BuildI {
                    targets: vec![Target::Compiler],
                    profile,
                    sys: SysTarget::Host,
                    llvm_link_behavior,
                }
                .run(&cx.llvm_manager, &mut item, Some(bp))
                .await?
            };

            let (llvm_k, llvm_v) = bins.llvm.unwrap().to_env();

            let mut cmd = process::Command::new("cargo");
            cmd.arg("test")
                .arg("--workspace")
                .arg("--profile")
                .arg(profile.cargo_name())
                .arg("--doc");
            if !fail_fast {
                cmd.arg("--no-fail-fast");
            }

            let s = cmd
                .arg("--")
                .arg("--test-threads")
                .arg("1")
                .env(llvm_k, llvm_v)
                .status()
                .into_diagnostic()?;
            if !s.success() {
                bail!("DocTest command failed with status: {}", s);
            }
        }
        App::Run {
            llvm_link_behavior,
            profile,
            args,
        } => {
            let profile = profile.get_profile();
            let llvm_link_behavior =
                llvm_link_behavior.unwrap_or_else(|| profile.default_llvm_link_behavior());
            let mut run_cmd = cargo_interface::RunCompilerCommand::default();
            run_cmd.args(args);

            let mut needed_bins_buffer = Vec::new();
            RunCompilerBins::needed_bins(&run_cmd, &mut needed_bins_buffer);

            let bins = {
                let mut item = cx.new_child("Building");
                item.init(None, None);

                let bp = cargo_interface::BuildCmdBuildingProgress::new(
                    item.add_child("Building progress"),
                    time::Instant::now(),
                );

                nexus::BuildI {
                    targets: needed_bins_buffer,
                    profile,
                    sys: SysTarget::Host,
                    llvm_link_behavior,
                }
                .run(&cx.llvm_manager, &mut item, Some(bp))
                .await?
            };

            let run_compiler_bins = RunCompilerBins::compile_from(&bins);

            run_cmd.compiler(run_compiler_bins.compiler);
            run_cmd.llvm(run_compiler_bins.llvm.clone());

            {
                let item = cx.new_child("Running");
                run_cmd.run(item)?;
            }
        }
        App::Dist {
            pkg,
            sys,
            llvm_link_behavior,
        } => {
            let cmd = BuildDistribCommand { pkg };

            let bins = {
                let mut item = cx.new_child("Building");
                item.init(None, None);

                let mut needed_bins_buffer = Vec::new();
                BuildDistribsBins::needed_bins(&cmd, &mut needed_bins_buffer);

                let bp = cargo_interface::BuildCmdBuildingProgress::new(
                    item.add_child("Building progress"),
                    time::Instant::now(),
                );

                nexus::BuildI {
                    targets: needed_bins_buffer,
                    profile: nexus::Profile::Release,
                    sys: match sys {
                        Some(sys) => SysTarget::Target { name: sys },
                        None => SysTarget::Host,
                    },
                    llvm_link_behavior,
                }
                .run(&cx.llvm_manager, &mut item, Some(bp))
                .await?
            };

            let build_distrib_bins = BuildDistribsBins::compile_from(&bins);

            {
                let mut item = cx.new_child("Building distributable");
                cmd.run(&mut item, &build_distrib_bins)?;
            }
        }
    }

    Ok(())
}

#[tokio::main]
async fn main() -> NexusR {
    narxia_log_impl::init();

    let color_config = match env::var("COLOR") {
        Ok(s) => match s.as_str() {
            "always" | "true" | "1" => ColorConfig::Always,
            "never" | "false" | "0" => ColorConfig::Never,
            _ => ColorConfig::Auto,
        },
        Err(_) => ColorConfig::Auto,
    };

    let groups = match (env::var("NEXUS_GROUP_BEGIN"), env::var("NEXUS_GROUP_END")) {
        (Ok(begin), Ok(end)) => Some(NexusOutputGroups::new(begin, end)),
        _ => None,
    };

    let (mut cx, tree) = NexusContext::new(groups)?;

    let start = time::Instant::now();

    let mut opts = line::Options {
        frames_per_second: 20.0,
        ..Default::default()
    }
    .auto_configure(line::StreamKind::Stderr);

    match color_config {
        ColorConfig::Always => {
            opts.colored = true;
            owo_colors::set_override(true);
        }
        ColorConfig::Never => {
            opts.colored = false;
            owo_colors::set_override(false);
        }
        ColorConfig::Auto => {
            owo_colors::unset_override();
        }
    }

    let handle = line::render(io::stderr(), tree, opts);

    let app = App::parse();

    let r = run_app(app, &mut cx).await;

    match r {
        Ok(()) => cx.done(format!("elapsed {}", NexusDuration::since(start))),
        Err(e) => {
            cx.fail(format!("error after {}", NexusDuration::since(start)));
            return Err(e);
        }
    }

    thread::sleep(time::Duration::from_millis(30));

    handle.shutdown_and_wait();

    Ok(())
}
