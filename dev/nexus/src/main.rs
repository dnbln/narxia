use core::fmt;
use std::path::PathBuf;

use clap::{ArgAction, Parser};
use nexus::bin_context::NexusContext;
use nexus::cargo_interface::SysTarget;
use nexus::duration::NexusDuration;
use nexus::{
    cargo_interface, BuildDistribCommand, BuildDistribsBins, BuildSysCmd, ColorConfig,
    NarxiaNeededBins, NexusOutputGroups, NexusR, ProfileDeterminer, RunCompilerBins, Target,
};
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

        /// Whether to fail fast.
        ///
        /// If this flag is used, the tests will stop running after the first failure.
        #[clap(long = "no-fail-fast", default_value_t = true, action = ArgAction::SetFalse)]
        fail_fast: bool,

        #[clap(flatten)]
        profile: ProfileDeterminer,

        /// Parser test mode.
        ///
        /// This mode will run the parser tests.
        #[clap(long, default_value_t = ParserTestsMode::Check)]
        parser_tests: ParserTestsMode,
    },
    /// Runs the narxia compiler driver.
    ///
    /// Call this command with -- --help to see the available options in the driver.
    #[clap(name = "run")]
    #[clap(alias = "r")]
    Run {
        #[clap(flatten)]
        profile: ProfileDeterminer,
        args: Vec<String>,
    },
    /// Build a distributalbe package.
    #[clap(name = "dist")]
    Dist {
        #[clap(long, default_value = "dist.zip")]
        pkg: PathBuf,
        #[clap(long)]
        sys: Option<String>,
    },
}

#[derive(Debug, clap::ValueEnum, Clone)]
enum ParserTestsMode {
    Check,
    Overwrite,
}

impl fmt::Display for ParserTestsMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserTestsMode::Check => write!(f, "check"),
            ParserTestsMode::Overwrite => write!(f, "overwrite"),
        }
    }
}

impl Default for ParserTestsMode {
    fn default() -> Self {
        ParserTestsMode::Check
    }
}

fn run_app(app: App, cx: &mut NexusContext) -> NexusR {
    match app {
        App::Build(cmd) => {
            cmd.run(cx)?;
        }
        App::Test {
            test_filter,
            #[cfg(debug_assertions)]
            capture_nextest,
            count_tests,
            fail_fast,
            profile,
            parser_tests,
        } => {
            let profile = profile.get_profile();
            let bins = {
                let mut item = cx.new_child("Building");
                item.init(None, None);

                let bp = cargo_interface::BuildCmdBuildingProgress::new(
                    item.add_child("Building progress"),
                    std::time::Instant::now(),
                );

                nexus::BuildI {
                    targets: vec![Target::Compiler, Target::Tests],
                    profile,
                    sys: SysTarget::Host,
                }
                .run(&cx.llvm_manager, &mut item, Some(bp))?
            };
            let mut item = cx.new_child("Test");
            let test_count = if count_tests {
                Some(
                    nexus::cargo_interface::tests::list_tests(
                        test_filter.as_ref(),
                        [bins
                            .llvm
                            .as_ref()
                            .cloned()
                            .map(|p| p.to_env())
                            .map(|(a, b)| (a.into(), b.into()))
                            .unwrap()],
                    )?
                    .test_count,
                )
            } else {
                None
            };
            item.init(test_count, Some(unit::label("tests")));
            let llvm_prefix = bins.llvm.as_ref().cloned().unwrap();
            let (llvm_k, llvm_v) = llvm_prefix.to_env();
            let run_tests = nexus::cargo_interface::tests::RunTests::new()
                .filter(test_filter.clone())
                .fail_fast(fail_fast)
                .profile(profile.cargo_name())
                .env(llvm_k, llvm_v)
                .parser_tests(match parser_tests {
                    ParserTestsMode::Check => nexus::cargo_interface::tests::ParserTestsMode::Check,
                    ParserTestsMode::Overwrite => {
                        nexus::cargo_interface::tests::ParserTestsMode::Overwrite
                    }
                });

            #[cfg(debug_assertions)]
            let run_tests = run_tests.capture_nextest_output(capture_nextest);

            run_tests.run(Some(&mut item), cx.groups())?;
        }
        App::Run { profile, args } => {
            let profile = profile.get_profile();
            let mut run_cmd = cargo_interface::RunCompilerCommand::default();
            run_cmd.args(args);

            let mut needed_bins_buffer = Vec::new();
            RunCompilerBins::needed_bins(&run_cmd, &mut needed_bins_buffer);

            let bins = {
                let mut item = cx.new_child("Building");
                item.init(None, None);

                let bp = cargo_interface::BuildCmdBuildingProgress::new(
                    item.add_child("Building progress"),
                    std::time::Instant::now(),
                );

                nexus::BuildI {
                    targets: needed_bins_buffer,
                    profile,
                    sys: SysTarget::Host,
                }
                .run(&cx.llvm_manager, &mut item, Some(bp))?
            };

            let run_compiler_bins = RunCompilerBins::compile_from(&bins);

            run_cmd.compiler(&run_compiler_bins.compiler);
            run_cmd.llvm(run_compiler_bins.llvm.clone());

            {
                let item = cx.new_child("Running");
                run_cmd.run(item)?;
            }
        }
        App::Dist { pkg, sys } => {
            let cmd = BuildDistribCommand { pkg };

            let bins = {
                let mut item = cx.new_child("Building");
                item.init(None, None);

                let mut needed_bins_buffer = Vec::new();
                BuildDistribsBins::needed_bins(&cmd, &mut needed_bins_buffer);

                let bp = cargo_interface::BuildCmdBuildingProgress::new(
                    item.add_child("Building progress"),
                    std::time::Instant::now(),
                );

                nexus::BuildI {
                    targets: needed_bins_buffer,
                    profile: nexus::Profile::Release,
                    sys: match sys {
                        Some(sys) => SysTarget::Target { name: sys },
                        None => SysTarget::Host,
                    },
                }
                .run(&cx.llvm_manager, &mut item, Some(bp))?
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

fn main() -> NexusR {
    narxia_log_impl::init();

    let color_config = match std::env::var("COLOR") {
        Ok(s) => match s.as_str() {
            "always" | "true" | "1" => ColorConfig::Always,
            "never" | "false" | "0" => ColorConfig::Never,
            _ => ColorConfig::Auto,
        },
        Err(_) => ColorConfig::Auto,
    };

    let groups = match (
        std::env::var("NEXUS_GROUP_BEGIN"),
        std::env::var("NEXUS_GROUP_END"),
    ) {
        (Ok(begin), Ok(end)) => Some(NexusOutputGroups::new(begin, end)),
        _ => None,
    };

    let (mut cx, tree) = NexusContext::new(groups);
    let start = std::time::Instant::now();

    let mut opts = prodash::render::line::Options {
        frames_per_second: 20.0,
        ..Default::default()
    }
    .auto_configure(prodash::render::line::StreamKind::Stderr);

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

    let handle = prodash::render::line::render(std::io::stderr(), tree, opts);

    let app = App::parse();

    let r = run_app(app, &mut cx);

    match r {
        Ok(()) => cx.done(format!("elapsed {}", NexusDuration::since(start))),
        Err(e) => {
            cx.fail(format!("error after {}", NexusDuration::since(start)));
            return Err(e);
        }
    }

    std::thread::sleep(std::time::Duration::from_millis(30));

    handle.shutdown_and_wait();

    Ok(())
}
