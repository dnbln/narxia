use clap::{ArgAction, Parser};
use nexus::bin_context::NexusContext;
use nexus::{
    cargo_interface, BuildSysCmd, NarxiaNeededBins, NexusR, Profile, ProfileDeterminer,
    RunCompilerBins,
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
        #[clap(long = "count-tests")]
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
    },
    #[clap(name = "run")]
    #[clap(alias = "r")]
    Run {
        #[clap(flatten)]
        profile: ProfileDeterminer,
        args: Vec<String>,
    },
}

fn main() -> NexusR {
    narxia_log_impl::init();
    let (cx, tree) = NexusContext::new();

    let handle = prodash::render::line::render(
        std::io::stderr(),
        tree,
        prodash::render::line::Options {
            frames_per_second: 20.0,
            ..Default::default()
        }
        .auto_configure(prodash::render::line::StreamKind::Stderr),
    );

    let app = App::parse();

    match app {
        App::Build(cmd) => {
            cmd.run(&cx)?;
        }
        App::Test {
            test_filter,
            #[cfg(debug_assertions)]
            capture_nextest,
            count_tests,
            fail_fast,
        } => {
            let mut item = cx.new_child("Test");
            let test_count = if count_tests {
                Some(nexus::cargo_interface::tests::list_tests(test_filter.as_ref())?.test_count)
            } else {
                None
            };
            item.init(test_count, Some(unit::label("tests")));
            let run_tests = nexus::cargo_interface::tests::RunTests::new()
                .filter(test_filter.clone())
                .fail_fast(fail_fast);

            #[cfg(debug_assertions)]
            let run_tests = run_tests.capture_nextest_output(capture_nextest);

            run_tests.run(Some(&mut item))?;
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
                }
                .run(&mut item, Some(bp))?
            };

            let run_compiler_bins = RunCompilerBins::compile_from(&bins);

            run_cmd.bin(run_compiler_bins.compiler);

            {
                let item = cx.new_child("Running");
                run_cmd.run(item)?;
            }
        }
    }

    handle.shutdown_and_wait();

    Ok(())
}
