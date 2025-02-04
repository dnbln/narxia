use clap::{ArgAction, Parser};
use nexus::bin_context::NexusContext;
use nexus::{cargo_interface, BuildSysCmd, NarxiaNeededBins, NexusR, RunCompilerBins};
use prodash::unit;

#[derive(Debug, Parser)]
enum App {
    /// Build system command.
    #[clap(name = "build-sys")]
    #[clap(alias = "b")]
    #[clap(subcommand)]
    Build(BuildSysCmd),
    #[clap(name = "test")]
    #[clap(alias = "t")]
    Test {
        #[clap(short, long)]
        test_filter: Option<String>,

        #[clap(long = "no-capture-nextest", action = ArgAction::SetFalse, default_value_t = true)]
        capture_nextest_output: bool,
    },
    #[clap(name = "run")]
    #[clap(alias = "r")]
    Run { args: Vec<String> },
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
            capture_nextest_output,
        } => {
            let mut item = cx.new_child("Test");
            let test_count =
                nexus::cargo_interface::tests::list_tests(test_filter.as_ref())?.test_count;
            item.init(Some(test_count), Some(unit::label("tests")));
            nexus::cargo_interface::tests::RunTests::new()
                .filter(test_filter.clone())
                .capture_nextest_output(capture_nextest_output)
                .run(Some(&mut item))?;
        }
        App::Run { args } => {
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

                nexus::BuildCmd {
                    targets: needed_bins_buffer,
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
