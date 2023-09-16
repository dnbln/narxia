use std::path::PathBuf;

use clap::Parser;
use colored::Colorize;
use miette::IntoDiagnostic;
use narxia_driver::DriverCtx;
use salsa::storage::HasJarsDyn;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
enum NarxiaDriverCommand {
    #[clap(name = "parse")]
    Parse(NarxiaDriverParseCommand),
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverParseCommand {
    file: PathBuf,
}

fn main() -> miette::Result<()> {
    let _span = narxia_log::span!(narxia_log::Level::INFO, "main").entered();

    let ctx = DriverCtx::initialize();
    ctx.init_log();

    let cmd = NarxiaDriverCommand::parse();
    match cmd {
        NarxiaDriverCommand::Parse(parse_cmd) => {
            narxia_log::i!("Parse command: {parse_cmd:?}");

            let file = parse_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;

            narxia_log::t!(
                "File contents:\n{}\n{}\n{}",
                ">>>>".bright_blue(),
                file.text(&ctx.db).bright_white().bold(),
                "<<<<".bright_blue(),
            );

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
            println!("{:?}", tree.tree(&ctx.db));
        }
    }

    Ok(())
}
