use std::path::PathBuf;

use clap::Parser;
use colored::Colorize;
use miette::IntoDiagnostic;

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

#[salsa::db(narxia_src_db::Jar, narxia_syn_db::Jar)]
#[derive(Default)]
struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {
    fn salsa_event(&self, event: salsa::Event) {}
}

fn main() -> miette::Result<()> {
    narxia_log_impl::init();

    let _span = narxia_log::span!(narxia_log::Level::INFO, "main").entered();

    let db = Database::default();

    let cmd = NarxiaDriverCommand::parse();
    match cmd {
        NarxiaDriverCommand::Parse(parse_cmd) => {
            narxia_log::i!("Parse command: {parse_cmd:?}");

            let file = parse_cmd.file;
            let file = narxia_src_db::load_from_disk(&db, file).into_diagnostic()?;

            narxia_log::t!(
                "File contents:\n{}\n{}\n{}",
                ">>>>".bright_blue(),
                file.text(&db).bright_white().bold(),
                "<<<<".bright_blue(),
            );

            let tree = narxia_syn_db::parse_file_and_assert_no_errors(&db, file);
            println!("{:?}", tree.tree(&db));
        }
    }

    Ok(())
}
