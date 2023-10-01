use std::fmt::Formatter;
use std::path::PathBuf;
use std::{fmt, io};

use colored::Colorize;
use narxia_src_db::SrcFile;
use narxia_syn::parse_error::ParseError;
use narxia_syn_db::SynFile;

#[salsa::db(narxia_src_db::Jar, narxia_syn_db::Jar, narxia_hir_db::Jar)]
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {
    fn salsa_event(&self, _event: salsa::Event) {}
}

pub struct DriverCtx {
    pub db: Database,
}

impl DriverCtx {
    pub fn initialize() -> Self {
        Self {
            db: Database::default(),
        }
    }

    pub fn init_log(&self) {
        init_log();
    }

    pub fn display_file(&self, file: SrcFile) -> DisplayFile {
        DisplayFile(&self.db, file)
    }

    #[track_caller]
    pub fn trace_file(&self, file: SrcFile) {
        narxia_log::t!("File contents:\n{}", self.display_file(file));
    }
}

pub struct DisplayFile<'a>(&'a Database, SrcFile);

impl<'a> fmt::Display for DisplayFile<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}\n{}\n{}",
            ">>>>".bright_blue(),
            self.1.text(self.0).bright_white().bold(),
            "<<<<".bright_blue(),
        )
    }
}

pub fn read_file(ctx: &DriverCtx, file: PathBuf) -> io::Result<SrcFile> {
    narxia_src_db::load_from_disk(&ctx.db, file)
}

pub fn parse_file(ctx: &DriverCtx, file: SrcFile) -> SynFile {
    parse_file_with_diagnostics(ctx, file).0
}

pub fn load_file(ctx: &DriverCtx, p: PathBuf, contents: String) -> SrcFile {
    narxia_src_db::load_from_memory(&ctx.db, p, contents)
}

pub fn parse_file_with_diagnostics(ctx: &DriverCtx, file: SrcFile) -> (SynFile, Vec<ParseError>) {
    let syn_file = narxia_syn_db::parse_file(&ctx.db, file);
    let errors = narxia_syn_db::ParsingErrors::get(&ctx.db, file);
    (syn_file, errors.unwrap_or_default())
}

pub fn parse_file_and_assert_no_errors(ctx: &DriverCtx, file: SrcFile) -> SynFile {
    narxia_syn_db::parse_file_and_assert_no_errors(&ctx.db, file)
}

pub fn parse_file_at_path_and_assert_no_errors(ctx: &DriverCtx, path: PathBuf) -> SynFile {
    let file = read_file(ctx, path).unwrap();
    parse_file_and_assert_no_errors(ctx, file)
}

pub fn lower_syn_file(ctx: &DriverCtx, file: SynFile) -> narxia_hir_db::HirFile {
    narxia_hir_db::lower_file(&ctx.db, file)
}

pub fn init_log() {
    narxia_log_impl::init();
}
