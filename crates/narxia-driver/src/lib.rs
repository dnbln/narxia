use std::io;
use std::path::PathBuf;

use narxia_src_db::SrcFile;
use narxia_syn::parse_error::ParseError;
use narxia_syn_db::SynFile;

#[salsa::db(narxia_src_db::Jar, narxia_syn_db::Jar)]
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {
    fn salsa_event(&self, event: salsa::Event) {}
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
}

pub fn read_file(ctx: &DriverCtx, file: PathBuf) -> io::Result<SrcFile> {
    narxia_src_db::load_from_disk(&ctx.db, file)
}

pub fn parse_file(ctx: &DriverCtx, file: SrcFile) -> SynFile {
    parse_file_with_diagnostics(ctx, file).0
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

pub fn init_log() {
    narxia_log_impl::init();
}