use std::io;
use std::path::PathBuf;

use narxia_src_db_impl::FilePathInfo;
use narxia_src_db_impl::Span;

#[salsa::input]
pub struct SrcFile {
    pub db_span: Span,
}

impl SrcFile {
    pub fn path(&self, db: &dyn SrcDb) -> FilePathInfo {
        db.src_file_path(self.db_span(db))
    }
}

impl SrcFile {
    pub fn get_presentable_path(&self, db: &dyn SrcDb) -> PathBuf {
        let fp = self.path(db);

        fp.short_path.unwrap_or(fp.full_path)
    }

    pub fn get_text(&self, db: &dyn SrcDb) -> String {
        let db_span = self.db_span(db);
        db.src_file_text(db_span)
    }
}

#[salsa::db]
pub trait SrcDb: salsa::Database {
    fn src_file_text(&self, span: Span) -> String;
    fn full_src_file_text(&self, span: Span) -> String;
    fn src_file_path(&self, span: Span) -> FilePathInfo;

    fn src_load_file(&self, path: FilePathInfo) -> io::Result<Span>;
    fn src_load_file_inmemory(&self, path: FilePathInfo, text: &str) -> Span;
}

pub fn load_from_disk(db: &dyn SrcDb, path: FilePathInfo) -> io::Result<SrcFile> {
    let span = db.src_load_file(path.clone())?;
    Ok(SrcFile::new(db, span))
}

pub fn load_from_memory(db: &dyn SrcDb, path: FilePathInfo, contents: &str) -> SrcFile {
    let span = db.src_load_file_inmemory(path.clone(), contents);
    SrcFile::new(db, span)
}
