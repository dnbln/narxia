use std::io;
use std::path::{Path, PathBuf};

use path_absolutize::Absolutize;
use salsa::DbWithJar;

pub struct SrcFileDatabase {
    db: String,
    loader: Box<dyn Fn(&Path) -> std::io::Result<String>>,
}

impl Default for SrcFileDatabase {
    fn default() -> Self {
        Self::new_with_loader(Box::new(|p| std::fs::read_to_string(p)))
    }
}

impl SrcFileDatabase {
    pub fn new_with_loader(loader: Box<dyn Fn(&Path) -> std::io::Result<String>>) -> Self {
        Self {
            db: String::new(),
            loader,
        }
    }

    pub fn load_file(&mut self, path: impl AsRef<Path>) -> std::io::Result<Span> {
        let text = (self.loader)(path.as_ref())?;
        Ok(self.load_file_from_memory(&text))
    }

    pub fn load_file_from_memory(&mut self, text: &str) -> Span {
        let start_index = self.db.len();
        self.db += &text;
        let end_index = self.db.len();
        Span {
            lo: start_index,
            hi: end_index,
        }
    }

    pub fn get_loaded_span(&self, span: Span) -> &str {
        debug_assert!(span.hi <= self.db.len());

        &self.db[span.lo..span.hi]
    }
}

#[salsa::input]
pub struct SrcFile {
    #[return_ref]
    pub short_path: Option<PathBuf>,
    #[return_ref]
    pub full_path: PathBuf,
    pub db_span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    lo: usize,
    hi: usize,
}

impl SrcFile {
    pub fn get_presentable_path<'a>(&self, db: &'a dyn Db) -> &'a PathBuf {
        self.short_path(db)
            .as_ref()
            .unwrap_or_else(|| self.full_path(db))
    }

    pub fn get_text<'a>(&self, db: &'a dyn Db) -> String {
        let db_span = self.db_span(db);
        db.src_file_text(db_span)
    }

    pub fn with_text<F, T: 'static>(&self, db: &dyn Db, f: F) -> T
    where
        F: for<'a> FnOnce(&'a str) -> T,
    {
        let span = self.db_span(db);
        let refr = db.src_file_db();
        let result = f(refr.get_loaded_span(span));
        result
    }
}

pub trait Db: DbWithJar<Jar> {
    fn src_file_text<'db>(&'db self, span: Span) -> String;
    fn src_file_db<'db>(&'db self) -> std::cell::Ref<'db, SrcFileDatabase>;
    fn src_file_db_mut<'db>(&'db self) -> std::cell::RefMut<'db, SrcFileDatabase>;
}

#[salsa::jar(db = Db)]
pub struct Jar(SrcFile);

pub fn load_from_disk(db: &dyn Db, path: PathBuf) -> io::Result<SrcFile> {
    let text = std::fs::read_to_string(&path)?;
    Ok(load_from_memory(db, path, text))
}

pub fn load_from_memory(db: &dyn Db, path: PathBuf, contents: String) -> SrcFile {
    let short_path = path.clone();
    let full_path = path.absolutize().unwrap().into_owned();
    let span = db.src_file_db_mut().load_file_from_memory(&contents);
    SrcFile::new(db, Some(short_path), full_path, span)
}
