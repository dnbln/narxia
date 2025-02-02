use core::fmt;
use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use std::io;

use path_absolutize::Absolutize;

struct SrcFileDatabaseInner {
    db: String,
    files: Vec<(FilePathInfo, Span)>,
    loader: Box<dyn Fn(&Path) -> std::io::Result<String> + Send + Sync>,
}

#[derive(Debug, Clone)]
pub struct FilePathInfo {
    short_path: Option<PathBuf>,
    full_path: PathBuf,
}

impl FilePathInfo {
    pub fn new(short_path: Option<PathBuf>, full_path: PathBuf) -> Self {
        Self {
            short_path,
            full_path,
        }
    }

    pub fn new_from_short(path: PathBuf) -> Self {
        let p_abs = path
            .absolutize()
            .unwrap_or(Cow::Borrowed(&path))
            .to_path_buf();
        Self::new(Some(path), p_abs)
    }
}

#[derive(Clone)]
pub struct SrcFileDatabase {
    inner: Arc<RwLock<SrcFileDatabaseInner>>,
}

impl fmt::Debug for SrcFileDatabase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SrcFileDatabase")
    }
}

impl Default for SrcFileDatabase {
    fn default() -> Self {
        Self::new_with_loader(Box::new(|p| std::fs::read_to_string(p)))
    }
}

impl SrcFileDatabase {
    pub fn new_with_loader(loader: Box<dyn Fn(&Path) -> std::io::Result<String> + Send + Sync>) -> Self {
        Self {
            inner: Arc::new(RwLock::new(SrcFileDatabaseInner {
                db: String::new(),
                files: Vec::new(),
                loader,
            })),
        }
    }

    pub fn load_file(&self, path: FilePathInfo) -> std::io::Result<Span> {
        let inner = self.inner.read().unwrap();
        let text = (inner.loader)(&path.full_path)?;
        drop(inner);

        Ok(self.load_file_from_memory(path, &text))
    }

    pub fn load_file_from_memory(&self, path: FilePathInfo, text: &str) -> Span {
        let mut inner_lock = self.inner.write().unwrap();

        let start_index = inner_lock.db.len();
        inner_lock.db += &text;
        let end_index = inner_lock.db.len();

        inner_lock.files.push((
            path,
            Span {
                lo: start_index,
                hi: end_index,
            },
        ));

        Span {
            lo: start_index,
            hi: end_index,
        }
    }

    pub fn get_loaded_span(&self, span: Span) -> String {
        let inner = self.inner.read().unwrap();

        debug_assert!(span.hi <= inner.db.len());

        inner.db[span.lo..span.hi].to_owned()
    }

    pub fn get_file_path(&self, span: Span) -> FilePathInfo {
        let inner = self.inner.read().unwrap();

        inner
            .files
            .iter()
            .find(|(_, s)| s.fully_contains(span))
            .map(|(p, _)| p.clone())
            .unwrap()
    }
}

#[salsa::input]
pub struct SrcFile {
    pub db_span: Span,
}

impl SrcFile {
    pub fn path(&self, db: &dyn SrcDb) -> FilePathInfo {
        db.src_file_path(self.db_span(db))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    lo: usize,
    hi: usize,
}

impl Span {
    pub fn fully_contains(&self, other: Span) -> bool {
        self.lo <= other.lo && self.hi >= other.hi
    }
}

impl SrcFile {
    pub fn get_presentable_path<'a>(&self, db: &'a dyn SrcDb) -> PathBuf {
        let fp = self.path(db);

        fp.short_path.unwrap_or_else(|| fp.full_path)
    }

    pub fn get_text<'a>(&self, db: &'a dyn SrcDb) -> String {
        let db_span = self.db_span(db);
        db.src_file_text(db_span)
    }
}

#[salsa::db]
pub trait SrcDb: salsa::Database {
    fn src_file_text<'db>(&'db self, span: Span) -> String;
    fn src_file_path<'db>(&'db self, span: Span) -> FilePathInfo;

    fn src_load_file<'db>(&'db self, path: FilePathInfo) -> io::Result<Span>;
    fn src_load_file_inmemory<'db>(&'db self, path: FilePathInfo, text: &str) -> Span;
}

pub fn load_from_disk(db: &dyn SrcDb, path: FilePathInfo) -> io::Result<SrcFile> {
    let span = db.src_load_file(path.clone())?;
    Ok(SrcFile::new(db, span))
}

pub fn load_from_memory(db: &dyn SrcDb, path: FilePathInfo, contents: &str) -> SrcFile {
    let span = db.src_load_file_inmemory(path.clone(), contents);
    SrcFile::new(db, span)
}
