use core::fmt;
use std::borrow::Cow;
use std::fs;
use std::io;
use std::ops::Range;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::RwLock;

use path_absolutize::Absolutize;

struct SrcFileDatabaseInner {
    db: String,
    files: Vec<(FilePathInfo, Span)>,
    loader: Box<dyn Fn(&Path) -> io::Result<String> + Send + Sync>,
}

#[derive(Debug, Clone)]
pub struct FilePathInfo {
    pub short_path: Option<PathBuf>,
    pub full_path: PathBuf,
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

    pub fn presentable(&self) -> &Path {
        self.short_path.as_ref().unwrap_or(&self.full_path)
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
        Self::new_with_loader(Box::new(|p| fs::read_to_string(p)))
    }
}

impl SrcFileDatabase {
    pub fn new_with_loader(loader: Box<dyn Fn(&Path) -> io::Result<String> + Send + Sync>) -> Self {
        Self {
            inner: Arc::new(RwLock::new(SrcFileDatabaseInner {
                db: String::new(),
                files: Vec::new(),
                loader,
            })),
        }
    }

    pub fn load_file(&self, path: FilePathInfo) -> io::Result<Span> {
        let inner = self.inner.read().unwrap();
        let text = (inner.loader)(&path.full_path)?;
        drop(inner);

        Ok(self.load_file_from_memory(path, &text))
    }

    pub fn load_file_from_memory(&self, path: FilePathInfo, text: &str) -> Span {
        let mut inner_lock = self.inner.write().unwrap();

        let start_index = inner_lock.db.len();
        inner_lock.db += text;
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

    pub fn full_file_containing_span(&self, span: Span) -> String {
        let inner = self.inner.read().unwrap();

        let file_span = inner
            .files
            .iter()
            .find(|(_, s)| s.fully_contains(span))
            .map(|(_, s)| s)
            .unwrap();

        inner.db[file_span.lo..file_span.hi].to_owned()
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

    pub fn containing_file_span(&self, span: Span) -> Span {
        let inner = self.inner.read().unwrap();
        inner
            .files
            .iter()
            .find(|(_, s)| s.fully_contains(span))
            .map(|(_, s)| s.clone())
            .unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Span {
    lo: usize,
    hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Self { lo, hi }
    }

    pub fn fully_contains(&self, other: Span) -> bool {
        self.lo <= other.lo && self.hi >= other.hi
    }

    pub fn get_start(&self) -> usize {
        self.lo
    }

    pub fn get_end(&self) -> usize {
        self.hi
    }
}

pub trait SrcFileDatabaseFileInfoProvider {
    fn get_file_text(&self, span: Span) -> String;
    fn get_full_file_text(&self, span: Span) -> String;
    fn get_file_path(&self, span: Span) -> FilePathInfo;

    fn containing_file_span(&self, span: Span) -> Span;

    fn span_to_file_range(&self, span: Span) -> Range<usize> {
        let containing_span = self.containing_file_span(span);
        containing_span.get_start() + span.get_start()..containing_span.get_start() + span.get_end()
    }
}

impl SrcFileDatabaseFileInfoProvider for SrcFileDatabase {
    fn get_file_text(&self, span: Span) -> String {
        self.get_loaded_span(span)
    }

    fn get_full_file_text(&self, span: Span) -> String {
        self.full_file_containing_span(span)
    }

    fn get_file_path(&self, span: Span) -> FilePathInfo {
        self.get_file_path(span)
    }

    fn containing_file_span(&self, span: Span) -> Span {
        self.containing_file_span(span)
    }
}
