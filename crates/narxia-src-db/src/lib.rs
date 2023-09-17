use std::io;
use std::path::PathBuf;

use path_absolutize::Absolutize;
use salsa::DbWithJar;

#[salsa::input]
pub struct SrcFile {
    #[return_ref]
    pub short_path: Option<PathBuf>,
    #[return_ref]
    pub full_path: PathBuf,
    #[return_ref]
    pub text: String,
}

impl SrcFile {
    pub fn get_presentable_path<'a>(&self, db: &'a dyn Db) -> &'a PathBuf {
        self.short_path(db)
            .as_ref()
            .unwrap_or_else(|| self.full_path(db))
    }
}

pub trait Db: DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + DbWithJar<Jar> {}

#[salsa::jar(db = Db)]
pub struct Jar(SrcFile);

pub fn load_from_disk(db: &dyn Db, path: PathBuf) -> io::Result<SrcFile> {
    let text = std::fs::read_to_string(&path)?;
    Ok(load_from_memory(db, path, text))
}

pub fn load_from_memory(db: &dyn Db, path: PathBuf, contents: String) -> SrcFile {
    let short_path = path.clone();
    let full_path = path.absolutize().unwrap().into_owned();
    SrcFile::new(db, Some(short_path), full_path, contents)
}
