use narxia_syn::syntree::SynTree;

#[salsa::jar(db = SynDb)]
pub struct Jar(
    SynFile,
    parse_file,
    parse_file_and_assert_no_errors,
    ParsingErrors,
);

pub trait SynDb: salsa::DbWithJar<Jar> + narxia_src_db::Db {}

impl<DB> SynDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + narxia_src_db::Db {}

#[salsa::tracked]
pub struct SynFile {
    pub file: SrcFile,
    #[return_ref]
    pub tree: SynTree,
}

use narxia_src_db::SrcFile;
use narxia_syn::parse_error::ParseError;
pub use Jar as SynDbJar;

#[salsa::accumulator]
pub struct ParsingErrors(Vec<ParseError>);

impl ParsingErrors {
    pub fn get(db: &dyn SynDb, file: SrcFile) -> Option<Vec<ParseError>> {
        parse_file::accumulated::<Self>(db, file).into_iter().next()
    }
}

#[salsa::tracked]
pub fn parse_file<'db>(db: &'db dyn SynDb, file: SrcFile) -> SynFile {
    let (tree, errors) = file.with_text(db, |text| {
        let mut ts = narxia_syn::token_source::text_ts::TextTokenSource::new(text);
        let mut parser = narxia_syn::parser::Parser::new(&mut ts);
        parser.parse();
        parser.finish_to_tree()
    });

    if !errors.is_empty() {
        narxia_log::e!("There were parsing errors:\n{errors:?}");
        narxia_log::w!("The tree might not be complete.");
        ParsingErrors::push(db, errors);
    }

    SynFile::new(db, file, SynTree::new(tree))
}

#[salsa::tracked]
pub fn parse_file_and_assert_no_errors(db: &dyn SynDb, src_file: SrcFile) -> SynFile {
    let file = parse_file(db, src_file);
    let errors = ParsingErrors::get(db, src_file);
    assert!(errors.is_none());
    file
}
