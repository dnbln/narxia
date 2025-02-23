#![feature(trait_upcasting)]

use narxia_syn::syntree::GreenTree;

#[salsa::db]
pub trait SynDb: salsa::Database + narxia_src_db::SrcDb {}

#[salsa::db]
impl<DB> SynDb for DB where DB: salsa::Database + narxia_src_db::SrcDb {}

#[salsa::tracked]
pub struct SynFile<'db> {
    pub file: SrcFile,
    #[return_ref]
    pub tree: GreenTree,
}

use narxia_src_db::SrcFile;
use narxia_syn::parse_error::ParseError;
use salsa::Accumulator;

#[salsa::accumulator]
pub struct ParsingErrors(Vec<ParseError>);

impl ParsingErrors {
    pub fn get(db: &dyn SynDb, file: SrcFile) -> Option<Vec<ParseError>> {
        parse_file::accumulated::<Self>(db, file)
            .into_iter()
            .next()
            .map(|v| v.0)
    }
}

#[salsa::tracked]
pub fn parse_file<'db>(db: &'db dyn SynDb, file: SrcFile) -> SynFile<'db> {
    let (tree, errors) = {
        let text = file.get_text(db);
        let mut ts = narxia_syn::token_source::text_ts::TextTokenSource::new(&text);
        let mut parser = narxia_syn::parser::Parser::new(&mut ts);
        parser.parse();
        parser.finish_to_tree()
    };

    if !errors.is_empty() {
        narxia_log::e!("There were parsing errors:\n{errors:?}");
        narxia_log::w!("The tree might not be complete.");
        ParsingErrors(errors).accumulate(db);
    }

    SynFile::new(db, file, tree)
}

#[salsa::tracked]
pub fn parse_file_and_assert_no_errors<'db>(db: &'db dyn SynDb, src_file: SrcFile) -> SynFile<'db> {
    let file = parse_file(db, src_file);
    let errors = ParsingErrors::get(db, src_file);
    assert!(errors.is_none());
    file
}
