//! This test checks that the visitor visits all non-whitespace text in the file.
//!
//! This is useful to ensure that the visitor is not missing any tokens.
//!
//! There are however some exceptions:
//! - Trivia tokens (whitespace, newlines, comments) are ignored.
//! - In a type bound list, the '+' character is ignored. (`T: Copy+Clone`). This is because
//!  the '+' character is not part of the HIR.
//! - In a use statement, the ':', '{', and '}' characters are ignored. This is because
//!  of the way we construct the use statements in HIR, we don't really want to
//!  keep track of these.
//! - Everywhere, the ',' and ';' characters are ignored. This is because they are
//!  never a part of the HIR, just to help the parser separate specific nodes
//!  (similarly to the + in type bounds).

use std::fmt::Write as _;

use dir_structure::FsVfs;
use dir_structure::NewtypeToInner;
use hir::hir_map::HirMap;
use hir::visitor::HirVisitor;
use miette::bail;
use miette::IntoDiagnostic;
use narxia_workspace::parser_tests::ParserTestSingleFolder;
use narxia_hir as hir;
use narxia_syn::syntax_kind::SyntaxKind;
use narxia_syn::token_source::TokenSource;
use narxia_test_runner::parser_tests::lower_to_hir;
use owo_colors::OwoColorize;

struct OrphanSpanVisitor<'hir> {
    hir_map: &'hir HirMap,
    buffer: String,
    ignore_sets: Vec<(std::ops::Range<usize>, char)>,
}

fn replace_in_utf8(s: &mut String, start: usize, end: usize, ch: char) {
    let mut new_str = String::with_capacity(s.len());

    for (i, c) in s.char_indices() {
        if i >= start && i < end {
            new_str.push(ch);
        } else {
            new_str.push(c);
        }
    }

    *s = new_str;
}

impl<'hir> HirVisitor<'hir> for OrphanSpanVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    fn visit_token_span(&mut self, span: narxia_hir::HirSpan) {
        replace_in_utf8(&mut self.buffer, span.get_start(), span.get_end(), ' ');
    }

    fn visit_ident(&mut self, ident: &'hir hir::Ident) {
        self.visit_token_span(ident.span);
    }

    fn visit_use_stmt(&mut self, use_stmt: &'hir hir::UseStmt) {
        // due to the way we construct use statements, we need to ignore these
        self.ignore_sets.push((use_stmt.span.get_range(), ':'));
        self.ignore_sets.push((use_stmt.span.get_range(), '{'));
        self.ignore_sets.push((use_stmt.span.get_range(), '}'));

        narxia_hir::visitor::walk_use_stmt(self, use_stmt);
    }

    fn visit_generic_param_ty_bounds(&mut self, ty_bounds: &'hir hir::GenericParamTyBounds) {
        self.ignore_sets.push((ty_bounds.span.get_range(), '+'));

        narxia_hir::visitor::walk_generic_param_ty_bounds(self, ty_bounds);
    }
}

fn trivia_tokens(input: &str) -> Vec<std::ops::Range<usize>> {
    let mut ranges = Vec::new();

    let mut ts = narxia_syn::token_source::text_ts::TextTokenSource::new(input);

    while let Some(next_token) = ts.next() {
        let kind = next_token.kind();
        if [
            SyntaxKind::WHITESPACE,
            SyntaxKind::NEWLINE,
            SyntaxKind::COMMENT,
        ]
        .contains(&kind)
        {
            ranges.push(next_token.span().range_usize());
        }
    }

    ranges
}

fn run_test(mut test: ParserTestSingleFolder<FsVfs>) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let orig = test.input.get().into_diagnostic()?.into_inner();
    let hir = lower_to_hir(&mut test, &ctx)?;

    let mod_def = hir.mod_def(&ctx.db);

    let hir_map = ctx.db.get_global_ty_ctxt().make_ty_ctxt().hir_map();
    let mut visitor = OrphanSpanVisitor {
        hir_map: &hir_map,
        buffer: orig.clone(),
        ignore_sets: Vec::new(),
    };

    visitor.visit_mod_id(mod_def);

    if !visitor.buffer.trim().is_empty() {
        let old = orig.char_indices().collect::<Vec<_>>();
        let new = visitor.buffer.chars().collect::<Vec<_>>();

        let mut s = String::new();

        let mut any_missing = false;
        let trivia_tokens = trivia_tokens(&orig);

        for ((ai, a), b) in old.iter().zip(new.iter()) {
            let is_missing = (a == b) &&
                // not part of HIR, we don't really care about these
                ![',', ';'].contains(a) && !trivia_tokens.iter().any(|range| range.contains(ai))
                && !visitor.ignore_sets.iter().filter(|(_, ch)| ch == a).any(|(range, _)| range.contains(ai));

            if is_missing {
                any_missing = true;
                write!(&mut s, "{}", a.red().bold()).into_diagnostic()?;
            } else {
                write!(&mut s, "{}", a.green().dimmed()).into_diagnostic()?;
            }
        }

        if any_missing {
            eprintln!("{}", s);

            bail!("Orphan tokens");
        }
    }

    Ok(())
}

narxia_test_runner::test_main_parser_tests_foreach!(|test| { run_test(test) });
