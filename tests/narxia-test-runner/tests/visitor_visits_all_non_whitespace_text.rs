use std::fmt::Write as _;

use dir_structure::NewtypeToInner;
use miette::{bail, IntoDiagnostic};
use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::HirVisitor;
use narxia_test_runner::parser_tests::ParserTestSingleFolder;
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

    fn visit_ident(&mut self, ident: &'hir narxia_hir::hir::Ident) {
        self.visit_token_span(ident.span);
    }

    fn visit_use_stmt(
        &mut self,
        _: narxia_hir::hir::UseStmtId,
        use_stmt: &'hir narxia_hir::hir::UseStmt,
    ) {
        // due to the way we construct use statements, we need to ignore these
        self.ignore_sets.push((use_stmt.span.get_range(), ':'));
        self.ignore_sets.push((use_stmt.span.get_range(), '{'));
        self.ignore_sets.push((use_stmt.span.get_range(), '}'));

        narxia_hir::visitor::walk_use_stmt(self, use_stmt);
    }

    fn visit_generic_param_ty_bounds(
        &mut self,
        ty_bounds: &'hir narxia_hir::hir::GenericParamTyBounds,
    ) {
        self.ignore_sets.push((ty_bounds.span.get_range(), '+'));

        narxia_hir::visitor::walk_generic_param_ty_bounds(self, ty_bounds);
    }
}

fn run_test(test: ParserTestSingleFolder) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let orig = test.input.perform_read().into_diagnostic()?.into_inner();
    let hir = test.lower_to_hir(&ctx)?;

    let mod_def = hir.mod_def(&ctx.db);

    let hir_map = ctx.db.get_global_ty_ctxt().make_ty_ctxt().hir_map();
    let mut visitor = OrphanSpanVisitor {
        hir_map: &*hir_map,
        buffer: orig.clone(),
        ignore_sets: Vec::new(),
    };

    visitor.visit_mod_id(mod_def);

    println!("Original:");
    println!("{}", orig);
    println!("Result:");
    println!("{}", visitor.buffer);

    if !visitor.buffer.trim().is_empty() {
        let old = orig.char_indices().collect::<Vec<_>>();
        let new = visitor.buffer.chars().collect::<Vec<_>>();

        let mut s = String::new();

        let mut any_missing = false;

        for ((ai, a), b) in old.iter().zip(new.iter()) {
            let is_missing = (a == b) &&
                // not part of HIR, we don't really care about these
                ![',', ';'].contains(a) && !a.is_whitespace()
                && !visitor.ignore_sets.iter().filter(|(_, ch)| ch == a).any(|(range, _)| range.contains(&ai));

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
