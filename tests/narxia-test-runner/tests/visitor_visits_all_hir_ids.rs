//! This test checks that the visitor visits all the HIR IDs in the HIR map.
//!
//! This is useful to ensure that the visitor is not missing any HIR nodes.

use miette::bail;
use narxia_dir_structures::ParserTestSingleFolder;
use narxia_driver::HirDbg;
use narxia_hir::visitor::HirVisitor;
use narxia_hir::HirId;
use narxia_test_runner::parser_tests::lower_to_hir;

struct Visitor<'hir> {
    hir_map: &'hir narxia_hir::hir_map::HirMap,
    hir_ids: Vec<narxia_hir::HirId>,
}

impl<'hir> HirVisitor<'hir> for Visitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir narxia_hir::hir_map::HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    fn visit_hir_id(&mut self, hir_id: narxia_hir::HirId) {
        self.hir_ids.push(hir_id);
    }
}

fn run_test(mut test: ParserTestSingleFolder) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let hir = lower_to_hir(&mut test, &ctx)?;
    ctx.trace_hir_file(hir);

    let mod_def = hir.mod_def(&ctx.db);
    let hir_map = ctx.db.get_global_ty_ctxt().make_ty_ctxt().hir_map();
    let mut visitor = Visitor {
        hir_map: &hir_map,
        hir_ids: Vec::new(),
    };

    visitor.visit_mod_id(mod_def);

    visitor.hir_ids.sort_by_key(|id| id.as_usize());

    let mut missed_any = false;

    for (a, b) in visitor.hir_ids.iter().copied().zip(
        visitor
            .hir_ids
            .iter()
            .copied()
            .skip(1)
            .chain(std::iter::once(hir_map.next_hir_id())),
    ) {
        let (a, b) = (a.as_usize(), b.as_usize());

        if a == b {
            // sometimes the same HIR ID is visited multiple times (e.g. expressions).
            continue;
        }

        if a + 1 != b {
            for id in a + 1..b {
                eprintln!("Missing hir_id: {:?}", id);
                eprintln!("{}", hir_map.get(HirId::new(id)).hir_dbg(&ctx));
            }

            missed_any = true;
        }
    }

    if missed_any {
        bail!("Missing hir_ids");
    }

    Ok(())
}

narxia_test_runner::test_main_parser_tests_foreach!(|test| { run_test(test) });
