use miette::{bail, IntoDiagnostic};
use narxia_dir_structures::ParserTestSingleFolder;
use narxia_driver::HirDbg;
use narxia_hir::hir::{HirIdNewtype, SpecialIdents};
use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::HirVisitor;
use narxia_test_runner::parser_tests::lower_to_hir;

struct OrphanHirIdVisitor<'hir> {
    hir_map: &'hir HirMap,
    hir_ids: Vec<narxia_hir::HirId>,
}

impl<'hir> HirVisitor<'hir> for OrphanHirIdVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir narxia_hir::hir_map::HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    fn visit_hir_id(&mut self, hir_id: narxia_hir::HirId) {
        let parent = self.hir_map.get_parent(hir_id);
        if parent.is_orphan_parent() {
            self.hir_ids.push(hir_id);
        }
    }

    fn visit_mod_id(&mut self, mod_id: narxia_hir::hir::ModId) {
        self.q_id_strategy(|this, hir_map| {
            this.visit_mod_def(mod_id, hir_map.get_mod(mod_id));
        });
    }

    fn visit_mod_def(
        &mut self,
        mod_id: narxia_hir::hir::ModId,
        mod_def: &'hir narxia_hir::hir::ModDef,
    ) {
        if mod_def.name.text != SpecialIdents::ROOT_MODULE {
            self.visit_hir_id(mod_id.hir_id());
        }
    }
}

fn run_test(mut test: ParserTestSingleFolder) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let hir = lower_to_hir(&mut test, &ctx)?;

    let mod_def = hir.mod_def(&ctx.db);

    let hir_map = ctx.db.get_global_ty_ctxt().make_ty_ctxt().hir_map();
    let mut visitor = OrphanHirIdVisitor {
        hir_map: &*hir_map,
        hir_ids: Vec::new(),
    };

    visitor.visit_mod_id(mod_def);

    if !visitor.hir_ids.is_empty() {
        for hir_id in visitor.hir_ids {
            eprintln!("Orphan hir_id: {:?}", hir_id);
            eprintln!("At:");
            eprintln!("{}", hir_map.get(hir_id).hir_dbg(&ctx));
            eprintln!("In:");
            eprintln!("{}", hir_map.get_mod(mod_def).hir_dbg(&ctx));
        }

        bail!("Orphan hir_ids found");
    }

    Ok(())
}

narxia_test_runner::test_main_parser_tests_foreach!(|test| { run_test(test) });
