//! This test checks that there are no orphan hir_ids after the parent
//! of each hir_id is computed, except for the root module.

use dir_structure::traits::vfs::fs_vfs::FsVfs;
use hir::hir_map::HirMap;
use hir::visitor::HirVisitor;
use hir::HirIdNewtype;
use hir::SpecialIdents;
use miette::bail;
use narxia_driver::HirDbg;
use narxia_hir as hir;
use narxia_test_runner::parser_tests::lower_to_hir;
use narxia_workspace::parser_tests::ParserTestSingleFolder;

struct OrphanHirIdVisitor<'hir> {
    hir_map: &'hir HirMap,
    hir_ids: Vec<narxia_hir::HirId>,
}

impl<'hir> HirVisitor<'hir> for OrphanHirIdVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    fn visit_hir_id(&mut self, hir_id: hir::HirId) {
        let parent = self.hir_map.get_parent(hir_id);
        if parent.is_orphan_parent() {
            self.hir_ids.push(hir_id);
        }
    }

    fn visit_mod_id(&mut self, mod_id: hir::ModId) {
        self.q_id_strategy(|this, hir_map| {
            this.visit_mod_def(hir_map.get_mod(mod_id));
        });
    }

    fn visit_mod_def(&mut self, mod_def: &'hir hir::ModDef) {
        if mod_def.name.text != SpecialIdents::ROOT_MODULE {
            self.visit_hir_id(mod_def.hir_id.hir_id());
        }
    }
}

fn run_test(mut test: ParserTestSingleFolder<FsVfs>) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize_in_test();
    let hir = lower_to_hir(&mut test, &ctx)?;

    let mod_def = hir.mod_def(&ctx.db);

    let hir_map = ctx.db.get_global_ty_ctxt().make_ty_ctxt().hir_map();
    let mut visitor = OrphanHirIdVisitor {
        hir_map: &hir_map,
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

fn run_test_main(test: ParserTestSingleFolder<'static, FsVfs>) -> miette::Result<()> {
    run_test(test)
}

include!(concat!(env!("OUT_DIR"), "/parser_tests.rs"));
