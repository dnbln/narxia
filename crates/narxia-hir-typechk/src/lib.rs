use std::fmt::Debug;

use def_id::DefId;
use narxia_data_structures::FxBTreeMap;
use narxia_hir::hir::ModDef;
use narxia_hir::visitor::{self, HirVisitor};
use narxia_hir::{hir, HirId};

pub struct TypechkResults {}

pub mod def_id;
pub mod ty_bounds;

pub struct TyCtxt {}

pub mod ty;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlaceBase {
    pub hir_id: HirId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Projection {
    pub kind: ProjectionKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectionKind {
    Field(String),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Place {
    pub place_base: PlaceBase,
    pub projections: Vec<Projection>,
}

struct NameRefContext<'hir> {
    names: FxBTreeMap<&'hir str, Place>,
}

struct NameResolveVisitor<'hir> {
    local_name_ref_context: NameRefContext<'hir>,
    resolved_names: FxBTreeMap<HirId, Place>,
}

impl<'hir> NameResolveVisitor<'hir> {
    fn lookup_local_name<'a>(&'a self, name_ref: &'a hir::Ident) -> Option<&'a Place> {
        self.local_name_ref_context
            .names
            .get(name_ref.text.as_str())
    }

    fn resolve_name(&mut self, name_ref: &hir::Ident, place: Place) {
        self.resolved_names
            .insert(place.place_base.hir_id, place.clone());
    }
}

impl<'hir> HirVisitor<'hir> for NameResolveVisitor<'hir> {
    fn visit_expr_atom(&mut self, hir_id: HirId, atom: &'hir hir::ExprAtom) {
        visitor::walk_expr_atom(self, hir_id, atom);

        match &atom.kind {
            hir::ExprAtomKind::Ident(name) => {
                let target_hir_id = self.lookup_local_name(name);

                if let Some(target_hir_id) = target_hir_id {
                    self.resolve_name(name, target_hir_id.clone());
                }
            }
            _ => {}
        }
    }

    fn visit_let_stmt(&mut self, let_stmt: &'hir hir::LetStmt) {
        visitor::walk_let_stmt(self, let_stmt);

        let tget_hir_id = let_stmt.hir_id;
        let name = match &let_stmt.pat.kind {
            hir::PatKind::Ident(name) => name,
            _ => todo!(),
        };

        self.local_name_ref_context.names.insert(
            &name.text,
            Place {
                place_base: PlaceBase {
                    hir_id: tget_hir_id,
                },
                projections: vec![],
            },
        );
    }

    fn visit_item_list(&mut self, item_list: &'hir hir::ItemList) {
        let current = self.local_name_ref_context.names.clone();

        for item in &item_list.items {
            if let hir::ItemKind::FnDef(fn_def) = &item.kind {
                self.local_name_ref_context.names.insert(
                    &fn_def.name.text,
                    Place {
                        place_base: PlaceBase {
                            hir_id: item.hir_id,
                        },
                        projections: vec![],
                    },
                );
            }
        }

        visitor::walk_item_list(self, item_list);

        self.local_name_ref_context.names = current;
    }
}

pub fn do_name_resolution<'hir>(mod_def: &'hir ModDef) -> FxBTreeMap<HirId, Place> {
    let mut visitor = NameResolveVisitor {
        local_name_ref_context: NameRefContext {
            names: FxBTreeMap::new(),
        },
        resolved_names: FxBTreeMap::new(),
    };

    visitor.visit_mod_def(mod_def);

    visitor.resolved_names
}
