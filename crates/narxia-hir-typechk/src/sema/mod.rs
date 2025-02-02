use narxia_hir::hir::HirIdNewtype;
use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::{self as vis, HirVisitor};
use narxia_hir::{hir, HirId};

use crate::def_id::DefId;
use crate::tyctxt::TyCtxt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElemId(usize);

#[derive(Debug)]
struct ScopeRepr {
    parent: Option<ScopeId>,
    children: Vec<ScopeId>,
    self_elem: Option<ElemId>,
    child_elements: Vec<ElemId>,
}

#[derive(Debug)]
struct ScopeTree {
    scopes: Vec<ScopeRepr>,
    elements: Vec<ScopeElement>,
}

impl ScopeTree {
    fn push_scope(&mut self, parent: Option<ScopeId>, self_elem: Option<ElemId>) -> ScopeId {
        let id = ScopeId(self.scopes.len());

        if let Some(parent) = parent {
            self.scopes[parent.0].children.push(id);
        }

        self.scopes.push(ScopeRepr {
            parent,
            children: Vec::new(),
            self_elem,
            child_elements: Vec::new(),
        });

        id
    }

    fn push_scope_element(&mut self, scope: Option<ScopeId>, element: ScopeElement) -> ElemId {
        let id = ElemId(self.elements.len());
        self.elements.push(element);

        if let Some(scope) = scope {
            self.scopes[scope.0].child_elements.push(id);
        }

        id
    }
}

impl core::ops::Index<ScopeId> for ScopeTree {
    type Output = ScopeRepr;

    fn index(&self, id: ScopeId) -> &Self::Output {
        &self.scopes[id.0]
    }
}

impl core::ops::Index<ElemId> for ScopeTree {
    type Output = ScopeElement;

    fn index(&self, id: ElemId) -> &Self::Output {
        &self.elements[id.0]
    }
}

#[derive(Debug)]
struct ScopeElement {
    hir_id: HirId,
}

fn scope_elem<T: HirIdNewtype>(hir_id: T) -> ScopeElement {
    ScopeElement {
        hir_id: hir_id.hir_id(),
    }
}

#[derive(Debug)]
pub struct ProgramStructure {
    scope_tree: ScopeTree,
}

impl ProgramStructure {
    pub fn scopes(&self) -> impl Iterator<Item = ScopeId> + '_ {
        self.scope_tree
            .scopes
            .iter()
            .enumerate()
            .map(|(i, _)| ScopeId(i))
    }

    pub fn parent(&self, scope: ScopeId) -> Option<ScopeId> {
        self.scope_tree[scope].parent
    }

    pub fn children(&self, scope: ScopeId) -> impl Iterator<Item = ScopeId> + '_ {
        self.scope_tree[scope].children.iter().copied()
    }

    pub fn self_element(&self, scope: ScopeId) -> Option<ElemId> {
        self.scope_tree[scope].self_elem
    }

    pub fn elements(&self, scope: ScopeId) -> impl Iterator<Item = ElemId> + '_ {
        self.scope_tree[scope].child_elements.iter().copied()
    }

    pub fn element(&self, elem: ElemId) -> HirId {
        self.scope_tree[elem].hir_id
    }
}

struct ProgramStructureVisitor<'hir> {
    hir_map: &'hir HirMap,
    program_structure: ProgramStructure,

    stack: Vec<ScopeId>,
}

macro_rules! scope_creating_elements {
    ($($vis_name:ident ($id:ty, $t:ty) => $walk_name:ident $($id_use:expr)? ;)*) => {
        $(
            fn $vis_name(&mut self, id: $id, t: &'hir $t) {
                let last_scope = self.stack.last().copied();
                let self_elem = self.program_structure.scope_tree.push_scope_element(None, scope_elem(id));
                let scope = self.program_structure.scope_tree.push_scope(last_scope, Some(self_elem));
                self.stack.push(scope);

                vis::$walk_name(self, $(if $id_use == () {id}else{id},)? t);

                let s = self.stack.pop();

                debug_assert_eq!(Some(scope), s);
            }
        )*
    };
}

impl<'hir> vis::HirVisitor<'hir> for ProgramStructureVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    scope_creating_elements! {
        visit_mod_def(hir::ModId, hir::ModDef) => walk_mod_def;
        visit_fn_def(hir::FnId, hir::FnDef) => walk_fn_def;
        visit_block(hir::BlockId, hir::Block) => walk_block;
    }
}

pub fn build_program_structure<'tcx>(tcx: TyCtxt<'tcx>, mod_id: hir::ModId) -> ProgramStructure {
    let hir_map = tcx.hir_map();
    let mut visitor = ProgramStructureVisitor {
        hir_map: &*hir_map,
        program_structure: ProgramStructure {
            scope_tree: ScopeTree {
                scopes: Vec::new(),
                elements: Vec::new(),
            },
        },
        stack: Vec::new(),
    };

    visitor.visit_mod_id(mod_id);
    visitor.program_structure
}

struct SemanticAnalysisResult {
    program_structure: ProgramStructure,
    scope_names: Vec<ScopeDefinedNamesBuffer>,
}

struct ScopeDefinedNamesBuffer {
    names: Vec<ScopeDefinedName>,
}

struct ScopeDefinedName {
    name: String,
    def_id: DefId,
}

pub fn analyze_program_structure<'tcx>(
    tcx: TyCtxt<'tcx>,
    mod_id: hir::ModId,
) -> SemanticAnalysisResult {
    let program_structure = build_program_structure(tcx, mod_id);

    let scope_names = program_structure
        .scopes()
        .map(|scope| {
            let mut names = Vec::new();

            for elem in program_structure
                .children(scope)
                .filter_map(|scope| program_structure.self_element(scope))
                .chain(program_structure.elements(scope))
            {
                let hir_id = program_structure.element(elem);
                let def_id = tcx.add_def_id(hir_id);

                names.push(ScopeDefinedName {
                    name: format!("Aaa"),
                    def_id,
                });
            }

            ScopeDefinedNamesBuffer { names }
        })
        .collect();

    SemanticAnalysisResult {
        program_structure,
        scope_names,
    }
}
