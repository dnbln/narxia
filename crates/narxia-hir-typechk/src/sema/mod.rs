use core::ops;
use std::mem;

use hir::hir_map;
use hir::hir_map::HirMap;
use hir::visitor as vis;
use hir::visitor::HirVisitor;
use hir::HirId;
use hir::HirIdNewtype;
use narxia_hir as hir;

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

impl ops::Index<ScopeId> for ScopeTree {
    type Output = ScopeRepr;

    fn index(&self, id: ScopeId) -> &Self::Output {
        &self.scopes[id.0]
    }
}

impl ops::Index<ElemId> for ScopeTree {
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

    pub fn scope_of_hir_node(&self, hir_id: HirId) -> Option<ScopeId> {
        let elem_id = ElemId(
            self.scope_tree
                .elements
                .iter()
                .position(|it| it.hir_id == hir_id)?,
        );

        Some(ScopeId(
            self.scope_tree
                .scopes
                .iter()
                .position(|scope| scope.self_elem == Some(elem_id))?,
        ))
    }

    pub fn immediate_parent_scope_of_hir_node(&self, hir_id: HirId) -> Option<ScopeId> {
        let elem_id = ElemId(
            self.scope_tree
                .elements
                .iter()
                .position(|it| it.hir_id == hir_id)?,
        );
        let scope_id = ScopeId(
            self.scope_tree
                .scopes
                .iter()
                .position(|scope| scope.child_elements.iter().any(|it| *it == elem_id))?,
        );
        Some(scope_id)
    }

    pub fn parent_scope_of_hir_node(&self, hir_map: &HirMap, hir_id: HirId) -> Option<ScopeId> {
        let mut current = hir_id;

        loop {
            if let Some(scope) = self.immediate_parent_scope_of_hir_node(current) {
                return Some(scope);
            }

            let parent = hir_map.get_parent(current);

            if parent.is_orphan_parent() {
                return None;
            }

            current = parent;
        }
    }
}

struct ProgramStructureVisitor<'hir> {
    hir_map: &'hir HirMap,
    program_structure: ProgramStructure,

    stack: Vec<ScopeId>,
}

macro_rules! scope_creating_elements {
    ($($vis_name:ident ($t:ty) => $walk_name:ident;)*) => {
        $(
            fn $vis_name(&mut self, t: &'hir $t) {
                let last_scope = self.stack.last().copied();
                let self_elem = self.program_structure.scope_tree.push_scope_element(None, scope_elem(t.hir_id));
                let scope = self.program_structure.scope_tree.push_scope(last_scope, Some(self_elem));
                self.stack.push(scope);

                vis::$walk_name(self, t);

                let s = self.stack.pop();

                debug_assert_eq!(Some(scope), s);
            }
        )*
    };
}

macro_rules! scope_adding_elements {
    ($($vis_name:ident ($t:ty) => $walk_name:ident;)*) => {
        $(
            fn $vis_name(&mut self, t: &'hir $t) {
                let last_scope = self.stack.last().copied();
                let _self_elem = self.program_structure.scope_tree.push_scope_element(last_scope, scope_elem(t.hir_id));

                vis::$walk_name(self, t);
            }
        )*
    };
}

impl<'hir> vis::HirVisitor<'hir> for ProgramStructureVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    scope_creating_elements! {
        visit_mod_def(hir::ModDef) => walk_mod_def;
        visit_fn_def(hir::FnDef) => walk_fn_def;
        visit_block(hir::Block) => walk_block;
    }

    scope_adding_elements! {
        visit_use_stmt(hir::UseStmt) => walk_use_stmt;
        visit_pat_ident(hir::PatIdent) => walk_pat_ident;
        visit_expr_atom_ident(hir::ExprAtomIdent) => walk_expr_atom_ident;
    }
}

pub fn build_program_structure(tcx: TyCtxt<'_>, mod_id: hir::ModId) -> ProgramStructure {
    let hir_map = tcx.hir_map();
    let mut visitor = ProgramStructureVisitor {
        hir_map: &hir_map,
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

#[derive(Debug)]
pub struct SemanticAnalysisResult {
    pub program_structure: ProgramStructure,
    scope_names: Vec<ScopeDefinedNamesBuffer>,
    references: Vec<ScopeUsedReferencesBuffer>,
}

#[derive(Debug)]
struct ScopeDefinedNamesBuffer {
    names: Vec<ScopeDefinedName>,
}

#[derive(Debug)]
struct ScopeDefinedName {
    name: String,
    def_id: DefId,
}

#[derive(Debug)]
struct ScopeUsedReferencesBuffer {
    references: Vec<ScopeUsedReference>,
}

#[derive(Debug)]
struct ScopeUsedReference {
    name: String,
    hir_id: HirId,
}

pub fn analyze_program_structure(tcx: TyCtxt<'_>, mod_id: hir::ModId) -> SemanticAnalysisResult {
    let program_structure = build_program_structure(tcx, mod_id);

    let (scope_names, references) = program_structure
        .scopes()
        .map(|scope| {
            let mut names = Vec::new();
            let mut references = Vec::new();

            for elem in program_structure
                .children(scope)
                .filter_map(|scope| program_structure.self_element(scope))
                .chain(program_structure.elements(scope))
            {
                let hir_id = program_structure.element(elem);

                let hir_map = tcx.hir_map();
                let hir_elem = hir_map.get(hir_id);

                match hir_elem {
                    hir_map::HirElem::Mod(mod_def) => {
                        let def_id = tcx.add_def_id(hir_id);
                        names.push(ScopeDefinedName {
                            name: mod_def.name.text.clone(),
                            def_id,
                        });
                    }
                    hir_map::HirElem::Fn(fn_def) => {
                        let def_id = tcx.add_def_id(hir_id);
                        names.push(ScopeDefinedName {
                            name: fn_def.name.text.clone(),
                            def_id,
                        });
                    }
                    hir_map::HirElem::UseStmt(use_stmt) => {
                        let def_id = tcx.add_def_id(hir_id);
                        let path = &use_stmt.path;

                        let imported_name = use_path_imported_name(&hir_map, path);

                        names.push(ScopeDefinedName {
                            name: imported_name,
                            def_id,
                        });
                    }
                    hir_map::HirElem::Block(block) => {}
                    hir_map::HirElem::PatIdent(pat_ident) => {
                        let def_id = tcx.add_def_id(hir_id);
                        names.push(ScopeDefinedName {
                            name: pat_ident.ident.text.clone(),
                            def_id,
                        });
                    }
                    hir_map::HirElem::ExprAtomIdent(ident) => {
                        references.push(ScopeUsedReference {
                            name: ident.ident.text.clone(),
                            hir_id,
                        });
                    }
                    _ => todo!(),
                }
            }

            (
                ScopeDefinedNamesBuffer { names },
                ScopeUsedReferencesBuffer { references },
            )
        })
        .unzip();

    SemanticAnalysisResult {
        program_structure,
        scope_names,
        references,
    }
}

fn use_path_imported_name(hir_map: &HirMap, path: &hir::UsePath) -> String {
    path.alias.as_ref().map_or_else(
        || {
            hir_map
                .get_use_segment(*path.segments.last().unwrap())
                .ident
                .text
                .clone()
        },
        |alias| alias.alias.text.clone(),
    )
}

struct ResolveWorkQueue {
    work_queue: Vec<HirId>,
    push_back: Vec<HirId>,
}

pub fn resolve_work(
    tcx: TyCtxt<'_>,
    mod_id: hir::ModId,
    analysis_results: &SemanticAnalysisResult,
) {
    let span = narxia_log::span!(narxia_log::Level::INFO, "resolve_work");
    let _enter = span.enter();
    let hir_map = tcx.hir_map();

    let mut work_queue = ResolveWorkQueue {
        work_queue: Vec::new(),
        push_back: Vec::new(),
    };

    for scope in analysis_results.program_structure.scopes() {
        for elem in analysis_results
            .program_structure
            .children(scope)
            .filter_map(|scope| analysis_results.program_structure.self_element(scope))
            .chain(analysis_results.program_structure.elements(scope))
        {
            let hir_id = analysis_results.program_structure.element(elem);

            work_queue.work_queue.push(hir_id);
        }
    }

    let mut prev_length = work_queue.work_queue.len();
    while !work_queue.work_queue.is_empty() {
        let ResolveWorkQueue {
            work_queue,
            push_back,
        } = &mut work_queue;
        narxia_log::info!("{:?}", work_queue);
        for hir_id in work_queue.drain(..) {
            match attempt_to_resolve(tcx, hir_id, &hir_map, analysis_results) {
                Ok(()) => {}
                Err(()) => {
                    push_back.push(hir_id);
                }
            }
        }

        mem::swap(work_queue, push_back);
        let current_length = work_queue.len();

        if current_length == prev_length {
            panic!("Unresolvable state");
        }

        prev_length = current_length;
    }
}

fn attempt_to_resolve(
    tcx: TyCtxt<'_>,
    hir_id: HirId,
    hir_map: &HirMap,
    analysis_results: &SemanticAnalysisResult,
) -> Result<(), ()> {
    match hir_map.get(hir_id) {
        hir_map::HirElem::Mod(mod_def) => {
            return Ok(());
        }
        hir_map::HirElem::Item(item) => {
            return Ok(());
        }
        hir_map::HirElem::ExprAtomIdent(ident) => {
            return attempt_to_resolve_expr_atom_ident(tcx, ident, hir_map, analysis_results);
        }
        hir_map::HirElem::PatIdent(pat_ident) => {
            return Ok(());
        }
        hir_map::HirElem::Fn(fn_def) => {
            return Ok(());
        }
        hir_map::HirElem::FnParam(fn_param) => {
            return Ok(());
        }
        hir_map::HirElem::FnRetTy(fn_ret_ty) => {
            return Ok(());
        }
        hir_map::HirElem::Expr(expr) => {
            return Ok(());
        }
        hir_map::HirElem::LoopExpr(loop_expr) => todo!(),
        hir_map::HirElem::BreakExpr(break_expr) => todo!(),
        hir_map::HirElem::ContinueExpr(continue_expr) => todo!(),
        hir_map::HirElem::ReturnExpr(return_expr) => todo!(),
        hir_map::HirElem::Pat(pat) => todo!(),
        hir_map::HirElem::Stmt(stmt) => todo!(),
        hir_map::HirElem::ForStmt(for_stmt) => todo!(),
        hir_map::HirElem::WhileStmt(while_stmt) => todo!(),
        hir_map::HirElem::UseStmt(use_stmt) => todo!(),
        hir_map::HirElem::UsePathSegment(use_path_segment) => todo!(),
        hir_map::HirElem::Block(block) => {
            return Ok(());
        }
        hir_map::HirElem::TyRef(ty_ref) => todo!(),
        hir_map::HirElem::TyGenericArg(ty_generic_arg) => todo!(),
        hir_map::HirElem::AssignmentStmt(assignment_stmt) => todo!(),
        hir_map::HirElem::StrLiteral(str_literal) => todo!(),
        hir_map::HirElem::StrLiteralDisplayFragment(str_literal_display_fragment) => todo!(),
        hir_map::HirElem::StrLiteralDebugFragment(str_literal_debug_fragment) => todo!(),
        x => todo!("{:?}", x),
    }
    Err(())
}

fn attempt_to_resolve_expr_atom_ident(
    tcx: TyCtxt<'_>,
    ident: &hir::ExprAtomIdent,
    hir_map: &HirMap,
    analysis_results: &SemanticAnalysisResult,
) -> Result<(), ()> {
    let mut current_scope = analysis_results
        .program_structure
        .parent_scope_of_hir_node(hir_map, ident.hir_id.hir_id())
        .ok_or(())?;
    loop {
        narxia_log::info!("{:?}", current_scope);
        for name in &analysis_results.scope_names[current_scope.0].names {
            narxia_log::info!("{:?}", name);
            if name.name == ident.ident.text {
                let target = tcx.lookup_def_id(name.def_id);
                match hir_map.get(target) {
                    hir_map::HirElem::PatIdent(pat_ident) => {
                        let parent = hir_map.get_parent(pat_ident.hir_id.hir_id());
                        match hir_map.parent_of_type::<hir_map::PatIdentParent>(pat_ident.hir_id) {
                            hir_map::PatIdentParent::LetStmt(stmt_id, let_stmt) => {
                                narxia_log::info!("let_stmt: {:?}", let_stmt);
                                tcx.resolved_name(ident.hir_id.hir_id(), name.def_id);
                                return Ok(());
                            }
                            hir_map::PatIdentParent::FnParam(fn_param) => {
                                tcx.resolved_name(ident.hir_id.hir_id(), name.def_id);
                                return Ok(());
                            }
                            _ => {
                                todo!()
                            }
                        }
                    }
                    hir_map::HirElem::Fn(fn_def) => {
                        tcx.resolved_name(ident.hir_id.hir_id(), name.def_id);
                        return Ok(());
                    }
                    hir_map::HirElem::FnParam(fn_param) => todo!(),
                    hir_map::HirElem::FnRetTy(fn_ret_ty) => todo!(),
                    hir_map::HirElem::Expr(expr) => todo!(),
                    hir_map::HirElem::ExprAtomIdent(expr_atom_ident) => todo!(),
                    hir_map::HirElem::LoopExpr(loop_expr) => todo!(),
                    hir_map::HirElem::BreakExpr(break_expr) => todo!(),
                    hir_map::HirElem::ContinueExpr(continue_expr) => todo!(),
                    hir_map::HirElem::ReturnExpr(return_expr) => todo!(),
                    hir_map::HirElem::Pat(pat) => todo!(),
                    hir_map::HirElem::Stmt(stmt) => todo!(),
                    hir_map::HirElem::ForStmt(for_stmt) => todo!(),
                    hir_map::HirElem::WhileStmt(while_stmt) => todo!(),
                    hir_map::HirElem::UseStmt(use_stmt) => {
                        tcx.resolved_name(ident.hir_id.hir_id(), name.def_id);
                        return Ok(());
                    }
                    hir_map::HirElem::UsePathSegment(use_path_segment) => todo!(),
                    hir_map::HirElem::Block(block) => todo!(),
                    hir_map::HirElem::TyRef(ty_ref) => todo!(),
                    hir_map::HirElem::TyGenericArg(ty_generic_arg) => todo!(),
                    hir_map::HirElem::AssignmentStmt(assignment_stmt) => todo!(),
                    hir_map::HirElem::StrLiteral(str_literal) => todo!(),
                    hir_map::HirElem::StrLiteralDisplayFragment(str_literal_display_fragment) => {
                        todo!()
                    }
                    hir_map::HirElem::StrLiteralDebugFragment(str_literal_debug_fragment) => {
                        todo!()
                    }
                    _ => todo!(),
                }
            }
        }
        current_scope = analysis_results
            .program_structure
            .parent(current_scope)
            .ok_or(())?;
    }
}
