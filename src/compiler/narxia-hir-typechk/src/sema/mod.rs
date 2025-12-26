use core::ops;
use std::mem;

use hir::HirId;
use hir::HirIdNewtype;
use hir::hir_map::HirMap;
use hir::visitor as vis;
use hir::visitor::HirVisitor;
use narxia_hir as hir;
use narxia_hir::ExprAtomIdentId;
use narxia_hir::FnParam;
use narxia_hir::Ident;
use narxia_hir::LetStmt;
use narxia_hir::PatKind;
use narxia_hir::Stmt;
use narxia_hir::StmtId;

use crate::def_id::DefId;
use crate::tyctxt::TyCtxt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElemId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RefId(usize);

#[derive(Debug)]
struct ScopeRepr {
    parent: Option<ScopeId>,
    children: Vec<ScopeId>,
    self_elem: Option<ElemId>,
    child_elements: Vec<ElemId>,
    child_refs: Vec<RefId>,
}

#[derive(Debug)]
struct ScopeTree {
    scopes: Vec<ScopeRepr>,
    elements: Vec<ScopeElement>,
    refs: Vec<ExprRef>,
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
            child_refs: Vec::new(),
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

    fn last_scope_element(&self, scope: ScopeId) -> Option<ElemId> {
        self.scopes[scope.0].child_elements.last().copied()
    }

    fn push_expr_ref(
        &mut self,
        scope: ScopeId,
        hir_id: ExprAtomIdentId,
        last_elem_in_scope: Option<ElemId>,
    ) {
        let expr_ref = ExprRef {
            hir_id,
            last_elem_in_scope,
        };
        let expr_id = RefId(self.refs.len());
        self.refs.push(expr_ref);

        self.scopes[scope.0].child_refs.push(expr_id);
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

impl ops::Index<RefId> for ScopeTree {
    type Output = ExprRef;

    fn index(&self, index: RefId) -> &Self::Output {
        &self.refs[index.0]
    }
}

#[derive(Debug)]
struct ExprRef {
    hir_id: ExprAtomIdentId,
    last_elem_in_scope: Option<ElemId>,
}

#[derive(Debug)]
struct ScopeElement {
    hir_id: HirId,
    kind: ScopeElementKind,
}

#[derive(Debug)]
enum ScopeElementKind {
    /// A module definition, such as the following:
    ///
    /// ```nrx
    /// module my_module {}
    /// ```
    ModuleDeclaration,
    /// A function definition, such as the following:
    ///
    /// ```nrx
    /// fn f() {}
    /// ```
    FunctionDeclaration,
    /// A local variable declaration, such as the x in the following statement:
    ///
    /// ```nrx
    /// let x = 4;
    /// ```
    LocalDeclaration,
    /// A local parameter declaration, such as the x in the following function declaration:
    ///
    /// ```nrx
    /// fn f(x: i32) {}
    /// ```
    LocalParam,
    LocalBlockScope,
    UseStmt,
}

impl ScopeElementKind {
    fn scope_element_allows_frefs(&self) -> bool {
        match self {
            ScopeElementKind::ModuleDeclaration => true,
            ScopeElementKind::FunctionDeclaration => true,
            ScopeElementKind::LocalDeclaration => false,
            ScopeElementKind::LocalParam => true,
            ScopeElementKind::LocalBlockScope => true,
            ScopeElementKind::UseStmt => true,
        }
    }
}

fn scope_elem<T: HirIdNewtype>(hir_id: T, kind: ScopeElementKind) -> ScopeElement {
    ScopeElement {
        hir_id: hir_id.hir_id(),
        kind,
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

    fn children_expr_ref_ids(&self, scope_id: ScopeId) -> impl Iterator<Item = RefId> + '_ {
        self.scope_tree[scope_id].child_refs.iter().copied()
    }

    fn children_expr_refs(&self, scope_id: ScopeId) -> impl Iterator<Item = &ExprRef> + '_ {
        self.children_expr_ref_ids(scope_id)
            .map(|r| &self.scope_tree[r])
    }
}

struct ProgramStructureVisitor<'hir> {
    hir_map: &'hir HirMap,
    program_structure: ProgramStructure,

    stack: Vec<ScopeId>,
    expr_last_elem_in_scope: Option<ElemId>,
}

macro_rules! scope_creating_elements {
    ($($vis_name:ident ($t:ty, $scope_elem_kind:expr) => $walk_name:ident;)*) => {
        $(
            fn $vis_name(&mut self, t: &'hir $t) {
                let last_scope = self.stack.last().copied();
                let self_elem = self.program_structure.scope_tree.push_scope_element(None, scope_elem(t.hir_id, $scope_elem_kind));
                let scope = self.program_structure.scope_tree.push_scope(last_scope, Some(self_elem));
                self.stack.push(scope);

                vis::$walk_name(self, t);

                let s = self.stack.pop();

                debug_assert_eq!(Some(scope), s);
            }
        )*
    };
}

impl<'hir> HirVisitor<'hir> for ProgramStructureVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    scope_creating_elements! {
        visit_mod_def(hir::ModDef, ScopeElementKind::ModuleDeclaration) => walk_mod_def;
        visit_fn_def(hir::FnDef, ScopeElementKind::FunctionDeclaration) => walk_fn_def;
        visit_block(hir::Block, ScopeElementKind::LocalBlockScope) => walk_block;
    }

    fn visit_use_stmt(&mut self, t: &'hir hir::UseStmt) {
        let last_scope = self.stack.last().copied();
        let _self_elem = self
            .program_structure
            .scope_tree
            .push_scope_element(last_scope, scope_elem(t.hir_id, ScopeElementKind::UseStmt));

        vis::walk_use_stmt(self, t);
    }

    fn visit_let_stmt(&mut self, id: StmtId, hir: &'hir LetStmt) {
        let last_scope = *self.stack.last().unwrap();
        vis::walk_let_stmt(self, id, hir);

        // push element only after processing the expression
        match &hir.pat.kind {
            PatKind::Ident(ident) => {
                let _self_elem = self.program_structure.scope_tree.push_scope_element(
                    Some(last_scope),
                    scope_elem(*ident, ScopeElementKind::LocalDeclaration),
                );
            }
            k => panic!("Unsupported pattern in let statement: {k:?}"),
        }
    }

    fn visit_stmt(&mut self, hir: &'hir Stmt) {
        if let Some(mut scope) = self.stack.last().copied() {
            self.expr_last_elem_in_scope = None;
            loop {
                let last_scope_element =
                    self.program_structure.scope_tree.last_scope_element(scope);
                if let Some(elem) = last_scope_element {
                    self.expr_last_elem_in_scope = Some(elem);
                    break;
                }
                let Some(p) = self.program_structure.parent(scope) else {
                    break;
                };
                scope = p;
            }
        }

        vis::walk_stmt(self, hir);
    }

    fn visit_fn_param(&mut self, hir: &'hir FnParam) {
        let last_scope = self.stack.last().copied();
        let _self_elem = self.program_structure.scope_tree.push_scope_element(
            last_scope,
            scope_elem(hir.hir_id, ScopeElementKind::LocalParam),
        );

        vis::walk_fn_param(self, hir);
    }

    fn visit_expr_atom_ident(&mut self, t: &'hir hir::ExprAtomIdent) {
        let last_scope = self.stack.last().copied().unwrap();
        let last_scope_element = self.expr_last_elem_in_scope;

        self.program_structure
            .scope_tree
            .push_expr_ref(last_scope, t.hir_id, last_scope_element);

        vis::walk_expr_atom_ident(self, t);
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
                refs: Vec::new(),
            },
        },
        stack: Vec::new(),
        expr_last_elem_in_scope: None,
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
    elem_id: ElemId,
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
                let elem_kind = &program_structure.scope_tree[elem].kind;

                match elem_kind {
                    ScopeElementKind::ModuleDeclaration => {
                        let mod_def = hir_elem.assert_is_module();
                        let def_id = tcx.add_def_id(hir_id);
                        names.push(ScopeDefinedName {
                            name: mod_def.name.text.clone(),
                            def_id,
                            elem_id: elem,
                        });
                    }
                    ScopeElementKind::FunctionDeclaration => {
                        let fn_def = hir_elem.assert_is_function();
                        let def_id = tcx.add_def_id(hir_id);
                        names.push(ScopeDefinedName {
                            name: fn_def.name.text.clone(),
                            def_id,
                            elem_id: elem,
                        });
                    }
                    ScopeElementKind::LocalDeclaration => {
                        let name = hir_elem.assert_is_pat_ident();
                        let def_id = tcx.add_def_id(name.hir_id.hir_id());
                        names.push(ScopeDefinedName {
                            name: name.ident.text.clone(),
                            def_id,
                            elem_id: elem,
                        });
                    }
                    ScopeElementKind::LocalParam => {
                        let param = hir_elem.assert_is_fn_param();
                        let name = match &param.pat.kind {
                            PatKind::Ident(name) => hir_map.get_pat_ident(*name).clone(),
                            p => panic!("Unsupported pattern in function parameter: {p:?}"),
                        };
                        let def_id = tcx.add_def_id(name.hir_id.hir_id());
                        names.push(ScopeDefinedName {
                            name: name.ident.text.clone(),
                            def_id,
                            elem_id: elem,
                        });
                    }
                    ScopeElementKind::LocalBlockScope => {}
                    ScopeElementKind::UseStmt => {}
                }

                // match hir_elem {
                //     hir_map::HirElem::UseStmt(use_stmt) => {
                //         let def_id = tcx.add_def_id(hir_id);
                //         let path = &use_stmt.path;
                //
                //         let imported_name = use_path_imported_name(&hir_map, path);
                //
                //         names.push(ScopeDefinedName {
                //             name: imported_name,
                //             def_id,
                //         });
                //     }
                //     hir_map::HirElem::Block(block) => {}
                //     hir_map::HirElem::PatIdent(pat_ident) => {
                //         let def_id = tcx.add_def_id(hir_id);
                //         names.push(ScopeDefinedName {
                //             name: pat_ident.ident.text.clone(),
                //             def_id,
                //         });
                //     }
                //     hir_map::HirElem::ExprAtomIdent(ident) => {
                //         references.push(ScopeUsedReference {
                //             name: ident.ident.text.clone(),
                //             hir_id,
                //         });
                //     }
                //     _ => todo!(),
                // }
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
    work_queue: Vec<(ScopeId, RefId)>,
    push_back: Vec<(ScopeId, RefId)>,
}

pub fn resolve_work(
    tcx: TyCtxt<'_>,
    mod_id: hir::ModId,
    analysis_results: &SemanticAnalysisResult,
) {
    let _span = narxia_log::einfo_span!("resolve_work");
    let hir_map = tcx.hir_map();

    let mut work_queue = ResolveWorkQueue {
        work_queue: Vec::new(),
        push_back: Vec::new(),
    };

    for scope in analysis_results.program_structure.scopes() {
        for elem in analysis_results
            .program_structure
            .children_expr_ref_ids(scope)
        {
            work_queue.work_queue.push((scope, elem));
        }
    }

    let mut prev_length = work_queue.work_queue.len();
    while !work_queue.work_queue.is_empty() {
        let ResolveWorkQueue {
            work_queue,
            push_back,
        } = &mut work_queue;
        narxia_log::info!("{:?}", work_queue);
        for (scope, ref_id) in work_queue.drain(..) {
            match attempt_to_resolve(
                tcx,
                scope,
                &analysis_results.program_structure.scope_tree[ref_id],
                &hir_map,
                analysis_results,
            ) {
                Ok(()) => {}
                Err(()) => {
                    push_back.push((scope, ref_id));
                }
            }
        }

        mem::swap(work_queue, push_back);
        let current_length = work_queue.len();

        if current_length == prev_length {
            panic!("Unresolvable state: {work_queue:?}");
        }

        prev_length = current_length;
    }
}

fn attempt_to_resolve(
    tcx: TyCtxt<'_>,
    scope_id: ScopeId,
    expr_ref: &ExprRef,
    hir_map: &HirMap,
    analysis_results: &SemanticAnalysisResult,
) -> Result<(), ()> {
    let ident = hir_map.get_expr_atom_ident(expr_ref.hir_id);
    let mut candidates = Vec::new();
    attempt_to_resolve_expr_atom_ident(
        tcx,
        scope_id,
        expr_ref,
        &ident.ident,
        hir_map,
        analysis_results,
        &mut candidates,
    );
    if candidates.is_empty() {
        return Err(());
    }

    if candidates.len() > 1 {
        narxia_log::warn!(
            "Multiple candidates for {}: {:?}",
            ident.ident.text,
            candidates
        );
    }

    let def_id = select_candidate_for_expr(&candidates, tcx, hir_map);
    tcx.resolved_name(ident.hir_id.hir_id(), def_id);
    narxia_log::info!(
        "Resolved {}@{} to {}",
        ident.ident.text,
        ident.hir_id,
        def_id
    );
    Ok(())
}

fn attempt_to_resolve_expr_atom_ident(
    tcx: TyCtxt<'_>,
    scope_id: ScopeId,
    expr_ref: &ExprRef,
    ident: &Ident,
    hir_map: &HirMap,
    analysis_results: &SemanticAnalysisResult,
    candidates: &mut Vec<DefId>,
) {
    // name-resolution-test:resolve-to-let-stmt
    // let s = 1
    // println(s)
    // fn println(s: str) {return}

    // name-resolution-test:resolve-to-let-stmt-shadowing
    // let s = 1
    // let s = 2
    // println(s)
    // fn println(s: str) {return}

    // name-resolution-test:resolve-to-let-stmt-shadowing-2
    // let s = 1
    // {
    //     let s = 2
    //     println(s)
    // }
    // fn println(s: str) {return}

    // name-resolution-test:resolve-to-let-stmt-shadowing-3
    // let s = 1
    // {
    //     let s = 2
    //     {
    //         let s = 3
    //         println(s)
    //     }
    //     println(s)
    // }
    // println(s)
    // fn println(s: str) {return}

    // name-resolution-test:resolve-to-let-stmt-multiple-in-same-scope
    // let s = 1
    // let s = 2
    // let s = s

    // name-resolution-test:resolve-to-fn-param
    // fn foo(s: str) {return s}

    // name-resolution-test:resolve-to-fn-param-shadowing
    // fn foo(s: str) {
    //     let s = 1
    //     return s
    // }

    // name-resolution-test:resolve-to-fn-param-shadowing-2
    // fn foo(s: str) {
    //     {
    //         let s = 1
    //         return s
    //     }
    //     return s
    // }

    let mut current = scope_id;
    loop {
        let scope_candidates = analysis_results.scope_names[current.0]
            .names
            .iter()
            .filter(|a| a.name == ident.text)
            .filter(|a| {
                let declared_before_used = match expr_ref.last_elem_in_scope {
                    Some(last) => a.elem_id <= last,
                    None => false,
                };

                if declared_before_used {
                    return true;
                }

                // here we are referring to something that was declared after the current use
                analysis_results.program_structure.scope_tree[a.elem_id]
                    .kind
                    .scope_element_allows_frefs()
            })
            .map(|a| analysis_results.program_structure.element(a.elem_id))
            .map(|a| tcx.add_def_id(a))
            .collect::<Vec<_>>();

        candidates.extend(scope_candidates);

        if !candidates.is_empty() {
            break;
        }

        let Some(c) = analysis_results.program_structure.parent(current) else {
            break;
        };

        current = c;
    }
}

fn select_candidate_for_expr(candidates: &[DefId], tcx: TyCtxt<'_>, hir_map: &HirMap) -> DefId {
    *candidates.last().unwrap()
}