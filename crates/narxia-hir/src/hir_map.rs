use std::fmt;

use narxia_data_structures::FxBTreeMap;
use narxia_src_db::SrcFile;

use crate::hir::*;
use crate::visitor::HirVisitor;
use crate::{HirId, HirSpan};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HirElem {
    Mod(ModDef),
    Item(Item),
    Ident(Ident),
    Fn(FnDef),
    FnParam(FnParam),
    FnRetTy(FnRetTy),
    Expr(Expr),
    LoopExpr(LoopExpr),
    BreakExpr(BreakExpr),
    ContinueExpr(ContinueExpr),
    ReturnExpr(ReturnExpr),
    Pat(Pat),
    Stmt(Stmt),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
    UseStmt(UseStmt),
    UsePathSegment(UsePathSegment),
    Block(Block),
    TyRef(TyRef),
    TyGenericArg(TyGenericArg),
    LetStmt(LetStmt),
    AssignmentStmt(AssignmentStmt),
    StrLiteral(StrLiteral),
    StrLiteralDisplayFragment(StrLiteralDisplayFragment),
    StrLiteralDebugFragment(StrLiteralDebugFragment),
    #[doc(hidden)]
    __Allocated(HirSpan),
}

impl HirElem {
    fn get_hir_id_in_self(&self) -> HirId {
        match self {
            HirElem::Mod(mod_def) => mod_def.hir_id.hir_id(),
            HirElem::Item(item) => item.hir_id.hir_id(),
            HirElem::Ident(ident) => todo!(),
            HirElem::Fn(fn_def) => fn_def.hir_id.hir_id(),
            HirElem::FnParam(fn_param) => todo!(),
            HirElem::FnRetTy(fn_ret_ty) => todo!(),
            HirElem::Expr(expr) => expr.hir_id.hir_id(),
            HirElem::LoopExpr(loop_expr) => todo!(),
            HirElem::BreakExpr(break_expr) => todo!(),
            HirElem::ContinueExpr(continue_expr) => todo!(),
            HirElem::ReturnExpr(return_expr) => todo!(),
            HirElem::Pat(pat) => todo!(),
            HirElem::Stmt(stmt) => stmt.hir_id.hir_id(),
            HirElem::ForStmt(for_stmt) => todo!(),
            HirElem::WhileStmt(while_stmt) => todo!(),
            HirElem::UseStmt(use_stmt) => use_stmt.hir_id.hir_id(),
            HirElem::UsePathSegment(use_path_segment) => use_path_segment.hir_id.hir_id(),
            HirElem::Block(block) => block.hir_id.hir_id(),
            HirElem::TyRef(ty_ref) => ty_ref.hir_id.hir_id(),
            HirElem::TyGenericArg(ty_generic_arg) => ty_generic_arg.hir_id.hir_id(),
            HirElem::LetStmt(let_stmt) => todo!(),
            HirElem::AssignmentStmt(assignment_stmt) => todo!(),
            HirElem::StrLiteral(str_literal) => todo!(),
            HirElem::StrLiteralDisplayFragment(str_literal_display_fragment) => todo!(),
            HirElem::StrLiteralDebugFragment(str_literal_debug_fragment) => todo!(),
            HirElem::__Allocated(_) => unreachable!(),
        }
    }
}

impl fmt::Display for HirElem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mod(m) => write!(f, "{}", m),
            Self::Item(i) => write!(f, "{}", i),
            Self::Ident(i) => write!(f, "{}", i),
            Self::Fn(fn_def) => write!(f, "{}", fn_def),
            Self::FnParam(p) => write!(f, "{}", p),
            Self::FnRetTy(r) => write!(f, "{}", r),
            Self::Expr(e) => write!(f, "{}", e),
            Self::ReturnExpr(e) => write!(f, "{}", e),
            Self::BreakExpr(e) => write!(f, "{}", e),
            Self::ContinueExpr(e) => write!(f, "{}", e),
            Self::LoopExpr(e) => write!(f, "{}", e),
            Self::Pat(p) => write!(f, "{}", p),
            Self::Stmt(s) => write!(f, "{}", s),
            Self::ForStmt(for_stmt) => write!(f, "{}", for_stmt),
            Self::WhileStmt(w) => write!(f, "{}", w),
            Self::UseStmt(u) => write!(f, "{}", u),
            Self::UsePathSegment(u) => write!(f, "{}", u),
            Self::Block(b) => write!(f, "{}", b),
            Self::TyRef(t) => write!(f, "{}", t),
            Self::TyGenericArg(t) => write!(f, "{}", t),
            Self::LetStmt(l) => write!(f, "{}", l),
            Self::AssignmentStmt(a) => write!(f, "{}", a),
            Self::StrLiteral(s) => write!(f, "{}", s),
            Self::StrLiteralDisplayFragment(s) => write!(f, "{}", s),
            Self::StrLiteralDebugFragment(s) => write!(f, "{}", s),
            Self::__Allocated(_) => write!(f, "<Allocated>"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HirMap {
    buffer: Vec<HirElem>,
    parents: Vec<HirId>,
    files: Vec<SrcFile>,
    current_file: Option<SrcFile>,
}

impl HirMap {
    pub fn new() -> Self {
        Self {
            buffer: Vec::new(),
            parents: Vec::new(),
            files: Vec::new(),
            current_file: None,
        }
    }

    pub fn set_current_file(&mut self, file: Option<SrcFile>) {
        self.current_file = file;
    }

    pub fn next_hir_id(&self) -> HirId {
        HirId::new(self.buffer.len())
    }

    pub fn allocate_hir_id(&mut self, span: HirSpan) -> HirId {
        let mut id = self.next_hir_id();
        #[cfg(hir_id_span)]
        {
            id.span = span;
        }
        self.buffer.push(HirElem::__Allocated(span));
        self.parents.push(HirId::ORPHAN_HIRID);
        self.files.push(self.current_file.unwrap());
        id
    }

    pub fn push_ref(&mut self, r: HirElem, span: HirSpan) -> HirId {
        let mut id = self.next_hir_id();
        #[cfg(hir_id_span)]
        {
            id.span = span;
        }
        self.buffer.push(r);
        self.parents.push(HirId::ORPHAN_HIRID);
        self.files.push(self.current_file.unwrap());
        id
    }

    pub fn push_ref_at_allocation(&mut self, r: HirElem, allocation: HirId) {
        debug_assert_eq!(self.buffer[allocation.id], HirElem::__Allocated(allocation.span));
        debug_assert_eq!(r.get_hir_id_in_self(), allocation);
        self.buffer[allocation.id] = r;
    }

    pub fn get(&self, at: HirId) -> &HirElem {
        &self.buffer[at.id]
    }

    pub fn get_item(&self, at: ItemId) -> &Item {
        match self.get(at.0) {
            HirElem::Item(i) => i,
            x => panic!("Expected Item, found {x:?}"),
        }
    }

    pub fn get_expr(&self, at: ExprId) -> &Expr {
        match self.get(at.0) {
            HirElem::Expr(e) => e,
            x => panic!("Expected Expr, found {x:?}"),
        }
    }

    pub fn get_fn(&self, at: FnId) -> &FnDef {
        match self.get(at.0) {
            HirElem::Fn(f) => f,
            x => panic!("Expected FnDef, found {x:?}"),
        }
    }

    pub fn get_mod(&self, at: ModId) -> &ModDef {
        match self.get(at.0) {
            HirElem::Mod(m) => m,
            x => panic!("Expected ModDef, found {x:?}"),
        }
    }

    pub fn get_stmt(&self, at: StmtId) -> &Stmt {
        match self.get(at.0) {
            HirElem::Stmt(s) => s,
            x => panic!("Expected Stmt, found {x:?}"),
        }
    }

    pub fn get_block(&self, at: BlockId) -> &Block {
        match self.get(at.0) {
            HirElem::Block(b) => b,
            x => panic!("Expected Block, found {x:?}"),
        }
    }

    pub fn get_ty_ref(&self, at: TyRefId) -> &TyRef {
        match self.get(at.0) {
            HirElem::TyRef(t) => t,
            x => panic!("Expected TyRef, found {x:?}"),
        }
    }

    pub fn get_use_stmt(&self, at: UseStmtId) -> &UseStmt {
        match self.get(at.0) {
            HirElem::UseStmt(u) => u,
            x => panic!("Expected UseStmt, found {x:?}"),
        }
    }

    pub fn get_use_segment(&self, at: UsePathSegmentId) -> &UsePathSegment {
        match self.get(at.0) {
            HirElem::UsePathSegment(u) => u,
            x => panic!("Expected UsePathSegment, found {x:?}"),
        }
    }

    pub fn get_lambda_expr(&self, at: LambdaExprId) -> &LambdaExpr {
        let expr = self.get_expr(at.0);
        match &expr.kind {
            ExprKind::Atom(ExprAtom {
                kind: ExprAtomKind::LambdaExpr(lambda),
                ..
            }) => lambda,
            x => panic!("Expected LambdaExpr, found {x:?}"),
        }
    }

    pub fn get_ty_generic_arg(&self, at: TyGenericArgId) -> &TyGenericArg {
        match self.get(at.0) {
            HirElem::TyGenericArg(t) => t,
            x => panic!("Expected TyGenericArg, found {x:?}"),
        }
    }

    fn update_parent(&mut self, at: HirId, parent: HirId) {
        self.parents[at.id] = parent;
    }

    pub fn get_parent(&self, at: HirId) -> HirId {
        self.parents[at.id]
    }

    pub fn get_file(&self, at: HirId) -> SrcFile {
        self.files[at.id]
    }

    pub fn __get_allocated_hirids(&self) -> Vec<HirId> {
        self.buffer
            .iter()
            .enumerate()
            .filter_map(|(i, x)| {
                if let HirElem::__Allocated(span) = x {
                    let mut hir_id = HirId::new(i);

                    #[cfg(hir_id_span)]
                    {
                        hir_id.span = *span;
                    }

                    Some(hir_id)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn __test_clean(&mut self) {
        self.buffer.clear();
        self.parents.clear();
        self.files.clear();
    }
}

struct ParentUpdateVisitor<'hir> {
    hir_map: &'hir HirMap,
    stack: Vec<HirId>,
    parents: FxBTreeMap<HirId, HirId>,
}

impl<'hir> HirVisitor<'hir> for ParentUpdateVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir HirMap)>(&mut self, q: Q) {
        q(self, self.hir_map);
    }

    fn visit_hir_id(&mut self, hir_id: HirId) {
        if let Some(parent) = self.stack.last() {
            self.parents.insert(hir_id, *parent);
        }

        self.stack.push(hir_id);
    }

    fn end_visit_hir_id(&mut self, _hir_id: HirId) {
        self.stack.pop();
    }
}

pub fn hir_map_update_parents_in_mod(hir_map: &mut HirMap, mod_id: ModId) {
    let mut visitor = ParentUpdateVisitor {
        hir_map,
        stack: Vec::new(),
        parents: FxBTreeMap::new(),
    };

    visitor.visit_mod_id(mod_id);

    for (hir_id, parent) in visitor.parents.iter() {
        hir_map.update_parent(*hir_id, *parent);
    }
}
