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
    Block(Block),
    TyRef(TyRef),
    TyGenericArg(TyGenericArg),
    LetStmt(LetStmt),
    AssignmentStmt(AssignmentStmt),
    StrLiteral(StrLiteral),
    StrLiteralDisplayFragment(StrLiteralDisplayFragment),
    StrLiteralDebugFragment(StrLiteralDebugFragment),
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
            Self::Block(b) => write!(f, "{}", b),
            Self::TyRef(t) => write!(f, "{}", t),
            Self::TyGenericArg(t) => write!(f, "{}", t),
            Self::LetStmt(l) => write!(f, "{}", l),
            Self::AssignmentStmt(a) => write!(f, "{}", a),
            Self::StrLiteral(s) => write!(f, "{}", s),
            Self::StrLiteralDisplayFragment(s) => write!(f, "{}", s),
            Self::StrLiteralDebugFragment(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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

    pub fn push_ref(&mut self, r: HirElem, span: HirSpan) -> HirId {
        let mut id = HirId::new(self.buffer.len());
        id.span = span;
        self.buffer.push(r);
        self.parents.push(HirId::new(0));
        self.files.push(self.current_file.unwrap());
        id
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

    fn update_parent(&mut self, at: HirId, parent: HirId) {
        self.parents[at.id] = parent;
    }

    pub fn get_parent(&self, at: HirId) -> HirId {
        self.parents[at.id]
    }

    pub fn get_file(&self, at: HirId) -> SrcFile {
        self.files[at.id]
    }
}

struct ParentUpdateVisitor<'hir> {
    hir_map: &'hir HirMap,
    stack: Vec<HirId>,
    parents: FxBTreeMap<HirId, HirId>,
}

struct VisIdStrategy;

impl<'hir> crate::visitor::IdHandleStrategy<'hir, ParentUpdateVisitor<'hir>> for VisIdStrategy {
    fn handle_item_id(&self, visitor: &mut ParentUpdateVisitor<'hir>, item_id: crate::hir::ItemId) {
        visitor.visit_hir_id(item_id.0);

        visitor.stack.push(item_id.0);

        let item = visitor.hir_map.get_item(item_id);
        visitor.visit_item(item_id, item);

        visitor.stack.pop();
    }

    fn handle_block_id(
        &self,
        visitor: &mut ParentUpdateVisitor<'hir>,
        block_id: crate::hir::BlockId,
    ) {
        visitor.visit_hir_id(block_id.0);

        visitor.stack.push(block_id.0);

        let block = visitor.hir_map.get_block(block_id);
        visitor.visit_block(block_id, block);

        visitor.stack.pop();
    }

    fn handle_expr_id(&self, visitor: &mut ParentUpdateVisitor<'hir>, expr_id: crate::hir::ExprId) {
        visitor.visit_hir_id(expr_id.0);

        visitor.stack.push(expr_id.0);

        let expr = visitor.hir_map.get_expr(expr_id);
        visitor.visit_expr(expr_id, expr);

        visitor.stack.pop();
    }

    fn handle_stmt_id(&self, visitor: &mut ParentUpdateVisitor<'hir>, stmt_id: crate::hir::StmtId) {
        visitor.visit_hir_id(stmt_id.0);

        visitor.stack.push(stmt_id.0);

        let stmt = visitor.hir_map.get_stmt(stmt_id);
        visitor.visit_stmt(stmt_id, stmt);

        visitor.stack.pop();
    }

    fn handle_fn_id(&self, visitor: &mut ParentUpdateVisitor<'hir>, fn_id: crate::hir::FnId) {
        visitor.visit_hir_id(fn_id.0);

        visitor.stack.push(fn_id.0);

        let fn_def = visitor.hir_map.get_fn(fn_id);
        visitor.visit_fn_def(fn_id, fn_def);

        visitor.stack.pop();
    }

    fn handle_mod_id(&self, visitor: &mut ParentUpdateVisitor<'hir>, mod_id: crate::hir::ModId) {
        visitor.visit_hir_id(mod_id.0);

        visitor.stack.push(mod_id.0);

        let mod_def = visitor.hir_map.get_mod(mod_id);
        visitor.visit_mod_def(mod_id, mod_def);

        visitor.stack.pop();
    }
}

impl<'hir> HirVisitor<'hir> for ParentUpdateVisitor<'hir> {
    type Strategy = VisIdStrategy;

    fn get_strategy(&self) -> Self::Strategy {
        VisIdStrategy
    }

    fn visit_hir_id(&mut self, hir_id: HirId) {
        if let Some(parent) = self.stack.last() {
            self.parents.insert(hir_id, *parent);
        }
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
