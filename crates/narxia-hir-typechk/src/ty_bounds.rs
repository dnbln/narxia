use std::fmt::Debug;

use narxia_data_structures::{FxBTreeMap, FxHashMap};
use narxia_hir::hir::{self, ExprId, FnId, HirIdNewtype, ModId};
use narxia_hir::hir_map::HirMap;
use narxia_hir::visitor::{self, HirVisitor, OwnedVisitable, RecursiveIdHandleStrategy};
use narxia_hir::HirId;

use crate::ty::{BinOpTyClass, FTyBuilder, PrimitiveTy, StdTyClass, Ty, TyClass, TyVar};

#[derive(Clone, PartialEq, Eq)]
pub struct TyBound {
    pub hir_id: HirId,
    kind: TyBoundKind,
}

impl Debug for TyBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{kind:?} @ {hir_id:?}",
            kind = self.kind,
            hir_id = self.hir_id
        )
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TyBoundKind {
    TyEq(Ty, Ty),
    TyImplClass { ty: Ty, klass: TyClass },
}

impl Debug for TyBoundKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyBoundKind::TyEq(ty1, ty2) => write!(f, "{ty1:?} == {ty2:?}"),
            TyBoundKind::TyImplClass { ty, klass } => {
                write!(f, "{ty:?} : {klass:?}")
            }
        }
    }
}

struct TyBoundsGenContext<'a> {
    bounds: &'a mut TyBounds,
    current_tvar_index: usize,
}

impl<'a> TyBoundsGenContext<'a> {
    fn push_bound_eq(&mut self, hir_id: impl HirIdNewtype, ty1: impl Into<Ty>, ty2: impl Into<Ty>) {
        self.bounds.push_bound(TyBound {
            hir_id: hir_id.hir_id(),
            kind: TyBoundKind::TyEq(ty1.into(), ty2.into()),
        });
    }

    fn push_bound_impl_class(
        &mut self,
        hir_id: impl HirIdNewtype,
        ty: impl Into<Ty>,
        klass: impl Into<TyClass>,
    ) {
        self.bounds.push_bound(TyBound {
            hir_id: hir_id.hir_id(),
            kind: TyBoundKind::TyImplClass {
                ty: ty.into(),
                klass: klass.into(),
            },
        });
    }

    fn new_ty_var(&mut self) -> TyVar {
        let id = self.current_tvar_index;
        self.current_tvar_index += 1;
        TyVar { id }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyBounds {
    pub bounds: Vec<TyBound>,
}

impl TyBounds {
    fn push_bound(&mut self, bound: TyBound) {
        self.bounds.push(bound);
    }
}

struct Visitor<'a> {
    ctxt: TyBoundsGenContext<'a>,
    resolv: TyEnvironment,

    name_resolve_results: &'a NameResolveResults,
    control_flow_target_tys: FxBTreeMap<HirId, TyVar>,
    hir_map: &'a HirMap,
}

struct TyEnvironment {
    tys: FxHashMap<HirId, Ty>,
}

impl TyEnvironment {
    fn insert(&mut self, hir_id: HirId, ty: impl Into<Ty>) {
        self.tys.insert(hir_id, ty.into());
    }
}

impl<'hir> HirVisitor<'hir> for Visitor<'hir> {
    type Strategy = visitor::RecursiveIdHandleStrategy<'hir>;

    fn get_strategy(&self) -> Self::Strategy {
        visitor::RecursiveIdHandleStrategy::new(self.hir_map)
    }

    fn visit_expr_call_expr(&mut self, expr_id: ExprId, call_expr: &'hir hir::CallExpr) {
        visitor::walk_expr_call_expr(self, call_expr);

        let callee_ty = &self.resolv.tys[call_expr.callee.hir_id()];

        let mut fty_builder = FTyBuilder::new();

        let args = &call_expr.args;

        for arg in &args.args {
            let ty = &self.resolv.tys[arg.hir_id()];
            fty_builder.add_input(ty.clone());
        }

        let out_ty = self.ctxt.new_ty_var();

        fty_builder.set_output(Ty::TyVar(out_ty));

        let fty = fty_builder.build();

        self.ctxt.push_bound_eq(expr_id, callee_ty.clone(), fty);
        self.resolv.insert(expr_id.hir_id(), out_ty);
    }

    fn visit_expr(&mut self, expr_id: hir::ExprId, expr: &'hir hir::Expr) {
        let tvar = self.ctxt.new_ty_var();
        self.resolv.insert(expr_id.hir_id(), tvar);

        visitor::walk_expr(self, expr_id, expr)
    }

    fn visit_expr_atom(&mut self, expr_id: hir::ExprId, atom: &'hir hir::ExprAtom) {
        visitor::walk_expr_atom(self, expr_id, atom);

        let t = self.resolv.tys[expr_id.hir_id()].clone();

        match &atom.kind {
            hir::ExprAtomKind::Ident(name) => {}
            hir::ExprAtomKind::Str(s) => {
                self.ctxt
                    .push_bound_eq(expr_id, t, Ty::Primitive(PrimitiveTy::Str))
            }
            hir::ExprAtomKind::Num(n) => {
                self.ctxt.push_bound_impl_class(expr_id, t, StdTyClass::Num)
            }
            hir::ExprAtomKind::LoopExpr(_) => {}
            hir::ExprAtomKind::IfExpr(if_expr) => {
                let cond_ty = &self.resolv.tys[if_expr.cond.hir_id()];
                self.ctxt
                    .push_bound_eq(expr_id, cond_ty.clone(), Ty::Primitive(PrimitiveTy::Bool));

                let then_branch_ty = &self.resolv.tys[if_expr.then.hir_id()];
                let else_branch_ty = if_expr
                    .else_
                    .as_ref()
                    .map(|it| &self.resolv.tys[it.expr.hir_id()]);

                let ty = self.ctxt.new_ty_var();

                match else_branch_ty {
                    Some(else_branch_ty) => {
                        self.ctxt.push_bound_eq(expr_id, ty, then_branch_ty.clone());
                        self.ctxt.push_bound_eq(expr_id, ty, else_branch_ty.clone());
                    }
                    None => {
                        self.ctxt.push_bound_eq(expr_id, ty, Ty::UNIT_TY);
                    }
                }

                self.ctxt.push_bound_eq(expr_id, t, ty);
            }
            hir::ExprAtomKind::ReturnExpr(r) => {
                let rty = r
                    .expr
                    .as_ref()
                    .map(|it| &self.resolv.tys[it.hir_id()])
                    .unwrap_or(&Ty::UNIT_TY);
                self.ctxt.push_bound_eq(expr_id, t, rty.clone());
            }
            hir::ExprAtomKind::BreakExpr(_) => {}
            hir::ExprAtomKind::ContinueExpr(_) => {}
            hir::ExprAtomKind::BlockExpr(b) => {
                let bty = &self.resolv.tys[b.block.hir_id()];
                self.ctxt.push_bound_eq(expr_id, t, bty.clone());
            }
            hir::ExprAtomKind::TupleExpr(tle) => {}
            hir::ExprAtomKind::LambdaExpr(_) => {}
        }
    }

    fn visit_assignment_stmt(
        &mut self,
        stmt_id: hir::StmtId,
        assignment_stmt: &'hir hir::AssignmentStmt,
    ) {
        visitor::walk_assignment_stmt(self, assignment_stmt);

        let lhs_ty = &self.resolv.tys[assignment_stmt.lhs.hir_id()];
        let rhs_ty = &self.resolv.tys[assignment_stmt.rhs.hir_id()];

        self.ctxt
            .push_bound_eq(stmt_id, lhs_ty.clone(), rhs_ty.clone());
    }

    fn visit_expr_binary_expr(&mut self, expr_id: ExprId, binary_expr: &'hir hir::BinaryOpExpr) {
        visitor::walk_expr_binary_expr(self, binary_expr);

        let lhs_ty = &self.resolv.tys[binary_expr.lhs.hir_id()];
        let rhs_ty = &self.resolv.tys[binary_expr.rhs.hir_id()];
        let output_ty = &self.resolv.tys[expr_id.hir_id()];

        match binary_expr.op {
            hir::BinOp::Add(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::BinPlus(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Sub(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::BinMinus(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Mul(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::Mul(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Div(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::Div(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Mod(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::Mod(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Eq(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::Eq(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Neq(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::Neq(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Lt(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::Lt(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::LtEq(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::LtEq(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Gt(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::Gt(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::GtEq(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::GtEq(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::And(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::And(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Or(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::Or(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::BitAnd(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::BitAnd(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::BitOr(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::BitOr(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Xor(_) => {
                self.ctxt.push_bound_impl_class(
                    expr_id,
                    lhs_ty.clone(),
                    StdTyClass::Xor(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
        }
    }

    fn visit_str_display_fragment(
        &mut self,
        str_display_fragment: &'hir hir::StrLiteralDisplayFragment,
    ) {
        visitor::walk_str_display_fragment(self, str_display_fragment);

        let t = &self.resolv.tys[str_display_fragment.expr.hir_id()];

        self.ctxt.push_bound_impl_class(
            str_display_fragment.expr.hir_id(),
            t.clone(),
            StdTyClass::Display,
        );
    }

    fn visit_str_debug_fragment(&mut self, str_debug_fragment: &'hir hir::StrLiteralDebugFragment) {
        visitor::walk_str_debug_fragment(self, str_debug_fragment);

        let t = &self.resolv.tys[str_debug_fragment.expr.hir_id()];

        self.ctxt.push_bound_impl_class(
            str_debug_fragment.expr.hir_id(),
            t.clone(),
            StdTyClass::Debug,
        );
    }

    fn visit_block(&mut self, block_id: hir::BlockId, block: &'hir hir::Block) {
        visitor::walk_block(self, block);

        let last_expr_ty = match block.items.items.last() {
            Some(item) => {
                if let hir::Item {
                    kind: hir::ItemKind::Stmt(stmt),
                    ..
                } = self.hir_map.get_item(*item)
                    && let hir::Stmt {
                        kind: hir::StmtKind::ExprStmt(e),
                        ..
                    } = self.hir_map.get_stmt(*stmt)
                {
                    self.resolv.tys[e.hir_id()].clone()
                } else {
                    Ty::UNIT_TY
                }
            }
            None => Ty::UNIT_TY,
        };

        self.resolv.insert(block_id.hir_id(), last_expr_ty);
    }

    fn visit_fn_def(&mut self, fn_id: hir::FnId, fn_def: &'hir hir::FnDef) {
        visitor::walk_fn_head(self, fn_def);

        let t = self.ctxt.new_ty_var();
        let ret_ty = self.ctxt.new_ty_var();

        self.control_flow_target_tys.insert(fn_id.hir_id(), ret_ty);

        let mut fn_ty_builder = FTyBuilder::new();

        for param in &fn_def.params {
            let param_ty = &self.resolv.tys[param.ty.hir_id()];
            fn_ty_builder.add_input(param_ty.clone());
        }

        let ret_ty_actual_for_bound = match &fn_def.ret_ty {
            Some(ret_ty) => &self.resolv.tys[ret_ty.ty.hir_id()],
            None => &Ty::UNIT_TY,
        };

        fn_ty_builder.set_output(Ty::TyVar(ret_ty));

        let fn_ty = fn_ty_builder.build();

        self.ctxt
            .push_bound_eq(fn_id, ret_ty, ret_ty_actual_for_bound.clone());

        self.ctxt.push_bound_eq(fn_id, t, fn_ty.clone());

        self.resolv.insert(fn_id.hir_id(), fn_ty);

        fn_def.body.accept(self)
    }

    fn visit_break_expr(&mut self, expr_id: hir::ExprId, break_expr: &'hir hir::BreakExpr) {
        visitor::walk_break_expr(self, break_expr);

        dbg!(break_expr);

        let target = self.name_resolve_results.resolved_control_flow[expr_id.hir_id()];

        dbg!(target);
        dbg!(&self.control_flow_target_tys);
        let target_ty = self.control_flow_target_tys[&target];

        let ty = match &break_expr.expr {
            Some(expr) => self.resolv.tys[expr.hir_id()].clone(),
            None => Ty::UNIT_TY,
        };

        self.ctxt.push_bound_eq(expr_id, ty, target_ty);
    }

    fn visit_return_expr(&mut self, expr_id: hir::ExprId, ret: &'hir hir::ReturnExpr) {
        visitor::walk_return_expr(self, ret);

        let target_fn = self.name_resolve_results.resolved_control_flow[expr_id.hir_id()];
        let decl_ret_ty = &self.control_flow_target_tys[target_fn];
        let actual_ret_ty = match &ret.expr {
            Some(expr) => &self.resolv.tys[expr.hir_id()],
            None => &Ty::UNIT_TY,
        };

        self.ctxt
            .push_bound_eq(expr_id, decl_ret_ty.clone(), actual_ret_ty.clone());
    }

    fn visit_loop_expr(&mut self, expr_id: hir::ExprId, loop_expr: &'hir hir::LoopExpr) {
        let ty = self.ctxt.new_ty_var();

        self.resolv.insert(expr_id.hir_id(), ty);
        self.control_flow_target_tys.insert(expr_id.hir_id(), ty);

        visitor::walk_loop_expr(self, loop_expr);
    }

    fn visit_for_stmt(&mut self, stmt_id: hir::StmtId, for_stmt: &'hir hir::ForStmt) {
        let ty = self.ctxt.new_ty_var();

        self.resolv.insert(stmt_id.hir_id(), ty);
        self.control_flow_target_tys.insert(stmt_id.hir_id(), ty);

        visitor::walk_for_stmt(self, for_stmt);
    }
}

pub fn collect_ty_bounds(
    mod_id: ModId,
    hir_map: &HirMap,
    name_resolve_results: &NameResolveResults,
) -> TyBounds {
    let mut bounds = TyBounds { bounds: vec![] };

    let ctxt = TyBoundsGenContext {
        bounds: &mut bounds,
        current_tvar_index: 0,
    };

    let mut visitor = Visitor {
        ctxt,
        resolv: TyEnvironment {
            tys: FxHashMap::default(),
        },
        control_flow_target_tys: FxBTreeMap::new(),
        name_resolve_results,
        hir_map,
    };

    visitor.visit_mod_id(mod_id);

    bounds
}

#[derive(Debug)]
pub struct NameResolveResults {
    resolved_control_flow: FxBTreeMap<HirId, HirId>,
}

enum ResolveStackElement {
    LoopRef(HirId),
    FnRef(FnId),
}

struct ResolveStack {
    stack: Vec<ResolveStackElement>,
}

impl ResolveStack {
    fn push_loop_ref(&mut self, loop_id: HirId) {
        self.stack.push(ResolveStackElement::LoopRef(loop_id));
    }

    fn push_fn_ref(&mut self, fn_id: FnId) {
        self.stack.push(ResolveStackElement::FnRef(fn_id));
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn last_loop_ref(&self) -> Option<HirId> {
        self.stack.iter().rev().find_map(|it| match it {
            ResolveStackElement::LoopRef(id) => Some(*id),
            _ => None,
        })
    }

    fn last_fn_ref(&self) -> Option<FnId> {
        self.stack.iter().rev().find_map(|it| match it {
            ResolveStackElement::FnRef(id) => Some(*id),
            _ => None,
        })
    }
}

pub struct ResolveNamesVisitor<'hir> {
    results: NameResolveResults,
    hir_map: &'hir HirMap,
    resolve_stack: ResolveStack,
}

impl<'hir> HirVisitor<'hir> for ResolveNamesVisitor<'hir> {
    type Strategy = RecursiveIdHandleStrategy<'hir>;

    fn get_strategy(&self) -> Self::Strategy {
        RecursiveIdHandleStrategy::new(self.hir_map)
    }

    fn visit_break_expr(&mut self, expr_id: hir::ExprId, break_expr: &'hir hir::BreakExpr) {
        let target = self.resolve_stack.last_loop_ref().unwrap();
        self.results
            .resolved_control_flow
            .insert(expr_id.hir_id(), target);

        visitor::walk_break_expr(self, break_expr)
    }

    fn visit_continue_expr(
        &mut self,
        expr_id: hir::ExprId,
        continue_expr: &'hir hir::ContinueExpr,
    ) {
        let target = self.resolve_stack.last_loop_ref().unwrap();
        self.results
            .resolved_control_flow
            .insert(expr_id.hir_id(), target);

        visitor::walk_continue_expr(self, continue_expr)
    }

    fn visit_for_stmt(&mut self, stmt_id: hir::StmtId, for_stmt: &'hir hir::ForStmt) {
        self.resolve_stack.push_loop_ref(stmt_id.hir_id());

        visitor::walk_for_stmt(self, for_stmt);

        self.resolve_stack.pop();
    }

    fn visit_loop_expr(&mut self, expr_id: hir::ExprId, loop_expr: &'hir hir::LoopExpr) {
        self.resolve_stack.push_loop_ref(expr_id.hir_id());

        visitor::walk_loop_expr(self, loop_expr);

        self.resolve_stack.pop();
    }

    fn visit_fn_def(&mut self, fn_id: hir::FnId, fn_def: &'hir hir::FnDef) {
        self.resolve_stack.push_fn_ref(fn_id);

        visitor::walk_fn_def(self, fn_def);

        self.resolve_stack.pop();
    }

    fn visit_return_expr(&mut self, expr_id: hir::ExprId, ret: &'hir hir::ReturnExpr) {
        let target = self.resolve_stack.last_fn_ref().unwrap();
        self.results
            .resolved_control_flow
            .insert(expr_id.hir_id(), target.hir_id());

        visitor::walk_return_expr(self, ret)
    }
}


pub fn resolve_names(mod_id: ModId, hir_map: &HirMap) -> NameResolveResults {
    let mut visitor = ResolveNamesVisitor {
        results: NameResolveResults {
            resolved_control_flow: FxBTreeMap::new(),
        },
        hir_map,
        resolve_stack: ResolveStack { stack: vec![] },
    };

    visitor.visit_mod_id(mod_id);

    visitor.results
}