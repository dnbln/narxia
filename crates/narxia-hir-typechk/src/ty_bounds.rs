use std::fmt::Debug;

use narxia_data_structures::FxHashMap;
use narxia_hir::{hir::{self, ModDef}, visitor::{self, HirVisitor}, HirId};

use crate::ty::{BinOpTyClass, FTyBuilder, PrimitiveTy, StdTyClass, TyClass, TyRef, TyVar};

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
    TyEq(TyRef, TyRef),
    TyImplClass { ty: TyRef, klass: TyClass },
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
    fn push_bound_eq(&mut self, hir_id: HirId, ty1: impl Into<TyRef>, ty2: impl Into<TyRef>) {
        self.bounds.push_bound(TyBound {
            hir_id,
            kind: TyBoundKind::TyEq(ty1.into(), ty2.into()),
        });
    }

    fn push_bound_impl_class(
        &mut self,
        hir_id: HirId,
        ty: impl Into<TyRef>,
        klass: impl Into<TyClass>,
    ) {
        self.bounds.push_bound(TyBound {
            hir_id,
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
    labels: Vec<LabelTyInfo>,
}

struct LabelTyInfo {
    hir_id: HirId,
    block_hir_id: HirId,
    label: String,
    ty: TyRef,
}

struct TyEnvironment {
    tys: FxHashMap<HirId, TyRef>,
}

impl TyEnvironment {
    fn insert(&mut self, hir_id: HirId, ty: impl Into<TyRef>) {
        self.tys.insert(hir_id, ty.into());
    }
}

impl<'hir> HirVisitor<'hir> for Visitor<'hir> {
    fn visit_expr_call_expr(&mut self, hir_id: HirId, call_expr: &'hir hir::CallExpr) {
        visitor::walk_expr_call_expr(self, call_expr);

        let callee_ty = &self.resolv.tys[call_expr.callee.hir_id];

        let mut fty_builder = FTyBuilder::new();

        let args = &call_expr.args;

        for arg in &args.args {
            let ty = &self.resolv.tys[arg.hir_id];
            fty_builder.add_input(ty.clone());
        }

        let out_ty = self.ctxt.new_ty_var();

        fty_builder.set_output(TyRef::TyVar(out_ty));

        let fty = fty_builder.build();

        self.ctxt.push_bound_eq(hir_id, callee_ty.clone(), fty);
        self.resolv.insert(hir_id, out_ty);
    }

    fn visit_expr(&mut self, expr: &'hir hir::Expr) {
        let tvar = self.ctxt.new_ty_var();
        self.resolv.insert(expr.hir_id, tvar);

        visitor::walk_expr(self, expr)
    }

    fn visit_expr_atom(&mut self, hir_id: HirId, atom: &'hir hir::ExprAtom) {
        visitor::walk_expr_atom(self, hir_id, atom);
        let t = self.resolv.tys[hir_id].clone();

        match &atom.kind {
            hir::ExprAtomKind::Ident(name) => {}
            hir::ExprAtomKind::Str(s) => {
                self.ctxt
                    .push_bound_eq(hir_id, t, TyRef::Primitive(PrimitiveTy::Str))
            }
            hir::ExprAtomKind::Num(n) => {
                self.ctxt.push_bound_impl_class(hir_id, t, StdTyClass::Num)
            }
            hir::ExprAtomKind::LoopExpr(_) => {}
            hir::ExprAtomKind::IfExpr(_) => {}
            hir::ExprAtomKind::ReturnExpr(_) => {}
            hir::ExprAtomKind::BreakExpr(_) => {}
            hir::ExprAtomKind::ContinueExpr(_) => {}
            hir::ExprAtomKind::BlockExpr(b) => {
                let bty = &self.resolv.tys[b.block.hir_id];
                self.ctxt.push_bound_eq(hir_id, t, bty.clone());
            }
            hir::ExprAtomKind::TupleLikeExpr(tle) => {}
            hir::ExprAtomKind::LambdaExpr(_) => {}
        }
    }

    fn visit_assignment_stmt(&mut self, assignment_stmt: &'hir hir::AssignmentStmt) {
        visitor::walk_assignment_stmt(self, assignment_stmt);

        let lhs_ty = &self.resolv.tys[assignment_stmt.lhs.hir_id];
        let rhs_ty = &self.resolv.tys[assignment_stmt.rhs.hir_id];

        self.ctxt
            .push_bound_eq(assignment_stmt.hir_id, lhs_ty.clone(), rhs_ty.clone());
    }

    fn visit_expr_binary_expr(&mut self, hir_id: HirId, binary_expr: &'hir hir::BinaryOpExpr) {
        visitor::walk_expr_binary_expr(self, binary_expr);

        let lhs_ty = &self.resolv.tys[binary_expr.lhs.hir_id];
        let rhs_ty = &self.resolv.tys[binary_expr.rhs.hir_id];
        let output_ty = &self.resolv.tys[hir_id];

        match binary_expr.op {
            hir::BinOp::Add(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::BinPlus(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Sub(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::BinMinus(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Mul(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::Mul(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Div(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::Div(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Mod(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::Mod(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Eq(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::Eq(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Neq(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::Neq(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Lt(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::Lt(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::LtEq(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::LtEq(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Gt(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::Gt(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::GtEq(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::GtEq(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::And(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::And(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Or(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::Or(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::BitAnd(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::BitAnd(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::BitOr(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
                    lhs_ty.clone(),
                    StdTyClass::BitOr(BinOpTyClass::new(rhs_ty, output_ty)),
                );
            }
            hir::BinOp::Xor(_) => {
                self.ctxt.push_bound_impl_class(
                    hir_id,
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

        let t = &self.resolv.tys[str_display_fragment.expr.hir_id];

        self.ctxt.push_bound_impl_class(
            str_display_fragment.hir_id,
            t.clone(),
            StdTyClass::Display,
        );
    }

    fn visit_str_debug_fragment(&mut self, str_debug_fragment: &'hir hir::StrLiteralDebugFragment) {
        visitor::walk_str_debug_fragment(self, str_debug_fragment);

        let t = &self.resolv.tys[str_debug_fragment.expr.hir_id];

        self.ctxt
            .push_bound_impl_class(str_debug_fragment.hir_id, t.clone(), StdTyClass::Debug);
    }

    fn visit_block(&mut self, block: &'hir hir::Block) {
        visitor::walk_block(self, block);

        let last_expr_ty = match block.items.items.last() {
            Some(item) => match item {
                hir::Item {
                    kind:
                        hir::ItemKind::Stmt(hir::Stmt {
                            kind: hir::StmtKind::ExprStmt(e),
                            ..
                        }),
                    ..
                } => self.resolv.tys[e.hir_id].clone(),
                _ => TyRef::UNIT_TY,
            },
            None => TyRef::UNIT_TY,
        };

        self.resolv.insert(block.hir_id, last_expr_ty);
    }
}

pub fn collect_ty_bounds(hir: &ModDef) -> TyBounds {
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
        labels: vec![],
    };

    visitor.visit_mod_def(hir);

    bounds
}
