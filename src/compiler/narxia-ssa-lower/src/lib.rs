use std::collections::BTreeMap;
use std::collections::BTreeSet;

use hir::hir_map::HirMap;
use narxia_hir as hir;
use narxia_hir::hir_map::HirElem;
use narxia_hir::HirIdNewtype;
use narxia_hir_typechk::def_id::DefId;
use narxia_hir_typechk::tyctxt::TyCtxt;
use narxia_ssa::BinaryExpr;
use narxia_ssa::BinaryOp;
use narxia_ssa::Block;
use narxia_ssa::BlockRef;
use narxia_ssa::CallExpr;
use narxia_ssa::EndInstr;
use narxia_ssa::EndInstrKind;
use narxia_ssa::Function;
use narxia_ssa::FunctionRef;
use narxia_ssa::FunctionTy;
use narxia_ssa::IValue;
use narxia_ssa::Instr;
use narxia_ssa::LocalRef;
use narxia_ssa::Module;
use narxia_ssa::PhiInstr;
use narxia_ssa::PlaceRef;
use narxia_ssa::TyRef;
use narxia_ssa::Value;

struct SsaBuilder {
    module: Module,
}

struct LocalSsaBuilder<'tcx> {
    tcx: TyCtxt<'tcx>,
    function: Function,
    current_block_ref: BlockRef,
    current_local_ref: usize,
    current_place_ref: usize,
    def_id_to_place: BTreeMap<DefId, PlaceRef>,
    init_place_phis: Vec<BTreeMap<PlaceRef, LocalRef>>,
    last_place_values: Vec<BTreeMap<PlaceRef, LocalRef>>,
    preds: Vec<Vec<BlockRef>>,
    all_placerefs: BTreeSet<PlaceRef>,
}

impl SsaBuilder {
    fn new() -> Self {
        Self {
            module: Module { functions: vec![] },
        }
    }

    fn build(&mut self, tcx: TyCtxt, hir_map: &HirMap, f: &hir::FnDef) {
        let params = if let Some(params) = &f.params {
            params.params.iter().map(|it| TyRef { id: 0 }).collect()
        } else {
            vec![]
        };
        let ret = match &f.ret_ty {
            Some(ret) => TyRef { id: 0 },
            None => TyRef { id: 0 },
        };
        let fn_id = FunctionRef {
            id: self.module.functions.len(),
        };
        let mut builder =
            LocalSsaBuilder::new(tcx, fn_id, f.name.text.clone(), FunctionTy { params, ret });
        builder.build(hir_map, f);
        self.module.functions.push(builder.function);
    }
}

impl<'tcx> LocalSsaBuilder<'tcx> {
    fn new(tcx: TyCtxt<'tcx>, fn_id: FunctionRef, fn_name: String, ty: FunctionTy) -> Self {
        Self {
            tcx,
            function: Function {
                name: fn_name,
                ty,
                blocks: vec![],
                fn_id,
            },
            current_local_ref: 0,
            current_block_ref: BlockRef { id: 0 },
            current_place_ref: 0,
            def_id_to_place: BTreeMap::new(),
            init_place_phis: vec![],
            last_place_values: vec![],
            preds: vec![],
            all_placerefs: BTreeSet::new(),
        }
    }

    #[track_caller]
    fn push_to_current_block(&mut self, value: IValue) -> LocalRef {
        let block = &mut self.function.blocks[self.current_block_ref.id];
        if block.end.is_some() {
            panic!("block end already set");
        }
        let lref = LocalRef {
            id: self.current_local_ref,
        };
        self.current_local_ref += 1;
        block.instrs.push(Instr {
            lhs: lref,
            rhs: value,
        });
        lref
    }

    fn push_phi_to_current_block(&mut self, phi: Vec<(BlockRef, LocalRef)>) -> LocalRef {
        let lref = self.new_local_ref();
        let block = &mut self.function.blocks[self.current_block_ref.id];
        block.phi.push(PhiInstr {
            lhs: lref,
            rhs: phi,
        });
        lref
    }

    fn push_var_phi_to_current_block(&mut self, phi: Vec<(BlockRef, LocalRef)>) -> LocalRef {
        let lref = self.new_local_ref();
        let block = &mut self.function.blocks[self.current_block_ref.id];
        block.var_phi.push(PhiInstr {
            lhs: lref,
            rhs: phi,
        });
        lref
    }

    fn new_local_ref(&mut self) -> LocalRef {
        let lref = LocalRef {
            id: self.current_local_ref,
        };
        self.current_local_ref += 1;
        lref
    }
    fn branch(&mut self, bref: BlockRef) -> LocalRef {
        let local_ref = self.new_local_ref();
        let block = &mut self.function.blocks[self.current_block_ref.id];
        if block.end.is_some() {
            panic!("block end already set");
        }

        block.end = Some(EndInstr {
            local_ref,
            kind: EndInstrKind::Branch(bref),
        });
        self.preds[bref.id].push(block.id);
        local_ref
    }

    fn conditional_branch(
        &mut self,
        cond: Value,
        true_block: BlockRef,
        false_block: BlockRef,
    ) -> LocalRef {
        let local_ref = self.new_local_ref();
        let block = &mut self.function.blocks[self.current_block_ref.id];
        if block.end.is_some() {
            panic!("block end already set");
        }
        block.end = Some(EndInstr {
            local_ref,
            kind: EndInstrKind::ConditionalBranch(cond, true_block, false_block),
        });
        self.preds[true_block.id].push(block.id);
        self.preds[false_block.id].push(block.id);
        local_ref
    }

    fn return_value(&mut self, value: Value) -> LocalRef {
        let local_ref = self.new_local_ref();
        let block = &mut self.function.blocks[self.current_block_ref.id];
        if block.end.is_some() {
            panic!("block end already set");
        }
        block.end = Some(EndInstr {
            local_ref,
            kind: EndInstrKind::Return(value),
        });
        local_ref
    }

    fn return_void(&mut self) -> LocalRef {
        let local_ref = self.new_local_ref();
        let block = &mut self.function.blocks[self.current_block_ref.id];
        if block.end.is_some() {
            panic!("block end already set");
        }
        block.end = Some(EndInstr {
            local_ref,
            kind: EndInstrKind::RetVoid,
        });
        local_ref
    }

    fn push_new_block(&mut self) -> BlockRef {
        let bref = BlockRef {
            id: self.function.blocks.len(),
        };
        let block = Block {
            id: bref,
            preds: vec![],
            var_phi: vec![],
            phi: vec![],
            instrs: vec![],
            end: None,
        };
        self.function.blocks.push(block);
        self.preds.push(Vec::new());
        self.current_block_ref = bref;
        let t = self.make_phi_place_table();
        self.last_place_values.push(t.clone());
        self.init_place_phis.push(t);
        bref
    }

    fn make_phi_place_table(&mut self) -> BTreeMap<PlaceRef, LocalRef> {
        let mut map = BTreeMap::new();
        for place in self.all_placerefs.clone() {
            map.insert(place, self.push_var_phi_to_current_block(vec![]));
        }
        map
    }

    fn position_at_block(&mut self, bref: BlockRef) {
        self.current_block_ref = bref;
    }

    fn build_expr(&mut self, hir_map: &HirMap, e: &hir::Expr) -> LocalRef {
        match &e.kind {
            hir::ExprKind::Binary(bin_expr) => {
                let lhs = self.build_expr(hir_map, hir_map.get_expr(bin_expr.lhs));
                let rhs = self.build_expr(hir_map, hir_map.get_expr(bin_expr.rhs));
                let lhs_value = Value::Local(lhs);
                let rhs_value = Value::Local(rhs);
                let bin_expr = BinaryExpr {
                    op: match bin_expr.op {
                        hir::BinOp::Add(_) => BinaryOp::Add,
                        hir::BinOp::Sub(_) => BinaryOp::Sub,
                        hir::BinOp::Mul(_) => BinaryOp::Mul,
                        hir::BinOp::Div(_) => BinaryOp::Div,
                        hir::BinOp::Mod(_) => BinaryOp::Mod,
                        hir::BinOp::Eq(_) => BinaryOp::Eq,
                        hir::BinOp::Neq(_) => BinaryOp::Ne,
                        hir::BinOp::Lt(_) => BinaryOp::Lt,
                        hir::BinOp::LtEq(_) => BinaryOp::Le,
                        hir::BinOp::Gt(_) => BinaryOp::Gt,
                        hir::BinOp::GtEq(_) => BinaryOp::Ge,
                        hir::BinOp::BitAnd(_) => BinaryOp::BitAnd,
                        hir::BinOp::BitOr(_) => BinaryOp::BitOr,
                        hir::BinOp::Xor(_) => BinaryOp::BitXor,
                        hir::BinOp::And(_) => BinaryOp::And,
                        hir::BinOp::Or(_) => BinaryOp::Or,
                    },
                    lhs: lhs_value,
                    rhs: rhs_value,
                };
                self.push_to_current_block(IValue::BinaryExpr(bin_expr))
            }
            hir::ExprKind::CallExpr(call_expr) => {
                let mut args = vec![];
                for arg in &call_expr.args.args {
                    args.push(Value::Local(
                        self.build_expr(hir_map, hir_map.get_expr(*arg)),
                    ));
                }
                let function = FunctionRef { id: 0 };
                self.push_to_current_block(IValue::Call(CallExpr { function, args }))
            }
            hir::ExprKind::Atom(atom) => match &atom.kind {
                hir::ExprAtomKind::Ident(ident) => {
                    // let place = self.block_scopes[self.current_block_ref.id].get(&ident.text);
                    // let place = place.unwrap();
                    // let value = self.last_place_values[self.current_block_ref.id].get(place);
                    // let value = value.unwrap();
                    // *value
                    let def_id = self.tcx.get_name_resolution(ident.hir_id());
                    // if it is a function, we need to generate a function ref
                    let hir_id = self.tcx.lookup_def_id(def_id);
                    let hir_map = self.tcx.hir_map();
                    let elem = hir_map.get(hir_id);
                    match dbg!(elem) {
                        HirElem::Fn(f) => {
                            self.push_to_current_block(IValue::FunctionRef(FunctionRef { id: 0 }))
                        }
                        _ => {
                            let place = self.def_id_to_place.get(&def_id).unwrap();
                            let value = self.last_place_values[self.current_block_ref.id]
                                .get(place)
                                .unwrap();
                            *value
                        }
                    }
                }
                hir::ExprAtomKind::Str(str_literal) => {
                    enum StrConcatElem {
                        Literal(String),
                        DebugLocal(LocalRef),
                        DisplayLocal(LocalRef),
                    }

                    let mut str_concats = vec![];
                    for fragment in &str_literal.fragments {
                        match &fragment.kind {
                            hir::StrLiteralFragmentKind::Text(str_lit) => {
                                if let Some(StrConcatElem::Literal(s)) = str_concats.last_mut() {
                                    s.push_str(&str_lit.token.text);
                                } else {
                                    str_concats
                                        .push(StrConcatElem::Literal(str_lit.token.text.clone()));
                                }
                            }
                            hir::StrLiteralFragmentKind::EscapedChar(e, ..) => {
                                let c = match e.text.as_str() {
                                    "\\n" => '\n',
                                    "\\r" => '\r',
                                    "\\t" => '\t',
                                    "\\\"" => '"',
                                    "\\\\" => '\\',
                                    t => {
                                        todo!("unexpected escape in string literal fragment: {t}");
                                    }
                                };
                                if let Some(StrConcatElem::Literal(s)) = str_concats.last_mut() {
                                    s.push(c);
                                } else {
                                    str_concats.push(StrConcatElem::Literal(c.to_string()));
                                }
                            }
                            hir::StrLiteralFragmentKind::Debug(expr) => {
                                let expr = hir_map.get_expr(expr.expr);
                                let local = self.build_expr(hir_map, expr);
                                str_concats.push(StrConcatElem::DebugLocal(local));
                            }
                            hir::StrLiteralFragmentKind::Display(expr) => {
                                let expr = hir_map.get_expr(expr.expr);
                                let local = self.build_expr(hir_map, expr);
                                str_concats.push(StrConcatElem::DisplayLocal(local));
                            }
                            hir::StrLiteralFragmentKind::EscapeSequence(..) => {
                                todo!()
                            }
                        }
                    }

                    let mut locals = vec![];
                    for elem in str_concats {
                        match elem {
                            StrConcatElem::Literal(s) => {
                                locals.push(
                                    self.push_to_current_block(IValue::Value(Value::ConstStr(s))),
                                );
                            }
                            StrConcatElem::DebugLocal(l) => {
                                locals.push(self.push_to_current_block(IValue::Debug(l)));
                            }
                            StrConcatElem::DisplayLocal(l) => {
                                locals.push(self.push_to_current_block(IValue::Display(l)));
                            }
                        }
                    }

                    self.push_to_current_block(IValue::SConcat(locals))
                }
                hir::ExprAtomKind::Num(num_lit) => {
                    match num_lit.parse_to_size(hir::NumLitSize::I32) {
                        hir::NumLitValue::I32(num) => {
                            self.push_to_current_block(IValue::Value(Value::Const(num)))
                        }
                        _ => todo!(),
                    }
                }
                hir::ExprAtomKind::LoopExpr(loop_expr) => todo!(),
                hir::ExprAtomKind::IfExpr(if_expr) => {
                    // ssa-test:if-expr
                    // fn main() {
                    //     if (1) {
                    //         println("Hello world")
                    //     } else {
                    //         println("Goodbye world")
                    //     }
                    //     println("Goodbye world2")
                    //     return
                    // }
                    //
                    // fn println(s: str) {
                    //     // ...
                    //     return
                    // }

                    // ssa-test:nested-if-exprs
                    // fn main() {
                    //     if (1) {
                    //         let s = "Hello world"
                    //         if (1) {
                    //             println(s)
                    //         } else {
                    //             println("Goodbye world1")
                    //         }
                    //     } else {
                    //         let s = "Goodbye world"
                    //         if (1) {
                    //             println(s)
                    //         } else {
                    //             println("Goodbye world3")
                    //         }
                    //     }
                    //
                    //     println("Goodbye world4")
                    //     return
                    // }
                    //
                    // fn println(s: str) {
                    //     // ...
                    //     return
                    // }
                    let cond = self.build_expr(hir_map, hir_map.get_expr(if_expr.cond));
                    let begin_block = self.current_block_ref;
                    let end = self.push_new_block();
                    let then_block = self.push_new_block();
                    self.position_at_block(then_block);
                    let then_value = self.build_expr(hir_map, hir_map.get_expr(if_expr.then));
                    let then_block_end = self.current_block_ref;
                    self.branch(end);
                    let (else_value, else_block, else_block_end) =
                        if let Some(else_expr) = &if_expr.else_ {
                            let else_block = self.push_new_block();
                            let value = self.build_expr(hir_map, hir_map.get_expr(else_expr.expr));
                            let else_block_end = self.current_block_ref;
                            self.branch(end);
                            (Some(value), else_block, else_block_end)
                        } else {
                            (None, end, end)
                        };

                    self.position_at_block(begin_block);
                    self.conditional_branch(Value::Local(cond), then_block, else_block);

                    self.position_at_block(end);

                    self.push_phi_to_current_block({
                        let mut phi = vec![(then_block_end, then_value)];
                        if let Some(else_value) = else_value {
                            phi.push((else_block_end, else_value));
                        }
                        phi
                    })
                }
                hir::ExprAtomKind::ReturnExpr(return_expr) => {
                    if let Some(ret_expr) = return_expr.expr {
                        let v = self.build_expr(hir_map, hir_map.get_expr(ret_expr));
                        self.return_value(Value::Local(v))
                    } else {
                        self.return_void()
                    }
                }
                hir::ExprAtomKind::BreakExpr(break_expr) => todo!(),
                hir::ExprAtomKind::ContinueExpr(continue_expr) => todo!(),
                hir::ExprAtomKind::BlockExpr(block_expr) => {
                    let b = hir_map.get_block(block_expr.block);
                    let mut last_value = None;
                    for item_id in &b.items.items {
                        let item = hir_map.get_item(*item_id);
                        let hir::ItemKind::Stmt(s) = item.kind else {
                            continue;
                        };
                        let stmt = hir_map.get_stmt(s);
                        match &stmt.kind {
                            hir::StmtKind::ExprStmt(expr_id) => {
                                let expr = hir_map.get_expr(*expr_id);
                                last_value = Some(self.build_expr(hir_map, expr));
                            }
                            hir::StmtKind::LetStmt(let_stmt) => {
                                last_value = None;
                                self.process_let_stmt(hir_map, let_stmt);
                            }
                            hir::StmtKind::ForStmt(for_stmt) => {
                                last_value = None;
                                todo!()
                            }
                            hir::StmtKind::WhileStmt(while_stmt) => {
                                last_value = None;
                                todo!()
                            }
                            hir::StmtKind::AssignmentStmt(assignment_stmt) => {
                                last_value = None;
                                self.process_assignment_stmt(hir_map, assignment_stmt);
                            }
                        }
                    }
                    last_value.unwrap_or_else(|| self.push_to_current_block(IValue::DoNothing))
                }
                hir::ExprAtomKind::TupleExpr(tuple_expr) => todo!(),
                hir::ExprAtomKind::LambdaExpr(lambda_expr) => todo!(),
            },
            hir::ExprKind::CustomInfix(custom_infix) => todo!(),
            hir::ExprKind::IndexExpr(index_expr) => {
                let lhs = self.build_expr(hir_map, hir_map.get_expr(index_expr.base));
                let rhs = self.build_expr(hir_map, hir_map.get_expr(index_expr.index));
                let lhs_value = Value::Local(lhs);
                let rhs_value = Value::Local(rhs);
                let bin_expr = BinaryExpr {
                    op: BinaryOp::Add,
                    lhs: lhs_value,
                    rhs: rhs_value,
                };
                self.push_to_current_block(IValue::BinaryExpr(bin_expr))
            }
            _ => todo!(),
        }
    }

    fn push_place_ref(&mut self) -> PlaceRef {
        let place_ref = PlaceRef {
            id: self.current_place_ref,
        };
        self.current_place_ref += 1;
        place_ref
    }

    fn process_let_stmt(&mut self, hir_map: &HirMap, let_stmt: &hir::LetStmt) {
        let init = if let Some((_, x)) = let_stmt.init {
            Some(self.build_expr(hir_map, hir_map.get_expr(x)))
        } else {
            None
        };

        match &let_stmt.pat.kind {
            hir::PatKind::Ident(ident) => {
                let new_place_ref = self.push_place_ref();
                let def_id = self.tcx.lookup_hir_id_def(ident.hir_id()).unwrap();
                self.insert_decl(def_id, new_place_ref);
                self.insert_last_place_value(new_place_ref, init.unwrap());
            }
            hir::PatKind::Tuple(pats) => {
                todo!()
            }
            hir::PatKind::Wildcard(ident) => todo!(),
        }
    }

    fn insert_decl(&mut self, def_id: DefId, place_ref: PlaceRef) {
        self.def_id_to_place.insert(def_id, place_ref);

        let Self {
            current_local_ref,
            function,
            init_place_phis,
            last_place_values,
            all_placerefs,
            ..
        } = self;

        all_placerefs.insert(place_ref);

        for (block_id, block) in function.blocks.iter_mut().enumerate() {
            let block_id = BlockRef { id: block_id };
            let lref = LocalRef {
                id: *current_local_ref,
            };
            *current_local_ref += 1;
            let phi = PhiInstr {
                lhs: lref,
                rhs: vec![],
            };
            block.var_phi.push(phi);
            init_place_phis[block_id.id].insert(place_ref, lref);
            last_place_values[block_id.id].insert(place_ref, lref);
        }
    }

    fn insert_last_place_value(&mut self, place_ref: PlaceRef, value: LocalRef) {
        self.last_place_values[self.current_block_ref.id].insert(place_ref, value);
    }

    fn process_assignment_stmt(&mut self, hir_map: &HirMap, assignment_stmt: &hir::AssignmentStmt) {
        todo!()
    }

    fn phi_var_transfers(&mut self) {
        let Self {
            preds,
            function,
            last_place_values,
            init_place_phis,
            ..
        } = self;

        for block in &mut function.blocks {
            for pred in &preds[block.id.id] {
                for (place_ref, local_ref) in &init_place_phis[block.id.id] {
                    let Some(last_value) = last_place_values[pred.id].get(place_ref) else {
                        panic!("last value not found");
                    };
                    let Some(vphi) = block.var_phi.iter_mut().find(|phi| phi.lhs == *local_ref)
                    else {
                        panic!("var phi not found");
                    };
                    vphi.rhs.push((*pred, *last_value));
                }
            }
        }
    }

    fn fill_block_preds(&mut self) {
        let Self {
            preds, function, ..
        } = self;
        for block in &mut function.blocks {
            for pred in &preds[block.id.id] {
                block.preds.push(*pred);
            }
        }
    }

    fn dead_phi_elimination(&mut self) {
        let Self {
            function,
            init_place_phis,
            ..
        } = self;

        let mut continue_eliminating = true;
        let mut eliminated_local_refs = BTreeSet::new();

        while continue_eliminating {
            continue_eliminating = false;
            for block in &mut function.blocks {
                if block.preds.is_empty() {
                    for phi in &mut block.var_phi {
                        eliminated_local_refs.insert(phi.lhs);
                    }
                    block.var_phi.clear();
                    init_place_phis.get_mut(block.id.id).unwrap().clear();
                    continue;
                }

                let mut alive_phis = Vec::new();
                for phi in &block.var_phi {
                    let mut preds_left_to_see = block.preds.clone();
                    for (pred, v) in &phi.rhs {
                        if let Some(pos) = preds_left_to_see.iter().position(|x| x == pred) {
                            if !eliminated_local_refs.contains(v) {
                                preds_left_to_see.remove(pos);
                            }
                        } else {
                            panic!("malformed phi");
                        }
                    }

                    if preds_left_to_see.is_empty() {
                        alive_phis.push(phi.clone());
                    } else {
                        continue_eliminating = true;
                        eliminated_local_refs.insert(phi.lhs);
                    }
                }

                block.var_phi = alive_phis;
            }
        }
    }

    fn build(&mut self, hir_map: &HirMap, f: &hir::FnDef) {
        self.function.name = f.name.text.clone();
        self.function.ty = FunctionTy {
            params: vec![],
            ret: TyRef { id: 0 },
        };
        self.function.blocks = vec![];
        self.push_new_block();

        if let Some(params) = &f.params {
            params.params.iter().enumerate().for_each(|(id, p)| {
                match &hir_map.get_fn_param(*p).pat.kind {
                    hir::PatKind::Ident(ident) => {
                        let new_place_ref = self.push_place_ref();
                        let param_v = self.push_to_current_block(IValue::Param(id));
                        self.insert_last_place_value(new_place_ref, param_v);
                    }
                    hir::PatKind::Tuple(pats) => todo!(),
                    hir::PatKind::Wildcard(ident) => todo!(),
                }
            });
        }

        let body = hir_map.get_block(f.body);

        for item_id in &body.items.items {
            let item = hir_map.get_item(*item_id);
            let hir::ItemKind::Stmt(s) = item.kind else {
                continue;
            };
            let stmt = hir_map.get_stmt(s);
            match &stmt.kind {
                hir::StmtKind::ExprStmt(expr_id) => {
                    let expr = hir_map.get_expr(*expr_id);
                    self.build_expr(hir_map, expr);
                }
                hir::StmtKind::LetStmt(let_stmt) => {
                    self.process_let_stmt(hir_map, let_stmt);
                }
                hir::StmtKind::ForStmt(for_stmt) => todo!(),
                hir::StmtKind::WhileStmt(while_stmt) => todo!(),
                hir::StmtKind::AssignmentStmt(assignment_stmt) => {
                    self.process_assignment_stmt(hir_map, assignment_stmt);
                }
            }
        }

        self.fill_block_preds();
        self.phi_var_transfers();
        self.dead_phi_elimination();
    }
}

pub fn convert(tcx: TyCtxt, hir_map: &HirMap, module: hir::ModId) -> Module {
    let mut builder = SsaBuilder::new();
    for item in &hir_map.get_mod(module).body.as_ref().unwrap().items.items {
        let item = hir_map.get_item(*item);
        if let hir::ItemKind::FnDef(f) = item.kind {
            builder.build(tcx, hir_map, hir_map.get_fn(f));
        }
    }
    builder.module
}
