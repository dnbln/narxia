use std::collections::BTreeMap;
use std::fmt;

use narxia_hir::hir::ExprAtomKind;
use narxia_hir::hir::{self};
use narxia_hir::hir_map::HirMap;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Instr {
    pub lhs: LocalRef,
    pub rhs: IValue,
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} = {:?}", self.lhs, self.rhs)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub id: BlockRef,
    pub instrs: Vec<Instr>,
}

#[derive(Clone, PartialEq, Eq, Hash, Copy, PartialOrd, Ord)]
pub struct PlaceRef {
    id: usize,
}

impl fmt::Debug for PlaceRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "p@{}", self.id)
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}:", self.id)?;
        for instr in &self.instrs {
            writeln!(f, "  {:?}", instr)?;
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Local(LocalRef),
    Const(i32),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Local(arg0) => arg0.fmt(f),
            Self::Const(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum IValue {
    Value(Value),
    Param(usize),
    BinaryExpr(BinaryExpr),
    Call(CallExpr),
    Phi(Vec<(BlockRef, LocalRef)>),
    Branch(BlockRef),
    ConditionalBranch(Value, BlockRef, BlockRef),
    Return(Value),
    RetVoid,
    DoNothing,
}

impl fmt::Debug for IValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(arg0) => arg0.fmt(f),
            Self::BinaryExpr(arg0) => arg0.fmt(f),
            Self::Call(arg0) => arg0.fmt(f),
            Self::Phi(arg0) => {
                write!(f, "phi(")?;
                for (id, (block, value)) in arg0.iter().enumerate() {
                    if id != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?} -> {:?}", block, value)?;
                }
                write!(f, ")")
            }
            Self::Branch(arg0) => {
                write!(f, "br {:?}", arg0)
            }
            Self::ConditionalBranch(arg0, arg1, arg2) => {
                write!(f, "br {:?} {:?} {:?}", arg0, arg1, arg2)
            }
            Self::Return(arg0) => {
                write!(f, "ret {:?}", arg0)
            }
            Self::RetVoid => write!(f, "ret"),
            Self::DoNothing => write!(f, "__"),
            Self::Param(p) => {
                write!(f, "param@{}", p)
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub lhs: Value,
    pub rhs: Value,
}

impl fmt::Debug for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?} {:?}", self.op, self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    And,
    Or,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CallExpr {
    pub function: FunctionRef,
    pub args: Vec<Value>,
}

impl fmt::Debug for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "call {:?}(", self.function)?;
        for (id, arg) in self.args.iter().enumerate() {
            if id != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", arg)?;
        }
        write!(f, ")")
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct FunctionRef {
    id: usize,
}

impl fmt::Debug for FunctionRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f@{}", self.id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct LocalRef {
    id: usize,
}

impl fmt::Debug for LocalRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "l@{}", self.id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Copy)]
pub struct BlockRef {
    id: usize,
}

impl fmt::Debug for BlockRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b@{}", self.id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Function {
    name: String,
    ty: FunctionTy,
    blocks: Vec<Block>,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "fn {}: {:?}", self.name, self.ty)?;
        for block in &self.blocks {
            writeln!(f, "{:?}", block)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTy {
    args: Vec<TyRef>,
    ret: TyRef,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TyRef {
    id: usize,
}

impl fmt::Debug for TyRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t@{}", self.id)
    }
}

struct SsaBuilder {
    functions: Vec<Function>,
}

struct LocalSsaBuilder {
    function: Function,
    current_block_ref: BlockRef,
    current_local_ref: usize,
    current_place_ref: usize,
    last_place_values: Vec<BTreeMap<PlaceRef, LocalRef>>,
    block_scopes: Vec<BTreeMap<String, PlaceRef>>,
}

impl SsaBuilder {
    fn new() -> Self {
        Self { functions: vec![] }
    }

    fn build(&mut self, hir_map: &HirMap, f: &hir::FnDef) {
        let mut builder = LocalSsaBuilder::new(
            "".to_string(),
            FunctionTy {
                args: vec![],
                ret: TyRef { id: 0 },
            },
        );
        builder.build(hir_map, f);
        self.functions.push(builder.function);
    }
}

impl LocalSsaBuilder {
    fn new(fn_name: String, ty: FunctionTy) -> Self {
        Self {
            function: Function {
                name: fn_name,
                ty,
                blocks: vec![],
            },
            current_local_ref: 0,
            current_block_ref: BlockRef { id: 0 },
            current_place_ref: 0,
            last_place_values: vec![],
            block_scopes: vec![],
        }
    }

    fn push_to_current_block(&mut self, value: IValue) -> LocalRef {
        let block = &mut self.function.blocks[self.current_block_ref.id];
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

    fn push_new_block(&mut self) -> BlockRef {
        let bref = BlockRef {
            id: self.function.blocks.len(),
        };
        let block = Block {
            id: bref,
            instrs: vec![],
        };
        self.last_place_values.push(BTreeMap::new());
        self.block_scopes.push(BTreeMap::new());
        self.function.blocks.push(block);
        self.current_block_ref = bref;
        bref
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
                    LocalRef { id: 1000 }
                }
                hir::ExprAtomKind::Str(str_literal) => {
                    todo!()
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
                    let cond = self.build_expr(hir_map, hir_map.get_expr(if_expr.cond));
                    let begin_block = self.current_block_ref;
                    let end = self.push_new_block();
                    let then_block = self.push_new_block();
                    let then_value = self.build_expr(hir_map, hir_map.get_expr(if_expr.then));
                    self.push_to_current_block(IValue::Branch(end));
                    let (else_value, else_block) = if let Some(else_expr) = &if_expr.else_ {
                        let else_block = self.push_new_block();
                        let value = self.build_expr(hir_map, hir_map.get_expr(else_expr.expr));
                        self.push_to_current_block(IValue::Branch(end));
                        (Some(value), else_block)
                    } else {
                        (None, end)
                    };

                    self.position_at_block(begin_block);
                    self.push_to_current_block(IValue::ConditionalBranch(
                        Value::Local(cond),
                        then_block,
                        else_block,
                    ));

                    self.position_at_block(end);

                    self.push_to_current_block(IValue::Phi({
                        let mut phi = vec![(then_block, then_value)];
                        if let Some(else_value) = else_value {
                            phi.push((else_block, else_value));
                        }
                        phi
                    }))
                }
                hir::ExprAtomKind::ReturnExpr(return_expr) => {
                    if let Some(ret_expr) = return_expr.expr {
                        let v = self.build_expr(hir_map, hir_map.get_expr(ret_expr));
                        self.push_to_current_block(IValue::Return(Value::Local(v)))
                    } else {
                        self.push_to_current_block(IValue::RetVoid)
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
                self.insert_last_place_value(new_place_ref, init.unwrap());
            }
            hir::PatKind::Tuple(pats) => todo!(),
            hir::PatKind::Wildcard(ident) => todo!(),
        }
    }

    fn insert_last_place_value(&mut self, place_ref: PlaceRef, value: LocalRef) {
        self.last_place_values[self.current_block_ref.id].insert(place_ref, value);
    }

    fn process_assignment_stmt(&mut self, hir_map: &HirMap, assignment_stmt: &hir::AssignmentStmt) {
        let lhs_name = if let hir::Expr {
            kind:
                hir::ExprKind::Atom(hir::ExprAtom {
                    kind: ExprAtomKind::Ident(ident),
                    ..
                }),
            ..
        } = hir_map.get_expr(assignment_stmt.lhs)
        {
            hir_map.get_expr_atom_ident(*ident).ident.text.clone()
        } else {
            todo!()
        };
        // let lhs = self.build_expr(hir_map, hir_map.get_expr(assignment_stmt.lhs));
        let rhs = self.build_expr(hir_map, hir_map.get_expr(assignment_stmt.rhs));
        let place = *self.block_scopes[self.current_block_ref.id]
            .get(&lhs_name)
            .unwrap();
        self.insert_last_place_value(place, rhs);
    }

    fn build(&mut self, hir_map: &HirMap, f: &hir::FnDef) {
        self.function.name = f.name.text.clone();
        self.function.ty = FunctionTy {
            args: vec![],
            ret: TyRef { id: 0 },
        };
        self.function.blocks = vec![];
        self.push_new_block();

        if let Some(params) = &f.params {
            params
                .params
                .iter()
                .enumerate()
                .for_each(|(id, p)| match &p.pat.kind {
                    hir::PatKind::Ident(ident) => {
                        let new_place_ref = self.push_place_ref();
                        let param_v = self.push_to_current_block(IValue::Param(id));
                        self.insert_last_place_value(new_place_ref, param_v);
                    }
                    hir::PatKind::Tuple(pats) => todo!(),
                    hir::PatKind::Wildcard(ident) => todo!(),
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
    }
}

pub fn convert(hir_map: &HirMap, f: &hir::FnDef) -> Function {
    let mut builder = SsaBuilder::new();
    builder.build(hir_map, f);
    builder.functions.pop().unwrap()
}
