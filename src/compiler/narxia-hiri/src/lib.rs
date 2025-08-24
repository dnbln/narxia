use std::cell::RefCell;
use std::rc::Rc;

use narxia_data_structures::FxBTreeMap;
use narxia_hir::hir_map::HirMap;
use narxia_hir::*;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct PlaceId {
    id: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone)]
pub struct Name {
    name: String,
}

impl AsRef<Name> for Name {
    fn as_ref(&self) -> &Name {
        self
    }
}

struct Store {
    store: Vec<InterpValue>,
}

struct InternalFunctionsDef;

impl InternalFunctionsDef {
    const PRINT: InternalFnId = InternalFnId(0);
    const PRINTLN: InternalFnId = InternalFnId(1);
}

pub struct InterpContext<'a> {
    environment: FxBTreeMap<Name, PlaceId>,
    store_ref: Rc<RefCell<Store>>,
    hir_map: &'a HirMap,
}

impl<'a> InterpContext<'a> {
    pub fn new(hir_map: &'a HirMap) -> Self {
        let mut ctxt = Self {
            environment: FxBTreeMap::new(),
            store_ref: Rc::new(RefCell::new(Store { store: Vec::new() })),
            hir_map,
        };

        ctxt.declare_internal_functions();

        ctxt
    }

    fn decl_internal_fn(&mut self, name: &str, id: InternalFnId) {
        let id = self.push_store(InterpValue::InternalFn(id));
        self.environment.insert(
            Name {
                name: name.to_string(),
            },
            id,
        );
    }

    fn declare_internal_functions(&mut self) {
        self.decl_internal_fn("print", InternalFunctionsDef::PRINT);
        self.decl_internal_fn("println", InternalFunctionsDef::PRINTLN);
    }

    fn push_store(&mut self, value: InterpValue) -> PlaceId {
        let mut sr = self.store_ref.borrow_mut();
        let l = sr.store.len();
        sr.store.push(value);

        PlaceId { id: l }
    }

    fn fetch_store(&self, place_id: PlaceId) -> InterpValue {
        self.store_ref.borrow().store[place_id.id].clone()
    }

    fn resolve_name(&self, name: &Name) -> PlaceId {
        self.environment[name]
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
enum NumValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F32(f32),
    F64(f64),
}

fn num_value_from_num_lit_value(num_lit_value: NumLitValue) -> NumValue {
    match num_lit_value {
        NumLitValue::I8(i) => NumValue::I8(i),
        NumLitValue::I16(i) => NumValue::I16(i),
        NumLitValue::I32(i) => NumValue::I32(i),
        NumLitValue::I64(i) => NumValue::I64(i),
        NumLitValue::I128(i) => NumValue::I128(i),
        NumLitValue::U8(i) => NumValue::U8(i),
        NumLitValue::U16(i) => NumValue::U16(i),
        NumLitValue::U32(i) => NumValue::U32(i),
        NumLitValue::U64(i) => NumValue::U64(i),
        NumLitValue::U128(i) => NumValue::U128(i),
    }
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
struct InternalFnId(usize);

#[derive(Clone, PartialEq, PartialOrd, Debug)]
enum InterpValue {
    Num(NumValue),
    Bool(bool),
    Str(String),
    Fn(FnId),
    Lambda(LambdaExprId),
    InternalFn(InternalFnId),
    Unit,
    Null,
    Tuple(Vec<InterpValue>),
}

pub fn interp_mod(ctx: &mut InterpContext, mod_def: &ModDef) {
    for item_id in &mod_def.body.as_ref().unwrap().items.items {
        let item = ctx.hir_map.get_item(*item_id);
        match &item.kind {
            ItemKind::FnDef(fn_id) => {
                def_fn(ctx, *fn_id);
            }
            ItemKind::Stmt(stmt_id) => {
                interp_stmt(ctx, ctx.hir_map.get_stmt(*stmt_id)).unwrap();
            }
            ItemKind::UseStmt(_) => {}
            ItemKind::ModDef(_) => {}
        }
    }
}

fn def_fn(ctx: &mut InterpContext, fn_id: FnId) {
    let fn_def = ctx.hir_map.get_fn(fn_id);
    let place_id = ctx.push_store(InterpValue::Fn(fn_id));
    ctx.environment.insert(
        Name {
            name: fn_def.name.text.clone(),
        },
        place_id,
    );
}

fn interp_stmt(ctx: &mut InterpContext, stmt: &Stmt) -> CFResult {
    match &stmt.kind {
        StmtKind::ExprStmt(expr_id) => interp_expr_id(ctx, *expr_id),
        StmtKind::LetStmt(let_stmt) => {
            let init = match let_stmt.init {
                Some((_, expr_id)) => interp_expr_id(ctx, expr_id)?,
                None => InterpValue::Unit,
            };
            let place_id = ctx.push_store(init);

            match &let_stmt.pat.kind {
                PatKind::Ident(ident) => {
                    ctx.environment.insert(
                        Name {
                            name: ctx.hir_map.get_pat_ident(*ident).ident.text.clone(),
                        },
                        place_id,
                    );
                }
                PatKind::Tuple(_) => todo!(),
                PatKind::Wildcard(_) => todo!(),
            }

            Ok(InterpValue::Unit)
        }
        StmtKind::ForStmt(for_stmt) => {
            todo!()
        }
        StmtKind::WhileStmt(_) => todo!(),
        StmtKind::AssignmentStmt(_) => todo!(),
    }
}

#[derive(Clone, Debug)]
pub struct LabelRef {
    name: String,
}

#[derive(Clone, Debug)]
enum ControlFlowAction {
    Continue(Option<LabelRef>),
    Break(Option<LabelRef>, InterpValue),
    Return(InterpValue),
}

type CFResult<T = InterpValue> = Result<T, ControlFlowAction>;

fn interp_expr_id(ctx: &mut InterpContext, expr_id: ExprId) -> CFResult {
    interp_expr(ctx, expr_id, ctx.hir_map.get_expr(expr_id))
}

fn interp_expr(ctx: &mut InterpContext, expr_id: ExprId, expr: &Expr) -> CFResult {
    match &expr.kind {
        ExprKind::Atom(atom) => match &atom.kind {
            ExprAtomKind::Num(num) => {
                let v = num_value_from_num_lit_value(num.parse_to_size(NumLitSize::I32));
                Ok(InterpValue::Num(v))
            }
            ExprAtomKind::Ident(id) => Ok(ctx.fetch_store(ctx.resolve_name(&Name {
                name: ctx.hir_map.get_expr_atom_ident(*id).ident.text.clone(),
            }))),
            ExprAtomKind::Str(s) => {
                let mut constructed_string = String::new();
                for fragment in &s.fragments {
                    match &fragment.kind {
                        StrLiteralFragmentKind::Text(t) => {
                            constructed_string.push_str(&t.token.text);
                        }
                        StrLiteralFragmentKind::EscapedChar(_, c) => {
                            constructed_string.push(*c);
                        }
                        StrLiteralFragmentKind::EscapeSequence(_, s) => {
                            todo!()
                        }
                        StrLiteralFragmentKind::Display(dis) => {
                            let r = interp_expr_id(ctx, dis.expr)?;
                            let displayed = interp_display_impl(ctx, &r);
                            constructed_string.push_str(&displayed);
                        }
                        StrLiteralFragmentKind::Debug(deb) => {
                            let r = interp_expr_id(ctx, deb.expr)?;
                            let displayed = interp_debug_impl(ctx, &r);
                            constructed_string.push_str(&displayed);
                        }
                    }
                }

                Ok(InterpValue::Str(constructed_string))
            }
            ExprAtomKind::LoopExpr(loop_expr) => {
                let loop_result = loop {
                    match interp_block_id(ctx, loop_expr.body) {
                        Ok(_) => {}
                        Err(ControlFlowAction::Continue(_)) => {}
                        Err(ControlFlowAction::Break(_, r)) => break r,
                        Err(e) => return Err(e),
                    }
                };

                Ok(loop_result)
            }
            ExprAtomKind::IfExpr(if_expr) => {
                let cond = interp_expr_id(ctx, if_expr.cond)?;
                if let InterpValue::Bool(b) = cond {
                    if b {
                        interp_expr_id(ctx, if_expr.then)
                    } else if let Some(else_branch) = &if_expr.else_ {
                        interp_expr_id(ctx, else_branch.expr)
                    } else {
                        Ok(InterpValue::Unit)
                    }
                } else {
                    panic!("Expected boolean value in if condition");
                }
            }
            ExprAtomKind::ReturnExpr(ret_expr) => {
                let r = if let Some(ret_expr) = ret_expr.expr {
                    interp_expr_id(ctx, ret_expr)?
                } else {
                    InterpValue::Unit
                };
                Err(ControlFlowAction::Return(r))
            }
            ExprAtomKind::BreakExpr(break_expr) => {
                let r = if let Some(break_expr) = break_expr.expr {
                    interp_expr_id(ctx, break_expr)?
                } else {
                    InterpValue::Unit
                };
                Err(ControlFlowAction::Break(None, r))
            }
            ExprAtomKind::ContinueExpr(continue_expr) => Err(ControlFlowAction::Continue(None)),
            ExprAtomKind::BlockExpr(block_expr) => interp_block_id(ctx, block_expr.block),
            ExprAtomKind::TupleExpr(tuple_expr) => {
                let tuple_values = tuple_expr
                    .exprs
                    .iter()
                    .map(|expr_id| interp_expr_id(ctx, *expr_id))
                    .collect::<CFResult<Vec<_>>>()?;

                Ok(InterpValue::Tuple(tuple_values))
            }
            ExprAtomKind::LambdaExpr(lambda_expr) => Ok(InterpValue::Lambda(LambdaExprId(expr_id))),
        },
        ExprKind::Binary(bin) => {
            let lhs = interp_expr_id(ctx, bin.lhs)?;
            let rhs = interp_expr_id(ctx, bin.rhs)?;

            match bin.op {
                BinOp::Add(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I8(lhs + rhs)))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I16(lhs + rhs)))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I32(lhs + rhs)))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I64(lhs + rhs)))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I128(lhs + rhs)))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U8(lhs + rhs)))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U16(lhs + rhs)))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U32(lhs + rhs)))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U64(lhs + rhs)))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U128(lhs + rhs)))
                            }
                            (NumValue::F32(lhs), NumValue::F32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F32(lhs + rhs)))
                            }
                            (NumValue::F64(lhs), NumValue::F64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F64(lhs + rhs)))
                            }
                            _ => panic!("Invalid types for addition"),
                        }
                    } else {
                        panic!("Invalid types for addition");
                    }
                }
                BinOp::Sub(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I8(lhs - rhs)))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I16(lhs - rhs)))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I32(lhs - rhs)))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I64(lhs - rhs)))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I128(lhs - rhs)))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U8(lhs - rhs)))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U16(lhs - rhs)))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U32(lhs - rhs)))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U64(lhs - rhs)))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U128(lhs - rhs)))
                            }
                            (NumValue::F32(lhs), NumValue::F32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F32(lhs - rhs)))
                            }
                            (NumValue::F64(lhs), NumValue::F64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F64(lhs - rhs)))
                            }
                            _ => panic!("Invalid types for subtraction"),
                        }
                    } else {
                        panic!("Invalid types for subtraction");
                    }
                }
                BinOp::Mul(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I8(lhs * rhs)))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I16(lhs * rhs)))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I32(lhs * rhs)))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I64(lhs * rhs)))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I128(lhs * rhs)))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U8(lhs * rhs)))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U16(lhs * rhs)))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U32(lhs * rhs)))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U64(lhs * rhs)))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U128(lhs * rhs)))
                            }
                            (NumValue::F32(lhs), NumValue::F32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F32(lhs * rhs)))
                            }
                            (NumValue::F64(lhs), NumValue::F64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F64(lhs * rhs)))
                            }
                            _ => {
                                panic!("Invalid types for multiplication");
                            }
                        }
                    } else {
                        panic!("Invalid types for multiplication");
                    }
                }
                BinOp::Div(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I8(lhs / rhs)))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I16(lhs / rhs)))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I32(lhs / rhs)))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I64(lhs / rhs)))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I128(lhs / rhs)))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U8(lhs / rhs)))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U16(lhs / rhs)))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U32(lhs / rhs)))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U64(lhs / rhs)))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U128(lhs / rhs)))
                            }
                            (NumValue::F32(lhs), NumValue::F32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F32(lhs / rhs)))
                            }
                            (NumValue::F64(lhs), NumValue::F64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F64(lhs / rhs)))
                            }
                            _ => {
                                panic!("Invalid types for division");
                            }
                        }
                    } else {
                        panic!("Invalid types for division");
                    }
                }
                BinOp::Mod(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I8(lhs % rhs)))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I16(lhs % rhs)))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I32(lhs % rhs)))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I64(lhs % rhs)))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I128(lhs % rhs)))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U8(lhs % rhs)))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U16(lhs % rhs)))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U32(lhs % rhs)))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U64(lhs % rhs)))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U128(lhs % rhs)))
                            }
                            (NumValue::F32(lhs), NumValue::F32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F32(lhs % rhs)))
                            }
                            (NumValue::F64(lhs), NumValue::F64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::F64(lhs % rhs)))
                            }
                            _ => {
                                panic!("Invalid types for modulo");
                            }
                        }
                    } else {
                        panic!("Invalid types for modulo");
                    }
                }
                BinOp::And(_) => {
                    if let (InterpValue::Bool(lhs), InterpValue::Bool(rhs)) = (lhs, rhs) {
                        Ok(InterpValue::Bool(lhs && rhs))
                    } else {
                        panic!("Invalid types for logical and");
                    }
                }
                BinOp::Or(_) => {
                    if let (InterpValue::Bool(lhs), InterpValue::Bool(rhs)) = (lhs, rhs) {
                        Ok(InterpValue::Bool(lhs || rhs))
                    } else {
                        panic!("Invalid types for logical or");
                    }
                }
                BinOp::Eq(_) => Ok(InterpValue::Bool(lhs == rhs)),
                BinOp::Neq(_) => Ok(InterpValue::Bool(lhs != rhs)),

                BinOp::Lt(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::F32(lhs), NumValue::F32(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            (NumValue::F64(lhs), NumValue::F64(rhs)) => {
                                Ok(InterpValue::Bool(lhs < rhs))
                            }
                            _ => panic!("Invalid types for less than"),
                        }
                    } else {
                        panic!("Invalid types for less than");
                    }
                }
                BinOp::LtEq(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::F32(lhs), NumValue::F32(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            (NumValue::F64(lhs), NumValue::F64(rhs)) => {
                                Ok(InterpValue::Bool(lhs <= rhs))
                            }
                            _ => panic!("Invalid types for less than or equal"),
                        }
                    } else {
                        panic!("Invalid types for less than or equal");
                    }
                }
                BinOp::Gt(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::F32(lhs), NumValue::F32(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            (NumValue::F64(lhs), NumValue::F64(rhs)) => {
                                Ok(InterpValue::Bool(lhs > rhs))
                            }
                            _ => panic!("Invalid types for greater than"),
                        }
                    } else {
                        panic!("Invalid types for greater than");
                    }
                }
                BinOp::GtEq(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::F32(lhs), NumValue::F32(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            (NumValue::F64(lhs), NumValue::F64(rhs)) => {
                                Ok(InterpValue::Bool(lhs >= rhs))
                            }
                            _ => panic!("Invalid types for greater than or equal"),
                        }
                    } else {
                        panic!("Invalid types for greater than or equal");
                    }
                }
                BinOp::BitAnd(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I8(lhs & rhs)))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I16(lhs & rhs)))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I32(lhs & rhs)))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I64(lhs & rhs)))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I128(lhs & rhs)))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U8(lhs & rhs)))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U16(lhs & rhs)))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U32(lhs & rhs)))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U64(lhs & rhs)))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U128(lhs & rhs)))
                            }
                            _ => panic!("Invalid types for bitwise and"),
                        }
                    } else {
                        panic!("Invalid types for bitwise and");
                    }
                }
                BinOp::BitOr(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I8(lhs | rhs)))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I16(lhs | rhs)))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I32(lhs | rhs)))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I64(lhs | rhs)))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I128(lhs | rhs)))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U8(lhs | rhs)))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U16(lhs | rhs)))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U32(lhs | rhs)))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U64(lhs | rhs)))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U128(lhs | rhs)))
                            }
                            _ => panic!("Invalid types for bitwise or"),
                        }
                    } else {
                        panic!("Invalid types for bitwise or");
                    }
                }
                BinOp::Xor(_) => {
                    if let (InterpValue::Num(lhs), InterpValue::Num(rhs)) = (lhs, rhs) {
                        match (lhs, rhs) {
                            (NumValue::I8(lhs), NumValue::I8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I8(lhs ^ rhs)))
                            }
                            (NumValue::I16(lhs), NumValue::I16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I16(lhs ^ rhs)))
                            }
                            (NumValue::I32(lhs), NumValue::I32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I32(lhs ^ rhs)))
                            }
                            (NumValue::I64(lhs), NumValue::I64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I64(lhs ^ rhs)))
                            }
                            (NumValue::I128(lhs), NumValue::I128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::I128(lhs ^ rhs)))
                            }
                            (NumValue::U8(lhs), NumValue::U8(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U8(lhs ^ rhs)))
                            }
                            (NumValue::U16(lhs), NumValue::U16(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U16(lhs ^ rhs)))
                            }
                            (NumValue::U32(lhs), NumValue::U32(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U32(lhs ^ rhs)))
                            }
                            (NumValue::U64(lhs), NumValue::U64(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U64(lhs ^ rhs)))
                            }
                            (NumValue::U128(lhs), NumValue::U128(rhs)) => {
                                Ok(InterpValue::Num(NumValue::U128(lhs ^ rhs)))
                            }
                            _ => panic!("Invalid types for bitwise xor"),
                        }
                    } else {
                        panic!("Invalid types for bitwise xor");
                    }
                }
            }
        }
        ExprKind::CallExpr(call) => {
            let callee = interp_expr_id(ctx, call.callee)?;
            match callee {
                InterpValue::Fn(fn_id) => Ok(InterpValue::Unit),
                InterpValue::Lambda(lambda) => Ok(InterpValue::Unit),
                InterpValue::InternalFn(internal_fn) => match internal_fn {
                    InternalFunctionsDef::PRINT => {
                        let arg = interp_expr_id(ctx, call.args.args[0])?;
                        print!("{}", interp_display_impl(ctx, &arg));
                        Ok(InterpValue::Unit)
                    }
                    InternalFunctionsDef::PRINTLN => {
                        let arg = interp_expr_id(ctx, call.args.args[0])?;
                        println!("{}", interp_display_impl(ctx, &arg));
                        Ok(InterpValue::Unit)
                    }
                    _ => todo!(),
                },
                x => panic!("Expected function or lambda, got {x:?}"),
            }
        }
        ExprKind::IndexExpr(index) => {
            let base = interp_expr_id(ctx, index.base)?;
            let index = interp_expr_id(ctx, index.index)?;

            match base {
                InterpValue::Tuple(t) => {
                    if let InterpValue::Num(NumValue::I32(i)) = index {
                        Ok(t[i as usize].clone())
                    } else {
                        panic!("Invalid index type");
                    }
                }
                _ => panic!("Invalid index base type"),
            }
        }
        ExprKind::FieldAccess(field) => todo!(),
        ExprKind::MethodCall(_) => todo!(),
        ExprKind::CustomInfix(_) => todo!(),
    }
}

fn interp_block_id(ctx: &mut InterpContext, block_id: BlockId) -> CFResult {
    interp_block(ctx, ctx.hir_map.get_block(block_id))
}

fn interp_block(ctx: &mut InterpContext, block: &Block) -> CFResult {
    let mut last = InterpValue::Unit;

    for item_id in &block.items.items {
        match &ctx.hir_map.get_item(*item_id).kind {
            ItemKind::Stmt(stmt_id) => {
                last = interp_stmt(ctx, ctx.hir_map.get_stmt(*stmt_id))?;
            }
            ItemKind::FnDef(fn_id) => {
                def_fn(ctx, *fn_id);
            }
            ItemKind::UseStmt(_) => {}
            ItemKind::ModDef(_) => {}
        }
    }

    Ok(last)
}

fn interp_debug_impl(ctx: &mut InterpContext, r: &InterpValue) -> String {
    match r {
        InterpValue::Num(n) => match n {
            NumValue::I8(i) => format!("{i:?}"),
            NumValue::I16(i) => format!("{i:?}"),
            NumValue::I32(i) => format!("{i:?}"),
            NumValue::I64(i) => format!("{i:?}"),
            NumValue::I128(i) => format!("{i:?}"),
            NumValue::U8(i) => format!("{i:?}"),
            NumValue::U16(i) => format!("{i:?}"),
            NumValue::U32(i) => format!("{i:?}"),
            NumValue::U64(i) => format!("{i:?}"),
            NumValue::U128(i) => format!("{i:?}"),
            NumValue::F32(f) => format!("{f:?}"),
            NumValue::F64(f) => format!("{f:?}"),
        },
        InterpValue::Bool(b) => format!("{b:?}"),
        InterpValue::Str(s) => format!("{s:?}"),
        InterpValue::Fn(f) => {
            let fn_def = ctx.hir_map.get_fn(*f);
            format!("{fn_def}")
        }
        InterpValue::Unit => "()".to_string(),
        InterpValue::Null => "null".to_string(),
        InterpValue::Tuple(t) => {
            let mut s = String::new();
            s.push('(');
            for (i, v) in t.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&interp_debug_impl(ctx, v));
            }
            s.push(')');
            s
        }

        InterpValue::Lambda(lambda) => {
            let lambda_expr = ctx.hir_map.get_lambda_expr(*lambda);
            format!("{lambda_expr}")
        }
        InterpValue::InternalFn(fn_id) => match *fn_id {
            InternalFunctionsDef::PRINT => "<fn:print>".to_string(),
            InternalFunctionsDef::PRINTLN => "<fn:println>".to_string(),
            _ => "<fn:unknown>".to_string(),
        },
    }
}

fn interp_display_impl(ctx: &mut InterpContext, r: &InterpValue) -> String {
    match r {
        InterpValue::Num(n) => match n {
            NumValue::I8(i) => format!("{i}"),
            NumValue::I16(i) => format!("{i}"),
            NumValue::I32(i) => format!("{i}"),
            NumValue::I64(i) => format!("{i}"),
            NumValue::I128(i) => format!("{i}"),
            NumValue::U8(i) => format!("{i}"),
            NumValue::U16(i) => format!("{i}"),
            NumValue::U32(i) => format!("{i}"),
            NumValue::U64(i) => format!("{i}"),
            NumValue::U128(i) => format!("{i}"),
            NumValue::F32(f) => format!("{f}"),
            NumValue::F64(f) => format!("{f}"),
        },
        InterpValue::Bool(b) => format!("{b}"),
        InterpValue::Str(s) => s.to_string(),
        InterpValue::Fn(f) => {
            let fn_def = ctx.hir_map.get_fn(*f);
            format!("{fn_def}")
        }
        InterpValue::Unit => "()".to_string(),
        InterpValue::Null => "null".to_string(),
        InterpValue::Tuple(t) => {
            let mut s = String::new();
            s.push('(');
            for (i, v) in t.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&interp_display_impl(ctx, v));
            }
            s.push(')');
            s
        }
        InterpValue::Lambda(lambda) => {
            let lambda_expr = ctx.hir_map.get_lambda_expr(*lambda);
            format!("{lambda_expr}")
        }
        InterpValue::InternalFn(fn_id) => match *fn_id {
            InternalFunctionsDef::PRINT => "<fn:print>".to_string(),
            InternalFunctionsDef::PRINTLN => "<fn:println>".to_string(),
            _ => "<fn:unknown>".to_string(),
        },
    }
}
