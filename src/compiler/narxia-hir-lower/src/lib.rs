//! Lowering the syntree to the HIR.
//!
//! The HIR is a high-level intermediate representation of the source code.
//! It is a tree-like structure that represents the source code in a more
//! abstract way than the syntax tree.
//!
//! The HIR is used to perform type checking and other analyses on the source
//! code.
//!
//! This module contains the code that lowers the syntax tree to the HIR.

use hir::hir_map::HirElem;
use hir::hir_map::HirMap;
use hir::*;
use narxia_hir as hir;
use narxia_syn::syntax_kind::SyntaxKind;
use narxia_syn::syntree;
use narxia_syn::syntree::Token;
use narxia_syn::syntree::TreeNode;
use narxia_syn::text_span::TextSpan;

struct HirLowerCtxt<'arena> {
    src_file_start_offset_in_db: usize,
    hir_ref_arena: &'arena mut HirMap,
}

trait HasHirSpan {
    fn span(&self, hir_lower_ctxt: &HirLowerCtxt) -> HirSpan;
}

impl<T> HasHirSpan for T
where
    T: TreeNode,
{
    fn span(&self, hir_lower_ctxt: &HirLowerCtxt) -> HirSpan {
        HirSpan::of_node(self, hir_lower_ctxt)
    }
}

impl HirLowerCtxt<'_> {
    fn allocate_hir_id<T: HirIdNewtype>(&mut self, span: HirSpan) -> T {
        T::new(self.hir_ref_arena.allocate_hir_id(span))
    }

    fn push_ref_at_allocation<T: HirIdNewtype>(&mut self, allocation: T, elem: HirElem) -> T {
        let hir_id = allocation.hir_id();
        self.hir_ref_arena.push_ref_at_allocation(elem, hir_id);
        allocation
    }
}

fn lower_binop(hir_lower_ctxt: &HirLowerCtxt, binop: &syntree::BinOp) -> BinOp {
    match binop {
        syntree::BinOp::Add(t) => BinOp::Add(Plus::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Sub(t) => BinOp::Sub(Minus::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Mul(t) => BinOp::Mul(Asterisk::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Div(t) => BinOp::Div(Slash::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Mod(t) => BinOp::Mod(Percent::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Eq(t) => BinOp::Eq(Eq2::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Neq(t) => BinOp::Neq(Neq::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Lt(t) => BinOp::Lt(LAngle::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::LtEq(t) => BinOp::LtEq(LEq::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Gt(t) => BinOp::Gt(RAngle::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::GtEq(t) => BinOp::GtEq(GEq::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::And(t) => BinOp::And(Amp2::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Or(t) => BinOp::Or(Pipe2::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::BitAnd(t) => BinOp::BitAnd(Amp::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::BitOr(t) => BinOp::BitOr(Pipe::from_token(t, hir_lower_ctxt)),
        syntree::BinOp::Xor(t) => BinOp::Xor(Caret::from_token(t, hir_lower_ctxt)),
    }
}

pub struct LowerCtxt<'ctx> {
    pub src_file_start_offset_in_db: usize,
    pub hir_map: &'ctx mut HirMap,
}

pub fn lower_mod_def(lower_ctxt: &mut LowerCtxt, root: syntree::Root) -> ModId {
    let mut hir_lower_ctxt = HirLowerCtxt {
        src_file_start_offset_in_db: lower_ctxt.src_file_start_offset_in_db,
        hir_ref_arena: lower_ctxt.hir_map,
    };
    lower_mod_def_impl(&mut hir_lower_ctxt, root)
}

fn lower_item_list(
    hir_lower_ctxt: &mut HirLowerCtxt,
    item_list: impl Iterator<Item = syntree::Item>,
) -> ItemList {
    ItemList {
        items: item_list
            .flat_map(|it| lower_item(hir_lower_ctxt, &it))
            .collect(),
    }
}

fn lower_mod_def_impl(hir_lower_ctxt: &mut HirLowerCtxt, root: syntree::Root) -> ModId {
    lower_mod_def_from_item_list(
        hir_lower_ctxt,
        LBrace::make_virtual(),
        root.get_item_list(),
        RBrace::make_virtual(),
        ModuleKw::make_virtual(),
        Ident::new_virtual(SpecialIdents::ROOT_MODULE),
        root.span(hir_lower_ctxt),
    )
}

fn lower_mod_def_from_item_list(
    hir_lower_ctxt: &mut HirLowerCtxt,
    lbrace: LBrace,
    item_list: impl Iterator<Item = syntree::Item>,
    rbrace: RBrace,
    mod_kw: ModuleKw,
    name: Ident,
    span: HirSpan,
) -> ModId {
    let hir_id = hir_lower_ctxt.allocate_hir_id(span);
    let mod_def = ModDef {
        mod_kw,
        name,
        body: Some(ModBody {
            lbrace,
            items: lower_item_list(hir_lower_ctxt, item_list),
            rbrace,
        }),
        hir_id,
    };

    hir_lower_ctxt.push_ref_at_allocation(hir_id, HirElem::Mod(mod_def))
}

fn lower_mod_def_to_include(
    hir_lower_ctxt: &mut HirLowerCtxt,
    mod_kw: ModuleKw,
    name: Ident,
    span: HirSpan,
) -> ModId {
    let hir_id = hir_lower_ctxt.allocate_hir_id(span);
    let mod_def = ModDef {
        mod_kw,
        name,
        body: None,
        hir_id,
    };

    hir_lower_ctxt.push_ref_at_allocation(hir_id, HirElem::Mod(mod_def))
}

fn lower_item(hir_lower_ctxt: &mut HirLowerCtxt, item: &syntree::Item) -> Vec<ItemId> {
    let attrs = lower_attr_list(hir_lower_ctxt, item.get_attr_list().as_ref());

    let hir_items = if let Some(fn_def) = item.get_fn_def() {
        let hir_id = hir_lower_ctxt.allocate_hir_id(item.span(hir_lower_ctxt));
        vec![Item {
            attrs,
            kind: ItemKind::FnDef(lower_fn_def(hir_lower_ctxt, &fn_def)),
            hir_id,
        }]
    } else if let Some(stmt) = item.get_stmt() {
        let hir_id = hir_lower_ctxt.allocate_hir_id(item.span(hir_lower_ctxt));
        vec![Item {
            attrs,
            kind: ItemKind::Stmt(lower_stmt(hir_lower_ctxt, &stmt)),
            hir_id,
        }]
    } else if let Some(use_stmt) = item.get_use_stmt() {
        lower_use_stmt(hir_lower_ctxt, &use_stmt)
            .into_iter()
            .map(|it| {
                let hir_id = hir_lower_ctxt.allocate_hir_id(item.span(hir_lower_ctxt));
                Item {
                    attrs: AttrList::new(),
                    kind: ItemKind::UseStmt(it),
                    hir_id,
                }
            })
            .collect()
    } else if let Some(mod_def) = item.get_module() {
        let hir_id = hir_lower_ctxt.allocate_hir_id(item.span(hir_lower_ctxt));
        let mod_kw = ModuleKw::from_token(&mod_def.get_module_kw(), hir_lower_ctxt);
        let name = lower_ident(
            hir_lower_ctxt,
            &mod_def.get_module_name().unwrap().get_ident(),
        );
        let mod_id = if let Some(mod_body) = mod_def.get_module_body() {
            let lbrace = LBrace::from_token(&mod_body.get_lbrace(), hir_lower_ctxt);
            let rbrace = RBrace::from_token(&mod_body.get_rbrace().unwrap(), hir_lower_ctxt);
            lower_mod_def_from_item_list(
                hir_lower_ctxt,
                lbrace,
                mod_body.get_item_list(),
                rbrace,
                mod_kw,
                name,
                item.span(hir_lower_ctxt),
            )
        } else {
            lower_mod_def_to_include(
                hir_lower_ctxt,
                ModuleKw::from_token(&mod_def.get_module_kw(), hir_lower_ctxt),
                name,
                item.span(hir_lower_ctxt),
            )
        };
        vec![Item {
            attrs,
            kind: ItemKind::ModDef(mod_id),
            hir_id,
        }]
    } else {
        todo!()
    };
    hir_items
        .into_iter()
        .map(|hir_item| {
            hir_lower_ctxt.push_ref_at_allocation(hir_item.hir_id, HirElem::Item(hir_item))
        })
        .collect()
}

fn lower_attr_list(
    hir_lower_ctxt: &mut HirLowerCtxt,
    attr_list: Option<&syntree::AttrList>,
) -> AttrList {
    let attrs = attr_list
        .into_iter()
        .flat_map(|it| it.get_attr_list())
        .map(|it| lower_attr(hir_lower_ctxt, &it))
        .collect();
    AttrList { attrs }
}

fn lower_attr(hir_lower_ctxt: &mut HirLowerCtxt, attr: &syntree::Attr) -> Attr {
    let hash = Hash::from_token(&attr.get_hash(), hir_lower_ctxt);
    let name = lower_ident(hir_lower_ctxt, &attr.get_attr_name().unwrap().get_ident());
    let meta = attr
        .get_attr_meta()
        .map(|it| lower_attr_meta(hir_lower_ctxt, &it));
    Attr {
        hash,
        name,
        meta,
        span: attr.span(hir_lower_ctxt),
    }
}

fn lower_attr_meta_item_list(
    hir_lower_ctxt: &mut HirLowerCtxt,
    attr_meta_item_list: impl Iterator<Item = syntree::AttrMetaItem>,
) -> Vec<AttrMetaItem> {
    attr_meta_item_list
        .map(|it| lower_attr_meta_item(hir_lower_ctxt, &it))
        .collect()
}

fn lower_attr_meta(hir_lower_ctxt: &mut HirLowerCtxt, attr_meta: &syntree::AttrMeta) -> AttrMeta {
    let lbrack = LBracket::from_token(&attr_meta.get_lbracket(), hir_lower_ctxt);
    let meta_list = lower_attr_meta_item_list(hir_lower_ctxt, attr_meta.get_attr_meta_item_list());
    let rbrack = RBracket::from_token(&attr_meta.get_rbracket().unwrap(), hir_lower_ctxt);

    AttrMeta {
        lbrack,
        meta_list,
        rbrack,
    }
}

fn lower_attr_meta_item(
    hir_lower_ctxt: &mut HirLowerCtxt,
    attr_meta_item: &syntree::AttrMetaItem,
) -> AttrMetaItem {
    let name = lower_ident(
        hir_lower_ctxt,
        &attr_meta_item.get_attr_meta_item_name().get_ident(),
    );

    let kind = if let Some(m) = attr_meta_item.get_attr_meta_item_eq() {
        let eq = Eq::from_token(&m.get_eq(), hir_lower_ctxt);
        let expr = lower_expr_node(hir_lower_ctxt, &m.get_expr_node().unwrap());
        AttrMetaItemKind::Eq(AttrMetaItemEq { eq, expr })
    } else if let Some(call) = attr_meta_item.get_attr_meta_item_call() {
        let lparen = LParen::from_token(&call.get_lparen(), hir_lower_ctxt);
        let meta_list = lower_attr_meta_item_list(hir_lower_ctxt, call.get_attr_meta_item_list());
        let rparen = RParen::from_token(&call.get_rparen().unwrap(), hir_lower_ctxt);
        AttrMetaItemKind::Call(AttrMetaItemCall {
            lparen,
            meta_list,
            rparen,
        })
    } else {
        AttrMetaItemKind::NameOnly
    };

    AttrMetaItem {
        name,
        kind,
        span: attr_meta_item.span(hir_lower_ctxt),
    }
}

fn lower_use_stmt(
    hir_lower_ctxt: &mut HirLowerCtxt,
    use_stmt: &syntree::UseStmt,
) -> Vec<UseStmtId> {
    lower_use_path(hir_lower_ctxt, &use_stmt.get_use_path().unwrap())
        .into_iter()
        .map(|path| {
            let hir_id = hir_lower_ctxt.allocate_hir_id(use_stmt.span(hir_lower_ctxt));
            let use_kw = UseKw::from_token(&use_stmt.get_use_kw(), hir_lower_ctxt);
            hir_lower_ctxt.push_ref_at_allocation(
                hir_id,
                HirElem::UseStmt(UseStmt {
                    use_kw,
                    path,
                    span: use_stmt.span(hir_lower_ctxt),
                    hir_id,
                }),
            )
        })
        .collect()
}

fn lower_use_path(hir_lower_ctxt: &mut HirLowerCtxt, use_path: &syntree::UsePath) -> Vec<UsePath> {
    if let Some(segment_and_path) = use_path.get_use_path_segment_and_path() {
        let segment = lower_ident(
            hir_lower_ctxt,
            &segment_and_path.get_use_path_segment().get_ident(),
        );
        let hir_id = hir_lower_ctxt.allocate_hir_id(segment.span);
        let segment = hir_lower_ctxt.push_ref_at_allocation(
            hir_id,
            HirElem::UsePathSegment(UsePathSegment {
                ident: segment,
                hir_id,
            }),
        );
        if let Some(ext) = segment_and_path.get_use_path_continuation() {
            if let Some(continuation) = ext.get_use_path_colon_continuation() {
                let mut paths =
                    lower_use_path(hir_lower_ctxt, &continuation.get_use_path().unwrap());
                for path in paths.iter_mut() {
                    path.segments.insert(0, segment);
                }
                paths
            } else if let Some(alias) = ext.get_use_alias() {
                let as_kw = AsKw::from_token(&alias.get_as_kw(), hir_lower_ctxt);

                vec![UsePath {
                    segments: vec![segment],
                    alias: Some(UseAlias {
                        as_kw,
                        alias: lower_ident(hir_lower_ctxt, &alias.get_ident().unwrap()),
                    }),
                }]
            } else {
                todo!()
            }
        } else {
            vec![UsePath {
                segments: vec![segment],
                alias: None,
            }]
        }
    } else if let Some(l) = use_path.get_use_path_list() {
        l.get_use_path_list()
            .flat_map(|it| lower_use_path(hir_lower_ctxt, &it))
            .collect()
    } else {
        todo!()
    }
}

fn lower_fn_def(hir_lower_ctxt: &mut HirLowerCtxt, fn_def: &syntree::FnDef) -> FnId {
    let hir_id = hir_lower_ctxt.allocate_hir_id(fn_def.span(hir_lower_ctxt));
    let head = fn_def.get_fn_head();
    let fn_kw = FnKw::from_token(&head.get_fn_kw(), hir_lower_ctxt);
    let name = lower_ident(hir_lower_ctxt, &head.get_fn_name().unwrap().get_ident());
    let generics = head
        .get_generic_param_list()
        .map(|gparams| lower_generic_param_list(hir_lower_ctxt, &gparams));
    let params = head
        .get_fn_param_list()
        .map(|fn_param_list| lower_fn_def_params(hir_lower_ctxt, &fn_param_list));
    let ret_ty = head
        .get_fn_ret_ty()
        .as_ref()
        .map(|r| lower_fn_ret_ty(hir_lower_ctxt, r));
    let body = lower_block(hir_lower_ctxt, &fn_def.get_block().unwrap());

    let hir_fn_def = FnDef {
        fn_kw,
        name,
        generics,
        params,
        ret_ty,
        body,
        hir_id,
    };

    hir_lower_ctxt.push_ref_at_allocation(hir_id, HirElem::Fn(hir_fn_def))
}

fn lower_generic_param_list(
    hir_lower_ctxt: &mut HirLowerCtxt,
    gparams: &syntree::GenericParamList,
) -> GenericParams {
    let langle = LAngle::from_token(&gparams.get_langle(), hir_lower_ctxt);
    let params = gparams
        .get_generic_param_list()
        .map(|it| lower_generic_param(hir_lower_ctxt, &it))
        .collect();
    let rangle = RAngle::from_token(&gparams.get_rangle().unwrap(), hir_lower_ctxt);

    GenericParams {
        langle,
        params,
        rangle,
        span: HirSpan::of_node(gparams, hir_lower_ctxt),
    }
}

fn lower_generic_param(
    hir_lower_ctxt: &mut HirLowerCtxt,
    gparam: &syntree::GenericParam,
) -> GenericParam {
    if let Some(ty_param) = gparam.get_generic_ty_param() {
        let ident = lower_ident(
            hir_lower_ctxt,
            &ty_param.get_generic_ty_param_name().get_ident(),
        );
        let bounds = ty_param.get_generic_ty_param_bound_list().map(|it| {
            lower_ty_param_bounds(
                hir_lower_ctxt,
                Colon::from_token(&ty_param.get_colon().unwrap(), hir_lower_ctxt),
                &it,
            )
        });

        let default = ty_param.get_generic_ty_param_default().map(|it| {
            (
                Eq::from_token(&it.get_eq(), hir_lower_ctxt),
                lower_ty_ref(hir_lower_ctxt, &it.get_ty_ref().unwrap()),
            )
        });

        GenericParam {
            kind: GenericParamKind::Type(GenericParamTy {
                name: ident,
                bounds,
                default,
            }),
            span: HirSpan::of_node(gparam, hir_lower_ctxt),
        }
    } else if let Some(const_param) = gparam.get_generic_const_param() {
        let const_kw = ConstKw::from_token(&const_param.get_const_kw(), hir_lower_ctxt);
        let ident = lower_ident(
            hir_lower_ctxt,
            &const_param
                .get_generic_const_param_name()
                .unwrap()
                .get_ident(),
        );
        let colon = Colon::from_token(&const_param.get_colon().unwrap(), hir_lower_ctxt);
        let ty = lower_ty_ref(hir_lower_ctxt, &const_param.get_ty_ref().unwrap());

        GenericParam {
            kind: GenericParamKind::Const(GenericParamConst {
                const_kw,
                name: ident,
                colon,
                ty,
            }),
            span: HirSpan::of_node(gparam, hir_lower_ctxt),
        }
    } else {
        todo!()
    }
}

fn lower_ty_param_bounds(
    hir_lower_ctxt: &mut HirLowerCtxt,
    colon: Colon,
    bound_list: &syntree::GenericTyParamBoundList,
) -> GenericParamTyBounds {
    let bounds = bound_list
        .get_generic_ty_param_bound_list()
        .map(|it| lower_ty_ref(hir_lower_ctxt, &it.get_ty_ref()))
        .collect();
    GenericParamTyBounds {
        colon,
        bounds,
        span: colon.span().join(bound_list.span(hir_lower_ctxt)),
    }
}

fn lower_fn_ret_ty(hir_lower_ctxt: &mut HirLowerCtxt, ret_ty: &syntree::FnRetTy) -> FnRetTy {
    FnRetTy {
        span: HirSpan::of_node(ret_ty, hir_lower_ctxt),
        arrow: ThinArrow::from_token(&ret_ty.get_arrow(), hir_lower_ctxt),
        ty: lower_ty_ref(hir_lower_ctxt, &ret_ty.get_ty_ref().unwrap()),
    }
}

fn lower_fn_def_params(
    hir_lower_ctxt: &mut HirLowerCtxt,
    fn_param_list: &syntree::FnParamList,
) -> FnParamList {
    let lparen = LParen::from_token(&fn_param_list.get_lparen(), hir_lower_ctxt);
    let params = fn_param_list
        .get_fn_param_list()
        .map(|it| lower_fn_def_param(hir_lower_ctxt, &it))
        .collect();
    let rparen = RParen::from_token(&fn_param_list.get_rparen().unwrap(), hir_lower_ctxt);

    FnParamList {
        lparen,
        params,
        rparen,
    }
}

fn lower_fn_def_param(hir_lower_ctxt: &mut HirLowerCtxt, fn_param: &syntree::FnParam) -> FnParamId {
    let hir_id = hir_lower_ctxt.allocate_hir_id(fn_param.span(hir_lower_ctxt));
    let pat = lower_pat(hir_lower_ctxt, &fn_param.get_fn_param_name().get_pat());
    let colon = Colon::from_token(&fn_param.get_colon().unwrap(), hir_lower_ctxt);
    let ty = lower_ty_ref(
        hir_lower_ctxt,
        &fn_param.get_fn_param_ty().unwrap().get_ty_ref(),
    );
    let default = fn_param.get_fn_param_default().as_ref().map(|e| {
        (
            Eq::from_token(&e.get_eq(), hir_lower_ctxt),
            lower_expr_node(hir_lower_ctxt, &e.get_expr_node().unwrap()),
        )
    });
    hir_lower_ctxt.push_ref_at_allocation(
        hir_id,
        HirElem::FnParam(FnParam {
            pat,
            colon,
            ty,
            default,
            param_span: HirSpan::of_node(fn_param, hir_lower_ctxt),
            hir_id,
        }),
    )
}

fn lower_expr(hir_lower_ctxt: &mut HirLowerCtxt, expr: &syntree::Expr) -> ExprId {
    let hir_id = hir_lower_ctxt.allocate_hir_id(expr.span(hir_lower_ctxt));
    let hir_expr = match expr {
        syntree::Expr::ExprAtom(atom) => Expr {
            kind: ExprKind::Atom(lower_expr_atom(hir_lower_ctxt, atom)),
            hir_id,
        },
        syntree::Expr::ExprNode(node) => return lower_expr_node(hir_lower_ctxt, node),
        syntree::Expr::BinaryOpExpr(binary_op_expr) => {
            let (left, op, right) = lower_binary_op_expr(hir_lower_ctxt, binary_op_expr);
            Expr {
                kind: ExprKind::Binary(BinaryOpExpr {
                    lhs: left,
                    op,
                    rhs: right,
                }),
                hir_id,
            }
        }
        syntree::Expr::CallExpr(call_expr) => Expr {
            kind: ExprKind::CallExpr(lower_call_expr(hir_lower_ctxt, call_expr)),
            hir_id,
        },
        syntree::Expr::IndexExpr(index_expr) => Expr {
            kind: ExprKind::IndexExpr(lower_index_expr(hir_lower_ctxt, index_expr)),
            hir_id,
        },
        syntree::Expr::FieldAccess(field_access) => Expr {
            kind: ExprKind::FieldAccess(lower_field_access(hir_lower_ctxt, field_access)),
            hir_id,
        },
        syntree::Expr::MethodCall(method_call) => Expr {
            kind: ExprKind::MethodCall(lower_method_call(hir_lower_ctxt, method_call)),
            hir_id,
        },
        syntree::Expr::Block(block) => {
            let block = lower_block(hir_lower_ctxt, block);
            Expr {
                kind: ExprKind::Atom(ExprAtom {
                    kind: ExprAtomKind::BlockExpr(BlockExpr { block }),
                }),
                hir_id,
            }
        }
        syntree::Expr::CustomInfixExpr(e) => {
            let base = lower_expr(hir_lower_ctxt, &e.get_expr());
            let infix = e.get_custom_infix_expr_infix().unwrap();
            let op = lower_ident(hir_lower_ctxt, &infix.get_name());
            let arg = lower_expr_node(
                hir_lower_ctxt,
                &infix
                    .get_custom_infix_expr_infix_arg()
                    .unwrap()
                    .get_expr_node(),
            );

            Expr {
                kind: ExprKind::CustomInfix(CustomInfixExpr {
                    base,
                    name: op,
                    arg,
                }),
                hir_id,
            }
        }
    };

    hir_lower_ctxt.push_ref_at_allocation(hir_id, HirElem::Expr(hir_expr))
}

fn lower_call_expr(hir_lower_ctxt: &mut HirLowerCtxt, call_expr: &syntree::CallExpr) -> CallExpr {
    let callee = lower_expr(hir_lower_ctxt, &call_expr.get_expr());
    let args = lower_call_expr_args(hir_lower_ctxt, &call_expr.get_call_expr_args().unwrap());
    CallExpr { callee, args }
}

fn lower_call_expr_args(
    hir_lower_ctxt: &mut HirLowerCtxt,
    call_expr_args: &syntree::CallExprArgs,
) -> CallExprArgs {
    let args = call_expr_args.get_call_expr_args_list();

    let (lparen, rparen) = args
        .as_ref()
        .map(|it| {
            (
                LParen::from_token(&it.get_lparen(), hir_lower_ctxt),
                RParen::from_token(&it.get_rparen().unwrap(), hir_lower_ctxt),
            )
        })
        .unzip();

    let mut args = args
        .as_ref()
        .map(|it| {
            it.get_expr_node_list()
                .map(|it| lower_expr_node(hir_lower_ctxt, &it))
        })
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

    args.extend(call_expr_args.get_call_expr_arg_lambda().map(|it| {
        let hir_id = hir_lower_ctxt.allocate_hir_id(it.span(hir_lower_ctxt));
        let expr = Expr {
            kind: ExprKind::Atom(ExprAtom {
                kind: ExprAtomKind::LambdaExpr(lower_lambda_expr(
                    hir_lower_ctxt,
                    &it.get_lambda_expr(),
                )),
            }),
            hir_id,
        };
        hir_lower_ctxt.push_ref_at_allocation(hir_id, HirElem::Expr(expr))
    }));
    CallExprArgs {
        lparen,
        args,
        rparen,
    }
}

fn lower_index_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    index_expr: &syntree::IndexExpr,
) -> IndexExpr {
    let base = lower_expr(hir_lower_ctxt, &index_expr.get_expr());
    let i = index_expr.get_index_expr_index().unwrap();
    let lbrack = LBracket::from_token(&i.get_lbracket(), hir_lower_ctxt);
    let index = lower_expr_node(hir_lower_ctxt, &i.get_expr_node().unwrap());
    let rbrack = RBracket::from_token(&i.get_rbracket().unwrap(), hir_lower_ctxt);

    IndexExpr {
        base,
        lbrack,
        index,
        rbrack,
    }
}

fn lower_field_access(
    hir_lower_ctxt: &mut HirLowerCtxt,
    field_access: &syntree::FieldAccess,
) -> FieldAccess {
    let base = lower_expr(hir_lower_ctxt, &field_access.get_expr());
    let dot = Dot::from_token(&field_access.get_dot().unwrap(), hir_lower_ctxt);
    let field = lower_ident(hir_lower_ctxt, &field_access.get_field_name().unwrap());
    FieldAccess { base, dot, field }
}

fn lower_method_call(
    hir_lower_ctxt: &mut HirLowerCtxt,
    method_call: &syntree::MethodCall,
) -> MethodCall {
    let base = lower_expr(hir_lower_ctxt, &method_call.get_expr());
    let dot = Dot::from_token(&method_call.get_dot().unwrap(), hir_lower_ctxt);
    let method = lower_ident(hir_lower_ctxt, &method_call.get_method_name().unwrap());
    let args = lower_call_expr_args(hir_lower_ctxt, &method_call.get_call_expr_args().unwrap());

    MethodCall {
        base,
        dot,
        method,
        args,
    }
}

fn lower_lambda_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    lambda_expr: &syntree::LambdaExpr,
) -> LambdaExpr {
    let lbrace = LBrace::from_token(&lambda_expr.get_lbrace(), hir_lower_ctxt);
    let lambda_param_list = lambda_expr
        .get_lambda_param_list()
        .as_ref()
        .map(|lpl| lower_lambda_param_list(hir_lower_ctxt, lpl));
    let body = lower_item_list(hir_lower_ctxt, lambda_expr.get_item_list());
    let rbrace = RBrace::from_token(&lambda_expr.get_rbrace().unwrap(), hir_lower_ctxt);
    LambdaExpr {
        lbrace,
        lambda_param_list,
        body,
        rbrace,
    }
}

fn lower_lambda_param_list(
    hir_lower_ctxt: &mut HirLowerCtxt,
    lambda_param_list: &syntree::LambdaParamList,
) -> LambdaParamList {
    let params = lambda_param_list
        .get_lambda_param_list()
        .map(|it| lower_lambda_param(hir_lower_ctxt, &it))
        .collect();
    let arrow = ThinArrow::from_token(&lambda_param_list.get_arrow().unwrap(), hir_lower_ctxt);
    LambdaParamList { params, arrow }
}

fn lower_lambda_param(
    hir_lower_ctxt: &mut HirLowerCtxt,
    lambda_param: &syntree::LambdaParam,
) -> LambdaParam {
    let pat = lower_pat(hir_lower_ctxt, &lambda_param.get_fn_param_name().get_pat());
    let ty = lambda_param.get_fn_param_ty().map(|ty_ref| {
        (
            Colon::from_token(&lambda_param.get_colon().unwrap(), hir_lower_ctxt),
            lower_ty_ref(hir_lower_ctxt, &ty_ref.get_ty_ref()),
        )
    });
    LambdaParam { pat, ty }
}

fn lower_binary_op_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    expr: &syntree::BinaryOpExpr,
) -> (ExprId, BinOp, ExprId) {
    let (left, op, right) = expr.lower_assume_complete();
    let left = lower_expr(hir_lower_ctxt, &left);
    let op = lower_binop(hir_lower_ctxt, &op);
    let right = lower_expr(hir_lower_ctxt, &right);
    (left, op, right)
}

fn lower_expr_node(hir_lower_ctxt: &mut HirLowerCtxt, expr_node: &syntree::ExprNode) -> ExprId {
    lower_expr(hir_lower_ctxt, &expr_node.get_expr())
}

fn lower_expr_atom(hir_lower_ctxt: &mut HirLowerCtxt, atom: &syntree::ExprAtom) -> ExprAtom {
    if let Some(ident) = atom.get_ident() {
        ExprAtom {
            kind: ExprAtomKind::Ident(lower_expr_atom_ident(hir_lower_ctxt, &ident)),
        }
    } else if let Some(str_literal) = atom.get_string_literal() {
        ExprAtom {
            kind: ExprAtomKind::Str(lower_str_literal(hir_lower_ctxt, &str_literal)),
        }
    } else if let Some(num_lit) = atom.get_num_lit() {
        ExprAtom {
            kind: ExprAtomKind::Num(lower_num_literal(hir_lower_ctxt, &num_lit)),
        }
    } else if let Some(loop_expr) = atom.get_loop_expr() {
        ExprAtom {
            kind: ExprAtomKind::LoopExpr(lower_loop_expr(hir_lower_ctxt, &loop_expr)),
        }
    } else if let Some(if_expr) = atom.get_if_expr() {
        ExprAtom {
            kind: ExprAtomKind::IfExpr(lower_if_expr(hir_lower_ctxt, &if_expr)),
        }
    } else if let Some(return_expr) = atom.get_return_expr() {
        ExprAtom {
            kind: ExprAtomKind::ReturnExpr(lower_return_expr(hir_lower_ctxt, &return_expr)),
        }
    } else if let Some(break_expr) = atom.get_break_expr() {
        ExprAtom {
            kind: ExprAtomKind::BreakExpr(lower_break_expr(hir_lower_ctxt, &break_expr)),
        }
    } else if let Some(continue_expr) = atom.get_continue_expr() {
        ExprAtom {
            kind: ExprAtomKind::ContinueExpr(lower_continue_expr(hir_lower_ctxt, &continue_expr)),
        }
    } else if let Some(block_expr) = atom.get_block_expr() {
        ExprAtom {
            kind: ExprAtomKind::BlockExpr(lower_block_expr(hir_lower_ctxt, &block_expr)),
        }
    } else if let Some(tuple_like) = atom.get_tuple_like_expr() {
        ExprAtom {
            kind: ExprAtomKind::TupleExpr(lower_tuple_like_expr(hir_lower_ctxt, &tuple_like)),
        }
    } else if let Some(lambda_expr) = atom.get_lambda_expr() {
        ExprAtom {
            kind: ExprAtomKind::LambdaExpr(lower_lambda_expr(hir_lower_ctxt, &lambda_expr)),
        }
    } else {
        todo!()
    }
}

fn lower_expr_atom_ident(
    hir_lower_ctxt: &mut HirLowerCtxt,
    ident: &syntree::Token,
) -> ExprAtomIdentId {
    let hir_id = hir_lower_ctxt.allocate_hir_id(HirSpan::of(ident, hir_lower_ctxt));
    let ident = lower_ident(hir_lower_ctxt, ident);

    hir_lower_ctxt.push_ref_at_allocation(
        hir_id,
        HirElem::ExprAtomIdent(ExprAtomIdent { ident, hir_id }),
    )
}

fn lower_num_literal(hir_lower_ctxt: &mut HirLowerCtxt, num_lit: &syntree::NumLit) -> NumLit {
    if let Some(num_bin) = num_lit.get_num_bin() {
        NumLit::Bin(GenericToken::from_token(&num_bin, hir_lower_ctxt))
    } else if let Some(num_oct) = num_lit.get_num_oct() {
        NumLit::Oct(GenericToken::from_token(&num_oct, hir_lower_ctxt))
    } else if let Some(num_dec) = num_lit.get_num_dec() {
        NumLit::Dec(GenericToken::from_token(&num_dec, hir_lower_ctxt))
    } else if let Some(num_hex) = num_lit.get_num_hex() {
        NumLit::Hex(GenericToken::from_token(&num_hex, hir_lower_ctxt))
    } else {
        todo!()
    }
}

fn lower_loop_expr(hir_lower_ctxt: &mut HirLowerCtxt, loop_expr: &syntree::LoopExpr) -> LoopExpr {
    let loop_kw = LoopKw::from_token(&loop_expr.get_loop_kw(), hir_lower_ctxt);
    let body = lower_block(hir_lower_ctxt, &loop_expr.get_block().unwrap());
    LoopExpr { loop_kw, body }
}

fn lower_tuple_like_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    tuple_like_expr: &syntree::TupleLikeExpr,
) -> TupleExpr {
    let lparen = LParen::from_token(&tuple_like_expr.get_lparen(), hir_lower_ctxt);
    let exprs = tuple_like_expr
        .get_expr_node_list()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it))
        .collect();
    let rparen = RParen::from_token(&tuple_like_expr.get_rparen().unwrap(), hir_lower_ctxt);
    TupleExpr {
        lparen,
        exprs,
        rparen,
    }
}

fn lower_block_extra_items(
    hir_lower_ctxt: &mut HirLowerCtxt,
    block: &syntree::Block,
    f: impl FnOnce(&mut HirLowerCtxt, &mut ItemList),
) -> BlockId {
    let hir_id = hir_lower_ctxt.allocate_hir_id(block.span(hir_lower_ctxt));
    let lbrace = LBrace::from_token(&block.get_lbrace(), hir_lower_ctxt);
    let mut items = lower_item_list(hir_lower_ctxt, block.get_item_list());
    let rbrace = RBrace::from_token(&block.get_rbrace().unwrap(), hir_lower_ctxt);

    f(hir_lower_ctxt, &mut items);

    let hir_block = Block {
        lbrace,
        items,
        rbrace,
        hir_id,
    };

    hir_lower_ctxt.push_ref_at_allocation(hir_id, HirElem::Block(hir_block))
}

fn lower_block(hir_lower_ctxt: &mut HirLowerCtxt, block: &syntree::Block) -> BlockId {
    lower_block_extra_items(hir_lower_ctxt, block, |_, _| {})
}

fn lower_stmt(hir_lower_ctxt: &mut HirLowerCtxt, stmt: &syntree::Stmt) -> StmtId {
    let hir_id = hir_lower_ctxt.allocate_hir_id(stmt.span(hir_lower_ctxt));
    let hir_stmt = if let Some(expr) = stmt.get_expr_node() {
        Stmt {
            kind: StmtKind::ExprStmt(lower_expr_node(hir_lower_ctxt, &expr)),
            hir_id,
        }
    } else if let Some(let_stmt) = stmt.get_let_stmt() {
        Stmt {
            kind: StmtKind::LetStmt(lower_let_stmt(hir_lower_ctxt, &let_stmt)),
            hir_id,
        }
    } else if let Some(for_stmt) = stmt.get_for_stmt() {
        Stmt {
            kind: StmtKind::ForStmt(lower_for_stmt(hir_lower_ctxt, &for_stmt)),
            hir_id,
        }
    } else if let Some(while_stmt) = stmt.get_while_stmt() {
        Stmt {
            kind: StmtKind::WhileStmt(lower_while_stmt(hir_lower_ctxt, &while_stmt)),
            hir_id,
        }
    } else if let Some(assignment_stmt) = stmt.get_assignment_stmt() {
        Stmt {
            kind: StmtKind::AssignmentStmt(lower_assignment_stmt(hir_lower_ctxt, &assignment_stmt)),
            hir_id,
        }
    } else {
        todo!()
    };

    hir_lower_ctxt.push_ref_at_allocation(hir_id, HirElem::Stmt(hir_stmt))
}

fn lower_let_stmt(hir_lower_ctxt: &mut HirLowerCtxt, let_stmt: &syntree::LetStmt) -> LetStmt {
    let let_kw = LetKw::from_token(&let_stmt.get_let_kw(), hir_lower_ctxt);
    let mutability = match let_stmt.get_mut_kw() {
        Some(t) => LetMutability::Mut(MutKw::from_token(&t, hir_lower_ctxt)),
        None => LetMutability::Imm,
    };
    let pat = lower_pat(hir_lower_ctxt, &let_stmt.get_pat().unwrap());
    let ty = let_stmt.get_ty_ref().as_ref().map(|ty_ref| {
        (
            Colon::from_token(&let_stmt.get_colon().unwrap(), hir_lower_ctxt),
            lower_ty_ref(hir_lower_ctxt, ty_ref),
        )
    });
    let init = let_stmt.get_expr_node().as_ref().map(|e| {
        (
            Eq::from_token(&let_stmt.get_eq().unwrap(), hir_lower_ctxt),
            lower_expr_node(hir_lower_ctxt, e),
        )
    });
    LetStmt {
        let_kw,
        mutability,
        pat,
        ty,
        init,
    }
}

fn lower_for_stmt(hir_lower_ctxt: &mut HirLowerCtxt, for_stmt: &syntree::ForStmt) -> ForStmt {
    let for_kw = ForKw::from_token(&for_stmt.get_for_kw(), hir_lower_ctxt);
    let lparen = LParen::from_token(&for_stmt.get_lparen().unwrap(), hir_lower_ctxt);
    let pat = lower_pat(hir_lower_ctxt, &for_stmt.get_for_pat().unwrap().get_pat());
    let in_kw = InKw::from_token(&for_stmt.get_in_kw().unwrap(), hir_lower_ctxt);
    let iter = lower_expr_node(
        hir_lower_ctxt,
        &for_stmt.get_for_in_expr().unwrap().get_expr_node(),
    );
    let rparen = RParen::from_token(&for_stmt.get_rparen().unwrap(), hir_lower_ctxt);
    let body = lower_block(hir_lower_ctxt, &for_stmt.get_block().unwrap());

    ForStmt {
        for_kw,
        lparen,
        pat,
        in_kw,
        iter,
        rparen,
        body,
    }
}

fn lower_while_stmt(
    hir_lower_ctxt: &mut HirLowerCtxt,
    while_stmt: &syntree::WhileStmt,
) -> WhileStmt {
    let while_kw = WhileKw::from_token(&while_stmt.get_while_kw(), hir_lower_ctxt);
    let wcond = while_stmt.get_while_condition().unwrap();
    let lparen = LParen::from_token(&wcond.get_lparen(), hir_lower_ctxt);
    let expr = lower_expr_node(hir_lower_ctxt, &wcond.get_expr_node().unwrap());
    let rparen = RParen::from_token(&wcond.get_rparen().unwrap(), hir_lower_ctxt);
    let body = lower_block(hir_lower_ctxt, &while_stmt.get_block().unwrap());

    WhileStmt {
        while_kw,
        lparen,
        expr,
        rparen,
        body,
    }
}

fn lower_assignment_stmt(
    hir_lower_ctxt: &mut HirLowerCtxt,
    assignment_stmt: &syntree::AssignmentStmt,
) -> AssignmentStmt {
    let lhs = lower_expr_node(
        hir_lower_ctxt,
        &assignment_stmt.get_assignment_lhs().get_expr_node(),
    );
    let op_and_rhs = assignment_stmt.get_assignment_op_and_rhs_expr().unwrap();
    let op = lower_assignment_stmt_op(hir_lower_ctxt, &op_and_rhs.get_assignment_op_node());
    let rhs = lower_expr_node(hir_lower_ctxt, &op_and_rhs.get_expr_node().unwrap());
    AssignmentStmt { lhs, op, rhs }
}

fn lower_assignment_stmt_op(
    hir_lower_ctxt: &mut HirLowerCtxt,
    op: &syntree::AssignmentOpNode,
) -> AssignmentOp {
    match &syntree::AssignmentOp::from_token(op.get_node().first_token().unwrap()).unwrap() {
        syntree::AssignmentOp::Eq(t) => AssignmentOp::Assign(Eq::from_token(t, hir_lower_ctxt)),
        syntree::AssignmentOp::PlusEq(t) => {
            AssignmentOp::AddAssign(PlusEq::from_token(t, hir_lower_ctxt))
        }
        syntree::AssignmentOp::MinusEq(t) => {
            AssignmentOp::SubAssign(MinusEq::from_token(t, hir_lower_ctxt))
        }
        syntree::AssignmentOp::AsteriskEq(t) => {
            AssignmentOp::MulAssign(AsteriskEq::from_token(t, hir_lower_ctxt))
        }
        syntree::AssignmentOp::SlashEq(t) => {
            AssignmentOp::DivAssign(SlashEq::from_token(t, hir_lower_ctxt))
        }
        syntree::AssignmentOp::PercentEq(t) => {
            AssignmentOp::ModAssign(PercentEq::from_token(t, hir_lower_ctxt))
        }
        syntree::AssignmentOp::AmpEq(t) => {
            AssignmentOp::BitAndAssign(AmpEq::from_token(t, hir_lower_ctxt))
        }
        syntree::AssignmentOp::PipeEq(t) => {
            AssignmentOp::BitOrAssign(PipeEq::from_token(t, hir_lower_ctxt))
        }
        syntree::AssignmentOp::CaretEq(t) => {
            AssignmentOp::BitXorAssign(CaretEq::from_token(t, hir_lower_ctxt))
        }
    }
}

fn lower_str_literal(
    hir_lower_ctxt: &mut HirLowerCtxt,
    str_literal: &syntree::StringLiteral,
) -> StrLiteral {
    StrLiteral {
        lquote: LQuote::from_token(&str_literal.get_begin_string(), hir_lower_ctxt),
        span: HirSpan::of_node(str_literal, hir_lower_ctxt),
        fragments: str_literal
            .get_string_literal_fragment_list()
            .map(|it| lower_str_literal_fragment(hir_lower_ctxt, &it))
            .collect(),
        rquote: RQuote::from_token(&str_literal.get_end_string().unwrap(), hir_lower_ctxt),
    }
}

fn lower_str_literal_fragment(
    hir_lower_ctxt: &mut HirLowerCtxt,
    fragment: &syntree::StringLiteralFragment,
) -> StrLiteralFragment {
    match fragment {
        syntree::StringLiteralFragment::StringLiteralFragTextPart(t) => StrLiteralFragment {
            kind: StrLiteralFragmentKind::Text(StrLiteralTextFragment {
                token: GenericToken::from_token(
                    &t.get_string_literal_frag_text_part(),
                    hir_lower_ctxt,
                ),
            }),
            span: HirSpan::of_node(fragment, hir_lower_ctxt),
        },
        syntree::StringLiteralFragment::StringLiteralFragEscapedChar(e) => {
            let t = e.get_string_literal_frag_escaped_char();
            let mut chars = t.text().chars();
            let c = chars.next().unwrap();
            assert_eq!(c, '\\');
            let c = chars.next().unwrap();

            let c = match c {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                x => x,
            };
            StrLiteralFragment {
                kind: StrLiteralFragmentKind::EscapedChar(
                    GenericToken::from_token(&t, hir_lower_ctxt),
                    c,
                ),
                span: HirSpan::of_node(fragment, hir_lower_ctxt),
            }
        }
        syntree::StringLiteralFragment::StringLiteralFragEscapeSequence(e) => {
            todo!()
        }
        syntree::StringLiteralFragment::StringLiteralFragDisplay(e) => StrLiteralFragment {
            kind: StrLiteralFragmentKind::Display(StrLiteralDisplayFragment {
                display_token: GenericToken::from_token(&e.get_display_tok(), hir_lower_ctxt),
                span: HirSpan::of_node(fragment, hir_lower_ctxt),
                expr: lower_displayable_to_expr(
                    hir_lower_ctxt,
                    &e.get_string_literal_frag_displayable().unwrap(),
                ),
            }),
            span: HirSpan::of_node(fragment, hir_lower_ctxt),
        },
        syntree::StringLiteralFragment::StringLiteralFragDebug(e) => StrLiteralFragment {
            kind: StrLiteralFragmentKind::Debug(StrLiteralDebugFragment {
                debug_token: GenericToken::from_token(&e.get_debug_tok(), hir_lower_ctxt),
                span: HirSpan::of_node(fragment, hir_lower_ctxt),
                expr: lower_displayable_to_expr(
                    hir_lower_ctxt,
                    &e.get_string_literal_frag_displayable().unwrap(),
                ),
            }),
            span: HirSpan::of_node(fragment, hir_lower_ctxt),
        },
    }
}

fn lower_displayable_to_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    displayable: &syntree::StringLiteralFragDisplayable,
) -> ExprId {
    let expr = match displayable {
        syntree::StringLiteralFragDisplayable::StringLiteralFragExpr(e) => {
            let hir_id = hir_lower_ctxt.allocate_hir_id(e.span(hir_lower_ctxt));
            Expr {
                kind: ExprKind::Atom(ExprAtom {
                    kind: ExprAtomKind::BlockExpr(lower_block_expr(
                        hir_lower_ctxt,
                        &e.get_block_expr(),
                    )),
                }),
                hir_id,
            }
        }
        syntree::StringLiteralFragDisplayable::StringLiteralFragIdent(e) => {
            let hir_id = hir_lower_ctxt.allocate_hir_id(e.span(hir_lower_ctxt));
            Expr {
                kind: ExprKind::Atom(ExprAtom {
                    kind: ExprAtomKind::Ident(lower_expr_atom_ident(
                        hir_lower_ctxt,
                        &e.get_ident(),
                    )),
                }),
                hir_id,
            }
        }
    };

    hir_lower_ctxt.push_ref_at_allocation(expr.hir_id, HirElem::Expr(expr))
}

fn lower_pat(hir_lower_ctxt: &mut HirLowerCtxt, pat: &syntree::Pat) -> Pat {
    if let pat_ident = pat.get_ident() {
        let (pat_ident, is_wild) = lower_pat_ident(hir_lower_ctxt, &pat_ident);
        let kind = match is_wild {
            IsWild::Yes => PatKind::Wildcard(pat_ident),
            IsWild::No => PatKind::Ident(pat_ident),
        };
        Pat { kind }
    } else {
        todo!()
    }
}

enum IsWild {
    Yes,
    No,
}

fn lower_pat_ident(
    hir_lower_ctxt: &mut HirLowerCtxt,
    pat_ident: &syntree::Token,
) -> (PatIdentId, IsWild) {
    let ident = lower_ident(hir_lower_ctxt, pat_ident);
    let hir_id = hir_lower_ctxt.allocate_hir_id(HirSpan::of(pat_ident, hir_lower_ctxt));
    let is_wild = match ident.text.as_str() {
        "_" => IsWild::Yes,
        _ => IsWild::No,
    };
    (
        hir_lower_ctxt
            .push_ref_at_allocation(hir_id, HirElem::PatIdent(PatIdent { ident, hir_id })),
        is_wild,
    )
}

fn lower_ty_ref(hir_lower_ctxt: &mut HirLowerCtxt, ty_ref: &syntree::TyRef) -> TyRefId {
    let span = HirSpan::of_node(ty_ref, hir_lower_ctxt);
    let hir_id = hir_lower_ctxt.allocate_hir_id(span);
    let ty_ref = if let Some(name) = ty_ref.get_ident() {
        let name = lower_ident(hir_lower_ctxt, &name);
        macro_rules! ty_refp {
            ($name:expr, $kind:ident) => {
                TyRefKind::Primitive(PrimitiveTy {
                    kind: PrimitiveTyKind::$kind,
                    ident: $name,
                })
            };
        }

        let kind = match name.text.as_str() {
            "i8" => ty_refp!(name, I8),
            "i16" => ty_refp!(name, I16),
            "i32" => ty_refp!(name, I32),
            "i64" => ty_refp!(name, I64),
            "i128" => ty_refp!(name, I128),
            "u8" => ty_refp!(name, U8),
            "u16" => ty_refp!(name, U16),
            "u32" => ty_refp!(name, U32),
            "u64" => ty_refp!(name, U64),
            "u128" => ty_refp!(name, U128),
            "f32" => ty_refp!(name, F32),
            "f64" => ty_refp!(name, F64),
            "char" => ty_refp!(name, Char),
            "bool" => ty_refp!(name, Bool),
            _ => TyRefKind::Named(name, TyGenericArgs { args: vec![] }), // TODO: do
        };

        TyRef { kind, span, hir_id }
    } else if let Some(fn_ty) = ty_ref.get_fn_ty() {
        let fn_ty = lower_fn_ty(hir_lower_ctxt, &fn_ty);
        TyRef {
            kind: TyRefKind::Fn(fn_ty),
            span,
            hir_id,
        }
    } else {
        todo!()
    };

    hir_lower_ctxt.push_ref_at_allocation(ty_ref.hir_id, HirElem::TyRef(ty_ref))
}

fn lower_fn_ty(hir_lower_ctxt: &mut HirLowerCtxt, fn_ty: &syntree::FnTy) -> FnTy {
    let fn_kw = FnKw::from_token(&fn_ty.get_fn_kw(), hir_lower_ctxt);
    let (lparen, params, rparen) = fn_ty.get_fn_ty_param_tys().map_or_else(
        || (LParen::make_virtual(), Vec::new(), RParen::make_virtual()),
        |params| {
            let lparen = LParen::from_token(&params.get_lparen(), hir_lower_ctxt);
            let rparen = RParen::from_token(&params.get_rparen().unwrap(), hir_lower_ctxt);
            (lparen, lower_fn_ty_params(hir_lower_ctxt, &params), rparen)
        },
    );
    let ret_ty = fn_ty.get_fn_ty_ret_ty().map(|r| {
        (
            ThinArrow::from_token(&r.get_arrow(), hir_lower_ctxt),
            lower_ty_ref(hir_lower_ctxt, &r.get_ty_ref().unwrap()),
        )
    });
    FnTy {
        fn_kw,
        lparen,
        params,
        rparen,
        ret_ty,
    }
}

fn lower_fn_ty_params(
    hir_lower_ctxt: &mut HirLowerCtxt,
    fn_ty_param_tys: &syntree::FnTyParamTys,
) -> Vec<TyRefId> {
    fn_ty_param_tys
        .get_ty_ref_list()
        .map(|it| lower_ty_ref(hir_lower_ctxt, &it))
        .collect()
}

fn lower_if_expr(hir_lower_ctxt: &mut HirLowerCtxt, if_expr: &syntree::IfExpr) -> IfExpr {
    let icond = if_expr.get_if_condition().unwrap();
    let cond = lower_expr_node(hir_lower_ctxt, &icond.get_expr_node().unwrap());
    let then = lower_expr_node(
        hir_lower_ctxt,
        &if_expr.get_if_then_clause().unwrap().get_expr_node(),
    );
    let else_ = if_expr
        .get_else_clause()
        .map(|it| lower_if_expr_else_clause(hir_lower_ctxt, &it));
    IfExpr {
        if_kw: IfKw::from_token(&if_expr.get_if_kw(), hir_lower_ctxt),
        lparen: LParen::from_token(&icond.get_lparen(), hir_lower_ctxt),
        cond,
        rparen: RParen::from_token(&icond.get_rparen().unwrap(), hir_lower_ctxt),
        then,
        else_,
    }
}

fn lower_if_expr_else_clause(
    hir_lower_ctxt: &mut HirLowerCtxt,
    if_expr_else_clause: &syntree::ElseClause,
) -> IfExprElseClause {
    IfExprElseClause {
        else_kw: ElseKw::from_token(&if_expr_else_clause.get_else_kw(), hir_lower_ctxt),
        expr: lower_expr_node(
            hir_lower_ctxt,
            &if_expr_else_clause.get_expr_node().unwrap(),
        ),
    }
}

fn lower_return_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    return_expr: &syntree::ReturnExpr,
) -> ReturnExpr {
    let return_kw = ReturnKw::from_token(&return_expr.get_return_kw(), hir_lower_ctxt);
    let expr = return_expr
        .get_expr_node()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it));
    ReturnExpr { return_kw, expr }
}

fn lower_break_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    break_expr: &syntree::BreakExpr,
) -> BreakExpr {
    let break_kw = BreakKw::from_token(&break_expr.get_break_kw(), hir_lower_ctxt);
    let expr = break_expr
        .get_expr_node()
        .map(|it| lower_expr_node(hir_lower_ctxt, &it));
    BreakExpr { break_kw, expr }
}

fn lower_continue_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    continue_expr: &syntree::ContinueExpr,
) -> ContinueExpr {
    let continue_kw = ContinueKw::from_token(&continue_expr.get_continue_kw(), hir_lower_ctxt);

    ContinueExpr { continue_kw }
}

fn lower_block_expr(
    hir_lower_ctxt: &mut HirLowerCtxt,
    block_expr: &syntree::BlockExpr,
) -> BlockExpr {
    let block = lower_block(hir_lower_ctxt, &block_expr.get_block());
    BlockExpr { block }
}

fn lower_ident(hir_lower_ctxt: &mut HirLowerCtxt, ident: &Token) -> Ident {
    assert_eq!(ident.kind(), SyntaxKind::IDENT);

    let span = HirSpan::of(ident, hir_lower_ctxt);
    let text = ident.text().to_owned();
    Ident { span, text }
}

trait FromTokenConstToken: ConstToken + Sized {
    const SYNTAX_KIND: SyntaxKind;

    fn from_token(token: &Token, hir_lower_ctxt: &HirLowerCtxt) -> Self {
        debug_assert_eq!(token.kind(), Self::SYNTAX_KIND);
        debug_assert_eq!(token.text(), Self::text());

        Self::from_span(HirSpan::of(token, hir_lower_ctxt))
    }
}

trait HirSpanImpl {
    fn of_node<T: syntree::TreeNode>(node: &T, hir_lower_ctxt: &HirLowerCtxt) -> Self;
    fn of(token: &Token, hir_lower_ctxt: &HirLowerCtxt) -> Self;
}

impl HirSpanImpl for HirSpan {
    fn of_node<T: syntree::TreeNode>(node: &T, hir_lower_ctxt: &HirLowerCtxt) -> Self {
        let span = TextSpan::of_node(node.get_node()).range_usize();
        Self::new(
            span.start + hir_lower_ctxt.src_file_start_offset_in_db,
            span.end + hir_lower_ctxt.src_file_start_offset_in_db,
        )
    }

    fn of(token: &Token, hir_lower_ctxt: &HirLowerCtxt) -> Self {
        let span = TextSpan::of(token).range_usize();
        Self::new(
            span.start + hir_lower_ctxt.src_file_start_offset_in_db,
            span.end + hir_lower_ctxt.src_file_start_offset_in_db,
        )
    }
}

trait GenericTokenImpl {
    fn from_token(token: &Token, hir_lower_ctxt: &HirLowerCtxt) -> Self;
}

impl GenericTokenImpl for GenericToken {
    fn from_token(token: &Token, hir_lower_ctxt: &HirLowerCtxt) -> Self {
        Self {
            span: HirSpan::of(token, hir_lower_ctxt),
            text: token.text().to_string(),
        }
    }
}

narxia_hir::const_tokens_impls!();
