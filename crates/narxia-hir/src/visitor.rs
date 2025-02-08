use crate::hir::HirIdNewtype;
use crate::hir_map::HirMap;
use crate::{hir, HirId, HirSpan};

pub trait HirMapQ<'hir> {
    fn run_hir_map_query<T: 'hir, Q: FnOnce(&'hir HirMap) -> T>(&self, q: Q) -> T;
}

impl<'hir> HirMapQ<'hir> for &'hir HirMap {
    fn run_hir_map_query<T: 'hir, Q: FnOnce(&'hir HirMap) -> T>(&self, q: Q) -> T {
        q(self)
    }
}

macro_rules! const_token_visit_fns {
    ($($visit_name:ident -> $walk_name:ident -> $kw_ty:path),* $(,)?) => {
        macro_rules! const_token_visit_fns_decl_visits {
            () => {
                $(
                    fn $visit_name(&mut self, kw: &'hir $kw_ty) {
                        $walk_name(self, kw)
                    }
                )*
            };
        }

        macro_rules! const_token_visit_fns_decl_walks {
            () => {
                $(
                    #[inline(always)]
                    fn $walk_name<'hir, V: HirVisitor<'hir> + ?Sized>(vis: &mut V, kw: &'hir $kw_ty) {
                        vis.visit_token_span(kw.span);
                    }
                )*
            };
        }

        macro_rules! const_token_implement_acceptors {
            () => {
                impl_visitable! {
                    $($visit_name($kw_ty)),*
                }
            };
        }
    };
}

macro_rules! hir_visitor_fns {
    ($($fn_name:ident($hir_ty:ty) -> $walk_name:ident),* $(,)?) => {
        macro_rules! visitor_fns_decl_visit_fns {
            () => {
                $(
                    fn $fn_name(&mut self, hir: &'hir $hir_ty) {
                        $walk_name(self, hir)
                    }
                )*
            };
        }

        macro_rules! visitor_fns_impl_visitable {
            () => {
                impl_visitable! {
                    $($fn_name($hir_ty)),*
                }
            };
        }
    };
}

macro_rules! hir_visitor_hir_ids {
    ($($visit_name:ident($hir_id_newtype:ty) -> $hm_getter:ident),* $(,)?) => {
        macro_rules! hir_visitor_hir_ids_visitor_fn_decls {
            () => {
                $(
                    fn $visit_name(&mut self, id: $hir_id_newtype) {
                        self.visit_hir_id(id.hir_id());
                        self.q_id_strategy(|v, hm| hm.$hm_getter(id).accept(v));
                        self.end_visit_hir_id(id.hir_id());
                    }
                )*
            };
        }

        macro_rules! hir_visitor_hir_ids_impl_visitable {
            () => {
                impl_visitable_id! {
                    $($visit_name($hir_id_newtype)),*
                }
            };
        }
    };
}

macro_rules! contextualised_hir_visitors {
    ($($visit_name:ident ($context_ty_id:ty, $hir_ty:ty) -> $walk_name:ident),* $(,)?) => {
        macro_rules! contextualised_hir_visitors_fn_decls {
            () => {
                $(
                    fn $visit_name(&mut self, id: $context_ty_id, hir: &'hir $hir_ty) {
                        $walk_name(self, id, hir)
                    }
                )*
            };
        }

        macro_rules! contextualised_hir_visitors_impl_acceptors {
            () => {
                impl_id_visitable! {
                    $($visit_name [$context_ty_id] ($hir_ty)),*
                }
            };
        }
    };
}

const_token_visit_fns! {
    visit_if_kw -> walk_if_kw -> hir::IfKw,
    visit_else_kw -> walk_else_kw -> hir::ElseKw,
    visit_let_kw -> walk_let_kw -> hir::LetKw,
    visit_mut_kw -> walk_mut_kw -> hir::MutKw,
    visit_fn_kw -> walk_fn_kw -> hir::FnKw,
    visit_module_kw -> walk_module_kw -> hir::ModuleKw,
    visit_const_kw -> walk_const_kw -> hir::ConstKw,
    visit_return_kw -> walk_return_kw -> hir::ReturnKw,
    visit_break_kw -> walk_break_kw -> hir::BreakKw,
    visit_continue_kw -> walk_continue_kw -> hir::ContinueKw,
    visit_while_kw -> walk_while_kw -> hir::WhileKw,
    visit_for_kw -> walk_for_kw -> hir::ForKw,
    visit_in_kw -> walk_in_kw -> hir::InKw,
    visit_loop_kw -> walk_loop_kw -> hir::LoopKw,
    visit_true_kw -> walk_true_kw -> hir::TrueKw,
    visit_false_kw -> walk_false_kw -> hir::FalseKw,
    visit_use_kw -> walk_use_kw -> hir::UseKw,
    visit_as_kw -> walk_as_kw -> hir::AsKw,

    vist_lquote -> walk_lquote -> hir::LQuote,
    visit_rquote -> walk_rquote -> hir::RQuote,

    visit_dot -> walk_dot -> hir::Dot,
    visit_comma -> walk_comma -> hir::Comma,
    visit_colon -> walk_colon -> hir::Colon,
    visit_colon2 -> walk_colon2 -> hir::Colon2,
    visit_lparen -> walk_lparen -> hir::LParen,
    visit_rparen -> walk_rparen -> hir::RParen,
    visit_lbrace -> walk_lbrace -> hir::LBrace,
    visit_rbrace -> walk_rbrace -> hir::RBrace,
    visit_lbracket -> walk_lbracket -> hir::LBracket,
    visit_rbracket -> walk_rbracket -> hir::RBracket,
    visit_langle -> walk_langle -> hir::LAngle,
    visit_rangle -> walk_rangle -> hir::RAngle,
    visit_thin_arrow -> walk_thin_arrow -> hir::ThinArrow,
    visit_fat_arrow -> walk_fat_arrow -> hir::FatArrow,
    visit_hash -> walk_hash -> hir::Hash,

    visit_eq -> walk_eq -> hir::Eq,
    visit_plus_eq -> walk_plus_eq -> hir::PlusEq,
    visit_minus_eq -> walk_minus_eq -> hir::MinusEq,
    visit_asterisk_eq -> walk_asterisk_eq -> hir::AsteriskEq,
    visit_slash_eq -> walk_slash_eq -> hir::SlashEq,
    visit_percent_eq -> walk_percent_eq -> hir::PercentEq,
    visit_amp_eq -> walk_amp_eq -> hir::AmpEq,
    visit_pipe_eq -> walk_pipe_eq -> hir::PipeEq,
    visit_caret_eq -> walk_caret_eq -> hir::CaretEq,
    visit_plus -> walk_plus -> hir::Plus,
    visit_minus -> walk_minus -> hir::Minus,
    visit_asterisk -> walk_asterisk -> hir::Asterisk,
    visit_slash -> walk_slash -> hir::Slash,
    visit_percent -> walk_percent -> hir::Percent,
    visit_amp -> walk_amp -> hir::Amp,
    visit_pipe -> walk_pipe -> hir::Pipe,
    visit_caret -> walk_caret -> hir::Caret,
    visit_eq2 -> walk_eq2 -> hir::Eq2,
    visit_neq -> walk_neq -> hir::Neq,
    visit_leq -> walk_leq -> hir::LEq,
    visit_geq -> walk_geq -> hir::GEq,
    visit_amp2 -> walk_amp2 -> hir::Amp2,
    visit_pipe2 -> walk_pipe2 -> hir::Pipe2,
}

hir_visitor_fns! {
    visit_mod_def(hir::ModDef) -> walk_mod_def,
    visit_mod_body(hir::ModBody) -> walk_mod_body,
    visit_attr_list(hir::AttrList) -> walk_attr_list,
    visit_attr(hir::Attr) -> walk_attr,
    visit_attr_meta(hir::AttrMeta) -> walk_attr_meta,
    visit_attr_meta_item(hir::AttrMetaItem) -> walk_attr_meta_item,
    visit_attr_meta_item_eq(hir::AttrMetaItemEq) -> walk_attr_meta_item_eq,
    visit_attr_meta_item_call(hir::AttrMetaItemCall) -> walk_attr_meta_item_call,
    visit_item(hir::Item) -> walk_item,
    visit_stmt(hir::Stmt) -> walk_stmt,
    visit_expr(hir::Expr) -> walk_expr,
    visit_block(hir::Block) -> walk_block,
    visit_ident(hir::Ident) -> walk_ident,
    visit_num_literal(hir::NumLit) -> walk_num_literal,
    visit_item_list(hir::ItemList) -> walk_item_list,
    visit_assignment_op(hir::AssignmentOp) -> walk_assignment_op,
    visit_generic_params(hir::GenericParams) -> walk_generic_params,
    visit_generic_param(hir::GenericParam) -> walk_generic_param,
    visit_generic_param_ty(hir::GenericParamTy) -> walk_generic_param_ty,
    visit_generic_param_ty_bounds(hir::GenericParamTyBounds) -> walk_generic_param_ty_bounds,
    visit_generic_param_const(hir::GenericParamConst) -> walk_generic_param_const,
    visit_fn_def(hir::FnDef) -> walk_fn_def,
    visit_fn_param_list(hir::FnParamList) -> walk_fn_param_list,
    visit_fn_param(hir::FnParam) -> walk_fn_param,
    visit_fn_ret_ty(hir::FnRetTy) -> walk_fn_ret_ty,
    visit_pat(hir::Pat) -> walk_pat,
    visit_ty_ref(hir::TyRef) -> walk_ty_ref,
    visit_ty_generic_args(hir::TyGenericArgs) -> walk_ty_generic_args,
    visit_ty_generic_arg(hir::TyGenericArg) -> walk_ty_generic_arg,
    visit_fn_ty_ref(hir::FnTy) -> walk_fn_ty_ref,
    visit_str_literal(hir::StrLiteral) -> walk_str_literal,
    visit_str_literal_fragment(hir::StrLiteralFragment) -> walk_str_literal_fragment,
    visit_str_literal_text_fragment(hir::StrLiteralTextFragment) -> walk_str_literal_text_fragment,
    visit_str_literal_display_fragment(hir::StrLiteralDisplayFragment) -> walk_str_literal_display_fragment,
    visit_str_literal_debug_fragment(hir::StrLiteralDebugFragment) -> walk_str_literal_debug_fragment,

    visit_call_args(hir::CallExprArgs) -> walk_call_args,
    visit_lambda_param_list(hir::LambdaParamList) -> walk_lambda_param_list,
    visit_lambda_param(hir::LambdaParam) -> walk_lambda_param,

    visit_use_stmt(hir::UseStmt) -> walk_use_stmt,
    visit_use_path(hir::UsePath) -> walk_use_path,
    visit_use_path_segment(hir::UsePathSegment) -> walk_use_path_segment,
    visit_use_alias(hir::UseAlias) -> walk_use_alias,
}

hir_visitor_hir_ids! {
    visit_item_id(hir::ItemId) -> get_item,
    visit_stmt_id(hir::StmtId) -> get_stmt,
    visit_block_id(hir::BlockId) -> get_block,
    visit_fn_id(hir::FnId) -> get_fn,
    visit_mod_id(hir::ModId) -> get_mod,
    visit_ty_ref_id(hir::TyRefId) -> get_ty_ref,
    visit_use_stmt_id(hir::UseStmtId) -> get_use_stmt,
    visit_use_path_segment_id(hir::UsePathSegmentId) -> get_use_segment,
    visit_expr_id(hir::ExprId) -> get_expr,
    visit_ty_generic_arg_id(hir::TyGenericArgId) -> get_ty_generic_arg,
}

contextualised_hir_visitors! {
    visit_let_stmt(hir::StmtId, hir::LetStmt) -> walk_let_stmt,
    visit_for_stmt(hir::StmtId, hir::ForStmt) -> walk_for_stmt,
    visit_while_stmt(hir::StmtId, hir::WhileStmt) -> walk_while_stmt,
    visit_assignment_stmt(hir::StmtId, hir::AssignmentStmt) -> walk_assignment_stmt,
    //
    visit_block_expr(hir::ExprId, hir::BlockExpr) -> walk_block_expr,
    visit_expr_atom(hir::ExprId, hir::ExprAtom) -> walk_expr_atom,
    visit_expr_binary_expr(hir::ExprId, hir::BinaryOpExpr) -> walk_expr_binary_expr,
    visit_expr_call_expr(hir::ExprId, hir::CallExpr) -> walk_expr_call_expr,
    visit_expr_index_expr(hir::ExprId, hir::IndexExpr) -> walk_expr_index_expr,
    visit_expr_field_access(hir::ExprId, hir::FieldAccess) -> walk_expr_field_access,
    visit_expr_method_call(hir::ExprId, hir::MethodCall) -> walk_expr_method_call,
    visit_custom_infix_expr(hir::ExprId, hir::CustomInfixExpr) -> walk_custom_infix_expr,
    visit_return_expr(hir::ExprId, hir::ReturnExpr) -> walk_return_expr,
    visit_break_expr(hir::ExprId, hir::BreakExpr) -> walk_break_expr,
    visit_continue_expr(hir::ExprId, hir::ContinueExpr) -> walk_continue_expr,
    visit_loop_expr(hir::ExprId, hir::LoopExpr) -> walk_loop_expr,
    visit_if_expr(hir::ExprId, hir::IfExpr) -> walk_if_expr,
    visit_if_expr_else_clause(hir::ExprId, hir::IfExprElseClause) -> walk_if_expr_else_clause,
    visit_tuple_like_expr(hir::ExprId, hir::TupleExpr) -> walk_tuple_like_expr,
    visit_lambda_expr(hir::ExprId, hir::LambdaExpr) -> walk_lambda_expr,
}

#[allow(unused_variables)]
pub trait HirVisitor<'hir> {
    fn q_id_strategy<Q: FnOnce(&mut Self, &'hir HirMap)>(&mut self, q: Q);

    visitor_fns_decl_visit_fns!();

    contextualised_hir_visitors_fn_decls!();

    fn visit_hir_id(&mut self, hir_id: HirId) {
        // Nothing to do.
        #[cfg(hir_id_span)]
        {
            self.visit_span(hir_id.span);
        }
    }

    #[inline(always)]
    fn end_visit_hir_id(&mut self, hir_id: HirId) {
        // Nothing to do.
    }

    #[inline(always)]
    fn visit_span(&mut self, span: HirSpan) {
        // Nothing to do.
    }

    #[inline(always)]
    fn visit_token_span(&mut self, span: HirSpan) {
        self.visit_span(span);
    }

    #[inline(always)]
    fn visit_generic_token(&mut self, token: &hir::Tk) {
        self.visit_token_span(token.span);
    }

    hir_visitor_hir_ids_visitor_fn_decls! {}
    const_token_visit_fns_decl_visits! {}
}

const_token_visit_fns_decl_walks!();

pub fn walk_mod_def<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    mod_def: &'hir hir::ModDef,
) {
    mod_def.mod_kw.accept(visitor);
    mod_def.name.accept(visitor);
    mod_def.body.accept(visitor);
}

pub fn walk_mod_body<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    mod_body: &'hir hir::ModBody,
) {
    mod_body.lbrace.accept(visitor);
    for item in &mod_body.items.items {
        item.accept(visitor);
    }
    mod_body.rbrace.accept(visitor);
}

pub fn walk_item_list<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    item_list: &'hir hir::ItemList,
) {
    for item in &item_list.items {
        item.accept(visitor);
    }
}

pub fn walk_item<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, item: &'hir hir::Item) {
    item.attrs.accept(visitor);

    match &item.kind {
        hir::ItemKind::FnDef(fn_def) => {
            fn_def.accept(visitor);
        }
        hir::ItemKind::Stmt(stmt) => {
            stmt.accept(visitor);
        }
        hir::ItemKind::UseStmt(use_stmt) => {
            use_stmt.accept(visitor);
        }
        hir::ItemKind::ModDef(mod_def) => {
            mod_def.accept(visitor);
        }
    }
}

pub fn walk_attr_list<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    attr_list: &'hir hir::AttrList,
) {
    for attr in &attr_list.attrs {
        attr.accept(visitor);
    }
}

pub fn walk_attr<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, attr: &'hir hir::Attr) {
    attr.hash.accept(visitor);
    attr.name.accept(visitor);
    attr.meta.accept(visitor);
}

pub fn walk_attr_meta<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    attr_meta: &'hir hir::AttrMeta,
) {
    attr_meta.lbrack.accept(visitor);
    for meta in &attr_meta.meta_list {
        meta.accept(visitor);
    }
    attr_meta.rbrack.accept(visitor);
}

pub fn walk_attr_meta_item<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    attr_meta_item: &'hir hir::AttrMetaItem,
) {
    attr_meta_item.span.accept(visitor);
    attr_meta_item.name.accept(visitor);
    match &attr_meta_item.kind {
        hir::AttrMetaItemKind::Eq(attr_meta_item_eq) => {
            attr_meta_item_eq.accept(visitor);
        }
        hir::AttrMetaItemKind::Call(attr_meta_item_call) => {
            attr_meta_item_call.accept(visitor);
        }
        hir::AttrMetaItemKind::NameOnly => {}
    }
}

pub fn walk_attr_meta_item_eq<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    attr_meta_item_eq: &'hir hir::AttrMetaItemEq,
) {
    attr_meta_item_eq.eq.accept(visitor);
    attr_meta_item_eq.expr.accept(visitor);
}

pub fn walk_attr_meta_item_call<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    attr_meta_item_call: &'hir hir::AttrMetaItemCall,
) {
    attr_meta_item_call.lparen.accept(visitor);
    for arg in &attr_meta_item_call.meta_list {
        arg.accept(visitor);
    }
    attr_meta_item_call.rparen.accept(visitor);
}

pub fn walk_fn_def<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_def: &'hir hir::FnDef) {
    walk_fn_head(visitor, fn_def);
    fn_def.body.accept(visitor);
}

pub fn walk_fn_head<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_def: &'hir hir::FnDef) {
    fn_def.fn_kw.accept(visitor);
    fn_def.name.accept(visitor);
    fn_def.generics.accept(visitor);
    fn_def.params.accept(visitor);
    fn_def.ret_ty.accept(visitor);
}

pub fn walk_generic_params<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    gparams: &'hir hir::GenericParams,
) {
    gparams.span.accept(visitor);
    gparams.langle.accept(visitor);
    for gparam in &gparams.params {
        gparam.accept(visitor);
    }
    gparams.rangle.accept(visitor);
}

pub fn walk_generic_param<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    gparam: &'hir hir::GenericParam,
) {
    gparam.span.accept(visitor);

    match &gparam.kind {
        hir::GenericParamKind::Type(generic_param_ty) => {
            generic_param_ty.accept(visitor);
        }
        hir::GenericParamKind::Const(generic_param_const) => {
            generic_param_const.accept(visitor);
        }
    }
}

pub fn walk_generic_param_ty<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    generic_param_ty: &'hir hir::GenericParamTy,
) {
    generic_param_ty.name.accept(visitor);

    generic_param_ty.bounds.accept(visitor);
    if let Some((eq, default)) = &generic_param_ty.default {
        eq.accept(visitor);
        default.accept(visitor);
    }
}

pub fn walk_generic_param_const<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    generic_param_const: &'hir hir::GenericParamConst,
) {
    generic_param_const.const_kw.accept(visitor);
    generic_param_const.name.accept(visitor);
    generic_param_const.colon.accept(visitor);
    generic_param_const.ty.accept(visitor);
}

pub fn walk_generic_param_ty_bounds<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    ty_bounds: &'hir hir::GenericParamTyBounds,
) {
    ty_bounds.colon.accept(visitor);
    for bound in &ty_bounds.bounds {
        bound.accept(visitor);
    }
}

pub fn walk_fn_param_list<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    param_list: &'hir hir::FnParamList,
) {
    param_list.lparen.accept(visitor);
    for param in &param_list.params {
        param.accept(visitor);
    }
    param_list.rparen.accept(visitor);
}

pub fn walk_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, stmt: &'hir hir::Stmt) {
    match &stmt.kind {
        hir::StmtKind::ExprStmt(expr) => {
            expr.accept(visitor);
        }
        hir::StmtKind::LetStmt(let_stmt) => {
            let_stmt.accept(stmt.hir_id, visitor);
        }
        hir::StmtKind::ForStmt(for_stmt) => {
            for_stmt.accept(stmt.hir_id, visitor);
        }
        hir::StmtKind::WhileStmt(while_stmt) => {
            while_stmt.accept(stmt.hir_id, visitor);
        }
        hir::StmtKind::AssignmentStmt(assignment_stmt) => {
            assignment_stmt.accept(stmt.hir_id, visitor);
        }
    }
}

pub fn walk_expr<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, expr: &'hir hir::Expr) {
    match &expr.kind {
        hir::ExprKind::Atom(atom) => {
            atom.accept(expr.hir_id, visitor);
        }
        hir::ExprKind::Binary(binary_expr) => {
            binary_expr.accept(expr.hir_id, visitor);
        }
        hir::ExprKind::CallExpr(call_expr) => {
            call_expr.accept(expr.hir_id, visitor);
        }
        hir::ExprKind::IndexExpr(index_expr) => {
            index_expr.accept(expr.hir_id, visitor);
        }
        hir::ExprKind::FieldAccess(field_access) => {
            field_access.accept(expr.hir_id, visitor);
        }
        hir::ExprKind::MethodCall(method_call) => {
            method_call.accept(expr.hir_id, visitor);
        }
        hir::ExprKind::CustomInfix(custom_infix) => {
            custom_infix.accept(expr.hir_id, visitor);
        }
    }
}

pub fn walk_expr_atom<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    expr_id: hir::ExprId,
    atom: &'hir hir::ExprAtom,
) {
    match &atom.kind {
        hir::ExprAtomKind::Ident(ident) => {
            ident.accept(visitor);
        }
        hir::ExprAtomKind::Num(num) => {
            num.accept(visitor);
        }
        hir::ExprAtomKind::Str(s) => {
            s.accept(visitor);
        }
        hir::ExprAtomKind::BlockExpr(block_expr) => {
            block_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::ReturnExpr(ret) => {
            ret.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::BreakExpr(break_expr) => {
            break_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::ContinueExpr(continue_expr) => {
            continue_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::LoopExpr(loop_expr) => {
            loop_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::IfExpr(if_expr) => {
            if_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::TupleExpr(tuple_like_expr) => {
            tuple_like_expr.accept(expr_id, visitor);
        }
        hir::ExprAtomKind::LambdaExpr(l) => {
            l.accept(expr_id, visitor);
        }
    }
}

pub fn walk_ty_ref<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, ty_ref: &'hir hir::TyRef) {
    match &ty_ref.kind {
        hir::TyRefKind::Named(name, generics) => {
            name.accept(visitor);
            generics.accept(visitor);
        }
        hir::TyRefKind::Primitive(p) => {
            p.ident.accept(visitor);
        }
        hir::TyRefKind::Fn(fn_ty) => {
            fn_ty.accept(visitor);
        }
    }
}

pub fn walk_ty_generic_args<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    generic_args: &'hir hir::TyGenericArgs,
) {
    for arg in &generic_args.args {
        arg.accept(visitor);
    }
}

pub fn walk_ty_generic_arg<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    generic_arg: &'hir hir::TyGenericArg,
) {
    match &generic_arg.kind {
        hir::TyGenericArgKind::Type(ty_ref) => {
            ty_ref.accept(visitor);
        }
        hir::TyGenericArgKind::ConstVal(const_val) => {
            const_val.accept(visitor);
        }
    }
}

pub fn walk_fn_param<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    fn_param: &'hir hir::FnParam,
) {
    fn_param.pat.accept(visitor);
    fn_param.colon.accept(visitor);
    fn_param.ty.accept(visitor);

    if let Some((eq, default)) = &fn_param.default {
        eq.accept(visitor);
        default.accept(visitor);
    }
}

pub fn walk_fn_ret_ty<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    fn_ret_ty: &'hir hir::FnRetTy,
) {
    fn_ret_ty.arrow.accept(visitor);
    fn_ret_ty.ty.accept(visitor);
}

pub fn walk_let_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::StmtId,
    let_stmt: &'hir hir::LetStmt,
) {
    let_stmt.let_kw.accept(visitor);
    match &let_stmt.mutability {
        hir::LetMutability::Imm => {}
        hir::LetMutability::Mut(mut_kw) => {
            mut_kw.accept(visitor);
        }
    }
    let_stmt.pat.accept(visitor);
    if let Some((colon, ty)) = &let_stmt.ty {
        colon.accept(visitor);
        ty.accept(visitor);
    }
    if let Some((eq, init)) = &let_stmt.init {
        eq.accept(visitor);
        init.accept(visitor);
    }
}

pub fn walk_for_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::StmtId,
    for_stmt: &'hir hir::ForStmt,
) {
    for_stmt.for_kw.accept(visitor);
    for_stmt.lparen.accept(visitor);
    for_stmt.pat.accept(visitor);
    for_stmt.in_kw.accept(visitor);
    for_stmt.iter.accept(visitor);
    for_stmt.rparen.accept(visitor);
    for_stmt.body.accept(visitor);
}

pub fn walk_while_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::StmtId,
    while_stmt: &'hir hir::WhileStmt,
) {
    while_stmt.while_kw.accept(visitor);
    while_stmt.lparen.accept(visitor);
    while_stmt.expr.accept(visitor);
    while_stmt.rparen.accept(visitor);
    while_stmt.body.accept(visitor);
}

pub fn walk_assignment_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::StmtId,
    assignment_stmt: &'hir hir::AssignmentStmt,
) {
    assignment_stmt.lhs.accept(visitor);
    assignment_stmt.op.accept(visitor);
    assignment_stmt.rhs.accept(visitor);
}

pub fn walk_assignment_op<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    assignment_op: &'hir hir::AssignmentOp,
) {
    match assignment_op {
        hir::AssignmentOp::Assign(eq) => eq.accept(visitor),
        hir::AssignmentOp::AddAssign(plus_eq) => plus_eq.accept(visitor),
        hir::AssignmentOp::SubAssign(minus_eq) => minus_eq.accept(visitor),
        hir::AssignmentOp::MulAssign(asterisk_eq) => asterisk_eq.accept(visitor),
        hir::AssignmentOp::DivAssign(slash_eq) => slash_eq.accept(visitor),
        hir::AssignmentOp::ModAssign(percent_eq) => percent_eq.accept(visitor),
        hir::AssignmentOp::BitAndAssign(amp_eq) => amp_eq.accept(visitor),
        hir::AssignmentOp::BitOrAssign(pipe_eq) => pipe_eq.accept(visitor),
        hir::AssignmentOp::BitXorAssign(caret_eq) => caret_eq.accept(visitor),
    }
}

pub fn walk_block<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, block: &'hir hir::Block) {
    block.lbrace.accept(visitor);
    block.items.accept(visitor);
    block.rbrace.accept(visitor);
}

pub fn walk_block_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    block_expr: &'hir hir::BlockExpr,
) {
    block_expr.block.accept(visitor);
}

pub fn walk_expr_binary_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    binary_expr: &'hir hir::BinaryOpExpr,
) {
    binary_expr.lhs.accept(visitor);
    match &binary_expr.op {
        hir::BinOp::Add(t) => t.accept(visitor),
        hir::BinOp::Sub(t) => t.accept(visitor),
        hir::BinOp::Mul(t) => t.accept(visitor),
        hir::BinOp::Div(t) => t.accept(visitor),
        hir::BinOp::Mod(t) => t.accept(visitor),
        hir::BinOp::Eq(t) => t.accept(visitor),
        hir::BinOp::Neq(t) => t.accept(visitor),
        hir::BinOp::Lt(t) => t.accept(visitor),
        hir::BinOp::LtEq(t) => t.accept(visitor),
        hir::BinOp::Gt(t) => t.accept(visitor),
        hir::BinOp::GtEq(t) => t.accept(visitor),
        hir::BinOp::And(t) => t.accept(visitor),
        hir::BinOp::Or(t) => t.accept(visitor),
        hir::BinOp::BitAnd(t) => t.accept(visitor),
        hir::BinOp::BitOr(t) => t.accept(visitor),
        hir::BinOp::Xor(t) => t.accept(visitor),
    }
    binary_expr.rhs.accept(visitor);
}

pub fn walk_expr_call_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    call_expr: &'hir hir::CallExpr,
) {
    call_expr.callee.accept(visitor);
    call_expr.args.accept(visitor);
}

pub fn walk_expr_index_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    index_expr: &'hir hir::IndexExpr,
) {
    index_expr.base.accept(visitor);
    index_expr.lbrack.accept(visitor);
    index_expr.index.accept(visitor);
    index_expr.rbrack.accept(visitor);
}

pub fn walk_expr_field_access<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    field_access: &'hir hir::FieldAccess,
) {
    field_access.base.accept(visitor);
    field_access.dot.accept(visitor);
    field_access.field.accept(visitor);
}

pub fn walk_expr_method_call<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    method_call: &'hir hir::MethodCall,
) {
    method_call.base.accept(visitor);
    method_call.dot.accept(visitor);
    method_call.method.accept(visitor);
    method_call.args.accept(visitor);
}

pub fn walk_return_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    ret: &'hir hir::ReturnExpr,
) {
    ret.return_kw.accept(visitor);
    ret.expr.accept(visitor);
}

pub fn walk_break_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    break_expr: &'hir hir::BreakExpr,
) {
    break_expr.break_kw.accept(visitor);
    break_expr.expr.accept(visitor);
}

pub fn walk_continue_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    continue_expr: &'hir hir::ContinueExpr,
) {
    continue_expr.continue_kw.accept(visitor);
}

pub fn walk_custom_infix_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    custom_infix: &'hir hir::CustomInfixExpr,
) {
    custom_infix.base.accept(visitor);
    custom_infix.name.accept(visitor);
    custom_infix.arg.accept(visitor);
}

pub fn walk_loop_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    loop_expr: &'hir hir::LoopExpr,
) {
    loop_expr.loop_kw.accept(visitor);
    loop_expr.body.accept(visitor);
}

pub fn walk_if_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    expr_id: hir::ExprId,
    if_expr: &'hir hir::IfExpr,
) {
    if_expr.if_kw.accept(visitor);
    if_expr.lparen.accept(visitor);
    if_expr.cond.accept(visitor);
    if_expr.rparen.accept(visitor);
    if_expr.then.accept(visitor);
    if let Some(else_clause) = &if_expr.else_ {
        else_clause.accept(expr_id, visitor);
    }
}

pub fn walk_if_expr_else_clause<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    else_clause: &'hir hir::IfExprElseClause,
) {
    else_clause.else_kw.accept(visitor);
    else_clause.expr.accept(visitor);
}

pub fn walk_tuple_like_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    tuple_like_expr: &'hir hir::TupleExpr,
) {
    tuple_like_expr.lparen.accept(visitor);
    for expr in &tuple_like_expr.exprs {
        expr.accept(visitor);
    }
    tuple_like_expr.rparen.accept(visitor);
}

pub fn walk_pat<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, pat: &'hir hir::Pat) {
    match &pat.kind {
        hir::PatKind::Ident(ident) => {
            ident.accept(visitor);
        }
        hir::PatKind::Tuple(tuple_like) => {
            for pat in tuple_like {
                pat.accept(visitor);
            }
        }
        hir::PatKind::Wildcard(w) => {
            w.accept(visitor);
        }
    }
}

pub fn walk_call_args<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    call_args: &'hir hir::CallExprArgs,
) {
    call_args.lparen.accept(visitor);
    for arg in &call_args.args {
        arg.accept(visitor);
    }
    call_args.rparen.accept(visitor);
}

pub fn walk_lambda_expr<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    _: hir::ExprId,
    lambda_expr: &'hir hir::LambdaExpr,
) {
    lambda_expr.lbrace.accept(visitor);
    if let Some(lpl) = &lambda_expr.lambda_param_list {
        lpl.accept(visitor);
    }
    lambda_expr.body.accept(visitor);
    lambda_expr.rbrace.accept(visitor);
}

pub fn walk_lambda_param_list<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    lambda_param_list: &'hir hir::LambdaParamList,
) {
    for param in &lambda_param_list.params {
        param.accept(visitor);
    }

    lambda_param_list.arrow.accept(visitor);
}

pub fn walk_lambda_param<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    lambda_param: &'hir hir::LambdaParam,
) {
    lambda_param.pat.accept(visitor);
    if let Some((colon, ty)) = &lambda_param.ty {
        colon.accept(visitor);
        ty.accept(visitor);
    }
}

pub fn walk_str_literal<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_literal: &'hir hir::StrLiteral,
) {
    str_literal.span.accept(visitor);
    str_literal.lquote.accept(visitor);
    for fragment in &str_literal.fragments {
        fragment.accept(visitor);
    }
    str_literal.rquote.accept(visitor);
}

pub fn walk_str_literal_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_literal_fragment: &'hir hir::StrLiteralFragment,
) {
    match &str_literal_fragment.kind {
        hir::StrLiteralFragmentKind::Text(t) => {
            t.accept(visitor);
        }
        hir::StrLiteralFragmentKind::EscapeSequence(..)
        | hir::StrLiteralFragmentKind::EscapedChar(..) => {
            // Nothing to do.
        }
        hir::StrLiteralFragmentKind::Display(f) => {
            f.accept(visitor);
        }
        hir::StrLiteralFragmentKind::Debug(f) => {
            f.accept(visitor);
        }
    }
}

pub fn walk_str_literal_text_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_literal_text_fragment: &'hir hir::StrLiteralTextFragment,
) {
    str_literal_text_fragment.token.accept(visitor);
}

pub fn walk_str_literal_display_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_display_fragment: &'hir hir::StrLiteralDisplayFragment,
) {
    str_display_fragment.expr.accept(visitor);
}

pub fn walk_str_literal_debug_fragment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    str_debug_fragment: &'hir hir::StrLiteralDebugFragment,
) {
    str_debug_fragment.expr.accept(visitor);
}

pub fn walk_fn_ty_ref<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, fn_ty: &'hir hir::FnTy) {
    fn_ty.fn_kw.accept(visitor);
    fn_ty.lparen.accept(visitor);

    for ty in &fn_ty.params {
        ty.accept(visitor);
    }

    fn_ty.rparen.accept(visitor);

    if let Some((arrow, ret_ty)) = &fn_ty.ret_ty {
        arrow.accept(visitor);
        ret_ty.accept(visitor);
    }
}

pub fn walk_use_stmt<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    use_stmt: &'hir hir::UseStmt,
) {
    use_stmt.span.accept(visitor);
    use_stmt.use_kw.accept(visitor);
    use_stmt.path.accept(visitor);
}

pub fn walk_use_path<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    use_path: &'hir hir::UsePath,
) {
    for segment in &use_path.segments {
        segment.accept(visitor);
    }

    use_path.alias.accept(visitor);
}

pub fn walk_use_path_segment<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    use_path_segment: &'hir hir::UsePathSegment,
) {
    use_path_segment.ident.accept(visitor);
}

pub fn walk_use_alias<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    use_alias: &'hir hir::UseAlias,
) {
    use_alias.as_kw.accept(visitor);
    use_alias.alias.accept(visitor);
}

pub fn walk_ident<'hir, V: HirVisitor<'hir> + ?Sized>(visitor: &mut V, ident: &'hir hir::Ident) {
    visitor.visit_token_span(ident.span);
}

pub fn walk_num_literal<'hir, V: HirVisitor<'hir> + ?Sized>(
    visitor: &mut V,
    num: &'hir hir::NumLit,
) {
    match num {
        hir::NumLit::Bin(tk)
        | hir::NumLit::Oct(tk)
        | hir::NumLit::Dec(tk)
        | hir::NumLit::Hex(tk) => tk.accept(visitor),
    }
}

pub trait Visitable<'hir> {
    fn accept<V: HirVisitor<'hir> + ?Sized>(&'hir self, visitor: &mut V);
}

impl<'hir, T: Visitable<'hir>> Visitable<'hir> for Option<T> {
    fn accept<V: HirVisitor<'hir> + ?Sized>(&'hir self, visitor: &mut V) {
        if let Some(inner) = self {
            inner.accept(visitor);
        }
    }
}

pub trait OwnedVisitable<'hir> {
    fn accept<V: HirVisitor<'hir> + ?Sized>(self, visitor: &mut V);
}

pub trait IdVisitable<'hir> {
    type Id;
    fn accept<V: HirVisitor<'hir> + ?Sized>(&'hir self, id: Self::Id, visitor: &mut V);
}

impl<'hir, T: IdVisitable<'hir>> IdVisitable<'hir> for Option<T> {
    type Id = T::Id;
    fn accept<V: HirVisitor<'hir> + ?Sized>(&'hir self, id: Self::Id, visitor: &mut V) {
        if let Some(inner) = self {
            inner.accept(id, visitor);
        }
    }
}

macro_rules! impl_visitable {
    ($($visit_name:ident ($ty:ty)),* $(,)?) => {
        $(
            impl<'hir> Visitable<'hir> for $ty {
                fn accept<V: HirVisitor<'hir>+ ?Sized>(&'hir self, visitor: &mut V) {
                    visitor.$visit_name (self);
                }
            }
        )*
    };
}

visitor_fns_impl_visitable!();

impl_visitable! {
    visit_generic_token(hir::Tk),
}

const_token_implement_acceptors!();

macro_rules! impl_visitable_id {
    ($($visit_name:ident ($ty:ty)),* $(,)?) => {
        $(
            impl<'hir> OwnedVisitable<'hir> for $ty {
                fn accept<V: HirVisitor<'hir> + ?Sized>(self, visitor: &mut V) {
                    visitor.$visit_name (self);
                }
            }

            impl<'hir> OwnedVisitable<'hir> for Option<$ty> {
                fn accept<V: HirVisitor<'hir> + ?Sized>(self, visitor: &mut V) {
                    if let Some(inner) = self {
                        inner.accept(visitor);
                    }
                }
            }
        )*
    }
}

hir_visitor_hir_ids_impl_visitable!();

impl_visitable_id! {
    visit_hir_id(HirId),
    visit_span(HirSpan),
}

macro_rules! impl_id_visitable {
    ($($visit_name:ident [$id:ty] ($ty:ty)),* $(,)?) => {
        $(
            impl<'hir> IdVisitable<'hir> for $ty {
                type Id = $id;
                fn accept<V: HirVisitor<'hir> + ?Sized>(&'hir self, id: Self::Id, visitor: &mut V) {
                    visitor.$visit_name (id, self);
                }
            }
        )*
    };
}

contextualised_hir_visitors_impl_acceptors!();
