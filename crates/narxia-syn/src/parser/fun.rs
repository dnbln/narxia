use narxia_syn_helpers::{parse_fn_decl, parse_fn};
use super::{Parser, parse_block, CompletedMarker, parse_list_simple2, AttemptRecoveryLevel, parse_ty_ref, expr, parse_list_rep_simple2, parse_pat};
use crate::syntax_kind::{SyntaxKind, T};

parse_fn_decl! {
    pub parse_fn_def: FnDef ::=
        $parse_fn_head()
        $/ws:wcn
        $parse_block()
}

parse_fn_decl! {
    parse_fn_head: FnHead ::=
        $![fn]
        $/ws:wcn
        $parse_fn_name()
        $/ws:wcn
        $/if at[<] {
            $parse_generic_param_list()
            $/ws:wcn
        }
        $parse_fn_param_list()
        $/state:s1
        $/ws:wcn
        $/if at[->] {
            $parse_fn_ret_ty()
        }
        $/else {
            $/restore_state:s1
        }
}

parse_fn_decl! {
    parse_generic_param_list: GenericParamList ::=
        $parse_list_simple2(
            T![<],
            parse_generic_param,
            T![,],
            T![>],
            AttemptRecoveryLevel::Shallow,
        )
}

parse_fn_decl! {
    parse_generic_param: GenericParam ::=
        $/match {
            [const] => {$parse_generic_const_param()}
            [ident] => {$parse_generic_ty_param()}
        }
}

parse_fn_decl! {
    parse_generic_const_param: GenericConstParam ::=
        $![const]
        $/ws:wcn
        $parse_generic_const_param_name()
        $/ws:wcn
        $![:]
        $/ws:wcn
        $parse_ty_ref()
        $/state:s1
        $/ws:wcn
        $/if at[=] {
            $parse_generic_const_param_default()
        }
        $/else {
            $/restore_state:s1
        }
}

parse_fn_decl! {
    parse_generic_const_param_name: GenericConstParamName ::=
        $![ident]
}

parse_fn_decl! {
    parse_generic_const_param_default: GenericConstParamDefault ::=
        $![=]
        $/ws:wcn
        $expr::parse_expr()
}

parse_fn_decl! {
    parse_fn_name: FnName ::=
        $![ident]
}

parse_fn_decl! {
    parse_generic_ty_param: GenericTyParam ::=
        $parse_generic_ty_param_name()
        $/state:s1
        $/ws:wcn
        $/if at[:] {
            $![:]
            $/ws:wcn
            $parse_generic_ty_param_bound_list()

            $/state:s2
            $/ws:wcn
            $/if at[=] {
                $parse_generic_ty_param_default()
            }
            $/else {
                $/restore_state:s2
            }
        }
        $/else if at[=] {
            $parse_generic_ty_param_default()
        }
        $/else {
            $/restore_state:s1
        }
}

parse_fn_decl! {
    parse_generic_ty_param_bound_list: GenericTyParamBoundList ::=
        $parse_list_rep_simple2(
            T![+],
            parse_generic_ty_param_bound,
            AttemptRecoveryLevel::Shallow,
        )
}

parse_fn_decl! {
    parse_generic_ty_param_bound: GenericTyParamBound ::=
        $parse_ty_ref()
}

parse_fn_decl! {
    parse_generic_ty_param_default: GenericTyParamDefault ::=
        $![=]
        $/ws:wcn
        $parse_ty_ref()
}

parse_fn_decl! {
    parse_generic_ty_param_name: GenericTyParamName ::=
        $![ident]
}

parse_fn_decl! {
    parse_fn_param_list: FnParamList ::=
        $parse_list_simple2(
            T!['('],
            parse_fn_param,
            T![,],
            T![')'],
            AttemptRecoveryLevel::Shallow,
        )
}

parse_fn_decl! {
    parse_fn_param: FnParam ::=
        $parse_fn_param_name()
        $/ws:wcn
        $![:]
        $/ws:wcn
        $parse_fn_param_ty()
        $/state:s1
        $/ws:wcn
        $/if at[=] {
            $parse_fn_param_default()
        }
        $/else {
            $/restore_state:s1
        }
}

parse_fn_decl! {
    pub parse_fn_param_name: FnParamName ::=
        $parse_pat()
}

parse_fn_decl! {
    pub parse_fn_param_ty: FnParamTy ::=
        $parse_ty_ref()
}

parse_fn_decl! {
    parse_fn_param_default: FnParamDefault ::=
        $![=]
        $/ws:wcn
        $expr::parse_expr()
}

parse_fn_decl! {
    parse_fn_ret_ty: FnRetTy ::=
        $![->]
        $/ws:wcn
        $parse_ty_ref()
}
