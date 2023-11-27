use narxia_syn_helpers::{parse_fn, parse_fn_decl};

use super::{
    fun, parse_block, parse_block_insides, parse_list_rep_simple2, parse_list_simple2,
    parse_ty_ref, AttemptRecoveryLevel, CompletedMarker, Parser, WsSkipConfig,
};
use crate::syntax_kind::{SyntaxKind, T};

parse_fn_decl! {
    parse_num_lit: NumLit ::=
        $/match {
            [num_bin]!
            [num_oct]!
            [num_dec]!
            [num_hex]!
        }
}

parse_fn_decl! {
    parse_expr_atom: ExprAtom ::=
        $/match {
            [ident]!
            [string]!
            [num_bin]
            [num_oct]
            [num_dec]
            [num_hex] => {$parse_num_lit()}
            [if] => {$parse_if_expr()}
            [loop] => {$parse_loop_expr()}
            [return] => {$parse_return_expr()}
            [break] => {$parse_break_expr()}
            [continue] => {$parse_continue_expr()}
            ['('] => {$parse_tuple_like_expr()}
            ['{'] => {$parse_block_expr()}
        }
}

parse_fn_decl! {
    parse_tuple_like_expr: TupleLikeExpr ::=
        $parse_list_simple2(
            T!['('],
            parse_expr,
            T![,],
            T![')'],
            AttemptRecoveryLevel::Shallow,
        )
}

parse_fn_decl! {
    parse_block_expr: BlockExpr ::=
        $parse_block()
}

fn infix_binary_op(
    p: &mut Parser,
    mut lower: impl FnMut(&mut Parser) -> CompletedMarker,
    mut handle_operator: impl FnMut(&mut Parser) -> bool,
) -> CompletedMarker {
    let mut m = lower(p);
    if p.is_recovering() {
        return m;
    }
    loop {
        let s = p.state();
        p.skip_ws_wc();
        if !handle_operator(p) {
            p.restore_state(s);
            break m;
        }
        if p.is_recovering() {
            return m;
        }
        p.skip_ws_wcn();
        let m0 = p.ev.precede_completed(&m);
        lower(p);
        m = p.ev.end(m0, SyntaxKind::BinaryOpExpr);
        if p.is_recovering() {
            return m;
        }
    }
}

fn infix_binary_op_simple<const N: usize>(
    p: &mut Parser,
    lower: impl FnMut(&mut Parser) -> CompletedMarker,
    operators: [SyntaxKind; N],
) -> CompletedMarker {
    infix_binary_op(p, lower, |p| {
        for op in &operators {
            // we need a special case for && and || because they have higher precedence than & and |
            // otherwise the parser will choke trying to parse the rhs beginning with & or |
            // similary, we need a special case for op= (+=, -=, etc) to avoid choking on
            // the `=` after `stmt::parse_expr_potential_assignment`
            const CHOKE_SETS: [(SyntaxKind, SyntaxKind); 10] = [
                (T![&], T![&&]),
                (T![|], T![||]),
                (T![+], T![+=]),
                (T![-], T![-=]),
                (T![*], T![*=]),
                (T![/], T![/=]),
                (T![%], T![%=]),
                (T![&], T![&=]),
                (T![|], T![|=]),
                (T![^], T![^=]),
            ];
            if CHOKE_SETS.into_iter().any(|it| it.0 == *op && p.at(it.1)) {
                return false;
            }
            if p.at(*op) {
                let m = p.ev.begin();
                p.expect(*op);
                p.ev.end(m, SyntaxKind::BinaryOpExprOp);
                return true;
            }
        }
        false
    })
}

#[parse_fn]
fn parse_precedence_1_expr(p: &mut Parser) -> CompletedMarker {
    let mut m = parse_expr_atom(p);
    if p.is_recovering() {
        return m;
    }

    loop {
        let s = p.state();
        p.skip_ws_wc();
        if p.at_eof() {
            p.restore_state(s);
            break m;
        }
        if p.at(T!['[']) {
            let m0 = p.ev.precede_completed(&m);
            parse_index_expr_index(p);
            m = p.ev.end(m0, SyntaxKind::IndexExpr);
            if p.is_recovering() {
                return m;
            }
        } else if p.at(T!['(']) || p.at(T!['{']) {
            let m0 = p.ev.precede_completed(&m);
            parse_call_expr_args(p);
            m = p.ev.end(m0, SyntaxKind::CallExpr);
            if p.is_recovering() {
                return m;
            }
        } else if p.at(T![.]) {
            let m0 = p.ev.precede_completed(&m);
            p.expect(T![.]);
            p.skip_ws_wcn();
            p.expect(T![ident]);
            let s = p.state();
            p.skip_ws_wc();
            if p.at(T!['(']) {
                parse_call_expr_args(p);
                m = p.ev.end(m0, SyntaxKind::MethodCall);
            } else {
                p.restore_state(s);
                m = p.ev.end(m0, SyntaxKind::FieldAccess);
            }
            if p.is_recovering() {
                return m;
            }
        } else if p.at(T![ident]) {
            let m0 = p.ev.precede_completed(&m);
            parse_custom_infix_expr_infix(p);
            m = p.ev.end(m0, SyntaxKind::CustomInfixExpr);
            if p.is_recovering() {
                return m;
            }
        } else {
            p.restore_state(s);
            return m;
        }
    }
}

parse_fn_decl! {
    parse_index_expr_index: IndexExprIndex ::=
        $!['[']
        $/ws:wcn
        $parse_expr()
        $/ws:wcn
        $![']']
}

parse_fn_decl! {
    parse_call_expr_args: CallExprArgs ::=
        $/match {
            ['('] => {
                $parse_call_expr_args_list()
                $/state:s1
                $/ws:wc
                $/if at['{'] {
                    $parse_call_expr_args_trailing_block()
                }
                $/else {
                    $/restore_state:s1
                }
            }
            ['{'] => {
                $parse_call_expr_args_trailing_block()
            }
        }
}

parse_fn_decl! {
    parse_call_expr_args_list: CallExprArgsList ::=
        $parse_list_simple2(
            T!['('],
            parse_expr,
            T![,],
            T![')'],
            AttemptRecoveryLevel::Shallow,
        )
}

parse_fn_decl! {
    parse_call_expr_args_trailing_block: CallExprArgLambda ::=
        $parse_lambda_expr()
}

#[parse_fn]
fn parse_lambda_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.ev.begin();
    p.expect(T!['{']);
    if p.is_recovering() {
        return p.ev.end(m, SyntaxKind::LambdaExpr);
    }
    p.skip_ws_wcn();

    let s = p.state();
    // attempt to parse list of args if there
    parse_lambda_param_list(p);
    if p.is_recovering() {
        p.recovered();
        p.restore_state(s);
    }

    parse_block_insides(p);
    p.expect(T!['}']);

    p.ev.end(m, SyntaxKind::LambdaExpr)
}

parse_fn_decl! {
    parse_lambda_param_list: LambdaParamList ::=
        $parse_list_rep_simple2(T![,], parse_lambda_param, AttemptRecoveryLevel::Shallow)
        $/ws:wc
        $![->]
}

parse_fn_decl! {
    parse_lambda_param: LambdaParam ::=
        $fun::parse_fn_param_name()
        $/state:s1
        $/ws:wc
        $/if at[:] {
            $![:]
            $/ws:wcn
            $fun::parse_fn_param_ty()
        }
        $/else {
            $/restore_state:s1
        }
}

parse_fn_decl! {
    parse_custom_infix_expr_infix: CustomInfixExprInfix ::=
        $![ident]
        $/ws:wc
        $parse_custom_infix_expr_infix_arg()
}

parse_fn_decl! {
    parse_custom_infix_expr_infix_arg: CustomInfixExprInfixArg ::=
        $parse_expr()
}

#[parse_fn]
fn parse_precedence_2_expr(p: &mut Parser) -> CompletedMarker {
    if p.at(T![+]) || p.at(T![-]) || p.at(T![!]) || p.at(T![*]) {
        let m = p.ev.begin();
        parse_prefix_unary_op(p);
        p.skip_ws_wc();
        parse_precedence_2_expr(p);
        p.ev.end(m, SyntaxKind::UnaryOpExpr)
    } else {
        parse_precedence_1_expr(p)
    }
}

parse_fn_decl! {
    parse_prefix_unary_op: UnaryPrefixOp ::=
        $/match {
            [+]!
            [-]!
            [!]!
            [*]!
        }
}

#[parse_fn]
fn parse_precedence_3_expr(p: &mut Parser) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_2_expr, [T![*], T![/], T![%]])
}

#[parse_fn]
fn parse_precedence_4_expr(p: &mut Parser) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_3_expr, [T![+], T![-]])
}

#[parse_fn]
fn parse_precedence_5_expr(p: &mut Parser) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_4_expr, [T![<=], T![>=], T![<], T![>]])
}

#[parse_fn]
fn parse_precedence_6_expr(p: &mut Parser) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_5_expr, [T![==], T![!=]])
}

#[parse_fn]
fn parse_precedence_7_expr(p: &mut Parser) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_6_expr, [T![&]])
}

#[parse_fn]
fn parse_precedence_8_expr(p: &mut Parser) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_7_expr, [T![^]])
}

#[parse_fn]
fn parse_precedence_9_expr(p: &mut Parser) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_8_expr, [T![|]])
}

#[parse_fn]
fn parse_precedence_10_expr(p: &mut Parser) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_9_expr, [T![&&]])
}

#[parse_fn]
fn parse_precedence_11_expr(p: &mut Parser) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_10_expr, [T![||]])
}

parse_fn_decl! {
    pub parse_expr: Expr ::= $parse_precedence_11_expr()
}

parse_fn_decl! {
    parse_if_expr: IfExpr ::=
        $![if]
        $/ws:wcn
        $parse_if_condition()
        $/ws:wcn
        $parse_then_clause()
        $/state:s1
        $/ws:wcn
        $/if at[else] {
            $parse_else_clause()
        }
        $/else {
            $/restore_state:s1
        }
}

parse_fn_decl! {
    parse_then_clause: IfThenClause ::=
        $parse_expr()
}

parse_fn_decl! {
    parse_else_clause: ElseClause ::=
            $![else]
            $/ws:wcn
            $parse_expr()
}

parse_fn_decl! {
    parse_if_condition: IfCondition ::=
        $!['(']
        $/ws:wcn
        $parse_expr()
        $/ws:wcn
        $![')']
}

parse_fn_decl! {
    parse_loop_expr: LoopExpr ::=
        $![loop]
        $/ws:wcn
        $parse_block()
}

parse_fn_decl! {
    parse_return_expr: ReturnExpr ::=
        $![return]
        $/state:s1
        $/ws:wc
        $/match {
            [ident] [+] [-] [!] [*] [string] [num_bin] [num_oct] [num_dec] [num_hex] [if] [loop] ['{'] => {$parse_expr()}
            _ => {$/restore_state:s1}
        }
}

parse_fn_decl! {
    parse_continue_expr: ContinueExpr ::=
        $![continue]
}

parse_fn_decl! {
    parse_break_expr: BreakExpr ::=
        $![break]
        $/state:s1
        $/ws:wc
        $/match {
            [ident] [+] [-] [!] [*] [string] [num_bin] [num_oct] [num_dec] [num_hex] [if] [loop] ['{'] => {$parse_expr()}
            _ => {$/restore_state:s1}
        }
}
