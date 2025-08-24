use narxia_proc::parse_fn;
use narxia_proc::parse_fn_decl;

use super::AttemptRecoveryLevel;
use super::CompletedMarker;
use super::Parser;
use super::fun;
use super::parse_block;
use super::parse_block_insides;
use super::parse_list_rep_simple2;
use super::parse_list_simple2;
use crate::syntax_kind::SyntaxKind;
use crate::syntax_kind::T;
use crate::token_source::TokenSource;

parse_fn_decl! {
    // parser-test:num-lit-dec
    // let x = 123

    // parser-test:num-lit-dec-with-underscore
    // let x = 123_456_789

    // parser-test:num-lit-bin
    // let x = 0b01_00

    // parser-test:num-lit-oct
    // let x = 01_234_567

    // parser-test-num-lit-hex
    // let x = 0x3_abc_def

    // parser-test:num-lit-all
    // let x = 123_456_789 + 0b01_00 + 01_234_567 + 0x3_abc_def

    // parser-test:num-lit-all-extra-underscores
    // let x = 123_4____56________789 + 0b____01_00 + 01________23___4_567 + 0x3_a______bc_de__f

    parse_num_lit: NumLit ::=
        $/match {
            [num_bin]!
            [num_oct]!
            [num_dec]!
            [num_hex]!
        }
}

#[parse_fn]
fn parse_string_lit<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    let m = p.ev.begin();

    p.expect_1(T![begin_string]);

    p.ts.enter_string();

    loop {
        if p.at(T![end_string]) {
            break;
        }
        if p.at_eof() {
            p.err_unexpected();
            break;
        }

        if p.at(T![string_literal_frag_text_part_t]) {
            parse_fn_decl! {
                parse_string_literal_frag_text_part: StringLiteralFragTextPart ::=
                    $![string_literal_frag_text_part_t]
            }
            parse_string_literal_frag_text_part(p);
        } else if p.at(T![string_literal_frag_escaped_char_t]) {
            parse_fn_decl! {
                parse_string_literal_frag_escaped_char: StringLiteralFragEscapedChar ::=
                    $![string_literal_frag_escaped_char_t]
            }
            parse_string_literal_frag_escaped_char(p);
        } else if p.at(T![string_literal_frag_escape_sequence_t]) {
            parse_fn_decl! {
                parse_string_literal_frag_escape_sequence: StringLiteralFragEscapeSequence ::=
                    $![string_literal_frag_escape_sequence_t]
            }
            parse_string_literal_frag_escape_sequence(p);
        } else if p.at(T![string_literal_frag_display_t]) {
            let m0 = p.ev.begin();
            p.expect(T![string_literal_frag_display_t]);
            p.ts.enter_normal();

            if p.at(T![ident]) {
                let m = p.ev.begin();
                p.expect(T![ident]);
                p.ev.end(m, SyntaxKind::StringLiteralFragIdent);
            } else if p.at(T!['{']) {
                let m = p.ev.begin();
                parse_block_expr(p);
                p.ev.end(m, SyntaxKind::StringLiteralFragExpr);
            } else {
                p.err_unexpected();
                break;
            }

            p.ev.end(m0, SyntaxKind::StringLiteralFragDisplay);

            p.ts.enter_string();
        } else if p.at(T![string_literal_frag_debug_t]) {
            let m0 = p.ev.begin();
            p.expect(T![string_literal_frag_debug_t]);
            p.ts.enter_normal();

            if p.at(T![ident]) {
                let m = p.ev.begin();
                p.expect(T![ident]);
                p.ev.end(m, SyntaxKind::StringLiteralFragIdent);
            } else if p.at(T!['{']) {
                let m = p.ev.begin();
                parse_block_expr(p);
                p.ev.end(m, SyntaxKind::StringLiteralFragExpr);
            } else {
                p.err_unexpected();
                break;
            }

            p.ev.end(m0, SyntaxKind::StringLiteralFragDebug);

            p.ts.enter_string();
        } else {
            p.err_unexpected();
            break;
        }
    }

    p.expect(T![end_string]);

    p.ts.enter_normal();

    p.ev.end(m, SyntaxKind::StringLiteral)
}

parse_fn_decl! {
    parse_expr_atom: ExprAtom ::=
        $/match {
            [ident]!
            [begin_string] => {$parse_string_lit()}
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
            ['{'] => {$parse_block_expr_or_lambda()}
        }
}

#[parse_fn]
fn parse_block_expr_or_lambda<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    let s = p.state();
    p.expect(T!['{']);
    p.skip_ws_wcn();

    // attempt to parse list of args if there
    parse_lambda_param_list(p);
    if p.is_recovering() {
        p.recovered();
        p.restore_state(s);
        parse_block_expr(p)
    } else {
        p.restore_state(s);
        parse_lambda_expr(p)
    }
}

parse_fn_decl! {
    // parser-test:simple-paren-expression
    // let x = (1)

    // parser-test:simple-paren-expression-with-comma
    // let x = (1,)

    // parser-test:paren-multiple-expr
    // let x = (1, 2, 3)

    // parser-test:paren-multiple-expr-with-comma-compact
    // let x = (1,2,3,)

    // parser-test:paren-multiple-expr-with-comma-newline
    // let x = (
    //     1,
    //     2,
    //     3,
    //     4,
    // )


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

fn infix_binary_op<'a, Ts: TokenSource<'a>>(
    p: &mut Parser<'a, Ts>,
    mut lower: impl FnMut(&mut Parser<'a, Ts>) -> CompletedMarker,
    mut handle_operator: impl FnMut(&mut Parser<'a, Ts>) -> bool,
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

fn infix_binary_op_simple<'a, Ts: TokenSource<'a>, const N: usize>(
    p: &mut Parser<'a, Ts>,
    lower: impl FnMut(&mut Parser<'a, Ts>) -> CompletedMarker,
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

// parser-test:precedence-parsing
// let x = a + b * c / d % x - y == e != f >= g.h * i[j[k]] <= l.m[n] / o.p.q[r] > s(t < u.v(w.x.y.z)) & a | b ^ c && d || e

#[parse_fn]
fn parse_precedence_1_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
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
            if p.at(T!['(']) || p.at(T!['{']) {
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
    // parser-test:call-with-simple-lambda
    // f { it + a }

    // parser-test:call-with-simple-lambda-explicit-param
    // f { it -> it + a }

    // parser-test:call-with-simple-lambda-explicit-param-block
    // f { it ->
    //     call();
    //     call2();
    //     it
    // }

    // parser-test:call-with-simple-lambda-explicit-param-explicit-type
    // f { it: i32 -> it + a }

    // parser-test:call-with-simple-lambda-multiple-explicit-param-explicit-type
    // f { it: i32, it2: i32 -> it + it2 }
    // f { it: i32, it2: i32, it3: i32 -> it + it2 + it3 }

    // parser-test:call-with-simple-lambda-after-call-args
    // f() { it + a }

    // parser-test:call-with-simple-lambda-after-call-args-explicit-param
    // f() { it -> it + a }

    // parser-test:call-with-simple-lambda-after-normal-single-arg-explicit-param-block
    // f(a) { it -> it + b }

    // parser-test:call-with-multiple-args
    // f(a, b, c)

    // parser-test:call-with-multi-args-and-lambda
    // f(a, b, c) { it + a }

    // parser-test:call-with-multi-args-and-explicit-lambda-param
    // f(a, b, c, { it -> it + a })

    // parser-test:method-call-with-simple-lambda
    // a.f { it + a }

    // parser-test:method-call-with-simple-lambda-after-empty-call-args
    // a.f() { it + a }

    // parser-test:method-call-with-simple-lambda-after-normal-single-arg
    // a.f(a) { it + b }


    parse_call_expr_args: CallExprArgs ::=
        $/match {
            ['('] => {
                $parse_call_expr_args_list()
                $/state:s1
                $/ws:wc // parser-test:call-with-simple-lambda-on-next-line
                        // f()
                        // { it + a }
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
fn parse_lambda_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
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
        $/ws:wcn    // parser-test:lambda-params-arrow
                    // { a,
                    //   b,
                    //   c
                    //   -> a + b + c }
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
fn parse_precedence_2_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
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
fn parse_precedence_3_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_2_expr, [T![*], T![/], T![%]])
}

#[parse_fn]
fn parse_precedence_4_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_3_expr, [T![+], T![-]])
}

#[parse_fn]
fn parse_precedence_5_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_4_expr, [T![<=], T![>=], T![<], T![>]])
}

#[parse_fn]
fn parse_precedence_6_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_5_expr, [T![==], T![!=]])
}

#[parse_fn]
fn parse_precedence_7_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_6_expr, [T![&]])
}

#[parse_fn]
fn parse_precedence_8_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_7_expr, [T![^]])
}

#[parse_fn]
fn parse_precedence_9_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_8_expr, [T![|]])
}

#[parse_fn]
fn parse_precedence_10_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_9_expr, [T![&&]])
}

#[parse_fn]
fn parse_precedence_11_expr<'a, Ts: TokenSource<'a>>(p: &mut Parser<'a, Ts>) -> CompletedMarker {
    infix_binary_op_simple(p, parse_precedence_10_expr, [T![||]])
}

parse_fn_decl! {
    pub parse_expr: Expr ::= $parse_precedence_11_expr()
}

parse_fn_decl! {
    // parser-test:if-expr
    // if (a == b) a else b

    // parser-test:if-expr-in-block
    // {
    //     if (a == b) a else b
    // }

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
    // parser-test:loop-expr
    // loop {}

    // parser-test:loop-expr-with-newline
    // loop {
    // }

    // parser-test:loop-expr-in-block
    // {
    //     loop {}
    // }

    parse_loop_expr: LoopExpr ::=
        $![loop]
        $/ws:wcn
        $parse_block()
}

parse_fn_decl! {
    // parser-test:return-expr-no-value
    // return

    // parser-test:return-expr-with-value
    // return 1

    parse_return_expr: ReturnExpr ::=
        $![return]
        $/state:s1
        $/ws:wc // parser-test:return-expr-with-value-on-newline
                // return
                // 1
        $/match {
            [ident] [+] [-] [!] [*] [begin_string] [num_bin] [num_oct] [num_dec] [num_hex] [if] [loop] ['{'] => {$parse_expr()}
            _ => {$/restore_state:s1}
        }
}

parse_fn_decl! {
    // parser-test:continue-expr
    // continue
    parse_continue_expr: ContinueExpr ::=
        $![continue]
}

parse_fn_decl! {
    // parser-test:break-expr-no-value
    // break

    // parser-test:break-expr-with-value
    // break 1

    // parser-test:break-expr-with-value-on-newline
    // break
    // 1

    parse_break_expr: BreakExpr ::=
        $![break]
        $/state:s1
        $/ws:wc
        $/match {
            [ident] [+] [-] [!] [*] [begin_string] [num_bin] [num_oct] [num_dec] [num_hex] [if] [loop] ['{'] => {$parse_expr()}
            _ => {$/restore_state:s1}
        }
}
