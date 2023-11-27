use narxia_syn_helpers::{parse_fn, parse_fn_decl};

use super::{expr, parse_block, parse_pat, parse_ty_ref, CompletedMarker, Parser, WsSkipConfig};
use crate::syntax_kind::{SyntaxKind, T};

parse_fn_decl! {
    pub parse_stmt: Stmt ::=
        $/match {
            [ident] [+] [-] [!] [*] [string] [num_bin] [num_oct] [num_dec] [num_hex] [if] [loop] [return] [continue] [break] ['('] ['{'] => {$parse_expr_potential_assignment()}
            [let] => {$parse_let_stmt()}
            [while] => {$parse_while_stmt()}
            [for] => {$parse_for_stmt()}
        }
}

#[parse_fn]
fn parse_expr_potential_assignment(p: &mut Parser) {
    let expr = expr::parse_expr(p);
    let s = p.state();
    let lhs = p.ev.precede_completed(&expr);
    let lhs = p.ev.end(lhs, SyntaxKind::AssignmentLhs);
    p.skip_ws_wc();
    if p.at(T![=])
        || p.at(T![+=])
        || p.at(T![-=])
        || p.at(T![*=])
        || p.at(T![/=])
        || p.at(T![%=])
        || p.at(T![&=])
        || p.at(T![|=])
        || p.at(T![^=])
    {
        let m = p.ev.precede_completed(&lhs);
        parse_assignment_op_rhs_expr(p);
        p.ev.end(m, SyntaxKind::AssignmentStmt);
    } else {
        p.restore_state(s);
    }
}

parse_fn_decl! {
    parse_assignment_op_rhs_expr: AssignmentOpAndRhsExpr ::=
        $parse_assignment_op()
        $/ws:wcn
        $expr::parse_expr()
}

parse_fn_decl! {
    parse_assignment_op: AssignmentOp ::=
        $/match {[=] [+=] [-=] [*=] [/=] [%=] [&=] [|=] [^=]!}
}

parse_fn_decl! {
    parse_for_stmt: ForStmt ::=
        $![for]
        $/ws:wcn
        $!['(']
        $/ws:wcn
        $parse_for_pat()
        $/ws:wcn
        $![in]
        $/ws:wcn
        $parse_for_in_expr()
        $/ws:wcn
        $![')']
        $/ws:wcn
        $parse_block()
}

parse_fn_decl! {
    parse_for_pat: ForPat ::= $parse_pat()
}

parse_fn_decl! {
    parse_for_in_expr: ForInExpr ::= $expr::parse_expr()
}

parse_fn_decl! {
    parse_while_stmt: WhileStmt ::=
        $![while]
        $/ws:wcn
        $parse_while_condition()
        $/ws:wcn
        $parse_block()
}

parse_fn_decl! {
    parse_while_condition: WhileCondition ::=
        $!['(']
        $/ws:wcn
        $expr::parse_expr()
        $/ws:wcn
        $![')']
}

parse_fn_decl! {
    parse_let_stmt: LetStmt ::=
        $![let]
        $/ws:wcn
        $/match {
            [mut] => {$![mut] $/ws:wcn}
            _ => {}
        }
        $parse_pat()
        $/state:s1
        $/ws:wcn
        $/if at[:] {
            $![:]
            $/ws:wcn
            $parse_ty_ref()
            $/state:s2
            $/ws:wcn

            $/if at[=] {
                $![=]
                $/ws:wcn
                $expr::parse_expr()
            }
            $/else {
                $/restore_state:s2
            }
        }
        $/else if at[=] {
            $![=]
            $/ws:wcn
            $expr::parse_expr()
        }
        $/else {
            $/restore_state:s1
        }
}
