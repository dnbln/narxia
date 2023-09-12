use narxia_syn_helpers::{parse_fn, parse_fn_decl};

use crate::parser::parse_event_handler::{
    CompletedMarker, GreenTreeBuilder, ParseError, ParseErrorInfo, ParseEventHandler, TreeBuilder,
};
use crate::parser::parse_stack::{ParseStack, ParseStackGuard};
use crate::syntax_kind::{SyntaxKind, T};
use crate::syntree::GreenTree;
use crate::token_source::TokenSource;

mod parse_event_handler;
mod parse_stack;

pub struct Parser<'a> {
    ts: &'a mut dyn TokenSource<'a>,
    ev: ParseEventHandler<'a>,
    pstk: ParseStack,
    recovering: Option<ParserRecoveringInfo>,
}

impl<'a> Parser<'a> {}

struct ParserRecoveringInfo {
    choked_syntax_kind: SyntaxKind,
}

impl<'a> Parser<'a> {
    pub fn new(ts: &'a mut dyn TokenSource<'a>) -> Self {
        Self {
            ts,
            ev: ParseEventHandler::new(),
            pstk: ParseStack::new(),
            recovering: None,
        }
    }

    fn is_recovering(&self) -> bool {
        self.recovering.is_some()
    }

    fn recovered(&mut self) {
        self.recovering = None;
    }

    fn skip_ws(&mut self) {
        while let Some(tok) = self.ts.lookahead(0) {
            if tok.kind() != SyntaxKind::WHITESPACE {
                break;
            }
            let text = self.ts.get_token_text(&tok);
            self.ev.token(SyntaxKind::WHITESPACE, text);

            self.ts.advance();
        }
    }

    fn err(&mut self, info: ParseErrorInfo) {
        self.recovering = Some(ParserRecoveringInfo {
            choked_syntax_kind: self.ts.lookahead(0).map(|t| t.kind()).unwrap_or(T![eof]),
        });
        self.ev
            .error(ParseError::new(info, self.ts.current_token_span()));
    }

    fn err_unexpected(&mut self) {
        self.dbg();
        let k = self.ts.lookahead(0).map(|tok| tok.kind()).unwrap();
        self.err(ParseErrorInfo::UnexpectedToken { got: k });
    }

    #[track_caller]
    fn guard(&mut self, name: &'static str, can_recover: &'static [SyntaxKind]) -> ParseStackGuard {
        self.pstk.push(name, can_recover, self.ts.current_pos())
    }

    fn expect(&mut self, k: SyntaxKind) -> bool {
        let size = Self::t_sizeof(k);
        if self.at(k) {
            let token = self.ts.compose_token(k, size).unwrap();
            self.ev.token(k, self.ts.get_token_text(&token));
            self.ts.advance_n(size);
            true
        } else {
            self.err(ParseErrorInfo::ExpectedKind(k));
            false
        }
    }

    fn t_sizeof(k: SyntaxKind) -> usize {
        match k {
            T![==] | T![!=] | T![<=] | T![>=] | T![->] | T![=>] | T![&&] | T![||] => 2,
            _ => 1,
        }
    }

    fn at_eof(&mut self) -> bool {
        self.ts.lookahead(0).is_none()
    }

    fn at(&mut self, k: SyntaxKind) -> bool {
        match k {
            T![==] => self.at2(T![=], T![=]),
            T![!=] => self.at2(T![!], T![=]),
            T![<=] => self.at2(T![<], T![=]),
            T![>=] => self.at2(T![>], T![=]),
            T![->] => self.at2(T![-], T![>]),
            T![=>] => self.at2(T![=], T![>]),
            T![&&] => self.at2(T![&], T![&]),
            T![||] => self.at2(T![|], T![|]),
            _ => self
                .ts
                .lookahead(0)
                .map(|tok| tok.kind() == k)
                .unwrap_or(false),
        }
    }

    fn at2(&mut self, k1: SyntaxKind, k2: SyntaxKind) -> bool {
        self.ts
            .lookahead(0)
            .map(|tok| tok.kind() == k1)
            .unwrap_or(false)
            && self
                .ts
                .lookahead(1)
                .map(|tok| tok.kind() == k2)
                .unwrap_or(false)
    }

    fn bump_any(&mut self) -> bool {
        let Some(token) = self.ts.lookahead(0) else {
            return false;
        };
        self.expect(token.kind());
        true
    }

    fn bump_until(&mut self, k: SyntaxKind) -> bool {
        while let Some(token) = self.ts.lookahead(0) {
            if token.kind() == k {
                return true;
            }
            self.bump_any();
        }
        false
    }

    pub fn parse(&mut self) {
        let _guard = self.guard("parse", &[]);
        let m = self.ev.begin();
        while let Some(token) = self.ts.lookahead(0) {
            match token.kind() {
                T![fn] => {
                    parse_fn_def(self);
                }
                T![whitespace] => {
                    self.skip_ws();
                }
                _ => {}
            }
        }
        self.ev.end(m, SyntaxKind::Root);
        self.dbg();
    }

    pub fn finish(self, tb: &mut dyn TreeBuilder<'a>) {
        self.ev.finish(tb);
    }

    pub fn finish_to_tree(self) -> (GreenTree, Vec<ParseError>) {
        let mut tb = GreenTreeBuilder::new();
        self.finish(&mut tb);
        tb.finish()
    }

    #[track_caller]
    fn dbg(&mut self) {
        eprintln!(
            "dbg[in {}, at {}]:",
            self.pstk.top_item().unwrap().name,
            std::panic::Location::caller()
        );
        eprintln!("  Tokens:");
        let mut i = 0;
        while let Some(tok) = self.ts.lookahead(i) {
            eprintln!("    +{i}  {} {:?}", tok, self.ts.get_token_text(&tok));
            i += 1;
            if i > 3 {
                break;
            }
        }

        if i == 0 {
            eprintln!("    +0  <EOF>");
        }

        eprintln!("  Current stack:");
        self.pstk.present(4, |presenter| eprint!("{presenter}"));

        eprintln!("  Recent events:");
        eprint!("{}", self.ev.present(4, 20));
    }
}

parse_fn_decl! {
    parse_fn_def: FnDef ::=
        $/dbg
        $parse_fn_head()
        $/ws:wcn
        $/eof:early_ret+err(ExpectedKind(T!['{']))
        $parse_block()
}

parse_fn_decl! {
    parse_fn_head: FnHead ::=
        $![fn]
        $/ws:wcn
        $parse_fn_name()
        $/ws:wcn
        $parse_fn_param_list()
}

parse_fn_decl! {
    parse_fn_name: FnName ::=
        $![ident]
}

trait NotAttemptingRecovery {
    fn not_attempting_recovery() -> Self;
}

impl NotAttemptingRecovery for () {
    fn not_attempting_recovery() -> Self {}
}

enum AttemptRecoveryLevel {
    None,
    Shallow,
    Deep,
}

fn parse_list<E: NotAttemptingRecovery>(
    p: &mut Parser,
    start: SyntaxKind,
    mut parse_item: impl FnMut(&mut Parser) -> Result<(), E>,
    mut handle_ws: impl FnMut(&mut Parser) -> Result<(), E>,
    mut is_at_sep: impl FnMut(&mut Parser) -> bool,
    mut handle_sep: impl FnMut(&mut Parser) -> Result<bool, E>,
    end: SyntaxKind,
    attempt_recovery: AttemptRecoveryLevel,
) -> Result<(), E> {
    p.expect(start);
    handle_ws(p)?;
    'outer: {
        if !p.at(end) {
            parse_item(p)?;
            'inner: {
                if p.is_recovering() {
                    match attempt_recovery {
                        AttemptRecoveryLevel::None => return Err(E::not_attempting_recovery()),
                        AttemptRecoveryLevel::Shallow => {
                            if p.at(end) {
                                p.recovered();
                                break 'outer;
                            } else if is_at_sep(p) {
                                p.recovered();
                                break 'inner;
                            }
                        }
                        AttemptRecoveryLevel::Deep => {
                            if p.bump_until(end) {
                                p.recovered();
                            } else if is_at_sep(p) {
                                p.recovered();
                                break 'inner;
                            }
                            break 'outer;
                        }
                    }
                }
                handle_ws(p)?;
            }

            loop {
                if !handle_sep(p)? {
                    break;
                }
                handle_ws(p)?;
                if p.at(end) {
                    break;
                }
                parse_item(p)?;
                if p.is_recovering() {
                    match attempt_recovery {
                        AttemptRecoveryLevel::None => return Err(E::not_attempting_recovery()),
                        AttemptRecoveryLevel::Shallow => {
                            if p.at(end) {
                                p.recovered();
                                break 'outer;
                            } else if is_at_sep(p) {
                                p.recovered();
                                continue;
                            }
                        }
                        AttemptRecoveryLevel::Deep => {
                            if is_at_sep(p) {
                                p.recovered();
                                continue;
                            }
                            if p.bump_until(end) {
                                p.recovered();
                            }
                            break 'outer;
                        }
                    }
                }
                handle_ws(p)?;
            }
        }
    }
    p.expect(end);
    Ok(())
}

fn parse_list_simple<T>(
    p: &mut Parser,
    start: SyntaxKind,
    mut parse_item: impl FnMut(&mut Parser) -> T,
    sep: SyntaxKind,
    end: SyntaxKind,
    attempt_recovery: AttemptRecoveryLevel,
) -> Result<(), ()> {
    parse_list(
        p,
        start,
        |p| {
            parse_item(p);
            Ok(())
        },
        |p| {
            p.skip_ws();
            if p.at_eof() {
                p.err(ParseErrorInfo::ExpectedKind(end));
                return Err(());
            }
            Ok(())
        },
        |p| p.at(T![,]),
        |p| {
            p.skip_ws();
            match p.at(sep) {
                true => {
                    p.expect(sep);
                    Ok(true)
                }
                false => Ok(false),
            }
        },
        end,
        attempt_recovery,
    )
}

fn parse_list_simple2<T>(
    p: &mut Parser,
    start: SyntaxKind,
    parse_item: impl FnMut(&mut Parser) -> T,
    sep: SyntaxKind,
    end: SyntaxKind,
    attempt_recovery: AttemptRecoveryLevel,
) {
    let _r = parse_list_simple(p, start, parse_item, sep, end, attempt_recovery);
}

fn parse_list_simple3<T>(
    p: &mut Parser,
    kind: SyntaxKind,
    start: SyntaxKind,
    parse_item: impl FnMut(&mut Parser) -> T,
    sep: SyntaxKind,
    end: SyntaxKind,
    attempt_recovery: AttemptRecoveryLevel,
) -> CompletedMarker {
    let m = p.ev.begin();
    let _r = parse_list_simple(p, start, parse_item, sep, end, attempt_recovery);
    p.ev.end(m, kind)
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
        $parse_ty_ref()
}

parse_fn_decl! {
    parse_fn_param_name: FnParamName ::=
        $![ident]
}

parse_fn_decl! {
    parse_ty_ref: TyRef ::=
        $![ident]
}

#[parse_fn]
fn parse_block(p: &mut Parser) -> CompletedMarker {
    let m = p.ev.begin();
    p.expect(T!['{']);
    p.skip_ws();
    if !p.at(T!['}']) {
        loop {
            p.skip_ws();
            while p.at(T![;]) || p.at(T![newline]) {
                if p.at(T![;]) {
                    p.expect(T![;]);
                } else if p.at(T![newline]) {
                    p.expect(T![newline]);
                }
                p.skip_ws();
            }

            if p.at(T!['}']) {
                break;
            }

            parse_stmt(p);
        }
    }
    p.expect(T!['}']);
    p.ev.end(m, SyntaxKind::Block)
}

#[parse_fn]
fn parse_stmt(p: &mut Parser) -> CompletedMarker {
    let m = p.ev.begin();
    if p.at(T![ident]) || p.at(T![string]) {
        parse_expr_potential_assignment(p);
    // } else if p.at(T![let]) {
    //     parse_let_stmt(p);
    // } else if p.at(T![if]) {
    //     parse_if_stmt(p);
    // } else if p.at(T![while]) {
    //     parse_while_stmt(p);
    // } else if p.at(T![for]) {
    //     parse_for_stmt(p);
    // } else if p.at(T![return]) {
    //     parse_return_stmt(p);
    // } else if p.at(T![continue]) {
    //     parse_continue_stmt(p);
    // } else if p.at(T![break]) {
    //     parse_break_stmt(p);
    } else {
        p.err_unexpected();
    }
    p.ev.end(m, SyntaxKind::Stmt)
}

fn parse_expr_atom(p: &mut Parser) -> CompletedMarker {
    let m = p.ev.begin();
    if p.at(T![ident]) {
        p.expect(T![ident]);
    } else if p.at(T![string]) {
        p.expect(T![string]);
    } else {
        p.err_unexpected();
    }
    p.ev.end(m, SyntaxKind::ExprAtom)
}

fn parse_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.ev.begin();
    parse_expr_atom(p);
    p.ev.end(m, SyntaxKind::Expr)
}

#[parse_fn]
fn parse_expr_potential_assignment(p: &mut Parser) {
    parse_expr(p);
}