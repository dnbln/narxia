//! Parser for the Narxia language.
//!
//! The parser is implemented as a recursive descent parser with backtracking.
//!
//! Backtracking is implemented by saving the state of the parser before attempting
//! to parse a construct that may fail. If the construct fails, the parser is restored
//! to the saved state and the parser continues parsing.
//!
//! With `debug_assertions` enabled, the parser will attempt to check that the backtracking
//! mechanism is used correctly, in the right states of the [`ParseEventHandler`].
//!
//! Namely, the parser ensures that all the events that have started since the state
//! was saved are either completed or rolled back before the state is restored.

// parser-test:hello-world
// println("Hello, world!")

use std::fmt;

use narxia_proc::{parse_fn, parse_fn_decl};
use owo_colors::{OwoColorize, Style};

use self::parse_event_handler::GreenTreeBuilderSD;
use crate::parse_error::{ParseError, ParseErrorInfo};
use crate::parser::parse_event_handler::{
    CompletedMarker, ParseEventHandler, ParseEventHandlerPos, TreeBuilder,
};
use crate::parser::parse_stack::{ParseStack, ParseStackGuard};
use crate::syntax_kind::{SyntaxKind, T};
use crate::syntree::GreenTree;
use crate::token_source::buffered_ts::BufferedTokenSource;
use crate::token_source::{DynTsContainer, TokenSource};

mod parse_event_handler;
mod parse_stack;

mod expr;
mod fun;
mod stmt;

struct ParserState {
    ts_pos: usize,
    ev_pos: ParseEventHandlerPos,
}

#[derive(Copy, Clone)]
pub(crate) struct ParserDbgStyling {
    pub(crate) top_name: fn() -> String,
    pub(crate) region_name: Style,
    pub(crate) top_stack_name: Style,

    pub(crate) token_offset: Style,
    pub(crate) token_kind: Style,
    pub(crate) token_span: Style,
    pub(crate) token_text: Style,

    pub(crate) stack_offset: Style,
    pub(crate) stack_fn_name: Style,
    pub(crate) token_stream_position: Style,

    pub(crate) recent_event_absolute_position: Style,
    pub(crate) recent_event_relative_position: Style,
    pub(crate) recent_event_kind: Style,
}

impl Default for ParserDbgStyling {
    fn default() -> Self {
        Self {
            top_name: || format!("{}::{}", "Parser".blue().bold(), "dbg".blue().bold()),
            region_name: Style::new().bold().red(),
            top_stack_name: Style::new().bright_red().bold(),

            token_offset: Style::new().bright_blue(),
            token_kind: Style::new().green().bold(),
            token_span: Style::new().bright_purple().bold(),
            token_text: Style::new().bright_blue(),

            stack_offset: Style::new().bright_blue(),
            stack_fn_name: Style::new().bright_green().bold(),
            token_stream_position: Style::new().bright_purple().bold(),

            recent_event_absolute_position: Style::new().bright_purple().bold(),
            recent_event_relative_position: Style::new().bright_blue().bold(),
            recent_event_kind: Style::new().bright_cyan().bold(),
        }
    }
}

pub struct Parser<'a> {
    ts: BufferedTokenSource<'a, DynTsContainer<'a>>,
    ev: ParseEventHandler,
    pstk: ParseStack,
    recovering: Option<ParserRecoveringInfo>,
}

struct ParserRecoveringInfo {
    choked_syntax_kind: SyntaxKind,
}

bitflags::bitflags! {
    #[derive(PartialEq, Eq)]
    struct WsSkipConfig: u8 {
        const W     = 0b001;
        const C     = 0b010;
        const N     = 0b100;
        const WC    = 0b011;
        const WN    = 0b101;
        const CN    = 0b110;
        const WCN   = 0b111;
    }
}

impl WsSkipConfig {
    const fn from_bits_truncated_macro_impl(b: u8) -> WsSkipConfig {
        Self::from_bits_truncate(b)
    }
}

impl<'a> Parser<'a> {
    pub fn new(ts: &'a mut dyn TokenSource<'a>) -> Self {
        Self {
            ts: BufferedTokenSource::new(DynTsContainer(ts)),
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

    #[inline(always)]
    fn skip_ws_wcn(&mut self) {
        // while let Some(token) = self.ts.lookahead0() {
        //     let kind = token.kind();
        //     if ![SyntaxKind::WHITESPACE, SyntaxKind::COMMENT, SyntaxKind::NEWLINE].contains(&kind) {
        //         break;
        //     }
        //     self.ev.token(kind, self.ts.get_token_text(&token));
        //     self.ts.advance();
        // }
        self.ts.skip_whitespace_wcn(|token| {
            self.ev.token(token.kind(), token.span());
        });
        debug_assert!(
            !self.at(SyntaxKind::WHITESPACE)
                && !self.at(SyntaxKind::COMMENT)
                && !self.at(SyntaxKind::NEWLINE)
        );
    }

    #[inline(always)]
    fn skip_ws_wc(&mut self) {
        self.ts.skip_whitespace_wc(|token| {
            self.ev.token(token.kind(), token.span());
        });
        debug_assert!(!self.at(SyntaxKind::WHITESPACE) && !self.at(SyntaxKind::COMMENT));
    }

    // #[inline(always)]
    // fn skip_ws(&mut self, ws_skip_config: WsSkipConfig) {
    //     match ws_skip_config {
    //         WsSkipConfig::WCN => self.skip_ws_wcn(),
    //         WsSkipConfig::WC => self.skip_ws_wc(),
    //         _ => unreachable!(),
    //     }
    // }

    fn err(&mut self, info: ParseErrorInfo) {
        let tkind = self.ts.lookahead0_kind().unwrap_or(T![eof]);
        self.recovering = Some(ParserRecoveringInfo {
            choked_syntax_kind: tkind,
        });
        let location = match info {
            ParseErrorInfo::ExpectedKind(_, at) => Some(at),
            ParseErrorInfo::UnexpectedToken { got: _, at } => Some(at),
        };
        self.ev.error(ParseError::new(
            info,
            self.ts.current_token_span(),
            tkind,
            location,
        ));
    }

    #[track_caller]
    fn err_unexpected(&mut self) {
        let location = std::panic::Location::caller();
        self.dbg();
        let k = self.ts.lookahead0_kind().unwrap();
        self.err(ParseErrorInfo::UnexpectedToken {
            got: k,
            at: location,
        });
    }

    #[inline(always)]
    #[track_caller]
    fn guard(&mut self, name: &'static str, can_recover: &'static [SyntaxKind]) -> ParseStackGuard {
        let g = self.pstk.push(name, can_recover, self.ts.current_pos());
        g
    }

    #[inline(always)]
    #[track_caller]
    pub fn expect(&mut self, k: SyntaxKind) {
        match k {
            T![==] => self.expect_2(T![=], T![=], T![==]),
            T![!=] => self.expect_2(T![!], T![=], T![!=]),
            T![<=] => self.expect_2(T![<], T![=], T![<=]),
            T![>=] => self.expect_2(T![>], T![=], T![>=]),
            T![->] => self.expect_2(T![-], T![>], T![->]),
            T![=>] => self.expect_2(T![=], T![>], T![=>]),
            T![&&] => self.expect_2(T![&], T![&], T![&&]),
            T![||] => self.expect_2(T![|], T![|], T![||]),
            T![+=] => self.expect_2(T![+], T![=], T![+=]),
            T![-=] => self.expect_2(T![-], T![=], T![-=]),
            T![*=] => self.expect_2(T![*], T![=], T![*=]),
            T![/=] => self.expect_2(T![/], T![=], T![/=]),
            T![%=] => self.expect_2(T![%], T![=], T![%=]),
            T![&=] => self.expect_2(T![&], T![=], T![&=]),
            T![|=] => self.expect_2(T![|], T![=], T![|=]),
            T![^=] => self.expect_2(T![^], T![=], T![^=]),
            T![::] => self.expect_2(T![:], T![:], T![::]),
            k => self.expect_1(k),
        }
    }

    #[inline(always)]
    #[track_caller]
    fn expect_1(&mut self, k: SyntaxKind) {
        if !self.ts.expect_1(k, |token| {
            self.ev.token(token.kind(), token.span());
        }) {
            self.err(ParseErrorInfo::ExpectedKind(
                k,
                std::panic::Location::caller(),
            ));
        }
    }

    #[inline(always)]
    #[track_caller]
    fn expect_2(&mut self, k1: SyntaxKind, k2: SyntaxKind, complete: SyntaxKind) {
        if !self.ts.expect_2(k1, k2, complete, |_t1, _t2, tcomplete| {
            self.ev.token(complete, tcomplete.span());
        }) {
            self.err(ParseErrorInfo::ExpectedKind(
                complete,
                std::panic::Location::caller(),
            ));
        }
    }

    fn at_eof(&mut self) -> bool {
        self.ts.at_eof()
    }

    #[inline(always)]
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
            T![+=] => self.at2(T![+], T![=]),
            T![-=] => self.at2(T![-], T![=]),
            T![*=] => self.at2(T![*], T![=]),
            T![/=] => self.at2(T![/], T![=]),
            T![%=] => self.at2(T![%], T![=]),
            T![&=] => self.at2(T![&], T![=]),
            T![|=] => self.at2(T![|], T![=]),
            T![^=] => self.at2(T![^], T![=]),
            T![::] => self.at2(T![:], T![:]),
            k => self.ts.at_1(k),
        }
    }

    #[inline(always)]
    fn at2(&mut self, k1: SyntaxKind, k2: SyntaxKind) -> bool {
        self.ts.at_2(k1, k2)
    }

    fn bump_until(&mut self, k: SyntaxKind) -> bool {
        self.ts.bump_until(k, |token| {
            self.ev.token(token.kind(), token.span());
        })
    }

    #[inline(always)]
    fn state(&mut self) -> ParserState {
        let ts_pos = self.ts.current_pos();
        let ev_pos = self.ev.state();
        ParserState { ts_pos, ev_pos }
    }

    #[inline(always)]
    fn restore_state(&mut self, state: ParserState) {
        self.ts.restore_pos(state.ts_pos);
        self.ev.rollback(state.ev_pos);
    }

    pub fn parse(&mut self) {
        let _guard = self.guard("parse", &[]);
        let m = self.ev.begin();
        while let Some(token_kind) = self.ts.lookahead0_kind() {
            match token_kind {
                T![#]
                | T![module]
                | T![fn]
                | T![const]
                | T![let]
                | T![ident]
                | T![+]
                | T![-]
                | T![!]
                | T![*]
                | T![begin_string]
                | T![num_bin]
                | T![num_oct]
                | T![num_dec]
                | T![num_hex]
                | T!['(']
                | T!['{']
                | T![loop]
                | T![while]
                | T![for]
                | T![return]
                | T![break]
                | T![continue]
                | T![if]
                | T![use] => {
                    parse_item(self);
                }
                T![whitespace] | T![newline] | T![comment] => {
                    self.skip_ws_wcn();
                }
                T![;] => {
                    self.expect(T![;]);
                }
                t => {
                    narxia_log::e!("Parser is stuck on a {t:?} token");
                    self.err_unexpected();
                    break;
                }
            }
        }
        self.ev.end(m, SyntaxKind::Root);
        // self.dbg();
    }

    pub fn finish(self, tb: &mut dyn TreeBuilder<'a>) {
        self.ev.finish(tb);
    }

    pub fn finish_to_tree(self) -> (GreenTree, Vec<ParseError>) {
        let Self { ts, ev, .. } = self;
        let mut tb = GreenTreeBuilderSD::new(move |span| ts.get_span_text(span));
        ev.finish(&mut tb);
        tb.finish()
    }

    #[track_caller]
    fn __private_dbg_log<W>(&mut self, w: &mut W, styling: ParserDbgStyling) -> fmt::Result
    where
        W: fmt::Write,
    {
        let region = |w: &mut W, name: &str| writeln!(w, "  {}:", name.style(styling.region_name));

        writeln!(
            w,
            "{} in {}",
            (styling.top_name)(),
            self.pstk
                .top_item()
                .unwrap()
                .name
                .style(styling.top_stack_name),
        )?;
        writeln!(w, "    at {}", std::panic::Location::caller(),)?;

        region(w, "Tokens")?;
        let mut i = 0;
        while let Some(tok) = self.ts.lookahead(i) {
            writeln!(
                w,
                "    {}  {} {}",
                format_args!("+{i}").style(styling.token_offset),
                tok.dbg_fmt_colorized(styling),
                format_args!("{:?}", self.ts.get_token_text(&tok)).style(styling.token_text),
            )?;
            i += 1;
            if i > 3 {
                break;
            }
        }

        if i == 0 {
            writeln!(
                w,
                "    {}  {}",
                "+0".style(styling.token_offset),
                "<EOF>".style(styling.token_kind),
            )?;
        }

        region(w, "Current stack")?;

        self.pstk
            .present(4, styling, |presenter| write!(w, "{presenter}"))?;

        region(w, "Recent events")?;
        write!(w, "{}", self.ev.present(4, 100, true, styling))?;

        Ok(())
    }

    #[track_caller]
    fn dbg(&mut self) {
        #[cfg(not(debug_assertions))]
        {
            return;
        }
        let styling = ParserDbgStyling::default();
        let _span = narxia_log::span!(narxia_log::Level::DEBUG, "Parser::dbg").entered();
        let mut log = String::new();
        self.__private_dbg_log(&mut log, styling).unwrap();
        narxia_log::debug!("{log}");
    }
}

parse_fn_decl! {parse_attr_meta_item_name: AttrMetaItemName ::= $![ident]}

parse_fn_decl! {parse_attr_meta_item_eq: AttrMetaItemEq ::= $![=] $/ws:wcn $expr::parse_expr()}

parse_fn_decl! {parse_attr_meta_item_call: AttrMetaItemCall ::= $parse_list_simple2(T!['('], parse_attr_meta_item, T![,], T![')'], AttemptRecoveryLevel::Shallow)}

parse_fn_decl! {
    parse_attr_meta_item: AttrMetaItem ::=
        $parse_attr_meta_item_name()
        $/state:s1
        $/ws:wcn
        $/match {
            [=] => {
                $parse_attr_meta_item_eq()
            }
            ['('] => {
                $parse_attr_meta_item_call()
            }
            _ => {
                $/restore_state:s1
            }
        }
}

parse_fn_decl! {
    parse_attr_name: AttrName ::= $![ident]
}

parse_fn_decl! {
    parse_attr_meta: AttrMeta ::=
        $parse_list_simple2(
            T!['['],
            parse_attr_meta_item,
            T![,],
            T![']'],
            AttemptRecoveryLevel::Shallow,
        )
}

parse_fn_decl! {
    // parser-test:attr
    // #attr
    // fn f() {}

    // parser-test:attr2
    // #attr2
    // fn f() {}

    // parser-test:attr-with-nameonly-meta
    // #attr[meta]
    // fn f() {}

    // parser-test:attr-with-meta-eq
    // #attr[meta = 1]
    // fn f() {}

    // parser-test:attr-with-multiple-meta-eq
    // #attr[meta = 1, meta2 = 2]
    // fn f() {}

    // parser-test:attr-with-multiple-meta-eq2
    // #attr[meta = 1, meta2 = 2, meta3 = 3]
    // fn f() {}

    // parser-test:attr-with-multiple-meta-eq-expressions
    // #attr[meta = 1, meta2 = {let x = 1; x}, meta3 = "x"]
    // fn f() {}

    // parser-test:attr-with-meta-call
    // #attr[meta(a = 1, b = 2, c = 3)]
    // fn f() {}

    // parser-test:attr-with-meta-call-with-eq
    // #attr[meta(v = 1, v2 = 2)]
    // fn f() {}

    // parser-test:attr-with-multiple-meta-call-expressions
    // #attr[
    //     meta(a=1, b=2, c="3", d={4}, e, f=6, g=7, h=8, i="9", j=10+2),
    //     meta2(a=1-3, b=0*2, c=3, d=4, e=5, f=6, g=7, h=8, i=9, j=10)
    // ]
    // fn f() {}

    // parser-test:attr-with-multiple-meta-eq-and-call-expressions
    // #attr[meta(v, v2 = 1, v3 = {let x = 1; x}, v4(v5 = 1, v6 = 2, v7 = {2}, v8(v9)))]
    // fn f() {}

    parse_attr: Attr ::=
        $![#]
        $parse_attr_name()
        $/state:s1
        $/ws:wc
        $/if at['['] {
            $parse_attr_meta()
        }
        $/else {
            $/restore_state:s1
        }
}

#[parse_fn]
fn parse_attr_list(p: &mut Parser) -> CompletedMarker {
    let m = p.ev.begin();
    while p.at(T![#]) {
        parse_attr(p);
        p.skip_ws_wcn();
        if p.is_recovering() {
            return p.ev.end(m, SyntaxKind::AttrList);
        }
    }
    p.ev.end(m, SyntaxKind::AttrList)
}

parse_fn_decl! {
    parse_item: Item ::=
        $/if at[#] {
            $parse_attr_list()
        }
        $/match {
            [module] => {$parse_mod()}
            [fn] => {$fun::parse_fn_def()}
            [use] => {$parse_use()}
            [let] [while] [for] [ident] [+] [-] [!] [*] [begin_string] [num_bin] [num_oct] [num_dec] [num_hex] [if] [loop] [return] [continue] [break] ['('] ['{'] => {$stmt::parse_stmt()}
        }
}

parse_fn_decl! {
    // parser-test:simple-use-stmt
    // use println

    // parser-test:simple-use-stmt-with-alias
    // use println as p

    // parser-test:simple-use-stmt-with-list
    // use {println}

    // parser-test:simple-use-stmt-with-list-in-path
    // use println::{a b}

    // parser-test:simple-use-stmt-with-list-in-path-and-alias
    // use println::aaaa::{
    //     a as b
    //     b as c
    //     c as a
    // }
    parse_use: UseStmt ::= $![use] $/ws:wcn $parse_use_path()
}

parse_fn_decl! {
    parse_use_path: UsePath ::= $/match {
        [ident] => {
            $parse_use_path_segment_and_path()
        }
        ['{'] => {$parse_use_path_list()}
    }
}

parse_fn_decl! {
    parse_use_path_segment_and_path: UsePathSegmentAndPath ::=
        $parse_use_path_segment()
        $/state:s1
        $/ws:wcn
        $/match {
            [::] [as] => {
                $parse_use_continuation()
            }
            _ => {
                $/restore_state:s1
            }
        }
}

parse_fn_decl! {
    parse_use_continuation: UsePathContinuation ::=
        $/match {
            [::]  => {
                $parse_use_path_colon_continuation()
            }
            [as] => {
                $parse_use_alias()
            }
        }
}

parse_fn_decl! {
    parse_use_path_colon_continuation: UsePathColonContinuation ::=
        $![::]
        $/ws:wcn
        $parse_use_path()
}

parse_fn_decl! {
    parse_use_alias: UseAlias ::= $![as] $/ws:wcn $![ident]
}

parse_fn_decl! {
    parse_use_path_list: UsePathList ::=
        $!['{']
        $/ws:wcn
        $repeat_until(
            T!['}'],
            parse_use_path,
        )
}

#[parse_fn]
fn repeat_until(
    p: &mut Parser,
    end: SyntaxKind,
    mut parse: impl FnMut(&mut Parser) -> CompletedMarker,
) {
    while !p.at(end) {
        parse(p);
        p.skip_ws_wcn();
        if p.is_recovering() {
            return;
        }
    }
    p.expect(end);
}

parse_fn_decl! {
    parse_use_path_segment: UsePathSegment ::= $![ident]
}

parse_fn_decl! {
    parse_module_name: ModuleName ::= $![ident]
}

parse_fn_decl! {
    // parser-test:module
    // module a

    // parser-test:module-with-empty-body
    // module a {}

    // parser-test:module-with-body-and-items
    // module a {
    //     fn f() {}
    // }

    parse_mod: Module ::=
        $![module]
        $/ws:wcn
        $parse_module_name()
        $/state:s1
        $/ws:wc
        $/match {
            ['{'] => {$parse_mod_body()}
            _ => {
                $/restore_state:s1
            }
        }
}

parse_fn_decl! {
    parse_mod_body: ModuleBody ::=
        $!['{']
        $parse_block_insides()
        $!['}']
}

fn parse_list_rep<E: NotAttemptingRecovery>(
    p: &mut Parser,
    sep: SyntaxKind,
    mut parse: impl FnMut(&mut Parser) -> Result<(), E>,
    recovery: AttemptRecoveryLevel,
) -> Result<(), E> {
    parse(p)?;
    p.skip_ws_wcn();

    'recovered: {
        if p.is_recovering() {
            match recovery {
                AttemptRecoveryLevel::None => return Err(E::not_attempting_recovery()),
                AttemptRecoveryLevel::Shallow => {
                    if p.at(sep) {
                        p.recovered();
                        break 'recovered;
                    }
                }
                AttemptRecoveryLevel::Deep => {
                    if p.bump_until(sep) {
                        p.recovered();
                        break 'recovered;
                    }
                }
            }
            return Ok(());
        }
    }

    'outer: {
        while p.at(sep) {
            p.expect(sep);
            p.skip_ws_wcn();
            parse(p)?;
            p.skip_ws_wcn();

            'recovered: {
                if p.is_recovering() {
                    match recovery {
                        AttemptRecoveryLevel::None => return Err(E::not_attempting_recovery()),
                        AttemptRecoveryLevel::Shallow => {
                            if p.at(sep) {
                                p.recovered();
                                break 'recovered;
                            }
                            break 'outer;
                        }
                        AttemptRecoveryLevel::Deep => {
                            if p.bump_until(sep) {
                                p.recovered();
                                break 'recovered;
                            }
                            break 'outer;
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

fn parse_list_rep_simple<T>(
    p: &mut Parser,
    sep: SyntaxKind,
    mut parse: impl FnMut(&mut Parser) -> T,
    recovery: AttemptRecoveryLevel,
) -> Result<(), ()> {
    parse_list_rep(
        p,
        sep,
        |p| {
            parse(p);
            Ok(())
        },
        recovery,
    )
}

fn parse_list_rep_simple2<T>(
    p: &mut Parser,
    sep: SyntaxKind,
    parse: impl FnMut(&mut Parser) -> T,
    recovery: AttemptRecoveryLevel,
) {
    let _r = parse_list_rep_simple(p, sep, parse, recovery);
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

#[inline(always)]
#[track_caller]
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

#[inline(always)]
#[track_caller]
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
        #[track_caller]
        |p| {
            p.skip_ws_wcn();
            if p.at_eof() {
                p.err(ParseErrorInfo::ExpectedKind(
                    end,
                    std::panic::Location::caller(),
                ));
                return Err(());
            }
            Ok(())
        },
        |p| p.at(T![,]),
        |p| {
            p.skip_ws_wcn();
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

#[inline(always)]
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

#[inline(always)]
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
    parse_ty_ref: TyRef ::= $/match {
        [ident]!
        [fn] => {$parse_fn_ty()}
    }
}

parse_fn_decl! {
    // parser-test:fn-ty
    // let x: fn()

    // parser-test:fn-ty-without-parens
    // let x: fn

    // parser-test:fn-ty-with-return
    // let x: fn() -> i32

    // parser-test:fn-ty-without-parens-with-return
    // let x: fn -> i32

    // parser-test:fn-ty-with-param
    // let x: fn(i32)

    // parser-test:fn-ty-with-param-and-return
    // let x: fn(i32) -> i32

    // parser-test:fn-ty-with-params
    // let x: fn(i32, i32)

    // parser-test:fn-ty-with-params-and-return
    // let x: fn(i32, i32) -> i32

    parse_fn_ty: FnTy ::=
        $![fn]
        $/state:s1
        $/ws:wcn
        $/if at['('] {
            $parse_fn_ty_param_tys()
            $/state:s2
            $/ws:wcn
            $/if at[->] {
                $parse_fn_ty_ret_ty()
            }
            $/else {
                $/restore_state:s2
            }
        }
        $/else if at[->] {
            $parse_fn_ty_ret_ty()
        }
        $/else {
            $/restore_state:s1
        }
}

parse_fn_decl! {
    parse_fn_ty_param_tys: FnTyParamTys ::=
        $parse_list_simple2(T!['('], parse_ty_ref, T![,], T![')'], AttemptRecoveryLevel::Shallow)
}

parse_fn_decl! {
    parse_fn_ty_ret_ty: FnTyRetTy ::= $![->] $/ws:wcn $parse_ty_ref()
}

#[parse_fn]
fn parse_block_insides(p: &mut Parser) {
    p.skip_ws_wcn();
    if !p.at(T!['}']) {
        loop {
            p.skip_ws_wc();

            // parser-test:block-with-semis
            // { let x = a; let y = b;;;
            //      ;;
            //      ;;
            //   x + y }

            while p.at(T![;]) || p.at(T![newline]) {
                if p.at(T![;]) {
                    p.expect(T![;]);
                } else if p.at(T![newline]) {
                    p.expect(T![newline]);
                }
                p.skip_ws_wc();
            }

            if p.at(T!['}']) {
                break;
            }

            parse_item(p);
        }
    }
}

parse_fn_decl! {
    parse_block: Block ::=
        $!['{']
        $parse_block_insides()
        $!['}']
}

parse_fn_decl! {
    parse_pat: Pat ::=
        $![ident]
}
