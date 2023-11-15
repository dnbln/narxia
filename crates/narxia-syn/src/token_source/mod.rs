use std::fmt;
use std::fmt::Formatter;
use std::marker::PhantomData;

use crate::parser::{ColorizeProcedure, ParserDbgStyling};
use crate::syntax_kind::SyntaxKind;
use crate::text_span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    kind: SyntaxKind,
    span: TextSpan,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{}", self.kind, self.span)
    }
}

impl Token {
    fn add_offset(self, offset: u32) -> Self {
        Self {
            kind: self.kind,
            span: self.span.add_offset(offset),
        }
    }

    pub fn kind(self) -> SyntaxKind {
        self.kind
    }

    pub fn kind_is(self, k: SyntaxKind) -> bool {
        self.kind == k
    }

    pub fn span(self) -> TextSpan {
        self.span
    }

    #[inline(always)]
    pub(crate) fn with_kind(mut self, kind: SyntaxKind) -> Self {
        self.kind = kind;
        self
    }

    #[inline(always)]
    pub(crate) unsafe fn compose(self, other: Token, kind: SyntaxKind) -> Token {
        Token {
            kind,
            span: TextSpan::new_unchecked(self.span.start, other.span.end),
        }
    }

    pub(crate) fn dbg_fmt_colorized(self, styling: ParserDbgStyling) -> DbgFmtColorizedToken {
        DbgFmtColorizedToken(self, styling)
    }
}

pub(crate) struct DbgFmtColorizedToken(Token, ParserDbgStyling);

impl fmt::Display for DbgFmtColorizedToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}@{}",
            self.1
                .token_kind
                .colorize(format_args!("{:?}", self.0.kind)),
            self.1.token_span.colorize(self.0.span),
        )
    }
}

pub(crate) struct BufferedTokenSource<'l, T: TokenSource<'l> + 'l> {
    ts: T,
    buffer: [Token; 4],
    buffer_len: usize,
    _pd: PhantomData<&'l ()>,
}

impl<'l, T> BufferedTokenSource<'l, T>
where
    T: TokenSource<'l> + 'l,
{
    pub fn new(ts: T) -> Self {
        Self {
            ts,
            buffer: [Token {
                kind: SyntaxKind::__TOMBSTONE,
                span: TextSpan { start: 0, end: 0 },
            }; 4],
            buffer_len: 0,
            _pd: PhantomData,
        }
    }

    pub fn current_pos(&mut self) -> usize {
        self.current_token_span().start as usize
    }

    pub fn current_token_span(&mut self) -> TextSpan {
        self.lookahead0()
            .map(|tok| tok.span())
            .unwrap_or_else(|| self.ts.eof_span())
    }

    #[inline(always)]
    pub fn get_token_text(&self, token: &Token) -> &'l str {
        self.ts.get_token_text(token)
    }

    #[inline(always)]
    #[track_caller]
    pub fn lookahead0(&mut self) -> Option<Token> {
        if self.buffer_len == 0 {
            let t0 = self.ts.next()?; // token at position = 0
            unsafe {
                *self.buffer.get_unchecked_mut(0) = t0;
            }
            self.buffer_len = 1;
            Some(t0)
        } else {
            // Safety: length != 0 means length >= 1 so we have one elem.
            Some(unsafe { self.get_buf_0() })
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn lookahead(&mut self, n: usize) -> Option<Token> {
        debug_assert!(n <= 3);
        unsafe {
            // perf: collapse tuple match into multiplication + match on result.
            // self.buffer_len is always in [0, 4].
            // n is always in [0, 3].
            // so (n * 5 + self.buffer_len) will give us all the values we care about
            match n * 5 + self.buffer_len {
                0 => {
                    // (0, 0)
                    let t0 = self.ts.next()?; // token at position = 0
                    *self.buffer.get_unchecked_mut(0) = t0;
                    self.buffer_len = 1;
                    Some(t0)
                }
                1 | 2 | 3 | 4 => {
                    // (0, 1) | (0, 2) | (0, 3) | (0, 4)
                    // Safety: length != 0 means length >= 1 so we can use get_unchecked(0)
                    Some(*self.buffer.get_unchecked(0))
                }
                5 => {
                    // (1, 0)
                    let t0 = self.ts.next()?; // token at position = 0
                    let t1 = self.ts.next()?; // token at position = 1
                    *self.buffer.get_unchecked_mut(0) = t0;
                    *self.buffer.get_unchecked_mut(1) = t1;
                    self.buffer_len = 2;
                    Some(t1)
                }
                6 => {
                    // (1, 1)
                    let t1 = self.ts.next()?; // token at position = 1
                    *self.buffer.get_unchecked_mut(1) = t1;
                    self.buffer_len = 2;
                    Some(t1)
                }
                7 | 8 | 9 => {
                    // (1, 2) | (1, 3) | (1, 4)
                    // Safety: length != 0 && length != 1 means length >= 2 so we can use get_unchecked(1)
                    Some(*self.buffer.get_unchecked(1))
                }
                10 => {
                    // (2, 0)
                    let t0 = self.ts.next()?; // token at position = 0
                    let t1 = self.ts.next()?; // token at position = 1
                    let t2 = self.ts.next()?; // token at position = 2
                    *self.buffer.get_unchecked_mut(0) = t0;
                    *self.buffer.get_unchecked_mut(1) = t1;
                    *self.buffer.get_unchecked_mut(2) = t2;
                    self.buffer_len = 3;
                    Some(t2)
                }
                11 => {
                    // (2, 1)
                    let t1 = self.ts.next()?; // token at position = 1
                    let t2 = self.ts.next()?; // token at position = 2
                    *self.buffer.get_unchecked_mut(1) = t1;
                    *self.buffer.get_unchecked_mut(2) = t2;
                    self.buffer_len = 3;
                    Some(t2)
                }
                12 => {
                    // (2, 2)
                    let t2 = self.ts.next()?; // token at position = 2
                    *self.buffer.get_unchecked_mut(2) = t2;
                    self.buffer_len = 3;
                    Some(t2)
                }
                13 | 14 => {
                    // (2, 3) | (2, 4)
                    // Safety: length != 0 && length != 1 && length != 2
                    // means length >= 3 so we can use get_unchecked(2)
                    Some(*self.buffer.get_unchecked(2))
                }
                15 => {
                    // (3, 0)
                    let t0 = self.ts.next()?; // token at position = 0
                    let t1 = self.ts.next()?; // token at position = 1
                    let t2 = self.ts.next()?; // token at position = 2
                    let t3 = self.ts.next()?; // token at position = 3
                    *self.buffer.get_unchecked_mut(0) = t0;
                    *self.buffer.get_unchecked_mut(1) = t1;
                    *self.buffer.get_unchecked_mut(2) = t2;
                    *self.buffer.get_unchecked_mut(3) = t3;
                    self.buffer_len = 4;
                    Some(t3)
                }
                16 => {
                    // (3, 1)
                    let t1 = self.ts.next()?; // token at position = 1
                    let t2 = self.ts.next()?; // token at position = 2
                    let t3 = self.ts.next()?; // token at position = 3
                    *self.buffer.get_unchecked_mut(1) = t1;
                    *self.buffer.get_unchecked_mut(2) = t2;
                    *self.buffer.get_unchecked_mut(3) = t3;
                    self.buffer_len = 4;
                    Some(t3)
                }
                17 => {
                    // (3, 2)
                    let t2 = self.ts.next()?; // token at position = 2
                    let t3 = self.ts.next()?; // token at position = 3
                    *self.buffer.get_unchecked_mut(2) = t2;
                    *self.buffer.get_unchecked_mut(3) = t3;
                    self.buffer_len = 4;
                    Some(t3)
                }
                18 => {
                    // (3, 3)
                    let t3 = self.ts.next()?; // token at position = 3
                    *self.buffer.get_unchecked_mut(3) = t3;
                    self.buffer_len = 4;
                    Some(t3)
                }
                19 => {
                    // (3, 4)
                    Some(*self.buffer.get_unchecked(3))
                }
                _ => core::hint::unreachable_unchecked(),
            }
        }
        // while n >= self.buffer.len() {
        //     let token = self.ts.next()?;
        //     self.buffer.push(token);
        // }
        // Some(self.buffer[n])
    }

    #[inline(always)]
    #[track_caller]
    pub fn at_1(&mut self, kind0: SyntaxKind) -> bool {
        unsafe {
            match self.buffer_len {
                0 => {
                    let Some(t0) = self.ts.next() else {
                        return false;
                    };
                    *self.buffer.get_unchecked_mut(0) = t0;
                    self.buffer_len = 1;
                    t0.kind == kind0
                }
                _ => self.get_buf_0().kind == kind0,
            }
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn at_1_and_token(&mut self, kind0: SyntaxKind) -> Option<Token> {
        unsafe {
            match self.buffer_len {
                0 => {
                    let Some(t0) = self.ts.next() else {
                        return None;
                    };
                    *self.buffer.get_unchecked_mut(0) = t0;
                    self.buffer_len = 1;
                    (t0.kind == kind0).then_some(t0)
                }
                _ => {
                    let t = self.get_buf_0();
                    (t.kind == kind0).then_some(t)
                }
            }
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn expect_1(
        &mut self,
        kind0: SyntaxKind,
        push_token_evt: impl FnOnce(Token, &'l str),
    ) -> bool {
        unsafe {
            let t0 = match self.buffer_len {
                0 => {
                    let Some(t0) = self.ts.next() else {
                        return false;
                    };
                    if t0.kind != kind0 {
                        *self.buffer.get_unchecked_mut(0) = t0;
                        self.buffer_len = 1;
                        return false;
                    }
                    t0
                }
                1 => {
                    let t = self.get_buf_0();
                    if t.kind != kind0 {
                        return false;
                    }
                    self.buffer_len = 0;
                    t
                }
                2 => {
                    let t = self.get_buf_0();
                    if t.kind != kind0 {
                        return false;
                    }
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(1);
                    self.buffer_len = 1;
                    t
                }
                3 => {
                    let t = self.get_buf_0();
                    if t.kind != kind0 {
                        return false;
                    }
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(1);
                    *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                    self.buffer_len = 2;
                    t
                }
                4 => {
                    let t = self.get_buf_0();
                    if t.kind != kind0 {
                        return false;
                    }
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(1);
                    *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                    *self.buffer.get_unchecked_mut(2) = *self.buffer.get_unchecked(3);
                    self.buffer_len = 3;
                    t
                }
                _ => core::hint::unreachable_unchecked(),
            };
            let text = self.ts.get_token_text(&t0);
            push_token_evt(t0, text);
            true
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn expect_2(
        &mut self,
        kind0: SyntaxKind,
        kind1: SyntaxKind,
        complete: SyntaxKind,
        push_token_evt: impl FnOnce(Token, Token, Token, &'l str),
    ) -> bool {
        unsafe {
            let (t0, t1) = match self.buffer_len {
                0 => {
                    let Some(t0) = self.ts.next() else {
                        return false;
                    };
                    if t0.kind != kind0 {
                        *self.buffer.get_unchecked_mut(0) = t0;
                        self.buffer_len = 1;
                        return false;
                    }
                    let Some(t1) = self.ts.next() else {
                        return false;
                    };
                    if t1.kind != kind1 {
                        *self.buffer.get_unchecked_mut(0) = t0;
                        *self.buffer.get_unchecked_mut(1) = t1;
                        self.buffer_len = 2;
                        return false;
                    }
                    self.buffer_len = 0;
                    (t0, t1)
                }
                1 => {
                    let t0 = self.get_buf_0();
                    if t0.kind != kind0 {
                        return false;
                    }
                    let Some(t1) = self.ts.next() else {
                        return false;
                    };
                    if t1.kind != kind1 {
                        *self.buffer.get_unchecked_mut(1) = t1;
                        self.buffer_len = 2;
                        return false;
                    }
                    self.buffer_len = 0;
                    (t0, t1)
                }
                2 => {
                    let t0 = self.get_buf_0();
                    if t0.kind != kind0 {
                        return false;
                    }
                    let t1 = *self.buffer.get_unchecked(1);
                    if t1.kind != kind1 {
                        return false;
                    }
                    self.buffer_len = 0;
                    (t0, t1)
                }
                3 => {
                    let t0 = self.get_buf_0();
                    if t0.kind != kind0 {
                        return false;
                    }
                    let t1 = *self.buffer.get_unchecked(1);
                    if t1.kind != kind1 {
                        return false;
                    }
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(2);
                    self.buffer_len = 1;
                    (t0, t1)
                }
                4 => {
                    let t0 = self.get_buf_0();
                    if t0.kind != kind0 {
                        return false;
                    }
                    let t1 = *self.buffer.get_unchecked(1);
                    if t1.kind != kind1 {
                        return false;
                    }
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(2);
                    *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(3);
                    self.buffer_len = 2;
                    (t0, t1)
                }
                _ => core::hint::unreachable_unchecked(),
            };
            let token = t0.compose(t1, complete);
            let text = self.ts.get_token_text(&token);
            push_token_evt(t0, t1, token, text);
            true
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn at_2(&mut self, kind0: SyntaxKind, kind1: SyntaxKind) -> bool {
        unsafe {
            match self.buffer_len {
                0 => {
                    let Some(t0) = self.ts.next() else {
                        return false;
                    };
                    *self.buffer.get_unchecked_mut(0) = t0;
                    self.buffer_len = 1;
                    if t0.kind != kind0 {
                        return false;
                    }
                    let Some(t1) = self.ts.next() else {
                        return false;
                    };
                    *self.buffer.get_unchecked_mut(1) = t1;
                    self.buffer_len = 2;
                    t1.kind == kind1
                }
                1 => {
                    if self.buffer.get_unchecked(0).kind != kind0 {
                        return false;
                    }
                    let Some(t1) = self.ts.next() else {
                        return false;
                    };
                    *self.buffer.get_unchecked_mut(1) = t1;
                    self.buffer_len = 2;
                    t1.kind == kind1
                }
                _ => {
                    self.buffer.get_unchecked(0).kind == kind0
                        && self.buffer.get_unchecked(1).kind == kind1
                }
            }
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn skip_whitespace_wc(&mut self, mut push_token_evt: impl FnMut(Token, &'l str)) {
        let check_sk =
            |sk: SyntaxKind| -> bool { SyntaxKind::WHITESPACE == sk || SyntaxKind::COMMENT == sk };

        match self.buffer_len {
            0 => {
                let token = self.ts.skip_ws_wc();

                match token {
                    Some(t) => {
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {}
                }
            }
            1 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                self.ts.restore_pos(t0.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wc();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {
                        push_token_evt(
                            t0.with_kind(SyntaxKind::COMPOSED_TRIVIA),
                            self.get_token_text(&t0),
                        );
                    }
                }
            }
            2 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { *self.buffer.get_unchecked(1) };
                if !check_sk(t1.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t1;
                    }
                    self.buffer_len = 1;
                    push_token_evt(
                        t0.with_kind(SyntaxKind::COMPOSED_TRIVIA),
                        self.get_token_text(&t0),
                    );
                    return;
                }
                self.ts.restore_pos(t1.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wc();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {
                        let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                }
            }
            3 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { *self.buffer.get_unchecked(1) };
                if !check_sk(t1.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t1;
                        *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                    }
                    self.buffer_len = 2;
                    push_token_evt(
                        t0.with_kind(SyntaxKind::COMPOSED_TRIVIA),
                        self.get_token_text(&t0),
                    );
                    return;
                }
                let t2 = unsafe { *self.buffer.get_unchecked(2) };
                if !check_sk(t2.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t2;
                    }
                    self.buffer_len = 1;
                    let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t, self.get_token_text(&t));
                    return;
                }
                self.ts.restore_pos(t2.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wc();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {
                        let t = unsafe { t0.compose(t2, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                }
            }
            4 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { *self.buffer.get_unchecked(1) };
                if !check_sk(t1.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t1;
                        *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                        *self.buffer.get_unchecked_mut(2) = *self.buffer.get_unchecked(3);
                    }
                    self.buffer_len = 3;
                    push_token_evt(
                        t0.with_kind(SyntaxKind::COMPOSED_TRIVIA),
                        self.get_token_text(&t0),
                    );
                    return;
                }
                let t2 = unsafe { *self.buffer.get_unchecked(2) };
                if !check_sk(t2.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t2;
                        *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(3);
                    }
                    self.buffer_len = 2;
                    let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t, self.get_token_text(&t));
                    return;
                }
                let t3 = unsafe { *self.buffer.get_unchecked(3) };
                if !check_sk(t3.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t3;
                    }
                    self.buffer_len = 1;
                    let t = unsafe { t0.compose(t2, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t, self.get_token_text(&t));
                    return;
                }
                self.ts.restore_pos(t3.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wc();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {
                        let t = unsafe { t0.compose(t3, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                }
            }
            _ => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn skip_whitespace_wcn(&mut self, mut push_token_evt: impl FnMut(Token, &'l str)) {
        let check_sk = |sk: SyntaxKind| -> bool {
            sk == SyntaxKind::WHITESPACE || sk == SyntaxKind::NEWLINE || sk == SyntaxKind::COMMENT
        };

        match self.buffer_len {
            0 => {
                let token = self.ts.skip_ws_wcn();

                match token {
                    Some(t) => {
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {}
                }
            }
            1 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                self.ts.restore_pos(t0.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wcn();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {
                        push_token_evt(
                            t0.with_kind(SyntaxKind::COMPOSED_TRIVIA),
                            self.get_token_text(&t0),
                        );
                    }
                }
            }
            2 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { *self.buffer.get_unchecked(1) };
                if !check_sk(t1.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t1;
                    }
                    self.buffer_len = 1;
                    push_token_evt(
                        t0.with_kind(SyntaxKind::COMPOSED_TRIVIA),
                        self.get_token_text(&t0),
                    );
                    return;
                }
                self.ts.restore_pos(t1.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wcn();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {
                        let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                }
            }
            3 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { *self.buffer.get_unchecked(1) };
                if !check_sk(t1.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t1;
                        *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                    }
                    self.buffer_len = 2;
                    push_token_evt(
                        t0.with_kind(SyntaxKind::COMPOSED_TRIVIA),
                        self.get_token_text(&t0),
                    );
                    return;
                }
                let t2 = unsafe { *self.buffer.get_unchecked(2) };
                if !check_sk(t2.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t2;
                    }
                    self.buffer_len = 1;
                    let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t, self.get_token_text(&t));
                    return;
                }
                self.ts.restore_pos(t2.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wcn();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {
                        let t = unsafe { t0.compose(t2, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                }
            }
            4 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { *self.buffer.get_unchecked(1) };
                if !check_sk(t1.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t1;
                        *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                        *self.buffer.get_unchecked_mut(2) = *self.buffer.get_unchecked(3);
                    }
                    self.buffer_len = 3;
                    push_token_evt(
                        t0.with_kind(SyntaxKind::COMPOSED_TRIVIA),
                        self.get_token_text(&t0),
                    );
                    return;
                }
                let t2 = unsafe { *self.buffer.get_unchecked(2) };
                if !check_sk(t2.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t2;
                        *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(3);
                    }
                    self.buffer_len = 2;
                    let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t, self.get_token_text(&t));
                    return;
                }
                let t3 = unsafe { *self.buffer.get_unchecked(3) };
                if !check_sk(t3.kind) {
                    unsafe {
                        *self.buffer.get_unchecked_mut(0) = t3;
                    }
                    self.buffer_len = 1;
                    let t = unsafe { t0.compose(t2, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t, self.get_token_text(&t));
                    return;
                }
                self.ts.restore_pos(t3.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wcn();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                    None => {
                        let t = unsafe { t0.compose(t3, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t, self.get_token_text(&t));
                    }
                }
            }
            _ => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    #[inline(always)]
    pub fn bump_until(
        &mut self,
        kind: SyntaxKind,
        mut push_token_evt: impl FnMut(Token, &'l str),
    ) -> bool {
        unsafe {
            match self.buffer_len {
                0 => {
                    while let Some(token) = self.ts.next() {
                        if token.kind == kind {
                            *self.buffer.get_unchecked_mut(0) = token;
                            self.buffer_len = 1;
                            return true;
                        }
                        push_token_evt(token, self.get_token_text(&token));
                    }
                    false
                }
                1 => {
                    let t0 = self.get_buf_0();
                    if t0.kind == kind {
                        return true;
                    } else {
                        self.buffer_len = 0;
                    }
                    push_token_evt(t0, self.get_token_text(&t0));
                    while let Some(token) = self.ts.next() {
                        if token.kind == kind {
                            *self.buffer.get_unchecked_mut(0) = token;
                            self.buffer_len = 1;
                            return true;
                        }
                        push_token_evt(token, self.get_token_text(&token));
                    }
                    false
                }
                2 => {
                    let t0 = self.get_buf_0();
                    if t0.kind == kind {
                        return true;
                    }
                    push_token_evt(t0, self.get_token_text(&t0));
                    let t1 = *self.buffer.get_unchecked(1);
                    if t1.kind == kind {
                        *self.buffer.get_unchecked_mut(0) = t1;
                        self.buffer_len = 1;
                        return true;
                    } else {
                        self.buffer_len = 0;
                    }
                    push_token_evt(t1, self.get_token_text(&t1));
                    while let Some(token) = self.ts.next() {
                        if token.kind == kind {
                            *self.buffer.get_unchecked_mut(0) = token;
                            self.buffer_len = 1;
                            return true;
                        }
                        push_token_evt(token, self.get_token_text(&token));
                    }
                    false
                }
                3 => {
                    let t0 = self.get_buf_0();
                    if t0.kind == kind {
                        return true;
                    }
                    push_token_evt(t0, self.get_token_text(&t0));
                    let t1 = *self.buffer.get_unchecked(1);
                    if t1.kind == kind {
                        *self.buffer.get_unchecked_mut(0) = t1;
                        *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                        self.buffer_len = 2;
                        return true;
                    }
                    push_token_evt(t1, self.get_token_text(&t1));
                    let t2 = *self.buffer.get_unchecked(2);
                    if t2.kind == kind {
                        *self.buffer.get_unchecked_mut(0) = t2;
                        self.buffer_len = 1;
                        return true;
                    } else {
                        self.buffer_len = 0;
                    }
                    push_token_evt(t2, self.get_token_text(&t2));
                    while let Some(token) = self.ts.next() {
                        if token.kind == kind {
                            *self.buffer.get_unchecked_mut(0) = token;
                            self.buffer_len = 1;
                            return true;
                        }
                        push_token_evt(token, self.get_token_text(&token));
                    }
                    false
                }
                _ => core::hint::unreachable_unchecked(),
            }
        }
    }

    unsafe fn get_buf_0(&self) -> Token {
        *self.buffer.get_unchecked(0)
    }

    #[track_caller]
    pub fn compose_token(&mut self, kind: SyntaxKind, n: usize) -> Option<Token> {
        debug_assert!(n > 0);
        debug_assert!(n <= 3);
        match n {
            1 => {
                let token = unsafe { self.get_buf_0() };
                Some(token.with_kind(kind))
            }
            2 => {
                let last = self.lookahead(1)?;
                let token = unsafe { self.get_buf_0() };
                Some(unsafe { token.compose(last, kind) })
            }
            3 => {
                let last = self.lookahead(2)?;
                let token = unsafe { self.get_buf_0() };
                Some(unsafe { token.compose(last, kind) })
            }
            _ => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn advance(&mut self) {
        unsafe {
            match self.buffer_len {
                0 => {
                    let _ = self.ts.next();
                }
                1 => {
                    self.buffer_len = 0;
                }
                2 => {
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(1);
                    self.buffer_len = 1;
                }
                3 => {
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(1);
                    *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                    self.buffer_len = 2;
                }
                4 => {
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(1);
                    *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                    *self.buffer.get_unchecked_mut(2) = *self.buffer.get_unchecked(3);
                    self.buffer_len = 3;
                }
                _ => core::hint::unreachable_unchecked(),
            }
        }
    }
    #[track_caller]
    pub fn advance_n(&mut self, n: usize) {
        debug_assert!(0 < n && n <= 3);
        // let Some(_) = self.lookahead(n - 1) else {
        //     return;
        // };
        // self.buffer.drain(0..n);
        unsafe {
            match (n, self.buffer_len) {
                (1, 0) => {
                    let _ = self.ts.next();
                }
                (1, 1) => {
                    self.buffer_len = 0;
                }
                (1, 2) => {
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(1);
                    self.buffer_len = 1;
                }
                (1, 3) => {
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(1);
                    *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                    self.buffer_len = 2;
                }
                (1, 4) => {
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(1);
                    *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(2);
                    *self.buffer.get_unchecked_mut(2) = *self.buffer.get_unchecked(3);
                    self.buffer_len = 3;
                }
                (2, 0) => match self.ts.next() {
                    Some(_) => {
                        self.ts.next();
                    }
                    None => {}
                },
                (2, 1) => {
                    let _ = self.ts.next();
                    self.buffer_len = 0;
                }
                (2, 2) => {
                    self.buffer_len = 0;
                }
                (2, 3) => {
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(2);
                    self.buffer_len = 1;
                }
                (2, 4) => {
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(2);
                    *self.buffer.get_unchecked_mut(1) = *self.buffer.get_unchecked(3);
                    self.buffer_len = 2;
                }
                (3, 0) => match self.ts.next() {
                    Some(_) => match self.ts.next() {
                        Some(_) => {
                            self.ts.next();
                        }
                        None => {}
                    },
                    None => {}
                },
                (3, 1) => {
                    match self.ts.next() {
                        Some(_) => {
                            self.ts.next();
                        }
                        None => {}
                    }
                    self.buffer_len = 0;
                }
                (3, 2) => {
                    self.ts.next();
                    self.buffer_len = 0;
                }
                (3, 3) => {
                    self.buffer_len = 0;
                }
                (3, 4) => {
                    *self.buffer.get_unchecked_mut(0) = *self.buffer.get_unchecked(3);
                    self.buffer_len = 1;
                }
                (_, _) => unsafe { core::hint::unreachable_unchecked() },
            }
        }
    }

    #[track_caller]
    pub fn restore_pos(&mut self, pos: usize) {
        unsafe {
            let pos_u32 = pos.try_into().unwrap_unchecked();
            match self.buffer_len {
                0 => {
                    self.ts.restore_pos(pos);
                }
                1 => {
                    let t0 = self.get_buf_0();
                    if t0.span.start == pos_u32 {
                        self.ts.restore_pos(t0.span.end as usize);
                        return;
                    } else if t0.span.end == pos_u32 {
                        self.ts.restore_pos(pos);
                        self.buffer_len = 0;
                        return;
                    }

                    self.ts.restore_pos(pos);
                    self.buffer_len = 0;
                }
                2 => {
                    let t0 = self.get_buf_0();
                    let t1 = *self.buffer.get_unchecked(1);
                    if t0.span.start == pos_u32 {
                        self.ts.restore_pos(t1.span.end as usize);
                        return;
                    } else if t1.span.start == pos_u32 {
                        self.ts.restore_pos(t1.span.end as usize);
                        *self.buffer.get_unchecked_mut(0) = t1;
                        self.buffer_len = 1;
                        return;
                    }

                    self.ts.restore_pos(pos);
                    self.buffer_len = 0;
                }
                3 => {
                    let t0 = self.get_buf_0();
                    let t1 = *self.buffer.get_unchecked(1);
                    let t2 = *self.buffer.get_unchecked(2);
                    if t0.span.start == pos_u32 {
                        self.ts.restore_pos(t2.span.end as usize);
                        return;
                    } else if t1.span.start == pos_u32 {
                        self.ts.restore_pos(t2.span.end as usize);
                        *self.buffer.get_unchecked_mut(0) = t1;
                        *self.buffer.get_unchecked_mut(1) = t2;
                        self.buffer_len = 2;
                        return;
                    } else if t2.span.start == pos_u32 {
                        self.ts.restore_pos(t2.span.end as usize);
                        *self.buffer.get_unchecked_mut(0) = t2;
                        self.buffer_len = 1;
                        return;
                    }

                    self.ts.restore_pos(pos);
                    self.buffer_len = 0;
                }
                4 => {
                    let t0 = self.get_buf_0();
                    let t1 = *self.buffer.get_unchecked(1);
                    let t2 = *self.buffer.get_unchecked(2);
                    let t3 = *self.buffer.get_unchecked(3);
                    if t0.span.start == pos_u32 {
                        self.ts.restore_pos(t3.span.end as usize);
                        return;
                    } else if t1.span.start == pos_u32 {
                        self.ts.restore_pos(t3.span.end as usize);
                        *self.buffer.get_unchecked_mut(0) = t1;
                        *self.buffer.get_unchecked_mut(1) = t2;
                        *self.buffer.get_unchecked_mut(2) = t3;
                        self.buffer_len = 3;
                        return;
                    } else if t2.span.start == pos_u32 {
                        self.ts.restore_pos(t3.span.end as usize);
                        *self.buffer.get_unchecked_mut(0) = t2;
                        *self.buffer.get_unchecked_mut(1) = t3;
                        self.buffer_len = 2;
                        return;
                    } else if t3.span.start == pos_u32 {
                        self.ts.restore_pos(t3.span.end as usize);
                        *self.buffer.get_unchecked_mut(0) = t3;
                        self.buffer_len = 1;
                        return;
                    }

                    self.ts.restore_pos(pos);
                    self.buffer_len = 0;
                }
                _ => core::hint::unreachable_unchecked(),
            }
        }
    }
}

pub trait TokenSource<'l> {
    #[track_caller]
    fn next(&mut self) -> Option<Token>;

    #[track_caller]
    fn skip_ws_wc(&mut self) -> Option<Token>;
    #[track_caller]
    fn skip_ws_wcn(&mut self) -> Option<Token>;

    fn get_token_text(&self, token: &Token) -> &'l str;
    fn get_error(&self) -> Option<TokenError> {
        None
    }
    fn eof_span(&self) -> TextSpan;
    fn restore_pos(&mut self, pos: usize);
}

pub(crate) struct DynTsContainer<'l>(pub &'l mut dyn TokenSource<'l>);

impl<'l> TokenSource<'l> for DynTsContainer<'l> {
    fn next(&mut self) -> Option<Token> {
        self.0.next()
    }

    fn skip_ws_wc(&mut self) -> Option<Token> {
        self.0.skip_ws_wc()
    }

    fn skip_ws_wcn(&mut self) -> Option<Token> {
        self.0.skip_ws_wcn()
    }

    fn get_token_text(&self, token: &Token) -> &'l str {
        self.0.get_token_text(token)
    }

    fn eof_span(&self) -> TextSpan {
        self.0.eof_span()
    }

    fn restore_pos(&mut self, pos: usize) {
        self.0.restore_pos(pos)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenError {
    UnexpectedChar(char),
    BlockCommentNotClosed,
    StringNotClosed,
}

pub mod text_ts;
