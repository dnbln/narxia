use std::marker::PhantomData;

use crate::{text_span::TextSpan, syntax_kind::SyntaxKind};

use super::{Token, TokenSource};

pub(crate) struct BufferedTokenSource<'l, T: TokenSource<'l> + 'l> {
    ts: T,
    buffer_kinds: [SyntaxKind; 4],
    // [0] => tokens[0].start
    // [1] => tokens[0].end == tokens[1].start
    // [2] => tokens[1].end == tokens[2].start
    // [3] => tokens[2].end == tokens[3].start
    // [4] => tokens[3].end
    buffer_spans: [u32; 5],
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
            buffer_kinds: [SyntaxKind::__TOMBSTONE; 4],
            buffer_spans: [0; 5],
            buffer_len: 0,
            _pd: PhantomData,
        }
    }

    pub fn current_pos(&mut self) -> usize {
        self.current_token_span().start as usize
    }

    pub fn current_token_span(&mut self) -> TextSpan {
        self.lookahead0_span().unwrap_or_else(|| self.ts.eof_span())
    }

    #[inline(always)]
    pub fn get_token_text(&self, token: &Token) -> &'l str {
        self.get_span_text(token.span)
    }

    #[inline(always)]
    pub fn get_span_text(&self, span: TextSpan) -> &'l str {
        self.ts.get_span_text(span)
    }

    #[inline(always)]
    #[track_caller]
    pub fn lookahead0(&mut self) -> Option<Token> {
        if std::intrinsics::unlikely(self.buffer_len == 0) {
            let t0 = self.ts.next()?; // token at position = 0
            unsafe {
                self.store_0(t0);
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
    pub fn lookahead0_kind(&mut self) -> Option<SyntaxKind> {
        if std::intrinsics::unlikely(self.buffer_len == 0) {
            let t0 = self.ts.next()?; // token at position = 0
            unsafe {
                self.store_0(t0);
            }
            self.buffer_len = 1;
            Some(t0.kind)
        } else {
            Some(unsafe { self.get_buf_0_k() })
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn lookahead0_span(&mut self) -> Option<TextSpan> {
        if std::intrinsics::unlikely(self.buffer_len == 0) {
            let t0 = self.ts.next()?; // token at position = 0
            unsafe {
                self.store_0(t0);
            }
            self.buffer_len = 1;
            Some(t0.span)
        } else {
            Some(unsafe { self.get_buf_0_span() })
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn at_eof(&mut self) -> bool {
        if std::intrinsics::unlikely(self.buffer_len == 0) {
            let t0 = self.ts.next(); // token at position = 0
            match t0 {
                Some(t0) => {
                    unsafe {
                        self.store_0(t0);
                    }
                    self.buffer_len = 1;
                    false
                }
                None => true,
            }
        } else {
            false
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
                    self.store_0(t0);
                    self.buffer_len = 1;
                    Some(t0)
                }
                1 | 2 | 3 | 4 => {
                    // (0, 1) | (0, 2) | (0, 3) | (0, 4)
                    // Safety: length != 0 means length >= 1 so we can use get_unchecked(0)
                    Some(self.get_buf_0())
                }
                5 => {
                    // (1, 0)
                    let t0 = self.ts.next()?; // token at position = 0
                    let t1 = self.ts.next()?; // token at position = 1
                    self.store_01(t0, t1);
                    self.buffer_len = 2;
                    Some(t1)
                }
                6 => {
                    // (1, 1)
                    let t1 = self.ts.next()?; // token at position = 1
                    self.store_1(t1);
                    self.buffer_len = 2;
                    Some(t1)
                }
                7 | 8 | 9 => {
                    // (1, 2) | (1, 3) | (1, 4)
                    // Safety: length != 0 && length != 1 means length >= 2 so we can use get_buf_1()
                    Some(self.get_buf_1())
                }
                10 => {
                    // (2, 0)
                    let t0 = self.ts.next()?; // token at position = 0
                    let t1 = self.ts.next()?; // token at position = 1
                    let t2 = self.ts.next()?; // token at position = 2
                    self.store_012(t0, t1, t2);
                    self.buffer_len = 3;
                    Some(t2)
                }
                11 => {
                    // (2, 1)
                    let t1 = self.ts.next()?; // token at position = 1
                    let t2 = self.ts.next()?; // token at position = 2
                    self.store_12(t1, t2);
                    self.buffer_len = 3;
                    Some(t2)
                }
                12 => {
                    // (2, 2)
                    let t2 = self.ts.next()?; // token at position = 2
                    self.store_2(t2);
                    self.buffer_len = 3;
                    Some(t2)
                }
                13 | 14 => {
                    // (2, 3) | (2, 4)
                    // Safety: length != 0 && length != 1 && length != 2
                    // means length >= 3 so we can use get_unchecked(2)
                    Some(self.get_buf_2())
                }
                15 => {
                    // (3, 0)
                    let t0 = self.ts.next()?; // token at position = 0
                    let t1 = self.ts.next()?; // token at position = 1
                    let t2 = self.ts.next()?; // token at position = 2
                    let t3 = self.ts.next()?; // token at position = 3
                    self.store_0123(t0, t1, t2, t3);
                    self.buffer_len = 4;
                    Some(t3)
                }
                16 => {
                    // (3, 1)
                    let t1 = self.ts.next()?; // token at position = 1
                    let t2 = self.ts.next()?; // token at position = 2
                    let t3 = self.ts.next()?; // token at position = 3
                    self.store_123(t1, t2, t3);
                    self.buffer_len = 4;
                    Some(t3)
                }
                17 => {
                    // (3, 2)
                    let t2 = self.ts.next()?; // token at position = 2
                    let t3 = self.ts.next()?; // token at position = 3
                    self.store_23(t2, t3);
                    self.buffer_len = 4;
                    Some(t3)
                }
                18 => {
                    // (3, 3)
                    let t3 = self.ts.next()?; // token at position = 3
                    self.store_3(t3);
                    self.buffer_len = 4;
                    Some(t3)
                }
                19 => {
                    // (3, 4)
                    Some(self.get_buf_3())
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
                    self.store_0(t0);
                    self.buffer_len = 1;
                    t0.kind == kind0
                }
                _ => self.get_buf_0_k() == kind0,
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
                    self.store_0(t0);
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
    pub fn expect_1(&mut self, kind0: SyntaxKind, push_token_evt: impl FnOnce(Token)) -> bool {
        unsafe {
            let t0 = match self.buffer_len {
                0 => {
                    let Some(t0) = self.ts.next() else {
                        return false;
                    };
                    if t0.kind != kind0 {
                        self.store_0(t0);
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
                    self.advance_n_1_bl_2();
                    t
                }
                3 => {
                    let t = self.get_buf_0();
                    if t.kind != kind0 {
                        return false;
                    }
                    self.advance_n_1_bl_3();
                    t
                }
                4 => {
                    let t = self.get_buf_0();
                    if t.kind != kind0 {
                        return false;
                    }
                    self.advance_n_1_bl_4();
                    t
                }
                _ => core::hint::unreachable_unchecked(),
            };
            push_token_evt(t0);
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
        push_token_evt: impl FnOnce(Token, Token, Token),
    ) -> bool {
        unsafe {
            let (t0, t1) = match self.buffer_len {
                0 => {
                    let Some(t0) = self.ts.next() else {
                        return false;
                    };
                    if t0.kind != kind0 {
                        self.store_0(t0);
                        self.buffer_len = 1;
                        return false;
                    }
                    let Some(t1) = self.ts.next() else {
                        return false;
                    };
                    if t1.kind != kind1 {
                        self.store_01(t0, t1);
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
                        self.store_1(t1);
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
                    let t1 = self.get_buf_1();
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
                    let t1 = self.get_buf_1();
                    if t1.kind != kind1 {
                        return false;
                    }
                    self.advance_n_2_bl_3();
                    (t0, t1)
                }
                4 => {
                    let t0 = self.get_buf_0();
                    if t0.kind != kind0 {
                        return false;
                    }
                    let t1 = self.get_buf_1();
                    if t1.kind != kind1 {
                        return false;
                    }
                    self.advance_n_2_bl_4();
                    (t0, t1)
                }
                _ => core::hint::unreachable_unchecked(),
            };
            let token = t0.compose(t1, complete);
            push_token_evt(t0, t1, token);
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
                    self.store_0(t0);
                    self.buffer_len = 1;
                    if t0.kind != kind0 {
                        return false;
                    }
                    let Some(t1) = self.ts.next() else {
                        return false;
                    };
                    self.store_1(t1);
                    self.buffer_len = 2;
                    t1.kind == kind1
                }
                1 => {
                    if self.get_buf_0_k() != kind0 {
                        return false;
                    }
                    let Some(t1) = self.ts.next() else {
                        return false;
                    };
                    self.store_1(t1);
                    self.buffer_len = 2;
                    t1.kind == kind1
                }
                _ => self.get_buf_0_k() == kind0 && self.get_buf_1_k() == kind1,
            }
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn skip_whitespace_wc(&mut self, mut push_token_evt: impl FnMut(Token)) {
        let check_sk =
            |sk: SyntaxKind| -> bool { SyntaxKind::WHITESPACE == sk || SyntaxKind::COMMENT == sk };

        match self.buffer_len {
            0 => {
                let token = self.ts.skip_ws_wc();

                match token {
                    Some(t) => {
                        push_token_evt(t);
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
                        push_token_evt(t);
                    }
                    None => {
                        push_token_evt(t0.with_kind(SyntaxKind::COMPOSED_TRIVIA));
                    }
                }
            }
            2 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { self.get_buf_1() };
                if !check_sk(t1.kind) {
                    unsafe {
                        self.advance_n_1_bl_2();
                    }
                    push_token_evt(t0.with_kind(SyntaxKind::COMPOSED_TRIVIA));
                    return;
                }
                self.ts.restore_pos(t1.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wc();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                    None => {
                        let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                }
            }
            3 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { self.get_buf_1() };
                if !check_sk(t1.kind) {
                    unsafe {
                        self.advance_n_1_bl_3();
                    }
                    push_token_evt(t0.with_kind(SyntaxKind::COMPOSED_TRIVIA));
                    return;
                }
                let t2 = unsafe { self.get_buf_2() };
                if !check_sk(t2.kind) {
                    unsafe {
                        self.advance_n_2_bl_3();
                    }
                    let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t);
                    return;
                }
                self.ts.restore_pos(t2.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wc();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                    None => {
                        let t = unsafe { t0.compose(t2, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                }
            }
            4 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { self.get_buf_1() };
                if !check_sk(t1.kind) {
                    unsafe {
                        self.advance_n_1_bl_4();
                    }
                    push_token_evt(t0.with_kind(SyntaxKind::COMPOSED_TRIVIA));
                    return;
                }
                let t2 = unsafe { self.get_buf_2() };
                if !check_sk(t2.kind) {
                    unsafe {
                        self.advance_n_2_bl_4();
                    }
                    let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t);
                    return;
                }
                let t3 = unsafe { self.get_buf_3() };
                if !check_sk(t3.kind) {
                    unsafe {
                        self.advance_n_3_bl_4();
                    }
                    let t = unsafe { t0.compose(t2, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t);
                    return;
                }
                self.ts.restore_pos(t3.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wc();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                    None => {
                        let t = unsafe { t0.compose(t3, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                }
            }
            _ => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn skip_whitespace_wcn(&mut self, mut push_token_evt: impl FnMut(Token)) {
        let check_sk = |sk: SyntaxKind| -> bool {
            sk == SyntaxKind::WHITESPACE || sk == SyntaxKind::NEWLINE || sk == SyntaxKind::COMMENT
        };

        match self.buffer_len {
            0 => {
                let token = self.ts.skip_ws_wcn();

                match token {
                    Some(t) => {
                        push_token_evt(t);
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
                        push_token_evt(t);
                    }
                    None => {
                        push_token_evt(t0.with_kind(SyntaxKind::COMPOSED_TRIVIA));
                    }
                }
            }
            2 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { self.get_buf_1() };
                if !check_sk(t1.kind) {
                    unsafe {
                        self.advance_n_1_bl_2();
                    }
                    push_token_evt(t0.with_kind(SyntaxKind::COMPOSED_TRIVIA));
                    return;
                }
                self.ts.restore_pos(t1.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wcn();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                    None => {
                        let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                }
            }
            3 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { self.get_buf_1() };
                if !check_sk(t1.kind) {
                    unsafe {
                        self.advance_n_1_bl_3();
                    }
                    push_token_evt(t0.with_kind(SyntaxKind::COMPOSED_TRIVIA));
                    return;
                }
                let t2 = unsafe { self.get_buf_2() };
                if !check_sk(t2.kind) {
                    unsafe {
                        self.advance_n_2_bl_3();
                    }
                    let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t);
                    return;
                }
                self.ts.restore_pos(t2.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wcn();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                    None => {
                        let t = unsafe { t0.compose(t2, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                }
            }
            4 => {
                let t0 = unsafe { self.get_buf_0() };
                if !check_sk(t0.kind) {
                    return;
                }
                let t1 = unsafe { self.get_buf_1() };
                if !check_sk(t1.kind) {
                    unsafe {
                        self.advance_n_1_bl_4();
                    }
                    push_token_evt(t0.with_kind(SyntaxKind::COMPOSED_TRIVIA));
                    return;
                }
                let t2 = unsafe { self.get_buf_2() };
                if !check_sk(t2.kind) {
                    unsafe {
                        self.advance_n_2_bl_4();
                    }
                    let t = unsafe { t0.compose(t1, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t);
                    return;
                }
                let t3 = unsafe { self.get_buf_3() };
                if !check_sk(t3.kind) {
                    unsafe {
                        self.advance_n_3_bl_4();
                    }
                    let t = unsafe { t0.compose(t2, SyntaxKind::COMPOSED_TRIVIA) };
                    push_token_evt(t);
                    return;
                }
                self.ts.restore_pos(t3.span.end as usize);
                self.buffer_len = 0;

                let token = self.ts.skip_ws_wcn();

                match token {
                    Some(t) => {
                        let t = unsafe { t0.compose(t, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                    None => {
                        let t = unsafe { t0.compose(t3, SyntaxKind::COMPOSED_TRIVIA) };
                        push_token_evt(t);
                    }
                }
            }
            _ => unsafe { core::hint::unreachable_unchecked() },
        }
    }

    #[inline(always)]
    pub fn bump_until(&mut self, kind: SyntaxKind, mut push_token_evt: impl FnMut(Token)) -> bool {
        unsafe {
            match self.buffer_len {
                0 => {
                    while let Some(token) = self.ts.next() {
                        if token.kind == kind {
                            self.store_0(token);
                            self.buffer_len = 1;
                            return true;
                        }
                        push_token_evt(token);
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
                    push_token_evt(t0);
                    while let Some(token) = self.ts.next() {
                        if token.kind == kind {
                            self.store_0(token);
                            self.buffer_len = 1;
                            return true;
                        }
                        push_token_evt(token);
                    }
                    false
                }
                2 => {
                    let t0 = self.get_buf_0();
                    if t0.kind == kind {
                        return true;
                    }
                    push_token_evt(t0);
                    let t1 = self.get_buf_1();
                    if t1.kind == kind {
                        self.advance_n_1_bl_2();
                        return true;
                    } else {
                        self.buffer_len = 0;
                    }
                    push_token_evt(t1);
                    while let Some(token) = self.ts.next() {
                        if token.kind == kind {
                            self.store_0(token);
                            self.buffer_len = 1;
                            return true;
                        }
                        push_token_evt(token);
                    }
                    false
                }
                3 => {
                    let t0 = self.get_buf_0();
                    if t0.kind == kind {
                        return true;
                    }
                    push_token_evt(t0);
                    let t1 = self.get_buf_1();
                    if t1.kind == kind {
                        self.advance_n_1_bl_3();
                        return true;
                    }
                    push_token_evt(t1);
                    let t2 = self.get_buf_2();
                    if t2.kind == kind {
                        self.advance_n_2_bl_3();
                        return true;
                    } else {
                        self.buffer_len = 0;
                    }
                    push_token_evt(t2);
                    while let Some(token) = self.ts.next() {
                        if token.kind == kind {
                            self.store_0(token);
                            self.buffer_len = 1;
                            return true;
                        }
                        push_token_evt(token);
                    }
                    false
                }
                _ => core::hint::unreachable_unchecked(),
            }
        }
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
                2 => self.advance_n_1_bl_2(),
                3 => self.advance_n_1_bl_3(),
                4 => self.advance_n_1_bl_4(),
                _ => core::hint::unreachable_unchecked(),
            }
        }
    }
    #[track_caller]
    pub fn advance_n(&mut self, n: usize) {
        debug_assert!(0 < n && n <= 3);
        unsafe {
            match (n, self.buffer_len) {
                (1, 0) => {
                    let _ = self.ts.next();
                }
                (1, 1) => {
                    self.buffer_len = 0;
                }
                (1, 2) => {
                    self.advance_n_1_bl_2();
                }
                (1, 3) => {
                    self.advance_n_1_bl_3();
                }
                (1, 4) => {
                    self.advance_n_1_bl_4();
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
                    self.advance_n_2_bl_3();
                }
                (2, 4) => {
                    self.advance_n_2_bl_4();
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
                    self.advance_n_3_bl_4();
                }
                (_, _) => core::hint::unreachable_unchecked(),
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
                    let t1 = self.get_buf_1();
                    if t0.span.start == pos_u32 {
                        self.ts.restore_pos(t1.span.end as usize);
                        return;
                    } else if t1.span.start == pos_u32 {
                        self.ts.restore_pos(t1.span.end as usize);
                        self.store_0(t1);
                        self.buffer_len = 1;
                        return;
                    }

                    self.ts.restore_pos(pos);
                    self.buffer_len = 0;
                }
                3 => {
                    let t0 = self.get_buf_0();
                    let t1 = self.get_buf_1();
                    let t2 = self.get_buf_2();
                    if t0.span.start == pos_u32 {
                        self.ts.restore_pos(t2.span.end as usize);
                        return;
                    } else if t1.span.start == pos_u32 {
                        self.ts.restore_pos(t2.span.end as usize);
                        self.store_01(t1, t2);
                        self.buffer_len = 2;
                        return;
                    } else if t2.span.start == pos_u32 {
                        self.ts.restore_pos(t2.span.end as usize);
                        self.store_0(t2);
                        self.buffer_len = 1;
                        return;
                    }

                    self.ts.restore_pos(pos);
                    self.buffer_len = 0;
                }
                4 => {
                    let t0 = self.get_buf_0();
                    let t1 = self.get_buf_1();
                    let t2 = self.get_buf_2();
                    let t3 = self.get_buf_3();
                    if t0.span.start == pos_u32 {
                        self.ts.restore_pos(t3.span.end as usize);
                        return;
                    } else if t1.span.start == pos_u32 {
                        self.ts.restore_pos(t3.span.end as usize);
                        self.store_012(t1, t2, t3);
                        self.buffer_len = 3;
                        return;
                    } else if t2.span.start == pos_u32 {
                        self.ts.restore_pos(t3.span.end as usize);
                        self.store_01(t2, t3);
                        self.buffer_len = 2;
                        return;
                    } else if t3.span.start == pos_u32 {
                        self.ts.restore_pos(t3.span.end as usize);
                        self.store_0(t3);
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

// Buffers manipulation convenience methods
impl<'l, T> BufferedTokenSource<'l, T>
where
    T: TokenSource<'l>,
{
    #[inline(always)]
    unsafe fn get_buf_0(&self) -> Token {
        Token {
            kind: self.get_buf_0_k(),
            span: self.get_buf_0_span(),
        }
    }

    #[inline(always)]
    unsafe fn get_buf_1(&self) -> Token {
        Token {
            kind: self.get_buf_1_k(),
            span: self.get_buf_1_span(),
        }
    }

    #[inline(always)]
    unsafe fn get_buf_2(&self) -> Token {
        Token {
            kind: self.get_buf_2_k(),
            span: self.get_buf_2_span(),
        }
    }

    #[inline(always)]
    unsafe fn get_buf_3(&self) -> Token {
        Token {
            kind: self.get_buf_3_k(),
            span: self.get_buf_3_span(),
        }
    }

    #[inline(always)]
    unsafe fn get_buf_0_k(&self) -> SyntaxKind {
        self.buffer_kinds[0]
    }

    #[inline(always)]
    unsafe fn get_buf_1_k(&self) -> SyntaxKind {
        self.buffer_kinds[1]
    }

    #[inline(always)]
    unsafe fn get_buf_2_k(&self) -> SyntaxKind {
        self.buffer_kinds[2]
    }

    #[inline(always)]
    unsafe fn get_buf_3_k(&self) -> SyntaxKind {
        self.buffer_kinds[3]
    }

    #[inline(always)]
    unsafe fn get_buf_0_span(&self) -> TextSpan {
        let [start, end, _, _, _] = self.buffer_spans;
        TextSpan::new_unchecked(start, end)
    }

    #[inline(always)]
    unsafe fn get_buf_1_span(&self) -> TextSpan {
        let [_, start, end, _, _] = self.buffer_spans;
        TextSpan::new_unchecked(start, end)
    }

    #[inline(always)]
    unsafe fn get_buf_2_span(&self) -> TextSpan {
        let [_, _, start, end, _] = self.buffer_spans;
        TextSpan::new_unchecked(start, end)
    }

    #[inline(always)]
    unsafe fn get_buf_3_span(&self) -> TextSpan {
        let [_, _, _, start, end] = self.buffer_spans;
        TextSpan::new_unchecked(start, end)
    }

    #[inline(always)]
    unsafe fn advance_n_1_bl_2(&mut self) {
        self.buffer_spans[0] = self.buffer_spans[1];
        self.buffer_spans[1] = self.buffer_spans[2];

        self.buffer_kinds[0] = self.buffer_kinds[1];

        self.buffer_len = 1;
    }

    #[inline(always)]
    unsafe fn advance_n_1_bl_3(&mut self) {
        self.buffer_spans[0] = self.buffer_spans[1];
        self.buffer_spans[1] = self.buffer_spans[2];
        self.buffer_spans[2] = self.buffer_spans[3];

        self.buffer_kinds[0] = self.buffer_kinds[1];
        self.buffer_kinds[1] = self.buffer_kinds[2];

        self.buffer_len = 2;
    }

    #[inline(always)]
    unsafe fn advance_n_2_bl_3(&mut self) {
        self.buffer_spans[0] = self.buffer_spans[2];
        self.buffer_spans[1] = self.buffer_spans[3];

        self.buffer_kinds[0] = self.buffer_kinds[2];

        self.buffer_len = 1;
    }

    #[inline(always)]
    unsafe fn advance_n_1_bl_4(&mut self) {
        self.buffer_spans[0] = self.buffer_spans[1];
        self.buffer_spans[1] = self.buffer_spans[2];
        self.buffer_spans[2] = self.buffer_spans[3];
        self.buffer_spans[3] = self.buffer_spans[4];

        self.buffer_kinds[0] = self.buffer_kinds[1];
        self.buffer_kinds[1] = self.buffer_kinds[2];
        self.buffer_kinds[2] = self.buffer_kinds[3];

        self.buffer_len = 3;
    }

    #[inline(always)]
    unsafe fn advance_n_2_bl_4(&mut self) {
        self.buffer_spans[0] = self.buffer_spans[2];
        self.buffer_spans[1] = self.buffer_spans[3];
        self.buffer_spans[2] = self.buffer_spans[4];

        self.buffer_kinds[0] = self.buffer_kinds[2];
        self.buffer_kinds[1] = self.buffer_kinds[3];

        self.buffer_len = 2;
    }

    #[inline(always)]
    unsafe fn advance_n_3_bl_4(&mut self) {
        self.buffer_spans[0] = self.buffer_spans[3];
        self.buffer_spans[1] = self.buffer_spans[4];

        self.buffer_kinds[0] = self.buffer_kinds[3];

        self.buffer_len = 1;
    }

    #[inline(always)]
    unsafe fn store_0(&mut self, t0: Token) {
        self.buffer_spans[0] = t0.span.start;
        self.buffer_spans[1] = t0.span.end;

        self.buffer_kinds[0] = t0.kind;
    }

    #[inline(always)]
    unsafe fn store_01(&mut self, t0: Token, t1: Token) {
        debug_assert_eq!(t0.span.end, t1.span.start);

        self.buffer_spans[0] = t0.span.start;
        self.buffer_spans[1] = t0.span.end;
        self.buffer_spans[2] = t1.span.end;

        self.buffer_kinds[0] = t0.kind;
        self.buffer_kinds[1] = t1.kind;
    }

    #[inline(always)]
    unsafe fn store_012(&mut self, t0: Token, t1: Token, t2: Token) {
        debug_assert_eq!(t0.span.end, t1.span.start);
        debug_assert_eq!(t1.span.end, t2.span.start);

        self.buffer_spans[0] = t0.span.start;
        self.buffer_spans[1] = t0.span.end;
        self.buffer_spans[2] = t1.span.end;
        self.buffer_spans[3] = t2.span.end;

        self.buffer_kinds[0] = t0.kind;
        self.buffer_kinds[1] = t1.kind;
        self.buffer_kinds[2] = t2.kind;
    }

    #[inline(always)]
    unsafe fn store_0123(&mut self, t0: Token, t1: Token, t2: Token, t3: Token) {
        debug_assert_eq!(t0.span.end, t1.span.start);
        debug_assert_eq!(t1.span.end, t2.span.start);
        debug_assert_eq!(t2.span.end, t3.span.start);

        self.buffer_spans[0] = t0.span.start;
        self.buffer_spans[1] = t0.span.end;
        self.buffer_spans[2] = t1.span.end;
        self.buffer_spans[3] = t2.span.end;
        self.buffer_spans[4] = t3.span.end;

        self.buffer_kinds[0] = t0.kind;
        self.buffer_kinds[1] = t1.kind;
        self.buffer_kinds[2] = t2.kind;
        self.buffer_kinds[3] = t3.kind;
    }

    #[inline(always)]
    unsafe fn store_1(&mut self, t1: Token) {
        debug_assert_eq!(t1.span.start, self.buffer_spans[1]);

        self.buffer_spans[2] = t1.span.end;

        self.buffer_kinds[1] = t1.kind;
    }

    #[inline(always)]
    unsafe fn store_12(&mut self, t1: Token, t2: Token) {
        debug_assert_eq!(t1.span.start, self.buffer_spans[1]);
        debug_assert_eq!(t1.span.end, t2.span.start);

        self.buffer_spans[2] = t1.span.end;
        self.buffer_spans[3] = t2.span.end;

        self.buffer_kinds[1] = t1.kind;
        self.buffer_kinds[2] = t2.kind;
    }

    #[inline(always)]
    unsafe fn store_123(&mut self, t1: Token, t2: Token, t3: Token) {
        debug_assert_eq!(t1.span.start, self.buffer_spans[1]);
        debug_assert_eq!(t1.span.end, t2.span.start);
        debug_assert_eq!(t2.span.end, t3.span.start);

        self.buffer_spans[2] = t1.span.end;
        self.buffer_spans[3] = t2.span.end;
        self.buffer_spans[4] = t3.span.end;

        self.buffer_kinds[1] = t1.kind;
        self.buffer_kinds[2] = t2.kind;
        self.buffer_kinds[3] = t3.kind;
    }

    #[inline(always)]
    unsafe fn store_2(&mut self, t2: Token) {
        debug_assert_eq!(t2.span.start, self.buffer_spans[2]);

        self.buffer_spans[3] = t2.span.end;

        self.buffer_kinds[2] = t2.kind;
    }

    #[inline(always)]
    unsafe fn store_23(&mut self, t2: Token, t3: Token) {
        debug_assert_eq!(t2.span.start, self.buffer_spans[2]);
        debug_assert_eq!(t2.span.end, t3.span.start);

        self.buffer_spans[3] = t2.span.end;
        self.buffer_spans[4] = t3.span.end;

        self.buffer_kinds[2] = t2.kind;
        self.buffer_kinds[3] = t3.kind;
    }

    #[inline(always)]
    unsafe fn store_3(&mut self, t3: Token) {
        debug_assert_eq!(t3.span.start, self.buffer_spans[3]);

        self.buffer_spans[4] = t3.span.end;

        self.buffer_kinds[3] = t3.kind;
    }
}
