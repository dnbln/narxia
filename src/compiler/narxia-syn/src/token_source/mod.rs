//! Token source module.
//!
//! This module contains the [`TokenSource`] trait and related types.
//!
//! A [`Token`] is a part of the source text. It has a [`SyntaxKind`] and a [`TextSpan`].

use std::fmt;
use std::fmt::Formatter;
use std::num::NonZeroU64;

use owo_colors::OwoColorize;

use crate::language::NarxiaLanguage;
use crate::parser::ParserDbgStyling;
use crate::syntax_kind::SyntaxKind;
use crate::text_span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct TokenRepr {
    /// upper 32 bits => span start
    /// bits 32..48 => span length
    /// bits 48..64 => SyntaxKind
    repr: NonZeroU64,
}

impl TokenRepr {
    #[inline(always)]
    pub(crate) fn new(kind: SyntaxKind, span: TextSpan) -> Self {
        #[expect(unsafe_code)]
        Self {
            repr: unsafe {
                NonZeroU64::new_unchecked(
                    ((span.start as u64) << 32)
                        | (((span.end - span.start) as u64) << 16)
                        | (kind as u64),
                )
            },
        }
    }

    #[inline(always)]
    #[expect(unsafe_code)]
    pub fn kind(self) -> SyntaxKind {
        NarxiaLanguage::kind_from_u16(unsafe {
            u16::try_from(self.kind_value()).unwrap_unchecked()
        })
    }

    #[inline(always)]
    fn kind_value(self) -> u64 {
        self.repr.get() & 0xFFFF
    }

    #[inline(always)]
    pub fn kind_is(self, kind: SyntaxKind) -> bool {
        self.repr.get() & 0xFFFF == kind as u64
    }

    #[inline(always)]
    pub fn kind_is_any<const N: usize>(self, kinds: [SyntaxKind; N]) -> bool {
        let kv = self.kind_value();
        kinds.into_iter().any(|k| k as u64 == kv)
    }

    #[inline(always)]
    pub fn with_kind(self, kind: SyntaxKind) -> Self {
        #[expect(unsafe_code)]
        TokenRepr {
            repr: unsafe {
                NonZeroU64::new_unchecked((self.repr.get() & (!0xFFFF)) | (kind as u64))
            },
        }
    }

    #[inline(always)]
    #[expect(unsafe_code)]
    pub fn span(self) -> TextSpan {
        let repr = self.repr.get();
        let start = Self::span_start_from_repr(repr);
        let end = start + Self::span_len_from_repr(repr);
        unsafe { TextSpan::new_unchecked(start, end) }
    }

    #[inline(always)]
    pub fn span_start(self) -> u32 {
        let repr = self.repr.get();
        Self::span_start_from_repr(repr)
    }

    #[inline(always)]
    #[expect(unsafe_code)]
    fn span_start_from_repr(repr: u64) -> u32 {
        unsafe { u32::try_from((repr & 0xFFFFFFFF00000000) >> 32).unwrap_unchecked() }
    }

    #[inline(always)]
    #[expect(unsafe_code)]
    fn span_len_from_repr(repr: u64) -> u32 {
        unsafe { u32::try_from((repr & 0xFFFF0000) >> 16).unwrap_unchecked() }
    }

    #[inline(always)]
    #[expect(unsafe_code)]
    pub(crate) unsafe fn compose(self, other: TokenRepr, kind: SyntaxKind) -> TokenRepr {
        TokenRepr::new(kind, TextSpan::new(self.span().start, other.span().end))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    kind: SyntaxKind,
    span: TextSpan,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{}", self.kind(), self.span())
    }
}

impl Token {
    fn add_offset(self, offset: u32) -> Self {
        Self {
            kind: self.kind,
            span: self.span.add_offset(offset),
        }
    }

    pub(crate) fn repr(self) -> TokenRepr {
        TokenRepr::new(self.kind, self.span)
    }

    pub(crate) fn from_repr(repr: TokenRepr) -> Self {
        Self {
            kind: repr.kind(),
            span: repr.span(),
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

    #[expect(unsafe_code)]
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
            format_args!("{:?}", self.0.kind).style(self.1.token_kind),
            self.0.span.style(self.1.token_span),
        )
    }
}

pub(crate) mod buffered_ts;

pub trait TokenSource<'l> {
    #[track_caller]
    fn next(&mut self) -> Option<TokenRepr>;

    #[track_caller]
    fn skip_ws_wc(&mut self) -> Option<TokenRepr>;
    #[track_caller]
    fn skip_ws_wcn(&mut self) -> Option<TokenRepr>;

    fn get_span_text(&self, span: TextSpan) -> &'l str;
    fn get_error(&self) -> Option<TokenError> {
        None
    }
    fn eof_span(&self) -> TextSpan;
    fn restore_pos(&mut self, pos: u32);
    fn set_parser_state(&mut self, state: TokParserState);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokParserState {
    Normal,
    InStringLiteral,
}

pub(crate) struct DynTsContainer<'l>(pub &'l mut dyn TokenSource<'l>);

impl<'l> TokenSource<'l> for DynTsContainer<'l> {
    #[inline(always)]
    fn next(&mut self) -> Option<TokenRepr> {
        self.0.next()
    }

    #[inline(always)]
    fn skip_ws_wc(&mut self) -> Option<TokenRepr> {
        self.0.skip_ws_wc()
    }

    #[inline(always)]
    fn skip_ws_wcn(&mut self) -> Option<TokenRepr> {
        self.0.skip_ws_wcn()
    }

    #[inline(always)]
    fn get_span_text(&self, span: TextSpan) -> &'l str {
        self.0.get_span_text(span)
    }

    #[inline(always)]
    fn eof_span(&self) -> TextSpan {
        self.0.eof_span()
    }

    #[inline(always)]
    fn restore_pos(&mut self, pos: u32) {
        self.0.restore_pos(pos)
    }

    #[inline(always)]
    fn set_parser_state(&mut self, state: TokParserState) {
        self.0.set_parser_state(state)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenError {
    UnexpectedChar(char),
    BlockCommentNotClosed,
    StringNotClosed,
}

pub mod text_ts;
