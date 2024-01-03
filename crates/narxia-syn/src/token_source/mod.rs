use std::fmt;
use std::fmt::Formatter;

use crate::language::NarxiaLanguage;
use crate::parser::{ColorizeProcedure, ParserDbgStyling};
use crate::syntax_kind::SyntaxKind;
use crate::text_span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TokenRepr {
    /// upper 32 bits => span start
    /// bits 32..48 => span length
    /// bits 48..64 => SyntaxKind
    repr: u64,
}

impl TokenRepr {
    pub fn new(kind: SyntaxKind, span: TextSpan) -> Self {
        Self {
            repr: ((span.start as u64) << 32)
                | (((span.end - span.start) as u64) << 16)
                | (kind as u64),
        }
    }

    pub fn kind(self) -> SyntaxKind {
        NarxiaLanguage::kind_from_u16(unsafe {
            u16::try_from(self.repr & 0xFFFF).unwrap_unchecked()
        })
    }

    pub fn with_kind(self, kind: SyntaxKind) -> Self {
        TokenRepr {
            repr: (self.repr & (!0xFFFF)) | (kind as u64),
        }
    }

    pub fn span(self) -> TextSpan {
        let start =
            unsafe { u32::try_from((self.repr & 0xFFFFFFFF00000000) >> 32).unwrap_unchecked() };
        let end =
            start + unsafe { u32::try_from((self.repr & 0xFFFF0000) >> 16).unwrap_unchecked() };
        unsafe { TextSpan::new_unchecked(start, end) }
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

pub(crate) mod buffered_ts;

pub trait TokenSource<'l> {
    #[track_caller]
    fn next(&mut self) -> Option<Token>;

    #[track_caller]
    fn skip_ws_wc(&mut self) -> Option<Token>;
    #[track_caller]
    fn skip_ws_wcn(&mut self) -> Option<Token>;

    fn get_span_text(&self, span: TextSpan) -> &'l str;
    fn get_error(&self) -> Option<TokenError> {
        None
    }
    fn eof_span(&self) -> TextSpan;
    fn restore_pos(&mut self, pos: usize);
}

pub(crate) struct DynTsContainer<'l>(pub &'l mut dyn TokenSource<'l>);

impl<'l> TokenSource<'l> for DynTsContainer<'l> {
    #[inline(always)]
    fn next(&mut self) -> Option<Token> {
        self.0.next()
    }

    #[inline(always)]
    fn skip_ws_wc(&mut self) -> Option<Token> {
        self.0.skip_ws_wc()
    }

    #[inline(always)]
    fn skip_ws_wcn(&mut self) -> Option<Token> {
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
