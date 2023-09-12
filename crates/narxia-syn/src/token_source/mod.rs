use std::fmt;
use std::fmt::Formatter;
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
    fn add_offset(&self, offset: usize) -> Self {
        Self {
            kind: self.kind,
            span: self.span.add_offset(offset),
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn span(&self) -> TextSpan {
        self.span
    }

    pub(crate) fn set_kind(&mut self, kind: SyntaxKind) {
        self.kind = kind;
    }

    pub(crate) fn compose(self, other: Token) -> Token {
        debug_assert!(self.span.end == other.span.start);
        Token {
            kind: self.kind,
            span: TextSpan::new(self.span.start, other.span.end),
        }
    }
}

pub trait TokenSource<'l> {
    #[track_caller]
    fn next(&mut self) -> Option<Token> {
        let token = self.lookahead(0);
        if token.is_some() {
            self.advance();
        }
        token
    }

    #[track_caller]
    fn lookahead(&mut self, n: usize) -> Option<Token>;
    #[track_caller]
    fn advance(&mut self) {
        self.advance_n(1);
    }
    #[track_caller]
    fn advance_n(&mut self, n: usize);

    #[track_caller]
    fn compose_token(&mut self, kind: SyntaxKind, n: usize) -> Option<Token> {
        debug_assert!(n > 0);
        debug_assert!(n <= 3);
        let mut token = self.lookahead(0)?;
        for i in 1..n {
            token = token.compose(self.lookahead(i)?);
        }
        token.set_kind(kind);
        Some(token)
    }
    fn get_token_text(&self, token: &Token) -> &'l str;
    fn get_error(&self) -> Option<TokenError> {
        None
    }
    fn current_token_span(&mut self) -> TextSpan {
        self.lookahead(0).map(|tok| tok.span()).unwrap_or_else(|| self.eof_span())
    }
    fn eof_span(&self) -> TextSpan;
    fn current_pos(&mut self) -> usize {
        self.current_token_span().start
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenError {
    UnexpectedChar(char),
    BlockCommentNotClosed,
    StringNotClosed,
}

pub mod text_ts;
