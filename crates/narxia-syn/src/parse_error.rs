use std::panic;

use crate::syntax_kind::SyntaxKind;
use crate::text_span::TextSpan;

#[derive(Debug, Clone)]
pub struct ParseError {
    info: ParseErrorInfo,
    at: TextSpan,
    tkind: SyntaxKind,
    location: Option<&'static panic::Location<'static>>,
}

impl ParseError {
    pub(crate) fn new(
        info: ParseErrorInfo,
        at: TextSpan,
        tkind: SyntaxKind,
        location: Option<&'static panic::Location<'_>>,
    ) -> Self {
        Self {
            info,
            at,
            tkind,
            location,
        }
    }
    pub fn get_info(&self) -> &ParseErrorInfo {
        &self.info
    }

    pub fn get_at(&self) -> TextSpan {
        self.at
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrorInfo {
    ExpectedKind(SyntaxKind, &'static panic::Location<'static>),
    UnexpectedToken {
        got: SyntaxKind,
        at: &'static panic::Location<'static>,
    },
}
