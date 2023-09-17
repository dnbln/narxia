use crate::syntax_kind::SyntaxKind;
use crate::text_span::TextSpan;

#[derive(Debug, Clone)]
pub struct ParseError {
    info: ParseErrorInfo,
    at: TextSpan,
}

impl ParseError {
    pub(crate) fn new(info: ParseErrorInfo, at: TextSpan) -> Self {
        Self { info, at }
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
    ExpectedKind(SyntaxKind),
    UnexpectedToken { got: SyntaxKind },
}
