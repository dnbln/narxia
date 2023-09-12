use std::ops::RangeInclusive;

use crate::syntax_kind::SyntaxKind;
use crate::text_span::TextSpan;
use crate::token_source::{Token, TokenError, TokenSource};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TextTokenSource<'text> {
    text: &'text str,
    pos: usize,
    buffer: Vec<Token>,
    error: Option<TokenError>,
}

impl<'text> TextTokenSource<'text> {
    pub fn new(text: &'text str) -> Self {
        Self {
            text,
            pos: 0,
            buffer: Vec::with_capacity(4),
            error: None,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        if self.pos >= self.text.len() {
            return None;
        }

        let to_parse = &self.text[self.pos..];
        let (token, advanced, error) = Self::parse_one_token(to_parse);
        let token = token.add_offset(self.pos);
        self.pos += advanced;
        self.error = error;
        Some(token)
    }

    fn parse_one_token(s: &str) -> (Token, usize, Option<TokenError>) {
        let mut parser = CharTokenParser::new(s);
        let (token, advance) = parser.parse_one_token();
        (token, advance, parser.error)
    }
}

impl<'text> TokenSource<'text> for TextTokenSource<'text> {
    fn lookahead(&mut self, n: usize) -> Option<Token> {
        debug_assert!(n <= 3);
        while n >= self.buffer.len() {
            let token = self.next_token()?;
            self.buffer.push(token);
        }
        Some(self.buffer[n])
    }

    fn advance_n(&mut self, n: usize) {
        debug_assert!(0 < n && n <= 3);
        let Some(_) = self.lookahead(n - 1) else { return; };
        self.buffer.drain(0..n);
    }

    fn get_token_text(&self, token: &Token) -> &'text str {
        &self.text[token.span]
    }

    fn get_error(&self) -> Option<TokenError> {
        self.error
    }

    fn eof_span(&self) -> TextSpan {
        TextSpan::new(self.text.len(), self.text.len())
    }
}

struct CharTokenParser<'text> {
    text: &'text str,
    chars: std::str::CharIndices<'text>,
    error: Option<TokenError>,
}

fn r(kind: SyntaxKind, start: usize, end: usize) -> (Token, usize) {
    (
        Token {
            kind,
            span: TextSpan { start, end },
        },
        end,
    )
}

fn r1(kind: SyntaxKind, start: usize) -> (Token, usize) {
    r(kind, start, start + 1)
}

impl<'text> CharTokenParser<'text> {
    fn new(text: &'text str) -> Self {
        Self {
            text,
            chars: text.char_indices(),
            error: None,
        }
    }

    fn consume_all<const NC: usize, const NR: usize>(
        &mut self,
        chars: [char; NC],
        ranges: [RangeInclusive<char>; NR],
    ) -> usize {
        let initial = self.chars.offset();
        let mut end = initial;

        loop {
            let Some((next, c)) = self.chars.next() else {
                end = self.chars.offset();
                break;
            };
            if !chars.contains(&c) && !ranges.iter().any(|r| r.contains(&c)) {
                end = next;
                break;
            }
        }

        end
    }

    fn consume_until<const NC: usize, const NR: usize>(
        &mut self,
        chars: [char; NC],
        ranges: [RangeInclusive<char>; NR],
    ) -> usize {
        let initial = self.chars.offset();
        let mut end = initial;

        loop {
            let Some((next, c)) = self.chars.next() else {
                end = self.chars.offset();
                break;
            };
            if chars.contains(&c) || ranges.iter().any(|r| r.contains(&c)) {
                end = next;
                break;
            }
        }

        end
    }

    fn parse_one_token(&mut self) -> (Token, usize) {
        let (start, c) = self.chars.next().unwrap();
        match c {
            'a'..='z' | 'A'..='Z' | '_' => {
                let end = self.consume_all(['_'], ['a'..='z', 'A'..='Z', '0'..='9']);
                let t = &self.text[start..end];
                let kind = match t {
                    "fn" => SyntaxKind::FN_KW,
                    "let" => SyntaxKind::LET_KW,
                    "if" => SyntaxKind::IF_KW,
                    "else" => SyntaxKind::ELSE_KW,
                    "while" => SyntaxKind::WHILE_KW,
                    "loop" => SyntaxKind::LOOP_KW,
                    "break" => SyntaxKind::BREAK_KW,
                    "continue" => SyntaxKind::CONTINUE_KW,
                    "return" => SyntaxKind::RETURN_KW,
                    "true" => SyntaxKind::TRUE_KW,
                    "false" => SyntaxKind::FALSE_KW,
                    _ => SyntaxKind::IDENT,
                };
                r(kind, start, end)
            }
            ' ' | '\t' => {
                let end = self.consume_all([' ', '\t'], []);
                r(SyntaxKind::WHITESPACE, start, end)
            }
            '\n' => r1(SyntaxKind::NEWLINE, start),
            '+' => r1(SyntaxKind::PLUS, start),
            '-' => r1(SyntaxKind::MINUS, start),
            '*' => r1(SyntaxKind::ASTERISK, start),
            '/' => {
                let (end, kind) = match self.chars.next() {
                    Some((_, '/')) => {
                        let end = self.consume_until(['\n'], []);
                        (end, SyntaxKind::COMMENT)
                    }
                    Some((_, '*')) => loop {
                        let Some((_, c)) = self.chars.next() else {
                            self.error = Some(TokenError::BlockCommentNotClosed);
                            break (self.chars.offset(), SyntaxKind::ERROR);
                        };
                        if c == '*' {
                            if let Some((next, '/')) = self.chars.next() {
                                break (next + 1, SyntaxKind::COMMENT);
                            }
                        }
                    },
                    _ => (start + 1, SyntaxKind::SLASH),
                };
                r(kind, start, end)
            }
            '%' => r1(SyntaxKind::PERCENT, start),
            '=' => r1(SyntaxKind::EQ, start),
            '!' => r1(SyntaxKind::BANG, start),
            '<' => r1(SyntaxKind::LT, start),
            '>' => r1(SyntaxKind::GT, start),
            '(' => r1(SyntaxKind::L_PAREN, start),
            ')' => r1(SyntaxKind::R_PAREN, start),
            '{' => r1(SyntaxKind::L_BRACE, start),
            '}' => r1(SyntaxKind::R_BRACE, start),
            '[' => r1(SyntaxKind::L_BRACK, start),
            ']' => r1(SyntaxKind::R_BRACK, start),
            ';' => r1(SyntaxKind::SEMI, start),
            ':' => r1(SyntaxKind::COLON, start),
            ',' => r1(SyntaxKind::COMMA, start),
            '.' => r1(SyntaxKind::DOT, start),
            '"' => {
                let mut escaped = false;
                let end = loop {
                    let Some((next, c)) = self.chars.next() else {
                        self.error = Some(TokenError::StringNotClosed);
                        break self.chars.offset();
                    };
                    if c == '"' && !escaped {
                        break next + 1;
                    }
                    if c == '\\' {
                        escaped = !escaped;
                    } else {
                        escaped = false;
                    }
                };
                r(SyntaxKind::STRING, start, end)
            }
            c => {
                let end = self.chars.offset();
                self.error = Some(TokenError::UnexpectedChar(c));
                r(SyntaxKind::ERROR, start, end)
            }
        }
    }
}
