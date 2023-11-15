use std::ops::RangeInclusive;

use crate::syntax_kind::SyntaxKind;
use crate::text_span::TextSpan;
use crate::token_source::{Token, TokenError, TokenSource};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TextTokenSource<'text> {
    text: &'text str,
    pos: usize,
    error: Option<TokenError>,
}

impl<'text> TextTokenSource<'text> {
    pub fn new(text: &'text str) -> Self {
        assert!(text.len() <= u32::MAX as usize);
        Self {
            text,
            pos: 0,
            error: None,
        }
    }

    #[inline(always)]
    fn next_token(&mut self) -> Option<Token> {
        if self.pos >= self.text.len() {
            return None;
        }

        let to_parse = &self.text[self.pos..];
        let (token, advanced, error) = Self::parse_one_token(to_parse);
        let token = token.add_offset(unsafe { self.pos.try_into().unwrap_unchecked() });
        self.pos += advanced;
        self.error = error;
        Some(token)
    }

    #[inline(always)]
    fn parse_one_token(s: &str) -> (Token, usize, Option<TokenError>) {
        let mut parser = CharTokenParser::new(s);
        let (token, advance) = parser.parse_one_token();
        (token, advance, parser.error)
    }

    #[inline(always)]
    fn ws_wc_skipped(&mut self) -> Option<Token> {
        if self.pos >= self.text.len() {
            return None;
        }

        let to_parse = &self.text[self.pos..];

        match to_parse.chars().next() {
            Some(' ' | '\r' | '\t' | '/') => {}
            _ => return None,
        }

        let (token, advanced, error) = Self::parse_ws_wc(to_parse);
        let token = token.add_offset(unsafe { self.pos.try_into().unwrap_unchecked() });
        self.pos += advanced;
        self.error = error;
        Some(token)
    }

    #[inline(always)]
    fn ws_wcn_skipped(&mut self) -> Option<Token> {
        if self.pos >= self.text.len() {
            return None;
        }

        let to_parse = &self.text[self.pos..];
        match to_parse.chars().next() {
            Some(' ' | '\r' | '\t' | '/' | '\n') => {}
            _ => return None,
        }
        let (token, advanced, error) = Self::parse_ws_wcn(to_parse);
        let token = token.add_offset(unsafe { self.pos.try_into().unwrap_unchecked() });
        self.pos += advanced;
        self.error = error;
        Some(token)
    }

    #[inline(always)]
    fn parse_ws_wc(s: &str) -> (Token, usize, Option<TokenError>) {
        let mut parser = CharTokenParser::new(s);
        let (token, advance) = parser.parse_ws_wc();
        (token, advance, parser.error)
    }

    #[inline(always)]
    fn parse_ws_wcn(s: &str) -> (Token, usize, Option<TokenError>) {
        let mut parser = CharTokenParser::new(s);
        let (token, advance) = parser.parse_ws_wcn();
        (token, advance, parser.error)
    }
}

impl<'text> TokenSource<'text> for TextTokenSource<'text> {
    fn next(&mut self) -> Option<Token> {
        self.next_token()
    }

    fn skip_ws_wc(&mut self) -> Option<Token> {
        self.ws_wc_skipped()
    }

    fn skip_ws_wcn(&mut self) -> Option<Token> {
        self.ws_wcn_skipped()
    }

    fn get_token_text(&self, token: &Token) -> &'text str {
        &self.text[token.span]
    }

    fn get_error(&self) -> Option<TokenError> {
        self.error
    }

    fn eof_span(&self) -> TextSpan {
        let l = self.text.len().try_into().unwrap();
        unsafe { TextSpan::new_unchecked(l, l) }
    }

    fn restore_pos(&mut self, pos: usize) {
        self.pos = pos;
    }
}

struct CharTokenParser<'text> {
    text: &'text str,
    chars: std::str::CharIndices<'text>,
    error: Option<TokenError>,
}

#[inline(always)]
fn r(kind: SyntaxKind, start: usize, end: usize) -> (Token, usize) {
    let s = unsafe { start.try_into().unwrap_unchecked() };
    let e = unsafe { end.try_into().unwrap_unchecked() };
    (
        Token {
            kind,
            span: unsafe { TextSpan::new_unchecked(s, e) },
        },
        end as usize,
    )
}

#[inline(always)]
fn r1(kind: SyntaxKind, start: usize) -> (Token, usize) {
    r(kind, start, start + 1)
}

impl<'text> CharTokenParser<'text> {
    #[inline(always)]
    fn new(text: &'text str) -> Self {
        narxia_log::d!("To parse: {text:?}");
        Self {
            text,
            chars: text.char_indices(),
            error: None,
        }
    }

    #[inline(always)]
    fn consume_all<const NC: usize, const NR: usize>(
        &mut self,
        chars: [char; NC],
        ranges: [RangeInclusive<char>; NR],
    ) -> usize {
        let end;

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

    #[inline(always)]
    fn consume_until<const NC: usize, const NR: usize>(
        &mut self,
        chars: [char; NC],
        ranges: [RangeInclusive<char>; NR],
    ) -> usize {
        let end;

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

    #[inline(always)]
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
                    "for" => SyntaxKind::FOR_KW,
                    "in" => SyntaxKind::IN_KW,
                    "break" => SyntaxKind::BREAK_KW,
                    "continue" => SyntaxKind::CONTINUE_KW,
                    "return" => SyntaxKind::RETURN_KW,
                    "true" => SyntaxKind::TRUE_KW,
                    "false" => SyntaxKind::FALSE_KW,
                    "const" => SyntaxKind::CONST_KW,
                    _ => SyntaxKind::IDENT,
                };
                r(kind, start, end)
            }
            '0' => match self.chars.next() {
                Some((_next, 'x')) => {
                    let end = self.consume_all(['_'], ['0'..='9', 'a'..='f', 'A'..='F']);
                    r(SyntaxKind::NUM_HEX, start, end)
                }
                Some((_next, 'b')) => {
                    let end = self.consume_all(['_', '0', '1'], []);
                    r(SyntaxKind::NUM_BIN, start, end)
                }
                Some((_next, '0'..='7')) => {
                    let end = self.consume_all(['_'], ['0'..='7']);
                    r(SyntaxKind::NUM_OCT, start, end)
                }
                Some((_, _)) | None => r1(SyntaxKind::NUM_DEC, start),
            },
            '1'..='9' => {
                let end = self.consume_all(['_'], ['0'..='9']);
                r(SyntaxKind::NUM_DEC, start, end)
            }
            ' ' | '\t' | '\r' => {
                let end = self.consume_all([' ', '\t', '\r'], []);
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
            '&' => r1(SyntaxKind::AMP, start),
            '|' => r1(SyntaxKind::PIPE, start),
            '^' => r1(SyntaxKind::CARET, start),
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

    #[inline(always)]
    fn parse_ws_wc(&mut self) -> (Token, usize) {
        'main: {
            let s = self.chars.offset();
            loop {
                let Some((start, c)) = self.chars.next() else {
                    break 'main r(SyntaxKind::COMPOSED_TRIVIA, s, self.chars.offset());
                };
                match c {
                    ' ' | '\t' | '\r' => {}
                    '/' => {
                        match self.chars.next() {
                            Some((_, '/')) => {
                                let end = self.consume_until(['\n'], []);
                                // here if we got to the end we should stop either way, since we ran into a \n
                                break 'main r(SyntaxKind::COMPOSED_TRIVIA, s, end);
                            }
                            Some((_, '*')) => loop {
                                let Some((_, c)) = self.chars.next() else {
                                    self.error = Some(TokenError::BlockCommentNotClosed);
                                    break 'main r1(SyntaxKind::ERROR, self.chars.offset());
                                };
                                if c == '*' {
                                    if let Some((_next, '/')) = self.chars.next() {
                                        break;
                                    }
                                }
                            },
                            _ => break 'main r(SyntaxKind::COMPOSED_TRIVIA, s, start),
                        }
                    }
                    _ => {
                        break 'main r(SyntaxKind::COMPOSED_TRIVIA, s, start);
                    }
                }
            }
        }
    }

    #[inline(always)]
    fn parse_ws_wcn(&mut self) -> (Token, usize) {
        'main: {
            let s = self.chars.offset();
            loop {
                let Some((start, c)) = self.chars.next() else {
                    break 'main r(SyntaxKind::COMPOSED_TRIVIA, s, self.chars.offset());
                };
                match c {
                    ' ' | '\t' | '\r' => {}
                    '\n' => {}
                    '/' => {
                        match self.chars.next() {
                            Some((_, '/')) => {
                                // here consuming the \n from the thing is alright
                                let _end = self.consume_until(['\n'], []);
                            }
                            Some((_, '*')) => loop {
                                let Some((_, c)) = self.chars.next() else {
                                    self.error = Some(TokenError::BlockCommentNotClosed);
                                    break 'main r1(SyntaxKind::ERROR, self.chars.offset());
                                };
                                if c == '*' {
                                    if let Some((_next, '/')) = self.chars.next() {
                                        break;
                                    }
                                }
                            },
                            _ => break 'main r(SyntaxKind::COMPOSED_TRIVIA, s, start),
                        }
                    }
                    _ => {
                        break 'main r(SyntaxKind::COMPOSED_TRIVIA, s, start);
                    }
                }
            }
        }
    }
}
