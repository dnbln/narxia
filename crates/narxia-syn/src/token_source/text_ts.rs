//! Text token source.
//!
//! This module contains the implementation of a token source that reads tokens from a text.

use std::ops::RangeInclusive;
use std::str::CharIndices;

use super::{TokParserState, TokenRepr};
use crate::syntax_kind::SyntaxKind;
use crate::syntax_kind::T;
use crate::text_span::TextSpan;
use crate::token_source::Token;
use crate::token_source::TokenError;
use crate::token_source::TokenSource;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TextTokenSource<'text> {
    text: &'text str,
    pos: usize,
    error: Option<TokenError>,
    state: TokParserState,
}

#[expect(unsafe_code)]
mod danger {
    use super::TextTokenSource;
    use crate::token_source::TokenRepr;

    impl TextTokenSource<'_> {
        #[inline(always)]
        pub fn next_token(&mut self) -> Option<TokenRepr> {
            if self.pos >= self.text.len() {
                return None;
            }

            let to_parse = &self.text[self.pos..];
            let (token, advanced, error) = Self::parse_one_token(self.state, to_parse);
            let token = token.add_offset(unsafe { self.pos.try_into().unwrap_unchecked() });
            self.pos += advanced;
            self.error = error;
            Some(token.repr())
        }

        #[inline(always)]
        pub fn ws_wc_skipped(&mut self) -> Option<TokenRepr> {
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
            Some(token.repr())
        }

        #[inline(always)]
        pub fn ws_wcn_skipped(&mut self) -> Option<TokenRepr> {
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
            Some(token.repr())
        }
    }
}

impl<'text> TextTokenSource<'text> {
    pub fn new(text: &'text str) -> Self {
        assert!(text.len() <= u32::MAX as usize);
        Self {
            text,
            pos: 0,
            error: None,
            state: TokParserState::Normal,
        }
    }

    #[inline(always)]
    fn parse_one_token(state: TokParserState, s: &str) -> (Token, usize, Option<TokenError>) {
        match state {
            TokParserState::Normal => {
                let mut parser = CharTokenParser::new(s);
                let (token, advance) = parser.parse_one_token();
                (token, advance, parser.error)
            }
            TokParserState::InStringLiteral => {
                let mut parser = CharInStringTokenParser::new(s);
                let (token, advance) = parser.parse_one_token();
                (token, advance, parser.error)
            }
        }
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

#[expect(unsafe_code)]
impl<'text> TokenSource<'text> for TextTokenSource<'text> {
    fn next(&mut self) -> Option<TokenRepr> {
        self.next_token()
    }

    fn skip_ws_wc(&mut self) -> Option<TokenRepr> {
        self.ws_wc_skipped()
    }

    fn skip_ws_wcn(&mut self) -> Option<TokenRepr> {
        self.ws_wcn_skipped()
    }

    fn get_span_text(&self, span: TextSpan) -> &'text str {
        &self.text[span]
    }

    fn get_error(&self) -> Option<TokenError> {
        self.error
    }

    fn eof_span(&self) -> TextSpan {
        let l = self.text.len().try_into().unwrap();
        unsafe { TextSpan::new_unchecked(l, l) }
    }

    fn restore_pos(&mut self, pos: u32) {
        self.pos = pos as usize;
    }

    fn set_parser_state(&mut self, state: TokParserState) {
        self.state = state;
    }
}

struct CharTokenParser<'text> {
    text: &'text str,
    chars: CharIndices<'text>,
    error: Option<TokenError>,
}

#[expect(unsafe_code)]
#[inline(always)]
fn r(kind: SyntaxKind, start: usize, end: usize) -> (Token, usize) {
    let s = unsafe { start.try_into().unwrap_unchecked() };
    let e = unsafe { end.try_into().unwrap_unchecked() };
    (
        Token {
            kind,
            span: unsafe { TextSpan::new_unchecked(s, e) },
        },
        end,
    )
}

#[inline(always)]
fn r1(kind: SyntaxKind, start: usize) -> (Token, usize) {
    r(kind, start, start + 1)
}

#[inline(always)]
fn consume_all<const NC: usize, const NR: usize>(
    c: &mut CharIndices,
    chars: [char; NC],
    ranges: [RangeInclusive<char>; NR],
) -> usize {
    let end;

    loop {
        let Some((next, c)) = c.next() else {
            end = c.offset();
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
    c: &mut CharIndices,
    chars: [char; NC],
    ranges: [RangeInclusive<char>; NR],
) -> usize {
    let end;

    loop {
        let Some((next, c)) = c.next() else {
            end = c.offset();
            break;
        };
        if chars.contains(&c) || ranges.iter().any(|r| r.contains(&c)) {
            end = next;
            break;
        }
    }

    end
}

impl<'text> CharTokenParser<'text> {
    #[inline(always)]
    fn new(text: &'text str) -> Self {
        Self {
            text,
            chars: text.char_indices(),
            error: None,
        }
    }

    #[inline(always)]
    fn parse_one_token(&mut self) -> (Token, usize) {
        let (start, c) = self.chars.next().unwrap();
        match c {
            'a'..='z' | 'A'..='Z' | '_' => {
                let end = consume_all(&mut self.chars, ['_'], ['a'..='z', 'A'..='Z', '0'..='9']);
                let t = &self.text[start..end];
                let kind = match t {
                    "module" => T![module],
                    "use" => T![use],
                    "as" => T![as],
                    "fn" => T![fn],
                    "let" => T![let],
                    "if" => T![if],
                    "else" => T![else],
                    "while" => T![while],
                    "loop" => T![loop],
                    "for" => T![for],
                    "in" => T![in],
                    "break" => T![break],
                    "continue" => T![continue],
                    "return" => T![return],
                    "true" => T![true],
                    "false" => T![false],
                    "const" => T![const],
                    "mut" => T![mut],
                    _ => T![ident],
                };
                r(kind, start, end)
            }
            '0' => match self.chars.next() {
                Some((_next, 'x')) => {
                    let end =
                        consume_all(&mut self.chars, ['_'], ['0'..='9', 'a'..='f', 'A'..='F']);
                    r(SyntaxKind::NUM_HEX, start, end)
                }
                Some((_next, 'b')) => {
                    let end = consume_all(&mut self.chars, ['_', '0', '1'], []);
                    r(SyntaxKind::NUM_BIN, start, end)
                }
                Some((_next, '0'..='7')) => {
                    let end = consume_all(&mut self.chars, ['_'], ['0'..='7']);
                    r(SyntaxKind::NUM_OCT, start, end)
                }
                Some((_, _)) | None => r1(SyntaxKind::NUM_DEC, start),
            },
            '1'..='9' => {
                let end = consume_all(&mut self.chars, ['_'], ['0'..='9']);
                r(SyntaxKind::NUM_DEC, start, end)
            }
            ' ' | '\t' | '\r' => {
                let end = consume_all(&mut self.chars, [' ', '\t', '\r'], []);
                r(SyntaxKind::WHITESPACE, start, end)
            }
            '\n' => r1(SyntaxKind::NEWLINE, start),
            '#' => r1(T![#], start),
            '+' => r1(T![+], start),
            '-' => r1(T![-], start),
            '*' => r1(T![*], start),
            '/' => {
                let (end, kind) = match self.chars.next() {
                    Some((_, '/')) => {
                        let end = consume_until(&mut self.chars, ['\n'], []);
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
                    _ => (start + 1, T![/]),
                };
                r(kind, start, end)
            }
            '%' => r1(T![%], start),
            '=' => r1(T![=], start),
            '!' => r1(T![!], start),
            '<' => r1(T![<], start),
            '>' => r1(T![>], start),
            '(' => r1(T!['('], start),
            ')' => r1(T![')'], start),
            '{' => r1(T!['{'], start),
            '}' => r1(T!['}'], start),
            '[' => r1(T!['['], start),
            ']' => r1(T![']'], start),
            ';' => r1(T![;], start),
            ':' => r1(T![:], start),
            ',' => r1(T![,], start),
            '.' => r1(T![.], start),
            '&' => r1(T![&], start),
            '|' => r1(T![|], start),
            '^' => r1(T![^], start),
            '"' => {
                // let mut escaped = false;
                // let end = loop {
                //     let Some((next, c)) = self.chars.next() else {
                //         self.error = Some(TokenError::StringNotClosed);
                //         break self.chars.offset();
                //     };
                //     if c == '"' && !escaped {
                //         break next + 1;
                //     }
                //     if c == '\\' {
                //         escaped = !escaped;
                //     } else {
                //         escaped = false;
                //     }
                // };
                // r(SyntaxKind::STRING, start, end)
                r1(SyntaxKind::BEGIN_STRING, start)
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
                                let end = consume_until(&mut self.chars, ['\n'], []);
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
                                let _end = consume_until(&mut self.chars, ['\n'], []);
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

struct CharInStringTokenParser<'text> {
    text: &'text str,
    chars: CharIndices<'text>,
    error: Option<TokenError>,
}

impl<'text> CharInStringTokenParser<'text> {
    #[inline(always)]
    fn new(text: &'text str) -> Self {
        Self {
            text,
            chars: text.char_indices(),
            error: None,
        }
    }

    #[inline(always)]
    fn parse_one_token(&mut self) -> (Token, usize) {
        let (start, c) = self.chars.next().unwrap();
        match c {
            '"' => r1(SyntaxKind::END_STRING, start),
            '\\' => {
                let Some((next, c)) = self.chars.next() else {
                    self.error = Some(TokenError::StringNotClosed);
                    return r1(SyntaxKind::ERROR, start);
                };
                match c {
                    'x' => {
                        let end =
                            consume_all(&mut self.chars, [], ['0'..='9', 'a'..='f', 'A'..='F']);
                        r(SyntaxKind::StringLiteralFragEscapeSequenceToken, start, end)
                    }
                    _ => r(
                        SyntaxKind::StringLiteralFragEscapedCharToken,
                        start,
                        next + 1,
                    ),
                }
            }
            '$' => {
                let Some((next, c)) = self.chars.next() else {
                    self.error = Some(TokenError::StringNotClosed);
                    return r1(SyntaxKind::ERROR, start);
                };

                match c {
                    '{' => r1(SyntaxKind::StringLiteralFragDisplayToken, start),
                    '?' => r(SyntaxKind::StringLiteralFragDebugToken, start, next + 1),
                    'a'..='z' | 'A'..='Z' | '_' => {
                        r1(SyntaxKind::StringLiteralFragDisplayToken, start)
                    }
                    _ => {
                        self.error = Some(TokenError::UnexpectedChar(c));
                        r1(SyntaxKind::ERROR, start)
                    }
                }
            }
            _ => {
                let end = consume_until(&mut self.chars, ['\\', '"', '$'], []);
                r(SyntaxKind::StringLiteralFragTextPartToken, start, end)
            }
        }
    }
}
