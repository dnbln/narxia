#![feature(char_indices_offset)]

pub mod language;
pub mod syntax_kind;
pub mod syntree;
pub mod token_source;
pub mod text_span;
pub mod parser;

pub(crate) use syntax_kind::SyntaxKind;