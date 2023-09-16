#![feature(char_indices_offset)]

pub extern crate narxia_log;

pub mod language;
pub mod parse_error;
pub mod parser;
pub mod syntax_kind;
pub mod syntree;
pub mod text_span;
pub mod token_source;

pub(crate) use syntax_kind::SyntaxKind;
