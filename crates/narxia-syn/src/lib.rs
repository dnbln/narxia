#![feature(char_indices_offset)]
#![feature(let_chains)]
#![feature(closure_track_caller)]

pub extern crate narxia_log;

pub mod language;
pub mod parse_error;
pub mod parser;
pub mod syntax_kind;
pub mod syntree;
pub mod text_span;
pub mod token_source;
