use std::cell::RefCell;
use std::{fmt, panic};
use std::rc::Rc;
use ::std::thread;

use owo_colors::OwoColorize;

use crate::parser::ParserDbgStyling;
use crate::syntax_kind::SyntaxKind;

type ParseStackInternalRef = Rc<RefCell<ParseStackInternal>>;

#[must_use]
pub struct ParseStackGuard {
    internal: ParseStackInternalRef,
    pos: usize,
    call_at: &'static panic::Location<'static>,
}

impl Drop for ParseStackGuard {
    fn drop(&mut self) {
        let mut stack_internal = self.internal.borrow_mut();
        if self.pos != stack_internal.items.len() - 1 && !thread::panicking() {
            panic!(
                "ParseStackGuard: pos ({}) != len - 1 ({}) (called at [{}]):\n{}",
                self.pos,
                stack_internal.items.len() - 1,
                self.call_at,
                stack_internal.present(2, ParserDbgStyling::default())
            );
        }
        stack_internal.items.truncate(self.pos);
    }
}

#[derive(Copy, Clone)]
pub struct ParseStackItem {
    pub name: &'static str,
    pub can_recover: &'static [SyntaxKind],
    pub text_pos: usize,
}

struct ParseStackInternal {
    items: Vec<ParseStackItem>,
}

impl ParseStackInternal {
    fn present(&self, offset: usize, parser_styling: ParserDbgStyling) -> ParseStackPresenter<'_> {
        ParseStackPresenter {
            internal: self,
            parser_styling,
            offset,
        }
    }
}

pub struct ParseStackPresenter<'a> {
    internal: &'a ParseStackInternal,
    offset: usize,
    parser_styling: ParserDbgStyling,
}

impl fmt::Display for ParseStackPresenter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, item) in self.internal.items.iter().rev().enumerate() {
            writeln!(
                f,
                "{:offset$}{} {} (@{})",
                "",
                format_args!("[-{index:3}]").style(self.parser_styling.stack_offset),
                item.name.style(self.parser_styling.stack_fn_name),
                format_args!("{}", item.text_pos).style(self.parser_styling.token_stream_position),
                offset = self.offset
            )?;
        }

        Ok(())
    }
}

pub struct ParseStack {
    internal: ParseStackInternalRef,
}

impl ParseStack {
    pub fn new() -> Self {
        Self {
            internal: Rc::new(RefCell::new(ParseStackInternal { items: Vec::new() })),
        }
    }

    #[inline(always)]
    #[track_caller]
    pub fn push(
        &mut self,
        name: &'static str,
        can_recover: &'static [SyntaxKind],
        text_pos: usize,
    ) -> ParseStackGuard {
        let internal = self.internal.clone();
        let pos;
        let call_at = panic::Location::caller();
        {
            let mut bw = internal.borrow_mut();
            if bw.items.len() > 10_000 {
                panic!(
                    "ParseStack: too many items; the parser is likely stuck in an infinite loop"
                );
            }
            pos = bw.items.len();
            bw.items.push(ParseStackItem {
                name,
                can_recover,
                text_pos,
            });
        }
        ParseStackGuard {
            internal,
            pos,
            call_at,
        }
    }

    pub fn top_item(&self) -> Option<ParseStackItem> {
        self.internal.borrow().items.last().copied()
    }

    pub fn present<T>(
        &self,
        offset: usize,
        parser_styling: ParserDbgStyling,
        f: impl FnOnce(ParseStackPresenter<'_>) -> T,
    ) -> T {
        f(self.internal.borrow().present(offset, parser_styling))
    }
}
