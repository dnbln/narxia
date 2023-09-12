use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::syntax_kind::SyntaxKind;

type ParseStackInternalRef = Rc<RefCell<ParseStackInternal>>;

#[must_use]
pub struct ParseStackGuard {
    internal: ParseStackInternalRef,
    pos: usize,
    call_at: &'static std::panic::Location<'static>,
}

impl Drop for ParseStackGuard {
    fn drop(&mut self) {
        let mut stack_internal = self.internal.borrow_mut();
        if self.pos != stack_internal.items.len() - 1 && !::std::thread::panicking() {
            panic!(
                "ParseStackGuard: pos ({}) != len - 1 ({}) (called at [{}]):\n{}",
                self.pos,
                stack_internal.items.len() - 1,
                self.call_at,
                stack_internal.present(2)
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
    fn present(&self, offset: usize) -> ParseStackPresenter<'_> {
        ParseStackPresenter {
            internal: self,
            offset,
        }
    }
}

pub struct ParseStackPresenter<'a> {
    internal: &'a ParseStackInternal,
    offset: usize,
}

impl<'a> fmt::Display for ParseStackPresenter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, item) in self.internal.items.iter().rev().enumerate() {
            writeln!(
                f,
                "{:offset$}[-{:3}] {} (@{})",
                "",
                index,
                item.name,
                item.text_pos,
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

    #[track_caller]
    pub fn push(
        &mut self,
        name: &'static str,
        can_recover: &'static [SyntaxKind],
        text_pos: usize,
    ) -> ParseStackGuard {
        let internal = self.internal.clone();
        let pos = internal.borrow().items.len();
        let call_at = std::panic::Location::caller();
        internal
            .borrow_mut()
            .items
            .push(ParseStackItem { name, can_recover, text_pos });
        ParseStackGuard {
            internal,
            pos,
            call_at,
        }
    }

    pub fn top_item(&self) -> Option<ParseStackItem> {
        self.internal.borrow().items.last().copied()
    }

    pub fn present<T>(&self, offset: usize, f: impl FnOnce(ParseStackPresenter<'_>) -> T) -> T {
        f(self.internal.borrow().present(offset))
    }
}
