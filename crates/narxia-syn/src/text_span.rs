use std::fmt;
use std::fmt::Formatter;
use std::ops::Index;
use crate::syntree::{Node, Token};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct TextSpan {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl TextSpan {
    pub fn of(token: &Token) -> Self {
        let r = token.text_range();
        Self::new(r.start().into(), r.end().into())
    }

    pub fn of_node(node: &Node) -> Self {
        let r = node.text_range();
        Self::new(r.start().into(), r.end().into())
    }
    pub fn from_range(range: std::ops::Range<usize>) -> Self {
        Self::new(range.start, range.end)
    }

    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end);
        Self { start, end }
    }

    pub fn add_offset(&self, offset: usize) -> Self {
        Self {
            start: self.start + offset,
            end: self.end + offset,
        }
    }

    pub fn get(self, slice: &str) -> &str {
        &slice[self.start..self.end]
    }

    pub fn range(self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

impl fmt::Display for TextSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Index<TextSpan> for str {
    type Output = str;

    fn index(&self, index: TextSpan) -> &Self::Output {
        &self[index.start..index.end]
    }
}