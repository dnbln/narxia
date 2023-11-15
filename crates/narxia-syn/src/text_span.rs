use std::fmt;
use std::fmt::Formatter;
use std::ops::Index;

use crate::syntree::{Node, Token};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct TextSpan {
    pub(crate) start: u32,
    pub(crate) end: u32,
}

impl TextSpan {
    pub fn of(token: &Token) -> Self {
        let r = token.text_range();
        unsafe { Self::new_unchecked(r.start().into(), r.end().into()) }
    }

    pub fn of_node(node: &Node) -> Self {
        let r = node.text_range();
        unsafe { Self::new_unchecked(r.start().into(), r.end().into()) }
    }
    pub fn from_range(range: std::ops::Range<u32>) -> Self {
        Self::new(range.start, range.end)
    }

    pub fn new(start: u32, end: u32) -> Self {
        debug_assert!(start <= end);
        unsafe { Self::new_unchecked(start, end) }
    }

    /// # Safety
    /// start <= end
    #[must_use]
    pub unsafe fn new_unchecked(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn add_offset(self, offset: u32) -> Self {
        Self {
            start: self.start + offset,
            end: self.end + offset,
        }
    }

    pub fn sub_offset(self, offset: u32) -> Self {
        Self {
            start: self.start - offset,
            end: self.end - offset,
        }
    }

    pub fn get(self, slice: &str) -> &str {
        &slice[self.range_usize()]
    }

    #[inline]
    pub fn range(self) -> std::ops::Range<u32> {
        self.start..self.end
    }

    #[inline]
    pub fn range_usize(self) -> std::ops::Range<usize> {
        self.start as usize..self.end as usize
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
        index.get(self)
    }
}
