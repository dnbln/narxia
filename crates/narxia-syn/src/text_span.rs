use std::{fmt, ops::Range};
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
        #[expect(unsafe_code)]
        unsafe { Self::new_unchecked(r.start().into(), r.end().into()) }
    }

    pub fn of_node(node: &Node) -> Self {
        let r = node.text_range();
        #[expect(unsafe_code)]
        unsafe { Self::new_unchecked(r.start().into(), r.end().into()) }
    }
    pub const fn from_range(range: Range<u32>) -> Self {
        Self::new(range.start, range.end)
    }

    pub const fn new(start: u32, end: u32) -> Self {
        debug_assert!(start <= end);
        #[expect(unsafe_code)]
        unsafe { Self::new_unchecked(start, end) }
    }

    /// # Safety
    /// start <= end
    #[must_use]
    #[expect(unsafe_code)]
    pub const unsafe fn new_unchecked(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub const fn add_offset(self, offset: u32) -> Self {
        Self {
            start: self.start + offset,
            end: self.end + offset,
        }
    }

    pub const fn sub_offset(self, offset: u32) -> Self {
        Self {
            start: self.start - offset,
            end: self.end - offset,
        }
    }

    pub fn get(self, slice: &str) -> &str {
        &slice[self.range_usize()]
    }

    #[inline]
    pub fn range(self) -> Range<u32> {
        self.start..self.end
    }

    #[inline]
    pub fn range_usize(self) -> Range<usize> {
        self.start as usize..self.end as usize
    }

    #[inline]
    pub fn get_span_start_line(self, text: &str) -> usize {
        text[..self.start as usize].lines().count()
    }

    #[inline(always)]
    pub fn len(self) -> u32 {
        self.end - self.start
    }

    #[inline(always)]
    pub fn is_empty(self) -> bool {
        self.start == self.end
    }

    #[inline(always)]
    pub fn len_usize(self) -> usize {
        self.len() as usize
    }

    #[inline]
    pub fn join(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
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
