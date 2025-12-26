use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId {
    pub(crate) idx: usize,
}

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Def {}", self.idx)
    }
}
