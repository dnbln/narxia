use rowan::Language;

use crate::syntax_kind::SyntaxKind;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct NarxiaLanguage;

impl NarxiaLanguage {
    pub fn kind_from_u16(v: u16) -> SyntaxKind {
        debug_assert!(v < SyntaxKind::__END as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(v) }
    }
}

impl Language for NarxiaLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::kind_from_u16(raw.0)
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}
