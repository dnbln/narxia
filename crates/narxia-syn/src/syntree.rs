use std::fmt;
use std::fmt::Formatter;

use crate::language::NarxiaLanguage;
use crate::text_span::TextSpan;

pub type Node = rowan::SyntaxNode<NarxiaLanguage>;
pub type Token = rowan::SyntaxToken<NarxiaLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<NarxiaLanguage>;
pub type SyntaxElementRef<'a> = rowan::NodeOrToken<&'a Node, &'a Token>;

pub struct SynTree {
    root: Node,
}

impl fmt::Debug for SynTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        TreePresenter {
            root: SyntaxElementRef::Node(&self.root),
            offset: 0,
        }
        .fmt_node(f)
    }
}

pub struct GreenTree {
    root: rowan::GreenNode,
}

impl GreenTree {
    pub(crate) fn new(root: rowan::GreenNode) -> Self {
        Self { root }
    }
}

pub struct TreePresenter<'a> {
    root: SyntaxElementRef<'a>,
    offset: usize,
}

impl<'a> TreePresenter<'a> {
    fn fmt_node(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.root {
            SyntaxElementRef::Node(n) => {
                writeln!(
                    f,
                    "{:width$}{:?}@{}",
                    "",
                    n.kind(),
                    TextSpan::of_node(n),
                    width = self.offset
                )?;
                for child in n.children_with_tokens() {
                    TreePresenter {
                        root: as_ref(&child),
                        offset: self.offset + 2,
                    }
                    .fmt_node(f)?;
                }
            }
            SyntaxElementRef::Token(t) => {
                writeln!(f, "{:width$}{:?}@{} {:?}", "", t.kind(), TextSpan::of(t), t.text(), width = self.offset)?;
            }
        }
        Ok(())
    }
}

fn as_ref(e: &SyntaxElement) -> SyntaxElementRef {
    match e {
        SyntaxElement::Node(n) => SyntaxElementRef::Node(n),
        SyntaxElement::Token(t) => SyntaxElementRef::Token(t),
    }
}

impl<'a> fmt::Debug for TreePresenter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_node(f)
    }
}

impl SynTree {
    pub fn new(root: GreenTree) -> Self {
        Self {
            root: Node::new_root(root.root),
        }
    }
}
