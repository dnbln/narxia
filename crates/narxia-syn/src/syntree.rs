use std::fmt;
use std::fmt::Formatter;

use narxia_syn_helpers::syntree_node;

use crate::language::NarxiaLanguage;
use crate::syntax_kind::{SyntaxKind, T};
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
    pub fn __private_new(root: SyntaxElementRef<'a>, offset: usize) -> Self {
        Self { root, offset }
    }

    pub fn __private_new_at_node(n: &'a Node, offset: usize) -> Self {
        Self::__private_new(SyntaxElementRef::Node(n), offset)
    }

    pub fn __private_new_at_node_struct(n: &'a (impl TreeNode + 'static), offset: usize) -> Self {
        Self::__private_new_at_node(n.get_node(), offset)
    }

    pub fn __private_new_at_token(t: &'a Token, offset: usize) -> Self {
        Self::__private_new(SyntaxElementRef::Token(t), offset)
    }

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
                writeln!(
                    f,
                    "{:width$}{:?}@{} {:?}",
                    "",
                    t.kind(),
                    TextSpan::of(t),
                    t.text(),
                    width = self.offset
                )?;
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
        debug_assert_eq!(
            <NarxiaLanguage as rowan::Language>::kind_from_raw(root.root.kind()),
            SyntaxKind::Root
        );
        Self {
            root: Node::new_root(root.root),
        }
    }

    pub fn get_root(&self) -> Root {
        <Root as TreeNode>::from(self.root.clone()).unwrap()
    }
}

pub trait TreeNode: Sized {
    fn from(n: Node) -> Option<Self>;
    fn get_syntax_kind(&self) -> SyntaxKind;
    fn get_node(&self) -> &Node;
}

syntree_node! {
    Root = *Item
}

syntree_node! {
    Item = |[FnDef]
}

syntree_node! {
    FnDef = (FnHead Block)
}

syntree_node! {
    FnHead = (fn_kw![fn] FnName FnParamList)
}

syntree_node! {
    FnName = ident![ident]
}

syntree_node! {
    FnParamList = (lparen!['('] *FnParam rparen![')'])
}

syntree_node! {
    FnParam = (FnParamName colon![:] FnParamTy ?FnParamDefault)
}

syntree_node! {
    FnParamName = ident![ident]
}

syntree_node! {
    FnParamTy = TyRef
}

syntree_node! {
    FnParamDefault = (eq![=] Expr)
}

syntree_node! {
    TyRef = ident![ident]
}

syntree_node! {
    Expr = |[ExprAtom, BinaryOpExpr]
}

syntree_node! {
    ExprAtom = |[ident![ident], str![string]]
}

syntree_node! {
    BinaryOpExpr!
}

impl BinaryOpExpr {
    pub fn get_left(&self) -> Expr {
        self.get_children_exprs().0
    }

    pub fn get_right(&self) -> Option<Expr> {
        self.get_children_exprs().1
    }

    pub fn get_children_exprs(&self) -> (Expr, Option<Expr>) {
        let mut children = get_children(&self.node);
        let left = children.next().unwrap();
        let right = children.next();
        (left, right)
    }

    pub fn get_op(&self) -> BinaryOpExprOp {
        get_child(&self.node)
    }

    pub fn lower(&self) -> (Expr, BinaryOpExprOp, Option<Expr>) {
        let (left, right) = self.get_children_exprs();
        (left, self.get_op(), right)
    }

    pub fn lower2(&self) -> (Expr, BinOp, Option<Expr>) {
        let (left, right) = self.get_children_exprs();
        (left, BinOp::from_binary_op_expr_op(self.get_op()), right)
    }

    #[track_caller]
    pub fn lower_assume_complete(&self) -> (Expr, BinOp, Expr) {
        let (left, op, right) = self.lower2();
        assert!(right.is_some());
        (left, op, right.unwrap())
    }
}

syntree_node! {
    BinaryOpExprOp = |[plus![+], minus![-], star![*], slash![/], percent![%], eqeq![==], neq![!=], lt![<], gt![>], lteq![<=], gteq![>=], and![&&], or![||]]
}

pub enum BinOp {
    Add(Token),
    Sub(Token),
    Mul(Token),
    Div(Token),
    Mod(Token),
    Eq(Token),
    Neq(Token),
    Lt(Token),
    Gt(Token),
    LtEq(Token),
    GtEq(Token),
    And(Token),
    Or(Token),
}

impl BinOp {
    fn from_token(t: Token) -> Option<Self> {
        match t.kind() {
            T![+] => Some(Self::Add(t)),
            T![-] => Some(Self::Sub(t)),
            T![*] => Some(Self::Mul(t)),
            T![/] => Some(Self::Div(t)),
            T![%] => Some(Self::Mod(t)),
            T![==] => Some(Self::Eq(t)),
            T![!=] => Some(Self::Neq(t)),
            T![<] => Some(Self::Lt(t)),
            T![>] => Some(Self::Gt(t)),
            T![<=] => Some(Self::LtEq(t)),
            T![>=] => Some(Self::GtEq(t)),
            T![&&] => Some(Self::And(t)),
            T![||] => Some(Self::Or(t)),
            _ => None,
        }
    }

    pub fn get_token(&self) -> &Token {
        match self {
            Self::Add(t) => t,
            Self::Sub(t) => t,
            Self::Mul(t) => t,
            Self::Div(t) => t,
            Self::Mod(t) => t,
            Self::Eq(t) => t,
            Self::Neq(t) => t,
            Self::Lt(t) => t,
            Self::Gt(t) => t,
            Self::LtEq(t) => t,
            Self::GtEq(t) => t,
            Self::And(t) => t,
            Self::Or(t) => t,
        }
    }

    fn from_binary_op_expr_op(bin_expr_op: BinaryOpExprOp) -> Self {
        if let Some(t) = bin_expr_op.get_plus() {
            Self::Add(t)
        } else if let Some(t) = bin_expr_op.get_minus() {
            Self::Sub(t)
        } else if let Some(t) = bin_expr_op.get_star() {
            Self::Mul(t)
        } else if let Some(t) = bin_expr_op.get_slash() {
            Self::Div(t)
        } else if let Some(t) = bin_expr_op.get_percent() {
            Self::Mod(t)
        } else if let Some(t) = bin_expr_op.get_eqeq() {
            Self::Eq(t)
        } else if let Some(t) = bin_expr_op.get_neq() {
            Self::Neq(t)
        } else if let Some(t) = bin_expr_op.get_lt() {
            Self::Lt(t)
        } else if let Some(t) = bin_expr_op.get_gt() {
            Self::Gt(t)
        } else if let Some(t) = bin_expr_op.get_lteq() {
            Self::LtEq(t)
        } else if let Some(t) = bin_expr_op.get_gteq() {
            Self::GtEq(t)
        } else if let Some(t) = bin_expr_op.get_and() {
            Self::And(t)
        } else if let Some(t) = bin_expr_op.get_or() {
            Self::Or(t)
        } else {
            unreachable!()
        }
    }
}

syntree_node! {
    Block = (lbrace!['{'] *Stmt rbrace!['}'])
}

syntree_node! {
    Stmt = |[Expr, LetStmt]
}

syntree_node! {
    LetStmt = (let_kw![let] Pat ?(colon![:] TyRef) ?(eq![=] Expr))
}

syntree_node! {
    Pat = ident![ident]
}

fn get_children<'a, T: TreeNode + 'static>(n: &'a Node) -> impl Iterator<Item = T> + 'a {
    n.children().filter_map(T::from)
}

fn get_child_opt<T: TreeNode + 'static>(n: &Node) -> Option<T> {
    get_children(n).next()
}

fn get_child<T: TreeNode + 'static>(n: &Node) -> T {
    get_child_opt(n).unwrap()
}

fn get_token_list<'a>(n: &'a Node, kind: SyntaxKind) -> impl Iterator<Item = Token> + 'a {
    n.children_with_tokens().filter_map(move |e| match e {
        SyntaxElement::Token(t) if t.kind() == kind => Some(t),
        _ => None,
    })
}

fn get_token_opt(n: &Node, kind: SyntaxKind) -> Option<Token> {
    get_token_list(n, kind).next()
}

fn get_token(n: &Node, kind: SyntaxKind) -> Token {
    get_token_opt(n, kind).unwrap()
}

#[macro_export]
macro_rules! dbg_node {
    ($node:expr) => {
        $crate::dbg_node!($node, >>> 4);
    };
    ($node:expr, >>> $offset:expr) => {{
        let _guard = $crate::narxia_log::span!($crate::narxia_log::Level::DEBUG, "dbg_node").entered();
        $crate::narxia_log::debug!("{:?}", $crate::syntree::TreePresenter::__private_new_at_node_struct(&$node, $offset));
    }};
}
