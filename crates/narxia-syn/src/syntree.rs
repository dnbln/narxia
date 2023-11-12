use std::fmt;
use std::fmt::{Debug, Formatter};

use colored::{ColoredString, Colorize};
use narxia_syn_helpers::{syntree_enum, syntree_node};

use crate::language::NarxiaLanguage;
use crate::parser::ColorizeProcedure;
use crate::syntax_kind::{SyntaxKind, T};
use crate::text_span::TextSpan;

pub type Node = rowan::SyntaxNode<NarxiaLanguage>;
pub type Token = rowan::SyntaxToken<NarxiaLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<NarxiaLanguage>;
pub type SyntaxElementRef<'a> = rowan::NodeOrToken<&'a Node, &'a Token>;

#[derive(Clone, PartialEq, Eq)]
pub struct SynTree {
    // invariant: root.kind() == SyntaxKind::Root
    root: Node,
}

impl Debug for SynTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        TreePresenter {
            root: SyntaxElementRef::Node(&self.root),
            offset: 0,
            style: Default::default(),
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

#[derive(Debug, Clone, Copy, Hash)]
pub struct TreePresenterStyle {
    pub node_name: fn(ColoredString) -> ColoredString,
    pub node_span: fn(ColoredString) -> ColoredString,
    pub token_name: fn(ColoredString) -> ColoredString,
    pub token_span: fn(ColoredString) -> ColoredString,
    pub token_text: fn(ColoredString) -> ColoredString,
}

impl TreePresenterStyle {
    pub fn plain() -> Self {
        Self {
            node_name: |s| s,
            node_span: |s| s,
            token_name: |s| s,
            token_span: |s| s,
            token_text: |s| s,
        }
    }
}

impl Default for TreePresenterStyle {
    fn default() -> Self {
        Self {
            node_name: |s| s.bright_cyan().bold(),
            node_span: |s| s.bright_purple(),
            token_name: |s| s.bright_blue().dimmed(),
            token_span: |s| s.bright_purple(),
            token_text: |s| s.bright_green(),
        }
    }
}

pub struct TreePresenter<'a> {
    root: SyntaxElementRef<'a>,
    offset: usize,
    style: TreePresenterStyle,
}

impl<'a> TreePresenter<'a> {
    pub fn __private_new(
        root: SyntaxElementRef<'a>,
        offset: usize,
        style: TreePresenterStyle,
    ) -> Self {
        Self {
            root,
            offset,
            style,
        }
    }

    pub fn __private_new_at_node(n: &'a Node, offset: usize, style: TreePresenterStyle) -> Self {
        Self::__private_new(SyntaxElementRef::Node(n), offset, style)
    }

    pub fn __private_new_at_node_struct(
        n: &'a (impl TreeNode + 'static),
        offset: usize,
        style: TreePresenterStyle,
    ) -> Self {
        Self::__private_new_at_node(n.get_node(), offset, style)
    }

    pub fn __private_new_at_token(t: &'a Token, offset: usize, style: TreePresenterStyle) -> Self {
        Self::__private_new(SyntaxElementRef::Token(t), offset, style)
    }

    fn fmt_node_info(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.root {
            SyntaxElementRef::Node(n) => {
                writeln!(
                    f,
                    "{:width$}{}@{}",
                    "",
                    self.style
                        .node_name
                        .colorize(format_args!("{:?}", n.kind())),
                    self.style
                        .node_span
                        .colorize(format_args!("{}", TextSpan::of_node(n))),
                    width = self.offset
                )?;
            }
            SyntaxElementRef::Token(t) => {
                writeln!(
                    f,
                    "{:width$}{}@{} {}",
                    "",
                    self.style
                        .token_name
                        .colorize(format_args!("{:?}", t.kind())),
                    self.style
                        .token_span
                        .colorize(format_args!("{}", TextSpan::of(t))),
                    self.style
                        .token_text
                        .colorize(format_args!("{:?}", t.text())),
                    width = self.offset
                )?;
            }
        }

        Ok(())
    }

    fn fmt_node(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_node_info(f)?;

        if let SyntaxElementRef::Node(n) = self.root {
            for child in n.children_with_tokens() {
                TreePresenter {
                    root: as_ref(&child),
                    offset: self.offset + 2,
                    style: self.style,
                }
                .fmt_node(f)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Offset(pub usize);

impl Offset {
    pub fn new(offset: usize) -> Self {
        Self(offset)
    }

    pub fn add(&self, other: usize) -> Self {
        Self(self.0 + other)
    }
}

#[derive(Clone, Copy)]
pub struct CustomTreePresenter<'a> {
    elem: SyntaxElementRef<'a>,
    offset: Offset,
    offset_increment: usize,
    fmt_node: &'a dyn Fn(&mut Formatter<'_>, Offset, &Node) -> fmt::Result,
    fmt_token: &'a dyn Fn(&mut Formatter<'_>, Offset, &Token) -> fmt::Result,
}

impl<'a> CustomTreePresenter<'a> {
    pub fn new_default_at_node(n: &'a Node) -> Self {
        Self::new(
            SyntaxElementRef::Node(n),
            Offset::new(0),
            2,
            &|f, offset, n| {
                TreePresenter {
                    root: SyntaxElementRef::Node(n),
                    offset: offset.0,
                    style: Default::default(),
                }
                .fmt_node_info(f)
            },
            &|f, offset, t| {
                TreePresenter {
                    root: SyntaxElementRef::Token(t),
                    offset: offset.0,
                    style: Default::default(),
                }
                .fmt_node_info(f)
            },
        )
    }

    pub fn new(
        elem: SyntaxElementRef<'a>,
        offset: Offset,
        offset_increment: usize,
        fmt_node: &'a dyn Fn(&mut Formatter<'_>, Offset, &Node) -> fmt::Result,
        fmt_token: &'a dyn Fn(&mut Formatter<'_>, Offset, &Token) -> fmt::Result,
    ) -> Self {
        Self {
            elem,
            offset,
            offset_increment,
            fmt_node,
            fmt_token,
        }
    }
}

impl<'a> Debug for CustomTreePresenter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.elem {
            SyntaxElementRef::Node(n) => {
                (self.fmt_node)(f, self.offset, n)?;

                for child in n.children_with_tokens() {
                    CustomTreePresenter {
                        elem: as_ref(&child),
                        offset: self.offset.add(self.offset_increment),
                        ..*self
                    }
                    .fmt(f)?;
                }

                Ok(())
            }
            SyntaxElementRef::Token(t) => (self.fmt_token)(f, self.offset, t),
        }
    }
}

fn as_ref(e: &SyntaxElement) -> SyntaxElementRef {
    match e {
        SyntaxElement::Node(n) => SyntaxElementRef::Node(n),
        SyntaxElement::Token(t) => SyntaxElementRef::Token(t),
    }
}

impl<'a> Debug for TreePresenter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_node(f)
    }
}

impl SynTree {
    pub fn new(root: GreenTree) -> Self {
        assert_eq!(
            <NarxiaLanguage as rowan::Language>::kind_from_raw(root.root.kind()),
            SyntaxKind::Root
        );
        Self {
            root: Node::new_root(root.root),
        }
    }

    pub fn get_root(&self) -> Root {
        // Safety: The following must hold before calling this.
        // self.root.kind() == SyntaxKind::Root
        unsafe { <Root as TreeNode>::cast_from_node_raw(self.root.clone()) }
    }

    pub fn present_with_style<T>(
        &self,
        style: TreePresenterStyle,
        f: impl FnOnce(TreePresenter) -> T,
    ) -> T {
        f(TreePresenter::__private_new_at_node(&self.root, 0, style))
    }
}

pub trait TreeNode: Sized {
    fn try_cast(n: Node) -> Option<Self> {
        if Self::can_cast_from_syntax_kind(n.kind()) {
            // Safety: Self::can_cast_from_syntax_kind(n.kind()) == true
            Some(unsafe { Self::cast_from_node_raw(n) })
        } else {
            None
        }
    }

    fn try_cast_ref(n: &Node) -> Option<Self> {
        if Self::can_cast_from_syntax_kind(n.kind()) {
            // Safety: Self::can_cast_from_syntax_kind(n.kind()) == true
            Some(unsafe { Self::cast_from_node_raw(n.clone()) })
        } else {
            None
        }
    }

    unsafe fn cast_from_node_raw(n: Node) -> Self; // Invariant: The following must hold before calling this.
                                                   // Self::can_cast_from_syntax_kind(n.kind()) == true
    fn can_cast_from_syntax_kind(kind: SyntaxKind) -> bool;
    fn get_syntax_kind(&self) -> SyntaxKind {
        self.get_node().kind()
    }
    fn get_node(&self) -> &Node;
}

syntree_node! {
    Root = *|[Item]
}

syntree_node! {
    Item = |[FnDef, Stmt]
}

syntree_node! {
    AssignmentStmt = (AssignmentLhs AssignmentEqRhs)
}

syntree_node! {
    AssignmentLhs = ExprNode
}

syntree_node! {
    AssignmentEqRhs = (eq![=] ExprNode)
}

syntree_node! {
    FnDef = (FnHead Block)
}

syntree_node! {
    FnHead = (fn_kw![fn] FnName ?GenericParamList FnParamList ?FnRetTy)
}

syntree_node! {
    FnName = ident![ident]
}

syntree_node! {
    GenericParamList = (langle![<] *|[GenericParam, comma![,]] rangle![>])
}

syntree_node! {
    GenericParam = |[GenericConstParam, GenericTyParam]
}

syntree_node! {
    GenericConstParam = (const_kw![const] GenericConstParamName colon![:] TyRef)
}

syntree_node! {
    GenericConstParamName = ident![ident]
}

syntree_node! {
    GenericTyParam = (GenericTyParamName ?(colon![:] GenericTyParamBoundList) ?GenericTyParamDefault)
}

syntree_node! {
    GenericTyParamName = ident![ident]
}

syntree_node! {
    GenericTyParamBoundList = *|[GenericTyParamBound, plus![+]]
}

syntree_node! {
    GenericTyParamBound = TyRef
}

syntree_node! {
    GenericTyParamDefault = (eq![=] TyRef)
}

syntree_node! {
    FnParamList = (lparen!['('] *|[FnParam, comma![,]] rparen![')'])
}

syntree_node! {
    FnParam = (FnParamName colon![:] FnParamTy ?FnParamDefault)
}

syntree_node! {
    FnParamName = Pat
}

syntree_node! {
    FnParamTy = TyRef
}

syntree_node! {
    FnParamDefault = (eq![=] ExprNode)
}

syntree_node! {
    FnRetTy = (arrow![->] TyRef)
}

syntree_node! {
    TyRef = ident![ident]
}

syntree_node! {
    ExprNode: Expr = Expr
}

syntree_node! {
    ForStmt = (for_kw![for] lparen!['('] ForPat in_kw![in] ForInExpr rparen![')'] Block)
}

syntree_node! {
    ForPat = Pat
}

syntree_node! {
    ForInExpr = ExprNode
}

syntree_enum! {
    Expr = ExprNode | ExprAtom | BinaryOpExpr | CallExpr | IndexExpr | FieldAccess | MethodCall | Block
}

syntree_node! {
    ExprAtom = |[ident![ident], str![string], NumLit, LoopExpr, IfExpr, ReturnExpr, BreakExpr, ContinueExpr, BlockExpr]
}

syntree_node! {
    NumLit = |[num_bin![num_bin], num_oct![num_oct], num_dec![num_dec], num_hex![num_hex]]
}

syntree_node! {
    BlockExpr = Block
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

    pub fn get_trivia(&self) -> impl Iterator<Item = Token> + '_ {
        get_token_list(&self.node, T![composed_trivia])
    }

    pub fn get_children_exprs(&self) -> (Expr, Option<Expr>) {
        let mut children = get_children(&self.node);
        let left = match children.next() {
            Some(child) => child,
            None => {
                eprintln!(
                    "{:?}",
                    TreePresenter::__private_new_at_node(&self.node, 0, Default::default())
                );
                panic!("BinaryOpExpr has no children")
            }
        };
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

    fn call_accessors(&self, tests_data: &mut tests_data::AccessorCalledDataList) {
        let (left, _op, right) = self.lower2();
        tests_data.push(
            tests_data::ElemRef::from(self),
            "BinaryOpExpr",
            "get_left",
            tests_data::AccessorCalledDataReturned::Returned(tests_data::ElemRef::from(&left)),
        );
        left.call_accessors(tests_data);
        if let Some(right) = right {
            right.call_accessors(tests_data);
            tests_data.push(
                tests_data::ElemRef::from(self),
                "BinaryOpExpr",
                "get_right",
                tests_data::AccessorCalledDataReturned::Returned(tests_data::ElemRef::from(&right)),
            );
        } else {
            tests_data.push(
                tests_data::ElemRef::from(self),
                "BinaryOpExpr",
                "get_right",
                tests_data::AccessorCalledDataReturned::Nothing,
            );
        }

        let _op = self.get_op();
        tests_data.push(
            tests_data::ElemRef::from(self),
            "BinaryOpExpr",
            "get_op",
            tests_data::AccessorCalledDataReturned::Returned(tests_data::ElemRef::from(&_op)),
        );
        _op.call_accessors(tests_data);

        let _trivia = self.get_trivia();
        let _trivia = _trivia
            .map(|it| tests_data::ElemRef::from(&it))
            .collect::<Vec<_>>();
        tests_data.push(
            tests_data::ElemRef::from(self),
            "BinaryOpExpr",
            "get_trivia",
            if _trivia.is_empty() {
                tests_data::AccessorCalledDataReturned::EmptyAllowed
            } else {
                tests_data::AccessorCalledDataReturned::ReturnedList(_trivia)
            },
        );
    }
}

syntree_node! {
    BinaryOpExprOp = |[
        plus![+],
        minus![-],
        star![*],
        slash![/],
        percent![%],
        eqeq![==],
        neq![!=],
        lt![<],
        gt![>],
        lteq![<=],
        gteq![>=],
        and![&&],
        or![||],
        bit_and![&],
        bit_or![|],
        xor![^]
    ]
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
    BitAnd(Token),
    BitOr(Token),
    Xor(Token),
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
            T![&] => Some(Self::BitAnd(t)),
            T![|] => Some(Self::BitOr(t)),
            T![^] => Some(Self::Xor(t)),
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
            Self::BitAnd(t) => t,
            Self::BitOr(t) => t,
            Self::Xor(t) => t,
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
        } else if let Some(t) = bin_expr_op.get_bit_and() {
            Self::BitAnd(t)
        } else if let Some(t) = bin_expr_op.get_bit_or() {
            Self::BitOr(t)
        } else if let Some(t) = bin_expr_op.get_xor() {
            Self::Xor(t)
        } else {
            unreachable!()
        }
    }
}

syntree_node! {
    CallExpr = (Expr CallExprArgs)
}

syntree_node! {
    CallExprArgs = (?CallExprArgsList ?CallExprArgLambda)
}

syntree_node! {
    CallExprArgsList = ?(lparen!['('] *ExprNode rparen![')'])
}

syntree_node! {
    CallExprArgLambda = LambdaExpr
}

syntree_node! {
    LambdaExpr = (lbrace!['{'] ?LambdaParamList *|[Item, semi![;], newline![newline]] rbrace!['}'])
}

syntree_node! {
    LambdaParamList = (*|[LambdaParam, comma![,]] rarrow![->])
}

syntree_node! {
    LambdaParam = (FnParamName ?(colon![:] FnParamTy))
}

syntree_node! {
    IndexExpr = (Expr IndexExprIndex)
}

syntree_node! {
    IndexExprIndex = (lbracket!['['] ExprNode rbracket![']'])
}

syntree_node! {
    FieldAccess = (Expr dot![.] field_name![ident])
}

syntree_node! {
    MethodCall = (Expr dot![.] method_name![ident] CallExprArgs)
}

syntree_node! {
    Block = (lbrace!['{'] *|[Item, semi![;], newline![newline]] rbrace!['}'])
}

syntree_node! {
    Stmt = |[ExprNode, LetStmt, ForStmt, WhileStmt, AssignmentStmt]
}

syntree_node! {
    LetStmt = (let_kw![let] Pat ?(colon![:] TyRef) ?(eq![=] ExprNode))
}

syntree_node! {
    Pat = ident![ident]
}

syntree_node! {
    LoopExpr = (loop_kw![loop] Block)
}

syntree_node! {
    IfExpr = (if_kw![if] IfCondition IfThenClause ?ElseClause)
}

syntree_node! {
    IfCondition = (lparen!['('] ExprNode rparen![')'])
}

syntree_node! {
    IfThenClause = ExprNode
}

syntree_node! {
    ElseClause = (else_kw![else] ExprNode)
}

syntree_node! {
    ReturnExpr = (return_kw![return] ?ExprNode)
}

syntree_node! {
    BreakExpr = (break_kw![break] ?ExprNode)
}

syntree_node! {
    ContinueExpr = continue_kw![continue]
}

syntree_node! {
    WhileStmt = (while_kw![while] WhileCondition Block)
}

syntree_node! {
    WhileCondition = (lparen!['('] ExprNode rparen![')'])
}

fn get_children<'a, T: TreeNode + 'static>(n: &'a Node) -> impl Iterator<Item = T> + 'a {
    n.children().filter_map(T::try_cast)
}

fn get_child_opt<T: TreeNode + 'static>(n: &Node) -> Option<T> {
    get_children(n).next()
}

fn get_child<T: TreeNode + 'static>(n: &Node) -> T {
    match get_child_opt(n) {
        Some(child) => child,
        None => {
            eprintln!(
                "{:?}",
                TreePresenter::__private_new_at_node(n, 0, Default::default())
            );
            panic!(
                "Node has no children of type {:?}",
                std::any::type_name::<T>()
            )
        }
    }
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
        $crate::narxia_log::debug!(
            "{:?}",
            $crate::syntree::TreePresenter::__private_new_at_node_struct(
                &$node,
                $offset,
                $crate::syntree::TreePresenterStyle::default()
            )
        );
    }};
}

pub mod tests_data {
    use crate::syntax_kind::SyntaxKind;
    use crate::syntree::{Token, TreeNode};
    use crate::text_span::TextSpan;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct AccessorInfo {
        pub ty_name: &'static str,
        pub accessor_name: &'static str,
    }

    pub struct AccessorCalledData {
        pub accessor_info: AccessorInfo,
        pub called_at: ElemRef,
        pub returned: AccessorCalledDataReturned,
    }

    pub struct ElemRef {
        pub kind: SyntaxKind,
        pub span: TextSpan,
    }

    impl<T: TreeNode> From<&T> for ElemRef {
        fn from(value: &T) -> Self {
            Self {
                kind: value.get_syntax_kind(),
                span: TextSpan::of_node(value.get_node()),
            }
        }
    }

    impl From<&Token> for ElemRef {
        fn from(value: &Token) -> Self {
            Self {
                kind: value.kind(),
                span: TextSpan::of(value),
            }
        }
    }

    pub enum AccessorCalledDataReturned {
        Returned(ElemRef),
        ReturnedList(Vec<ElemRef>),
        Nothing,
        EmptyAllowed,
    }

    pub struct AccessorCalledDataList {
        pub accessors: Vec<AccessorCalledData>,
    }

    impl AccessorCalledDataList {
        pub fn new() -> Self {
            Self {
                accessors: Vec::new(),
            }
        }

        pub fn push(
            &mut self,
            called_at: ElemRef,
            ty_name: &'static str,
            accessor_name: &'static str,
            returned: AccessorCalledDataReturned,
        ) {
            self.accessors.push(AccessorCalledData {
                accessor_info: AccessorInfo {
                    ty_name,
                    accessor_name,
                },
                called_at,
                returned,
            });
        }
    }
}
