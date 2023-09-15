#![allow(non_camel_case_types)]

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, narxia_syn_helpers::DeriveT)]
#[repr(u16)]
pub enum SyntaxKind {
    // Tokens
    #[T(ident)]
    IDENT,
    #[T(number)]
    NUMBER,
    #[T(string)]
    STRING,
    // Keywords
    #[T(fn)]
    FN_KW,
    #[T(let)]
    LET_KW,
    #[T(const)]
    CONST_KW,
    #[T(as)]
    AS_KW,
    #[T(for)]
    FOR_KW,
    #[T(while)]
    WHILE_KW,
    #[T(loop)]
    LOOP_KW,
    #[T(if)]
    IF_KW,
    #[T(else)]
    ELSE_KW,
    #[T(return)]
    RETURN_KW,
    #[T(break)]
    BREAK_KW,
    #[T(continue)]
    CONTINUE_KW,
    #[T(true)]
    TRUE_KW,
    #[T(false)]
    FALSE_KW,
    #[T(in)]
    IN_KW,
    // Punctuation
    #[T('(')]
    L_PAREN,
    #[T(')')]
    R_PAREN,
    #[T('{')]
    L_BRACE,
    #[T('}')]
    R_BRACE,
    #[T('[')]
    L_BRACK,
    #[T(']')]
    R_BRACK,
    #[T(,)]
    COMMA,
    #[T(:)]
    COLON,
    #[T(;)]
    SEMI,
    #[T(.)]
    DOT,
    #[T(=>)]
    FAT_ARROW,
    #[T(->)]
    THIN_ARROW,
    // Operators
    #[T(=)]
    EQ,
    #[T(==)]
    EQ2,
    #[T(!=)]
    NEQ,
    #[T(>)]
    GT,
    #[T(<)]
    LT,
    #[T(>=)]
    GE,
    #[T(<=)]
    LE,
    #[T(+)]
    PLUS,
    #[T(-)]
    MINUS,
    #[T(*)]
    ASTERISK,
    #[T(/)]
    SLASH,
    #[T(%)]
    PERCENT,
    #[T(&)]
    AMP,
    #[T(|)]
    PIPE,
    #[T(^)]
    CARET,
    #[T(!)]
    BANG,
    #[T(&&)]
    AMP2,
    #[T(||)]
    PIPE2,
    // Others
    #[T(whitespace)]
    WHITESPACE,
    #[T(comment)]
    COMMENT,
    #[T(error)]
    ERROR,
    #[T(newline)]
    NEWLINE,
    #[T(eof)]
    EOF,

    // Nodes
    Root,
    FnDef,
    FnHead,
    FnName,
    FnParamList,
    FnParam,
    FnParamName,
    Block,
    Stmt,

    ExprAtom,
    Expr,
    BinaryOpExpr,
    IndexExpr,
    IndexExprIndex,
    CallExpr,
    CallExprArgs,
    CustomInfixExpr,
    CustomInfixExprInfixArg,
    MethodCall,
    FieldAccess,
    UnaryOpExpr,
    UnaryPrefixOp,
    IfExpr,
    IfCondition,
    IfThenClause,
    ElseClause,
    BlockExpr,

    LetStmt,
    Pat,

    TyRef,
    TyParamList,
    GenericParam,

    GenericConstParam,
    GenericConstParamName,
    GenericConstParamDefault,

    GenericTyParam,
    GenericTyParamName,
    GenericTyParamBoundList,
    GenericTyParamDefault,

    WhileStmt,
    WhileCondition,

    ForStmt,
    ForPat,
    ForInExpr,

    LoopExpr,

    #[doc(hidden)]
    __TOMBSTONE,
    #[doc(hidden)]
    __END,
}
