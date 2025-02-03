#![allow(non_camel_case_types)]

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, narxia_proc::DeriveT)]
#[repr(u16)]
#[T(syntax_kind::SyntaxKind)]
pub enum SyntaxKind {
    // Tokens
    #[T(ident)]
    IDENT,
    #[T(num_dec)]
    NUM_DEC,
    #[T(num_bin)]
    NUM_BIN,
    #[T(num_oct)]
    NUM_OCT,
    #[T(num_hex)]
    NUM_HEX,
    #[T(begin_string)]
    BEGIN_STRING,
    #[T(end_string)]
    END_STRING,
    // Keywords
    #[T(module)]
    MODULE_KW,
    #[T(fn)]
    FN_KW,
    #[T(let)]
    LET_KW,
    #[T(mut)]
    MUT_KW,
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
    #[T(use)]
    USE_KW,
    // Punctuation
    #[T(#)]
    HASH,
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
    R_ANGLE,
    #[T(<)]
    L_ANGLE,
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
    #[T(+=)]
    PLUS_EQ,
    #[T(-=)]
    MINUS_EQ,
    #[T(*=)]
    ASTERISK_EQ,
    #[T(/=)]
    SLASH_EQ,
    #[T(%=)]
    PERCENT_EQ,
    #[T(&=)]
    AMP_EQ,
    #[T(|=)]
    PIPE_EQ,
    #[T(^=)]
    CARET_EQ,
    #[T(::)]
    COLON2,
    // Others
    #[T(whitespace)]
    WHITESPACE,
    #[T(comment)]
    COMMENT,
    #[T(error)]
    ERROR,
    #[T(newline)]
    NEWLINE,
    #[T(composed_trivia)]
    COMPOSED_TRIVIA,
    #[T(eof)]
    EOF,

    // Nodes
    Root,
    AttrList,
    Attr,
    AttrName,
    AttrMeta,
    AttrMetaItem,
    AttrMetaItemName,
    AttrMetaItemEq,
    AttrMetaItemCall,

    Item,
    Module,
    ModuleName,
    ModuleBody,
    FnDef,
    FnHead,
    FnName,
    FnParamList,
    FnParam,
    FnParamName,
    FnParamTy,
    FnParamDefault,
    FnRetTy,
    Block,
    Stmt,

    NumLit,
    ExprAtom,
    Expr,
    BinaryOpExpr,
    BinaryOpExprOp,
    IndexExpr,
    IndexExprIndex,
    CallExpr,
    CallExprArgs,
    CallExprArgsList,
    CallExprArgLambda,
    CustomInfixExpr,
    CustomInfixExprInfix,
    CustomInfixExprInfixArg,
    MethodCall,
    FieldAccess,
    UnaryOpExpr,
    UnaryPrefixOp,
    IfExpr,
    IfCondition,
    IfThenClause,
    ElseClause,
    TupleLikeExpr,
    BlockExpr,
    LambdaExpr,
    LambdaParamList,
    LambdaParam,
    ReturnExpr,
    BreakExpr,
    ContinueExpr,

    LetStmt,
    Pat,

    TyRef,
    FnTy,
    FnTyParamTys,
    FnTyRetTy,
    GenericParamList,
    GenericParam,

    GenericConstParam,
    GenericConstParamName,
    GenericConstParamDefault,

    GenericTyParam,
    GenericTyParamName,
    GenericTyParamBoundList,
    GenericTyParamBound,
    GenericTyParamDefault,

    WhileStmt,
    WhileCondition,

    ForStmt,
    ForPat,
    ForInExpr,

    LoopExpr,

    AssignmentStmt,
    AssignmentLhs,
    AssignmentOpAndRhsExpr,
    AssignmentOp,

    StringLiteral,
    StringLiteralFragTextPart,
    // raw text in a string
    #[T(string_literal_frag_text_part_t)]
    StringLiteralFragTextPartToken,
    StringLiteralFragEscapedChar,
    // \n, \r, \t, \", \', \\ ...
    #[T(string_literal_frag_escaped_char_t)]
    StringLiteralFragEscapedCharToken,
    StringLiteralFragEscapeSequence,
    // \x12, \u1234
    #[T(string_literal_frag_escape_sequence_t)]
    StringLiteralFragEscapeSequenceToken,
    // $
    #[T(string_literal_frag_display_t)]
    StringLiteralFragDisplayToken,
    // $?
    #[T(string_literal_frag_debug_t)]
    StringLiteralFragDebugToken,
    // the name after the $ or $?
    StringLiteralFragIdent,
    // the expression after the $ or $?
    StringLiteralFragExpr,

    StringLiteralFragDisplay,
    StringLiteralFragDebug,

    UseStmt,
    UsePath,
    UsePathSegment,
    UsePathSegmentAndPath,
    UsePathContinuation,
    UsePathColonContinuation,
    UseAlias,
    UsePathList,

    #[doc(hidden)]
    __TOMBSTONE,
    #[doc(hidden)]
    __END,
}
