use std::fmt;

use owo_colors::{OwoColorize, Style};

use super::*;

pub struct HirDebugContext {
    get_path_fn: fn(HirId) -> String,
}

thread_local! {
    static DEBUG_CONTEXT: std::cell::RefCell<Option<HirDebugContext>> = std::cell::RefCell::new(None);
}

pub fn dbg_hir(
    get_path_fn: fn(HirId) -> String,
    cb: impl FnOnce() -> std::fmt::Result,
) -> std::fmt::Result {
    DEBUG_CONTEXT.with(move |f| {
        if f.borrow().is_some() {
            panic!("Hir debug context already set");
        }

        struct HirDebugContextGuard<'a>(&'a std::cell::RefCell<Option<HirDebugContext>>);

        impl<'a> Drop for HirDebugContextGuard<'a> {
            fn drop(&mut self) {
                *self.0.borrow_mut() = None;
            }
        }

        *f.borrow_mut() = Some(HirDebugContext { get_path_fn });

        let _guard = HirDebugContextGuard(f);

        cb()
    })
}

pub fn display_hir(
    get_path_fn: fn(HirId) -> String,
    cb: impl FnOnce() -> std::fmt::Result,
) -> std::fmt::Result {
    dbg_hir(get_path_fn, cb)
}

impl fmt::Debug for HirId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let path = DEBUG_CONTEXT.with(|f| {
            let f = f.borrow();
            if let Some(ctx) = &*f {
                (ctx.get_path_fn)(*self)
            } else {
                panic!("Hir debug context not set");
            }
        });
        #[cfg(hir_id_span)]
        write!(f, "{path}:{} ~ {}", self.span, self.id)?;
        #[cfg(not(hir_id_span))]
        write!(f, "{path}{}", self.id)?;

        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct HirDisplayContext {
    depth: usize,
    attempt_no_line_breaks: bool,
    // style: HirDisplayStyle,
}

impl HirDisplayContext {
    fn new() -> Self {
        Self {
            depth: 0,
            attempt_no_line_breaks: false,
        }
    }

    fn make_child(&self) -> Self {
        Self {
            depth: self.depth + 4,
            attempt_no_line_breaks: false,
        }
    }

    fn attempt_no_line_breaks(&self) -> Self {
        Self {
            depth: self.depth,
            attempt_no_line_breaks: true,
        }
    }
}

pub fn display_mod_def(
    f: &mut fmt::Formatter,
    mod_def: &ModDef,
    hdc: HirDisplayContext,
) -> fmt::Result {
    display_item_list(f, &mod_def.items, hdc)?;

    Ok(())
}

impl fmt::Display for ModDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_mod_def(f, self, HirDisplayContext::new())
    }
}

pub fn display_item_list(
    f: &mut fmt::Formatter,
    item_list: &ItemList,
    hdc: HirDisplayContext,
) -> fmt::Result {
    let newlines = item_list.items.len() > 1;

    for item in &item_list.items {
        display_item(f, item, hdc)?;

        if newlines {
            writeln!(f)?;
            write!(f, "{:indent$}", "", indent = hdc.depth)?;
        }
    }

    Ok(())
}

impl fmt::Display for ItemList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_item_list(f, self, HirDisplayContext::new())
    }
}

pub fn display_item(f: &mut fmt::Formatter, item: &Item, hdc: HirDisplayContext) -> fmt::Result {
    match &item.kind {
        ItemKind::FnDef(fn_def) => {
            display_fn_def(f, fn_def, hdc)?;
        }
        ItemKind::Stmt(stmt) => {
            display_stmt(f, stmt, hdc)?;
        }
    }

    Ok(())
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_item(f, self, HirDisplayContext::new())
    }
}

trait Styling: Sized {
    fn keyword(self) -> owo_colors::Styled<Self>;
    fn operator(self) -> owo_colors::Styled<Self>;
    fn punctuation(self) -> owo_colors::Styled<Self>;
    fn num(self) -> owo_colors::Styled<Self>;
}

impl<'a> Styling for &'a str {
    fn keyword(self) -> owo_colors::Styled<Self> {
        Style::new().bright_blue().style(self)
    }

    fn operator(self) -> owo_colors::Styled<Self> {
        Style::new().bright_magenta().style(self)
    }

    fn punctuation(self) -> owo_colors::Styled<Self> {
        Style::new().bright_purple().style(self)
    }

    fn num(self) -> owo_colors::Styled<Self> {
        Style::new().cyan().dimmed().style(self)
    }
}

pub fn display_fn_def(
    f: &mut fmt::Formatter,
    fn_def: &FnDef,
    hdc: HirDisplayContext,
) -> fmt::Result {
    write!(
        f,
        "{:indent$}{} {}",
        "",
        "fn".keyword(),
        fn_def.name.text,
        indent = hdc.depth
    )?;

    if let Some(generics) = &fn_def.generics {
        write!(f, "<")?;
        if generics.params.len() > 1 {
            write!(f, "\n")?;
            write!(f, "{:indent$}", "", indent = hdc.depth + 4)?;
        }
        for (i, param) in generics.params.iter().enumerate() {
            if i != 0 {
                write!(f, "{:indent$}", "", indent = hdc.depth + 4)?;
            }

            match &param.kind {
                GenericParamKind::Type(t) => {
                    write!(f, "{}", t.text)?;
                }
                GenericParamKind::Const(name, ty) => {
                    write!(f, "{}: ", name.text)?;
                    display_ty(f, ty, hdc.make_child())?;
                }
            }

            writeln!(f, ",")?;
        }

        if generics.params.len() > 1 {
            write!(f, "{:indent$}", "", indent = hdc.depth)?;
        }

        write!(f, ">")?;
    }

    write!(f, "(")?;

    write!(f, "\n")?;

    for param in &fn_def.params {
        write!(f, "{:indent$}", "", indent = hdc.depth + 4)?;
        display_param(f, param, hdc.make_child())?;
        writeln!(f, ",")?;
    }

    write!(f, "{:indent$}) ", "", indent = hdc.depth)?;

    if let Some(ret_ty) = &fn_def.ret_ty {
        display_fn_ret_ty(f, ret_ty, hdc)?;
        write!(f, " ")?;
    }

    display_block(f, &fn_def.body, hdc.make_child())?;

    Ok(())
}

fn display_fn_ret_ty(
    f: &mut fmt::Formatter,
    ret_ty: &FnRetTy,
    hdc: HirDisplayContext,
) -> fmt::Result {
    write!(f, "{} ", "->".punctuation())?;
    display_ty(f, &ret_ty.ty, hdc)?;

    Ok(())
}

impl fmt::Display for FnRetTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_fn_ret_ty(f, self, HirDisplayContext::new())
    }
}

impl fmt::Display for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_fn_def(f, self, HirDisplayContext::new())
    }
}

pub fn display_block(f: &mut fmt::Formatter, block: &Block, hdc: HirDisplayContext) -> fmt::Result {
    let attempt_no_line_breaks = hdc.attempt_no_line_breaks && block.items.items.len() <= 1;

    write!(f, "{}", "{".punctuation())?;

    if !attempt_no_line_breaks {
        writeln!(f)?;
        write!(f, "{:indent$}", "", indent = hdc.depth)?;
    }

    display_item_list(
        f,
        &block.items,
        HirDisplayContext {
            attempt_no_line_breaks,
            ..hdc
        },
    )?;

    if !attempt_no_line_breaks {
        writeln!(f)?;
    }

    write!(f, "{}", "}".punctuation())?;

    Ok(())
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_block(f, self, HirDisplayContext::new())
    }
}

fn display_param(f: &mut fmt::Formatter, param: &FnParam, hdc: HirDisplayContext) -> fmt::Result {
    display_pat(f, &param.pat, hdc)?;
    write!(f, ": ")?;
    display_ty(f, &param.ty, hdc)?;

    Ok(())
}

impl fmt::Display for FnParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_param(f, self, HirDisplayContext::new())
    }
}

fn display_pat(f: &mut fmt::Formatter, pat: &Pat, hdc: HirDisplayContext) -> fmt::Result {
    match &pat.kind {
        PatKind::Ident(ident) => {
            write!(f, "{}", ident.text)?;
        }
        PatKind::TupleLike(pats) => {
            write!(f, "(")?;
            for (i, pat) in pats.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }

                display_pat(f, pat, hdc.make_child())?;
            }
            write!(f, ")")?;
        }
        PatKind::Wildcard(_) => {
            write!(f, "_")?;
        }
    }

    Ok(())
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_pat(f, self, HirDisplayContext::new())
    }
}

fn display_ty(f: &mut fmt::Formatter, ty: &TyRef, hdc: HirDisplayContext) -> fmt::Result {
    match &ty.kind {
        TyRefKind::Named(ident, generic_args) => {
            write!(f, "{}", ident.text)?;

            if !generic_args.args.is_empty() {
                write!(f, "<")?;
                for (i, arg) in generic_args.args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    display_ty_generic_arg(f, arg, hdc.make_child())?;
                }
                write!(f, ">")?;
            }
        }
        TyRefKind::Primitive(primitive) => {
            write!(f, "{}", primitive)?;
        }
        TyRefKind::Fn(fn_ty) => {
            write!(f, "{}(", "fn".keyword())?;
            for (i, param) in fn_ty.params.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }

                display_ty(f, param, hdc.make_child())?;
            }
            write!(f, ")")?;

            if let Some(ret_ty) = &fn_ty.ret_ty {
                write!(f, " -> ")?;
                display_ty(f, ret_ty, hdc.make_child())?;
            }
        }
    }

    Ok(())
}

impl fmt::Display for TyRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_ty(f, self, HirDisplayContext::new())
    }
}

fn display_ty_generic_arg(
    f: &mut fmt::Formatter,
    arg: &TyGenericArg,
    hdc: HirDisplayContext,
) -> fmt::Result {
    match &arg.kind {
        TyGenericArgKind::Type(ty) => {
            display_ty(f, ty, hdc)?;
        }
        TyGenericArgKind::ConstVal(val) => {
            write!(f, "{}", val)?;
        }
    }

    Ok(())
}

impl fmt::Display for TyGenericArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_ty_generic_arg(f, self, HirDisplayContext::new())
    }
}

fn display_primitive_ty(f: &mut fmt::Formatter, primitive: &PrimitiveTy) -> fmt::Result {
    let primitive_str = match primitive {
        PrimitiveTy::Bool => "bool",
        PrimitiveTy::I8 => "i8",
        PrimitiveTy::I16 => "i16",
        PrimitiveTy::I32 => "i32",
        PrimitiveTy::I64 => "i64",
        PrimitiveTy::I128 => "i128",
        PrimitiveTy::U8 => "u8",
        PrimitiveTy::U16 => "u16",
        PrimitiveTy::U32 => "u32",
        PrimitiveTy::U64 => "u64",
        PrimitiveTy::U128 => "u128",
        PrimitiveTy::F32 => "f32",
        PrimitiveTy::F64 => "f64",
        PrimitiveTy::Char => "char",
        PrimitiveTy::Str => "str",
    };

    write!(f, "{}", primitive_str.bright_white())
}

impl fmt::Display for PrimitiveTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_primitive_ty(f, self)
    }
}

fn display_stmt(f: &mut fmt::Formatter, stmt: &Stmt, hdc: HirDisplayContext) -> fmt::Result {
    match &stmt.kind {
        StmtKind::ExprStmt(expr) => {
            display_expr(f, expr, hdc)?;
        }
        StmtKind::LetStmt(let_stmt) => {
            display_let_stmt(f, let_stmt, hdc)?;
        }
        StmtKind::AssignmentStmt(assignment) => {
            display_assignment_stmt(f, assignment, hdc)?;
        }
        StmtKind::ForStmt(for_stmt) => {
            write!(f, "for ")?;
            display_pat(f, &for_stmt.pat, hdc)?;
            write!(f, " in ")?;
            display_expr(f, &for_stmt.iter, hdc)?;
            display_block(f, &for_stmt.body, hdc.make_child())?;
        }
        StmtKind::WhileStmt(while_stmt) => {
            write!(f, "while ")?;
            display_expr(f, &while_stmt.expr, hdc)?;
            display_block(f, &while_stmt.body, hdc.make_child())?;
        }
    }

    Ok(())
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_stmt(f, self, HirDisplayContext::new())
    }
}

fn display_let_stmt(
    f: &mut fmt::Formatter,
    let_stmt: &LetStmt,
    hdc: HirDisplayContext,
) -> fmt::Result {
    write!(f, "{} ", "let".keyword())?;

    if matches!(let_stmt.mutability, LetMutability::Mut(_)) {
        write!(f, "{} ", "mut".keyword())?;
    }

    display_pat(f, &let_stmt.pat, hdc)?;
    if let Some(init) = &let_stmt.init {
        write!(f, " {} ", "=".operator())?;
        display_expr(f, init, hdc)?;
    }

    Ok(())
}

impl fmt::Display for LetStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_let_stmt(f, self, HirDisplayContext::new())
    }
}

fn display_assignment_stmt(
    f: &mut fmt::Formatter,
    assignment: &AssignmentStmt,
    hdc: HirDisplayContext,
) -> fmt::Result {
    display_expr(f, &assignment.lhs, hdc)?;
    write!(f, " {} ", assignment.op)?;
    display_expr(f, &assignment.rhs, hdc)?;

    Ok(())
}

impl fmt::Display for AssignmentStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_assignment_stmt(f, self, HirDisplayContext::new())
    }
}

fn display_expr(f: &mut fmt::Formatter, expr: &Expr, hdc: HirDisplayContext) -> fmt::Result {
    match &expr.kind {
        ExprKind::Atom(atom) => {
            display_expr_atom(f, atom, hdc)?;
        }
        ExprKind::Binary(bin) => {
            display_expr(f, &bin.lhs, hdc)?;
            write!(f, " {} ", bin.op)?;
            display_expr(f, &bin.rhs, hdc)?;
        }
        ExprKind::CallExpr(call) => {
            display_expr(f, &call.callee, hdc)?;
            write!(f, "{}", "(".operator())?;

            for (i, arg) in call.args.args.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }

                display_expr(f, arg, hdc.make_child())?;
            }

            write!(f, "{}", ")".operator())?;
        }
        ExprKind::IndexExpr(index) => {
            display_expr(f, &index.base, hdc)?;
            write!(f, "{}", "[".operator())?;
            display_expr(f, &index.index, hdc)?;
            write!(f, "{}", "]".operator())?;
        }
        ExprKind::FieldAccess(field) => {
            display_expr(f, &field.base, hdc)?;
            write!(f, "{}{}", ".".operator(), field.field.text)?;
        }
        ExprKind::MethodCall(method) => {
            display_expr(f, &method.base, hdc)?;
            write!(
                f,
                "{}{}{}",
                ".".operator(),
                method.method.text,
                "(".operator()
            )?;

            for (i, arg) in method.args.args.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }

                display_expr(f, arg, hdc.make_child())?;
            }

            write!(f, "{}", ")".operator())?;
        }
    }

    Ok(())
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_expr(f, self, HirDisplayContext::new())
    }
}

fn display_bin_op(f: &mut fmt::Formatter, op: &BinOp) -> fmt::Result {
    let s = match op {
        BinOp::Add(_) => "+",
        BinOp::Sub(_) => "-",
        BinOp::Mul(_) => "*",
        BinOp::Div(_) => "/",
        BinOp::Mod(_) => "%",
        BinOp::And(_) => "&&",
        BinOp::Or(_) => "||",
        BinOp::BitAnd(_) => "&",
        BinOp::BitOr(_) => "|",
        BinOp::Xor(_) => "^",
        BinOp::Eq(_) => "==",
        BinOp::Neq(_) => "!=",
        BinOp::Lt(_) => "<",
        BinOp::LtEq(_) => "<=",
        BinOp::Gt(_) => ">",
        BinOp::GtEq(_) => ">=",
    };

    write!(f, "{}", s.operator())
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_bin_op(f, self)
    }
}

fn display_expr_atom(
    f: &mut fmt::Formatter,
    atom: &ExprAtom,
    hdc: HirDisplayContext,
) -> fmt::Result {
    match &atom.kind {
        ExprAtomKind::BlockExpr(block_expr) => {
            display_block(f, &block_expr.block, hdc.attempt_no_line_breaks())?;
        }
        ExprAtomKind::BreakExpr(b) => {
            write!(f, "{}", "break".keyword())?;
            if let Some(x) = &b.expr {
                write!(f, " ")?;
                display_expr(f, x, hdc)?;
            }
        }
        ExprAtomKind::ContinueExpr(_) => {
            write!(f, "{}", "continue".keyword())?;
        }
        ExprAtomKind::ReturnExpr(r) => {
            write!(f, "{}", "return".keyword())?;
            if let Some(x) = &r.expr {
                write!(f, " ")?;
                display_expr(f, x, hdc)?;
            }
        }
        ExprAtomKind::Ident(name) => {
            write!(f, "{}", name.text)?;
        }
        ExprAtomKind::IfExpr(if_expr) => {
            write!(f, "if ")?;
            display_expr(f, &if_expr.cond, hdc)?;
            display_expr(f, &if_expr.then, hdc.make_child())?;

            if let Some(else_block) = &if_expr.else_ {
                write!(f, " else ")?;
                display_expr(f, else_block, hdc.make_child())?;
            }
        }
        ExprAtomKind::LoopExpr(loop_expr) => {
            write!(f, "loop ")?;
            display_block(f, &loop_expr.body, hdc.make_child())?;
        }
        ExprAtomKind::Num(num) => {
            write!(f, "{}", num)?;
        }
        ExprAtomKind::Str(str_lit) => {
            display_str_literal(f, str_lit, hdc)?;
        }
        ExprAtomKind::TupleLikeExpr(tuple_like) => {
            write!(f, "(")?;
            for (i, expr) in tuple_like.exprs.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }

                display_expr(f, expr, hdc.make_child())?;
            }
            write!(f, ")")?;
        }
        ExprAtomKind::LambdaExpr(l) => {
            write!(f, "{}", "{".punctuation())?;
            if l.body.items.len() > 1 {
                writeln!(f)?;
                write!(f, "{:indent$}", "", indent = hdc.depth + 4)?;
            }
            if let Some(pl) = &l.lambda_param_list {
                for (i, param) in pl.params.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }

                    display_lambda_param(f, param, hdc.make_child())?;
                }

                if l.body.items.len() > 1 {
                    writeln!(f, " {}", "->".punctuation())?;
                    write!(f, "{:indent$}", "", indent = hdc.depth + 4)?;
                } else {
                    write!(f, " {} ", "->".punctuation())?;
                }
            }
            display_item_list(f, &l.body, hdc.make_child())?;
            if l.body.items.len() > 1 {
                writeln!(f)?;
                write!(f, "{:indent$}", "", indent = hdc.depth)?;
            }
            write!(f, "{}", "}".punctuation())?;
        }
    }

    Ok(())
}

fn display_lambda_param(
    f: &mut fmt::Formatter,
    param: &LambdaParam,
    hdc: HirDisplayContext,
) -> fmt::Result {
    display_pat(f, &param.pat, hdc)?;

    if let Some(ty) = &param.ty {
        write!(f, ": ")?;
        display_ty(f, &ty, hdc)?;
    }

    Ok(())
}

fn display_num_lit(f: &mut fmt::Formatter, num_lit: &NumLit) -> fmt::Result {
    match num_lit {
        NumLit::Bin(token) => write!(f, "{}", token.text().num()),
        NumLit::Oct(token) => write!(f, "{}", token.text().num()),
        NumLit::Dec(token) => write!(f, "{}", token.text().num()),
        NumLit::Hex(token) => write!(f, "{}", token.text().num()),
    }
}

impl fmt::Display for NumLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_num_lit(f, self)
    }
}

fn display_assignment_op(f: &mut fmt::Formatter, op: &AssignmentOp) -> fmt::Result {
    let op_str = match op {
        AssignmentOp::Assign(_) => "=",
        AssignmentOp::AddAssign(_) => "+=",
        AssignmentOp::SubAssign(_) => "-=",
        AssignmentOp::MulAssign(_) => "*=",
        AssignmentOp::DivAssign(_) => "/=",
        AssignmentOp::ModAssign(_) => "%=",
        AssignmentOp::BitAndAssign(_) => "&=",
        AssignmentOp::BitOrAssign(_) => "|=",
        AssignmentOp::BitXorAssign(_) => "^=",
    };

    write!(f, "{}", op_str.operator())
}

impl fmt::Display for AssignmentOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_assignment_op(f, self)
    }
}

fn display_str_literal(
    f: &mut fmt::Formatter,
    str_lit: &StrLiteral,
    hdc: HirDisplayContext,
) -> fmt::Result {
    write!(f, "{}", "\"".green())?;

    for fragment in &str_lit.fragments {
        display_str_literal_fragment(f, fragment, hdc)?;
    }

    write!(f, "{}", "\"".green())?;

    Ok(())
}

impl fmt::Display for StrLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_str_literal(f, self, HirDisplayContext::new())
    }
}

fn display_str_literal_fragment(
    f: &mut fmt::Formatter,
    fragment: &StrLiteralFragment,
    hdc: HirDisplayContext,
) -> fmt::Result {
    match &fragment.kind {
        StrLiteralFragmentKind::Text(text) => write!(f, "{}", text.text().green())?,
        StrLiteralFragmentKind::EscapeSequence(t, _) => write!(f, "{}", t.text().yellow())?,
        StrLiteralFragmentKind::EscapedChar(t, _) => write!(f, "{}", t.text().yellow())?,
        StrLiteralFragmentKind::Display(display) => {
            display_str_literal_display_fragment(f, display, hdc)?;
        }
        StrLiteralFragmentKind::Debug(debug) => {
            display_str_literal_debug_fragment(f, debug, hdc)?;
        }
    }

    Ok(())
}

impl fmt::Display for StrLiteralFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_str_literal_fragment(f, self, HirDisplayContext::new())
    }
}

fn display_str_literal_display_fragment(
    f: &mut fmt::Formatter,
    fragment: &StrLiteralDisplayFragment,
    hdc: HirDisplayContext,
) -> fmt::Result {
    write!(f, "{}", fragment.display_token.text().blue())?;

    display_expr(f, &fragment.expr, hdc.make_child().attempt_no_line_breaks())?;

    Ok(())
}

impl fmt::Display for StrLiteralDisplayFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_str_literal_display_fragment(f, self, HirDisplayContext::new())
    }
}

fn display_str_literal_debug_fragment(
    f: &mut fmt::Formatter,
    fragment: &StrLiteralDebugFragment,
    hdc: HirDisplayContext,
) -> fmt::Result {
    write!(f, "{}", fragment.debug_token.text().blue())?;

    display_expr(f, &fragment.expr, hdc.make_child().attempt_no_line_breaks())?;

    Ok(())
}

impl fmt::Display for StrLiteralDebugFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_str_literal_debug_fragment(f, self, HirDisplayContext::new())
    }
}
