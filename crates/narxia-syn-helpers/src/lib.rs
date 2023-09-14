use proc_macro2::extra::DelimSpan;
use proc_macro2::{Delimiter, Ident, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::parse::discouraged::{AnyDelimiter, Speculative};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{parse_quote, Data, DataEnum, DeriveInput, Expr, ExprCall, FnArg, MetaList, Token};

#[proc_macro_derive(DeriveT, attributes(T))]
pub fn derive_t(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    let DeriveInput {ident, data: Data::Enum(data), .. } = input else {
        return syn::Error::new_spanned(input, "expected struct").to_compile_error().into();
    };
    if ident != "SyntaxKind" {
        return syn::Error::new_spanned(ident, "expected struct SyntaxKind")
            .to_compile_error()
            .into();
    }
    let expanded = expand_t(ident, data).map_or_else(|e| e.to_compile_error().into(), |t| t.into());
    expanded
}

fn expand_t(ident: Ident, data: DataEnum) -> syn::Result<TokenStream> {
    let mut variants = Vec::new();
    for variant in &data.variants {
        let variant_name = &variant.ident;
        let fields = &variant.fields;
        if !fields.is_empty() {
            return Err(syn::Error::new_spanned(fields, "expected no fields"));
        }
        let Some(attr) = variant.attrs.iter().find(|it| it.path().is_ident("T")) else {
            continue;
        };
        let value = attr.parse_args::<TokenStream>()?;
        variants.push(quote! {
            (#value) => {$crate::#ident::#variant_name};
        });
    }

    Ok(quote! {
        macro_rules! T {
            #(#variants)*
        }

        pub(crate) use T;
    })
}

#[proc_macro_attribute]
pub fn parse_fn(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut input_fn = syn::parse_macro_input!(input as syn::ItemFn);
    let fn_name = input_fn.sig.ident.to_string();
    struct MLL {
        can_recover: Vec<Ident>,
        recovery: Option<Ident>,
    }
    impl Parse for MLL {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let mut can_recover = Vec::new();
            let mut recovery = None;
            for meta in Punctuated::<MetaList, Token![,]>::parse_terminated(input)? {
                if meta.path.is_ident("can_recover") {
                    can_recover.push(meta.parse_args::<Ident>()?);
                } else if meta.path.is_ident("recovery") {
                    recovery = Some(meta.parse_args::<Ident>()?);
                }
            }

            if !can_recover.is_empty() && recovery.is_none() {
                return Err(syn::Error::new(can_recover[0].span(), "expected recovery"));
            }

            Ok(Self {
                can_recover,
                recovery,
            })
        }
    }
    let MLL {
        can_recover,
        recovery,
    } = syn::parse_macro_input!(attr as MLL);
    let recovery_fn = match recovery {
        Some(recovery) => quote! { Some(recovery) },
        None => {
            quote! {None}
        }
    };
    input_fn.block.stmts.insert(
        0,
        parse_quote! {
            let _guard = p.guard(#fn_name, &[#(SyntaxKind::#can_recover,)*]);
        },
    );
    let expanded = quote! {
        #input_fn
    };
    expanded.into()
}

#[proc_macro]
pub fn parse_fn_decl(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ts = syn::parse(tokens).map_or_else(|e| e.to_compile_error(), |p| expand_fn_decl(p));
    // eprintln!("{}", ts);
    ts.into()
}

struct ParserSpecRule {
    name: Ident,
    args: Option<ParserFnArgs>,
    colon: Token![:],
    syntax_kind: Ident,
    sep: ParserSeparator,
    body: ParserSpecBody,
}

struct ParserFnArgs {
    args: Vec<FnArg>,
}

impl ParserFnArgs {
    fn none_if_empty(self) -> Option<Self> {
        if self.args.is_empty() {
            None
        } else {
            Some(self)
        }
    }
}

impl Parse for ParserFnArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut args = Vec::new();
        let fork = input.fork();
        let Ok((_, _, inside)) = input.parse_any_delimiter() else {
            input.advance_to(&fork);
            return Ok(Self {
                args,
            });
        };
        inside
            .parse_terminated(FnArg::parse, Token![,])
            .map(|args| Self {
                args: args.into_iter().collect(),
            })
    }
}

struct ParserSpecBody {
    instructions: ParserInstructionSet,
}

impl Parse for ParserSpecBody {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let instructions = input.parse()?;
        Ok(Self { instructions })
    }
}

struct ParserInstructionSet {
    instructions: Vec<ParserSpecInstruction>,
}

impl Parse for ParserInstructionSet {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut instructions = Vec::new();
        while !input.is_empty() {
            instructions.push(input.parse()?);
        }
        Ok(Self { instructions })
    }
}

enum ParserSpecInstruction {
    ParseCall(Token![$], ExprCall),
    Token(Token![$], Token![!], Delimiter, DelimSpan, TokenStream),
    ParseStmtExtra(Token![$], Token![/], ParseStmtExtra),
}

impl Parse for ParserSpecInstruction {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let dollar_sign = input.parse()?;
        if input.peek(Token![/]) {
            let slash = input.parse()?;
            let extra = input.parse()?;
            Ok(Self::ParseStmtExtra(dollar_sign, slash, extra))
        } else if input.peek(Token![!]) {
            let exclamation = input.parse()?;
            let (delim, delim_span, pb) = input.parse_any_delimiter()?;
            let ts = pb.parse()?;
            Ok(Self::Token(dollar_sign, exclamation, delim, delim_span, ts))
        } else {
            let call = input.parse()?;
            Ok(Self::ParseCall(dollar_sign, call))
        }
    }
}

enum ParseStmtExtra {
    Ws(WsExtra),
    Eof(EofExtra),
    Dbg(Ident),
    State(Ident, Token![:], Ident),
    RestoreState(Ident, Token![:], Ident),
    ErrUnexpected(Ident),
    If(IfExtra),
}

impl Parse for ParseStmtExtra {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![if]) {
            let if_extra = input.parse::<IfExtra>()?;
            return Ok(Self::If(if_extra));
        }

        let ident = input.parse::<Ident>()?;
        let name = ident.to_string();
        if name == "ws" {
            let ws_ident = ident;
            let colon = input.parse()?;
            let behavior = input.parse()?;
            Ok(Self::Ws(WsExtra {
                ws_ident,
                colon,
                behavior,
            }))
        } else if name == "eof" {
            let eof_ident = ident;
            let colon = input.parse()?;
            let behavior = input.parse()?;
            Ok(Self::Eof(EofExtra {
                eof_ident,
                colon,
                behavior,
            }))
        } else if name == "dbg" {
            Ok(Self::Dbg(ident))
        } else if name == "state" {
            Ok(Self::State(ident, input.parse()?, input.parse()?))
        } else if name == "restore_state" {
            Ok(Self::RestoreState(ident, input.parse()?, input.parse()?))
        } else if name == "err_unexpected" {
            Ok(Self::ErrUnexpected(ident))
        } else {
            Err(syn::Error::new(ident.span(), "expected ws, eof, if, state, restore_state, or err_unexpected"))
        }
    }
}

struct WsExtra {
    ws_ident: Ident,
    colon: Token![:],
    behavior: WsBehaviorParsed,
}

struct WsBehaviorParsed {
    behavior: WsBehavior,
    ident: Ident,
}

struct WsBehavior {
    v: u8,
}

impl WsBehavior {
    const WS: WsBehavior = WsBehavior { v: 0b001 };
    const COMMENT: WsBehavior = WsBehavior { v: 0b010 };
    const NEWLINE: WsBehavior = WsBehavior { v: 0b100 };
}

impl std::ops::BitOr for WsBehavior {
    type Output = WsBehavior;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self { v: self.v | rhs.v }
    }
}

impl Parse for WsBehaviorParsed {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        let name = ident.to_string();
        let mut behavior = WsBehavior { v: 0 };
        for c in name.chars() {
            match c {
                'w' => behavior = behavior | WsBehavior::WS,
                'c' => behavior = behavior | WsBehavior::COMMENT,
                'n' => behavior = behavior | WsBehavior::NEWLINE,
                _ => return Err(syn::Error::new(ident.span(), "invalid whitespace behavior")),
            }
        }
        Ok(Self { behavior, ident })
    }
}

struct EofExtra {
    eof_ident: Ident,
    colon: Token![:],
    behavior: EofBehaviorParsed,
}

enum EofBehavior {
    EarlyRet,
}

struct EofBehaviorParsed {
    behavior: EofBehavior,
    error: Option<(Token![+], ExprCall)>,
}

impl Parse for EofBehaviorParsed {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        let behavior = match ident.to_string().as_str() {
            "early_ret" => EofBehavior::EarlyRet,
            _ => return Err(syn::Error::new(ident.span(), "invalid eof behavior")),
        };
        let error = if input.peek(Token![+]) {
            let plus = input.parse()?;
            let call = input.parse::<ExprCall>()?;
            let is_err = match &*call.func {
                Expr::Path(p) => p.path.is_ident("err"),
                _ => false,
            };
            if !is_err {
                return Err(syn::Error::new(call.func.span(), "expected err(...)"));
            }
            Some((plus, call))
        } else {
            None
        };
        Ok(Self { behavior, error })
    }
}

struct ParserSeparator {
    colon1: Token![:],
    colon2: Token![:],
    eq: Token![=],
}

impl Parse for ParserSeparator {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let colon1 = input.parse()?;
        let colon2 = input.parse()?;
        let eq = input.parse()?;
        Ok(Self { colon1, colon2, eq })
    }
}

impl Parse for ParserSpecRule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let args = input.parse::<ParserFnArgs>()?.none_if_empty();
        let colon = input.parse()?;
        let syntax_kind = input.parse()?;
        let sep = input.parse()?;
        let body = input.parse()?;
        Ok(Self {
            name,
            args,
            colon,
            syntax_kind,
            sep,
            body,
        })
    }
}

fn expand_fn_decl(p: ParserSpecRule) -> TokenStream {
    let name = &p.name;
    let syntax_kind = &p.syntax_kind;
    let end_expr = quote! {
        p.ev.end(m, SyntaxKind::#syntax_kind)
    };
    let args = p.args.as_ref().map_or_else(
        || quote! {},
        |args| {
            let a = &args.args;
            quote! { #(#a,)* }
        },
    );
    let mut body = TokenStream::new();
    expand_parser_spec_instruction_set(&p.body.instructions, &end_expr, &mut body);
    quote! {
        #[parse_fn]
        fn #name(p: &mut Parser, #args) -> CompletedMarker {
            let m = p.ev.begin();
            #body
            #end_expr
        }
    }
}

fn expand_parser_spec_instruction_set(
    is: &ParserInstructionSet,
    end_expr: &TokenStream,
    to: &mut TokenStream,
) {
    let recovery_check_and_return = quote! {
        if p.is_recovering() {
            return #end_expr;
        }
    };
    for instruction in &is.instructions {
        match instruction {
            ParserSpecInstruction::ParseCall(_, call) => {
                let mut call = call.clone();
                call.args.insert(0, parse_quote! {p});
                to.extend(quote! {
                    #call;
                    #recovery_check_and_return
                });
            }
            ParserSpecInstruction::Token(_, _, _, _, ts) => {
                to.extend(quote_spanned! {ts.span()=>p.expect(T![#ts]);#recovery_check_and_return});
            }
            ParserSpecInstruction::ParseStmtExtra(_, _, extra) => match extra {
                ParseStmtExtra::Ws(ws) => {
                    expand_ws_extra(ws, to);
                }
                ParseStmtExtra::Eof(eof) => {
                    expand_eof_behavior(eof, end_expr, to);
                }
                ParseStmtExtra::Dbg(ident) => {
                    to.extend(quote_spanned! {ident.span()=>
                        p.dbg();
                    });
                }
                ParseStmtExtra::State(state, _colon, ident) => {
                    to.extend(quote_spanned! {state.span()=>
                        let #ident = p.state();
                    });
                }
                ParseStmtExtra::RestoreState(state, _colon, ident) => {
                    to.extend(quote_spanned! {state.span()=>
                        p.restore_state(#ident);
                    });
                }
                ParseStmtExtra::ErrUnexpected(ident) => {
                    to.extend(quote_spanned! {ident.span()=>
                        p.err_unexpected();
                    });
                }
                ParseStmtExtra::If(if_extra) => {
                    expand_if_extra(if_extra, end_expr, to);
                }
            },
        }
    }
}

fn expand_ws_extra(ws_extra: &WsExtra, to: &mut TokenStream) {
    to.extend(quote_spanned! {
        ws_extra.ws_ident.span()=>
        p.skip_ws();
    })
}

fn expand_eof_behavior(eof_extra: &EofExtra, end_expr: &TokenStream, to: &mut TokenStream) {
    match eof_extra.behavior.behavior {
        EofBehavior::EarlyRet => {
            let err_call = eof_extra
                .behavior
                .error
                .clone()
                .map(|(_, call)| {
                    quote_spanned! {
                        call.span()=>
                        {
                            use crate::parser::parse_event_handler::ParseErrorInfo::*;
                            p.#call
                        }
                    }
                })
                .into_iter();
            to.extend(quote_spanned! {
                eof_extra.eof_ident.span()=>
                if p.at_eof() {
                    #(#err_call;)*
                    return #end_expr;
                }
            })
        }
    }
}

enum IfCondition {
    At {
        at_token: Ident,
        delimiter: (Delimiter, DelimSpan),
        at_token_tt: TokenStream,
    },
    Either(Token![||], Delimiter, DelimSpan, Vec<IfCondition>),
}

impl Parse for IfCondition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![||]) {
            let pipe2 = input.parse()?;
            let (delimiter, delim_span, conditions) = input.parse_any_delimiter()?;
            let conditions = conditions
                .parse_terminated(IfCondition::parse, Token![,])
                .map(|conditions| conditions.into_iter().collect())?;
            Ok(Self::Either(pipe2, delimiter, delim_span, conditions))
        } else {
            let ident = input.parse::<Ident>()?;
            if ident == "at" {
                let (delimiter, delim_span, at_token_tt) = input.parse_any_delimiter()?;
                Ok(Self::At {
                    at_token: ident,
                    delimiter: (delimiter, delim_span),
                    at_token_tt: at_token_tt.parse()?,
                })
            } else {
                Err(syn::Error::new(ident.span(), "expected at"))
            }
        }
    }
}

fn expand_condition_eval(condition: &IfCondition, ts: &mut TokenStream) {
    match condition {
        IfCondition::At {
            at_token,
            delimiter: _,
            at_token_tt,
        } => {
            ts.extend(quote_spanned! {at_token.span()=>
                p.#at_token (T![#at_token_tt])
            });
        }
        IfCondition::Either(oror, delim, delim_span, conditions) => {
            for (idx, condition) in conditions.iter().enumerate() {
                if idx > 0 {
                    ts.extend(quote_spanned! {oror.span()=>
                        ||
                    });
                }
                expand_condition_eval(condition, ts);
            }
        }
    }
}

struct IfExtra {
    if_token: Token![if],
    condition_expr: IfCondition,
    then_block: IfInstrBlock,
    else_ifs: Vec<IfExtraElseIf>,
    else_clause: Option<IfExtraElseClause>,
}

impl Parse for IfExtra {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let if_token = input.parse()?;
        let condition_expr = input.parse()?;
        let then_block = input.parse()?;
        let mut else_ifs = Vec::new();
        let mut fork = input.fork();
        while let Ok(else_if) = input.parse::<IfExtraElseIf>() {
            else_ifs.push(else_if);
            fork = input.fork();
        }
        input.advance_to(&fork);
        let else_clause = match input.parse() {
            Ok(else_clause) => Some(else_clause),
            Err(_) => {
                input.advance_to(&fork);
                None
            }
        };
        Ok(Self {
            if_token,
            condition_expr,
            then_block,
            else_ifs,
            else_clause,
        })
    }
}

struct IfInstrBlock {
    delimiter: (Delimiter, DelimSpan),
    instructions: ParserInstructionSet,
}

impl Parse for IfInstrBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let (delimiter, delim_span, instructions) = input.parse_any_delimiter()?;
        if !matches!(delimiter, Delimiter::Brace) {
            return Err(syn::Error::new(delim_span.span(), "expected block"));
        }
        let instructions = instructions.parse()?;
        Ok(Self {
            delimiter: (delimiter, delim_span),
            instructions,
        })
    }
}

struct IfExtraElseIf {
    dollar_sign: Token![$],
    slash: Token![/],
    else_token: Token![else],
    if_token: Token![if],
    condition: IfCondition,
    then_block: IfInstrBlock,
}

impl Parse for IfExtraElseIf {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let dollar_sign = input.parse()?;
        let slash = input.parse()?;
        let else_token = input.parse()?;
        let if_token = input.parse()?;
        let condition = input.parse()?;
        let then_block = input.parse()?;
        Ok(Self {
            dollar_sign,
            slash,
            else_token,
            if_token,
            condition,
            then_block,
        })
    }
}

struct IfExtraElseClause {
    dollar_sign: Token![$],
    slash: Token![/],
    else_token: Token![else],
    else_block: IfInstrBlock,
}

impl Parse for IfExtraElseClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let dollar_sign = input.parse()?;
        let slash = input.parse()?;
        let else_token = input.parse()?;
        let else_block = input.parse()?;
        Ok(Self {
            dollar_sign,
            slash,
            else_token,
            else_block,
        })
    }
}

fn expand_if_extra(if_extra: &IfExtra, end_expr: &TokenStream, to: &mut TokenStream) {
    let mut if_condition = TokenStream::new();
    expand_condition_eval(&if_extra.condition_expr, &mut if_condition);

    let mut if_true = TokenStream::new();
    expand_parser_spec_instruction_set(&if_extra.then_block.instructions, end_expr, &mut if_true);

    to.extend(quote_spanned! {if_extra.if_token.span()=>
        if #if_condition {
            #if_true
        }
    });

    let mut else_ifs = Vec::new();
    for else_if in &if_extra.else_ifs {
        let mut else_if_true = TokenStream::new();
        expand_parser_spec_instruction_set(
            &else_if.then_block.instructions,
            end_expr,
            &mut else_if_true,
        );
        let mut else_if_condition = TokenStream::new();
        expand_condition_eval(&else_if.condition, &mut else_if_condition);
        else_ifs.push(quote_spanned! {else_if.if_token.span()=>
            else if #else_if_condition {
                #else_if_true
            }
        });
    }

    to.extend(else_ifs);

    if let Some(else_clause) = &if_extra.else_clause {
        let mut else_clause_true = TokenStream::new();
        expand_parser_spec_instruction_set(
            &else_clause.else_block.instructions,
            end_expr,
            &mut else_clause_true,
        );
        to.extend(quote_spanned! {else_clause.else_token.span()=>
            else {
                #else_clause_true
            }
        });
    }
}
