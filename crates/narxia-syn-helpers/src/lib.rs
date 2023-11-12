#![allow(dead_code, unused_imports, unused_variables)] // FIXME: fix

use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::parse::discouraged::Speculative;
use syn::parse::{Parse, ParseStream, Peek};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{
    braced, bracketed, parenthesized, parse_quote, parse_quote_spanned, token, Block, Data,
    DataEnum, DeriveInput, Expr, ExprCall, ExprIf, FnArg, MetaList, Path, Token, Visibility,
};

#[proc_macro_derive(DeriveT, attributes(T))]
pub fn derive_t(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);
    let DeriveInput {
        ident,
        data: Data::Enum(data),
        attrs,
        ..
    } = input
    else {
        return syn::Error::new_spanned(input, "expected enum")
            .to_compile_error()
            .into();
    };
    if ident != "SyntaxKind" {
        return syn::Error::new_spanned(ident, "expected enum SyntaxKind")
            .to_compile_error()
            .into();
    }
    let Some(sk_path) = attrs.iter().find(|it| it.path().is_ident("T")) else {
        return syn::Error::new(Span::call_site(), "expected #[T(...)]")
            .to_compile_error()
            .into();
    };
    let sk_path = match sk_path.parse_args::<Path>() {
        Ok(p) => p,
        Err(e) => return e.to_compile_error().into(),
    };
    let expanded =
        expand_t(ident, data, sk_path).map_or_else(|e| e.to_compile_error().into(), |t| t.into());
    expanded
}

fn expand_t(ident: Ident, data: DataEnum, sk_path: Path) -> syn::Result<TokenStream> {
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
            (#value) => {$crate::#sk_path::#variant_name};
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
    let ts = syn::parse(tokens)
        .and_then(expand_fn_decl)
        .unwrap_or_else(|e| e.to_compile_error());
    // eprintln!("{}", ts);
    ts.into()
}

struct ParserSpecRule {
    vis: Visibility,
    name: Ident,
    args: Option<ParserFnArgs>,
    colon: Token![:],
    syntax_kind: Ident,
    sep: ParserSeparator,
    body: ParserSpecBody,
}

impl Parse for ParserSpecRule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let vis = input.parse()?;
        let name = input.parse()?;
        let args = if input.peek(token::Paren) {
            Some(input.parse::<ParserFnArgs>()?)
        } else {
            None
        };
        let colon = input.parse()?;
        let syntax_kind = input.parse()?;
        let sep = input.parse()?;
        let body = input.parse()?;
        Ok(Self {
            vis,
            name,
            args,
            colon,
            syntax_kind,
            sep,
            body,
        })
    }
}

struct ParserFnArgs {
    paren: token::Paren,
    args: Punctuated<FnArg, Comma>,
}

impl Parse for ParserFnArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let paren;
        Ok(Self {
            paren: parenthesized!(paren in input),
            args: paren.parse_terminated(FnArg::parse, Token![,])?,
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
    Token(Token![$], Token![!], token::Bracket, TokenStream),
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
            let ts;
            Ok(Self::Token(
                dollar_sign,
                input.parse()?,
                bracketed!(ts in input),
                ts.parse()?,
            ))
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
    Match(MatchExtra),
}

impl Parse for ParseStmtExtra {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![if]) {
            let if_extra = input.parse::<IfExtra>()?;
            return Ok(Self::If(if_extra));
        } else if input.peek(Token![match]) {
            let match_extra = input.parse::<MatchExtra>()?;
            return Ok(Self::Match(match_extra));
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
            Err(syn::Error::new(
                ident.span(),
                "expected ws, eof, if, match, state, restore_state, or err_unexpected",
            ))
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

#[derive(PartialEq, Eq)]
struct WsBehavior {
    v: u8,
}

impl WsBehavior {
    const WS: WsBehavior = WsBehavior { v: 0b001 };
    const COMMENT: WsBehavior = WsBehavior { v: 0b010 };
    const NEWLINE: WsBehavior = WsBehavior { v: 0b100 };
    const WCN: WsBehavior = WsBehavior { v: 0b111 };
    const WC: WsBehavior = WsBehavior { v: 0b011 };
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

fn expand_fn_decl(p: ParserSpecRule) -> syn::Result<TokenStream> {
    let name = &p.name;
    let syntax_kind = &p.syntax_kind;
    let end_expr = quote! {
        p.ev.end(m, SyntaxKind::#syntax_kind)
    };
    let args = p.args.as_ref().map_or_else(
        || quote! {},
        |args| {
            let a = &args.args;
            quote! { #a }
        },
    );
    let mut body = TokenStream::new();
    expand_parser_spec_instruction_set(&p.body.instructions, &end_expr, &mut body)?;
    let vis = p.vis;
    Ok(quote! {
        #[parse_fn]
        #vis fn #name(p: &mut Parser, #args) -> CompletedMarker {
            let m = p.ev.begin();
            #body
            #end_expr
        }
    })
}

fn expand_parser_spec_instruction_set(
    is: &ParserInstructionSet,
    end_expr: &TokenStream,
    to: &mut TokenStream,
) -> syn::Result<()> {
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
            ParserSpecInstruction::Token(_, _, _, ts) => {
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
                    expand_if_extra(if_extra, end_expr, to)?;
                }
                ParseStmtExtra::Match(match_extra) => {
                    expand_match(match_extra, end_expr, to)?;
                }
            },
        }
    }

    Ok(())
}

fn expand_ws_extra(ws_extra: &WsExtra, to: &mut TokenStream) {
    let span = ws_extra.behavior.ident.span();
    let name = match ws_extra.behavior.behavior {
        WsBehavior::WCN => {
            format_ident!("skip_ws_wcn", span = span)
        }
        WsBehavior::WC => {
            format_ident!("skip_ws_wc", span = span)
        }
        _ => {
            unreachable!()
        }
    };
    to.extend(quote_spanned! {
        ws_extra.ws_ident.span()=>
        p.#name();
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
        bracket: token::Bracket,
        at_token_tt: TokenStream,
    },
    Either(Token![||], token::Paren, Punctuated<IfCondition, Token![,]>),
}

impl Parse for IfCondition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![||]) {
            let conditions;
            Ok(Self::Either(
                input.parse()?,
                parenthesized!(conditions in input),
                conditions.parse_terminated(IfCondition::parse, Token![,])?,
            ))
        } else {
            let ident = input.parse::<Ident>()?;
            if ident == "at" {
                let at_token_tt;
                Ok(Self::At {
                    at_token: ident,
                    bracket: bracketed!(at_token_tt in input),
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
            bracket: _,
            at_token_tt,
        } => {
            ts.extend(quote_spanned! {at_token.span()=>
                p.#at_token (T![#at_token_tt])
            });
        }
        IfCondition::Either(oror, paren, conditions) => {
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
    then_block: InstrBlock,
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

struct InstrBlock {
    delimiter: token::Brace,
    instructions: ParserInstructionSet,
}

impl Parse for InstrBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let instructions;
        Ok(Self {
            delimiter: braced!(instructions in input),
            instructions: instructions.parse()?,
        })
    }
}

struct IfExtraElseIf {
    dollar_sign: Token![$],
    slash: Token![/],
    else_token: Token![else],
    if_token: Token![if],
    condition: IfCondition,
    then_block: InstrBlock,
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
    else_block: InstrBlock,
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

fn expand_if_extra(
    if_extra: &IfExtra,
    end_expr: &TokenStream,
    to: &mut TokenStream,
) -> syn::Result<()> {
    let mut if_condition = TokenStream::new();
    expand_condition_eval(&if_extra.condition_expr, &mut if_condition);

    let mut if_true = TokenStream::new();
    expand_parser_spec_instruction_set(&if_extra.then_block.instructions, end_expr, &mut if_true)?;

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
        )?;
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
        )?;
        to.extend(quote_spanned! {else_clause.else_token.span()=>
            else {
                #else_clause_true
            }
        });
    }

    Ok(())
}

struct IfChain {
    ifs: Vec<ExprIf>,
    else_clause: Option<Block>,
}

impl IfChain {
    fn new() -> Self {
        Self {
            ifs: Vec::new(),
            else_clause: None,
        }
    }

    #[track_caller]
    fn add_if(&mut self, if_: ExprIf) {
        assert!(if_.else_branch.is_none());
        self.ifs.push(if_);
    }

    fn add_else_branch(&mut self, else_branch: Block) {
        assert!(self.else_clause.is_none());
        self.else_clause = Some(else_branch);
    }

    fn add_else_branch_default(&mut self, else_branch: Block) {
        if self.else_clause.is_some() {
            return;
        }
        self.else_clause = Some(else_branch);
    }

    fn compile(&self, ts: &mut TokenStream) {
        for (idx, if_) in self.ifs.iter().enumerate() {
            if idx == 0 {
                ts.extend(quote_spanned! {if_.if_token.span()=>
                    #if_
                });
            } else {
                ts.extend(quote_spanned! {if_.if_token.span()=>
                    else #if_
                });
            }
        }
        if let Some(else_clause) = &self.else_clause {
            ts.extend(quote_spanned! {else_clause.brace_token.span=>
                else #else_clause
            });
        }
    }
}

fn expand_match(
    extra: &MatchExtra,
    end_expr: &TokenStream,
    to: &mut TokenStream,
) -> syn::Result<()> {
    let mut if_chain = IfChain::new();
    for arm in &extra.arms {
        expand_match_arm(arm, end_expr, &mut if_chain)?;
    }

    if_chain.add_else_branch_default(parse_quote_spanned! {extra.delimiter.span.span()=>
        {p.err_unexpected();}
    });

    if_chain.compile(to);

    Ok(())
}

fn expand_match_arm(
    arm: &MatchExtraArm,
    end_expr: &TokenStream,
    if_chain: &mut IfChain,
) -> syn::Result<()> {
    match &arm.arm_code {
        MatchExtraArmCode::ImmediateExpect(_) => {
            for pat in &arm.pat.pat {
                match pat {
                    MatchExtraArmSelector::T { delimiter: _, tt } => {
                        if_chain.add_if(parse_quote_spanned! {tt.span()=>
                            if p.at(T![#tt]) {
                                p.expect(T![#tt]);
                            }
                        });
                    }
                    MatchExtraArmSelector::CatchAll { wild } => {
                        return Err(syn::Error::new_spanned(
                            wild,
                            "_ cannot be combined with ! in a match arm",
                        ));
                    }
                }
            }
        }
        MatchExtraArmCode::InstrBlock(MatchExtraArmCodeInstrBlock {
            arrow,
            instructions,
        }) => {
            let mut ins = TokenStream::new();
            expand_parser_spec_instruction_set(&instructions.instructions, end_expr, &mut ins)?;
            match &*arm.pat.pat {
                [MatchExtraArmSelector::CatchAll { .. }] => {
                    if_chain.add_else_branch(parse_quote_spanned! {arrow.span()=>{#ins}});
                }
                no_catch_all => {
                    let pats = no_catch_all
                        .iter()
                        .map(|it| match it {
                            MatchExtraArmSelector::CatchAll { wild } => {
                                return Err(syn::Error::new_spanned(
                                    wild,
                                    "_ should be alone in an match arm",
                                ));
                            }
                            MatchExtraArmSelector::T { delimiter: _, tt } => Ok(tt),
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    if_chain.add_if(parse_quote_spanned! {arrow.span()=>
                        if #( p.at(T![#pats]) )||* {
                            #ins
                        }
                    });
                }
            }
        }
    }

    Ok(())
}

struct MatchExtra {
    match_token: Token![match],
    delimiter: token::Brace,
    arms: Vec<MatchExtraArm>,
}

impl Parse for MatchExtra {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let arms_buffer;
        Ok(Self {
            match_token: input.parse()?,
            delimiter: braced!(arms_buffer in input),
            arms: arms_buffer.parse::<VecParser<MatchExtraArm>>()?.0,
        })
    }
}

struct MatchExtraArm {
    pat: MatchExtraArmPat,
    arm_code: MatchExtraArmCode,
}

impl Parse for MatchExtraArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pat = input.parse()?;
        let arm_code = input.parse()?;
        Ok(Self { pat, arm_code })
    }
}

enum MatchExtraArmCode {
    ImmediateExpect(Token![!]),
    InstrBlock(MatchExtraArmCodeInstrBlock),
}

impl Parse for MatchExtraArmCode {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![!]) {
            let exclamation = input.parse()?;
            Ok(Self::ImmediateExpect(exclamation))
        } else if input.peek(Token![=>]) {
            let instr_block = input.parse()?;
            Ok(Self::InstrBlock(instr_block))
        } else {
            Err(syn::Error::new(input.span(), "expected ! or =>"))
        }
    }
}

struct MatchExtraArmCodeInstrBlock {
    arrow: Token![=>],
    instructions: InstrBlock,
}

impl Parse for MatchExtraArmCodeInstrBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let arrow = input.parse()?;
        let instructions = input.parse()?;
        Ok(Self {
            arrow,
            instructions,
        })
    }
}

struct MatchExtraArmPat {
    pat: Vec<MatchExtraArmSelector>,
}

impl Parse for MatchExtraArmPat {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pat = input.parse::<VecParser<MatchExtraArmSelector>>()?.0;
        Ok(Self { pat })
    }
}

struct VecParser<T>(Vec<T>);

impl<T: Parse> Parse for VecParser<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut vec = Vec::new();
        let mut fork = input.fork();
        while let Ok(t) = input.parse::<T>() {
            vec.push(t);
            fork = input.fork();
        }
        input.advance_to(&fork);
        Ok(Self(vec))
    }
}

enum MatchExtraArmSelector {
    T {
        delimiter: token::Bracket,
        tt: TokenStream,
    },
    CatchAll {
        wild: Token![_],
    },
}

impl Parse for MatchExtraArmSelector {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![_]) {
            return Ok(Self::CatchAll {
                wild: input.parse()?,
            });
        }
        let tt;
        Ok(Self::T {
            delimiter: bracketed!(tt in input),
            tt: tt.parse()?,
        })
    }
}

#[proc_macro]
pub fn syntree_node(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syn::parse_macro_input!(input as SyntreeNodeDef)
        .expand()
        .into()
}

struct SyntreeNodeDef {
    name: Ident,
    kind_name: Option<SyntreeNodeKindName>,
    children_info: ChildrenInfo,
}

enum ChildrenInfo {
    Some {
        eq: Token![=],
        children_def: SyntreeNodeChildrenDef,
    },
    None {
        not: Token![!],
    },
}

impl Parse for ChildrenInfo {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![=]) {
            let eq = input.parse()?;
            let children_def = input.parse()?;
            Ok(Self::Some { eq, children_def })
        } else if input.peek(Token![!]) {
            let not = input.parse()?;
            Ok(Self::None { not })
        } else {
            Err(input.error("expected = or !"))
        }
    }
}

impl SyntreeNodeDef {
    fn expand(&self) -> TokenStream {
        self.expand_true()
            .unwrap_or_else(syn::Error::into_compile_error)
    }

    fn expand_true(&self) -> syn::Result<TokenStream> {
        let name = &self.name;
        let sk_name = self
            .kind_name
            .as_ref()
            .map(|it| &it.kind_name)
            .unwrap_or(name);

        let mut expanded = quote! {
            pub struct #name {
                pub(crate) node: Node,
            }

            impl TreeNode for #name {
                unsafe fn cast_from_node_raw(n: Node) -> Self {
                    debug_assert!(Self::can_cast_from_syntax_kind(n.kind()));
                    Self { node: n }
                }

                fn can_cast_from_syntax_kind(kind: SyntaxKind) -> bool {
                    kind == SyntaxKind::#sk_name
                }

                fn get_syntax_kind(&self) -> SyntaxKind {
                    self.node.kind()
                }

                fn get_node(&self) -> &Node {
                    &self.node
                }
            }
        };

        if let ChildrenInfo::Some { children_def, .. } = &self.children_info {
            let mut accessors = Vec::new();
            let mut token_accessors = Vec::new();
            children_def
                .child_ref_expr
                .compile(&mut accessors, &mut token_accessors);

            let add_ws_accessor =
                // if the node only consists of just one token, then there won't be any whitespace.
                !matches!(&children_def.child_ref_expr,SyntreeNodeChildrenRefExpr::Token(..));

            if add_ws_accessor {
                token_accessors.push(TokenAccessor {
                    name: format_ident!("get_trivia"),
                    kind: AccessorKind::List,
                    token: quote! {composed_trivia},
                    allow_empty: true,
                });
            }

            let mut accessors_ts = TokenStream::new();
            let mut accessor_calls = TokenStream::new();

            for accessor in accessors {
                let (accessor, accessor_call_list) = accessor.expand(name);
                accessors_ts.extend(accessor);
                accessor_calls.extend(accessor_call_list);
            }
            for token_accessor in token_accessors {
                let (accessor, accessor_call_list) = token_accessor.expand(name);
                accessors_ts.extend(accessor);
                accessor_calls.extend(accessor_call_list);
            }
            expanded.extend(quote! {
                impl #name {
                    #accessors_ts

                    pub fn call_accessors(&self, tests_data: &mut tests_data::AccessorCalledDataList) {
                        #accessor_calls
                    }
                }
            })
        }
        Ok(expanded)
    }
}

impl Parse for SyntreeNodeDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let kind_name = if input.peek(Token![:]) {
            Some(input.parse()?)
        } else {
            None
        };
        let children_info = input.parse()?;
        Ok(Self {
            name,
            kind_name,
            children_info,
        })
    }
}

struct SyntreeNodeKindName {
    colon: Token![:],
    kind_name: Ident,
}

impl Parse for SyntreeNodeKindName {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let colon = input.parse()?;
        let kind_name = input.parse()?;
        Ok(Self { colon, kind_name })
    }
}

struct SyntreeNodeChildrenDef {
    child_ref_expr: SyntreeNodeChildrenRefExpr,
}

impl Parse for SyntreeNodeChildrenDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            child_ref_expr: input.parse()?,
        })
    }
}

struct SyntreeNodeChildrenRefExprSet {
    set: Vec<SyntreeNodeChildrenRefExpr>,
}

impl Parse for SyntreeNodeChildrenRefExprSet {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            set: input.parse::<VecParser<_>>()?.0,
        })
    }
}

enum SyntreeNodeChildrenRefExpr {
    SelfRef,
    Name(Ident),
    NamedRef(Ident, Token![:], Ident),
    Token(Ident, Token![!], token::Bracket, TokenStream),
    Repeat(Token![*], Box<SyntreeNodeChildrenRefExpr>),
    Maybe(Token![?], Box<SyntreeNodeChildrenRefExpr>),
    ParenSet(token::Paren, SyntreeNodeChildrenRefExprSet),
    Either(
        Token![|],
        token::Bracket,
        Punctuated<SyntreeNodeChildrenRefExpr, Comma>,
    ),
}

impl Parse for SyntreeNodeChildrenRefExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![|]) {
            let either;
            Ok(Self::Either(
                input.parse()?,
                bracketed!(either in input),
                either.parse_terminated(Self::parse, Token![,])?,
            ))
        } else if input.peek(Token![*]) {
            let star = input.parse()?;
            let expr = input.parse()?;
            Ok(Self::Repeat(star, Box::new(expr)))
        } else if input.peek(Token![?]) {
            let question = input.parse()?;
            let expr = input.parse()?;
            Ok(Self::Maybe(question, Box::new(expr)))
        } else if input.peek(token::Paren) {
            let paren;
            Ok(Self::ParenSet(
                parenthesized!(paren in input),
                paren.parse()?,
            ))
        } else if input.peek(Token![self]) {
            Ok(Self::SelfRef)
        } else {
            let Ok(id) = input.parse::<Ident>() else {
                return Err(input.error("expected |, *, ?, (, self, or identifier"));
            };

            if input.peek(Token![:]) {
                let colon = input.parse()?;
                let refr = input.parse()?;
                Ok(Self::NamedRef(id, colon, refr))
            } else if input.peek(Token![!]) {
                let exclamation = input.parse()?;
                let tt;
                let bracket = bracketed!(tt in input);
                let tt = tt.parse()?;
                Ok(Self::Token(id, exclamation, bracket, tt))
            } else {
                Ok(Self::Name(id))
            }
        }
    }
}

struct CompileConfig {
    either_passed: bool,
    rep_passed: bool,
    maybe_passed: bool,
    is_at_beginning: bool,
}

impl SyntreeNodeChildrenRefExpr {
    fn compile(&self, accessors: &mut Vec<Accessor>, token_accessors: &mut Vec<TokenAccessor>) {
        self.compile_impl(
            accessors,
            token_accessors,
            CompileConfig {
                either_passed: false,
                rep_passed: false,
                maybe_passed: false,
                is_at_beginning: true,
            },
        )
    }

    fn compile_impl(
        &self,
        accessors: &mut Vec<Accessor>,
        token_accessors: &mut Vec<TokenAccessor>,
        compile_config: CompileConfig,
    ) {
        match self {
            SyntreeNodeChildrenRefExpr::SelfRef => {
                let kind = if compile_config.rep_passed {
                    AccessorKind::List
                } else if compile_config.either_passed
                    || compile_config.maybe_passed
                    || !compile_config.is_at_beginning
                {
                    AccessorKind::Opt
                } else {
                    AccessorKind::FirstGuaranteed
                };

                accessors.push(Accessor {
                    name: parse_quote! {self_},
                    kind,
                    target_ty: parse_quote! {Self},
                });
            }
            SyntreeNodeChildrenRefExpr::Name(name) => {
                let kind = if compile_config.rep_passed {
                    AccessorKind::List
                } else if compile_config.either_passed
                    || compile_config.maybe_passed
                    || !compile_config.is_at_beginning
                {
                    AccessorKind::Opt
                } else {
                    AccessorKind::FirstGuaranteed
                };

                let is_list = matches!(kind, AccessorKind::List);

                let n = heck::AsSnakeCase(name.to_string());
                let accessor_name = match is_list {
                    false => format_ident!("get_{n}"),
                    true => format_ident!("get_{n}_list"),
                };

                accessors.push(Accessor {
                    name: accessor_name,
                    kind,
                    target_ty: name.clone(),
                });
            }
            SyntreeNodeChildrenRefExpr::NamedRef(name, _, target_ty) => {
                let kind = if compile_config.rep_passed {
                    AccessorKind::List
                } else if compile_config.either_passed
                    || compile_config.maybe_passed
                    || !compile_config.is_at_beginning
                {
                    AccessorKind::Opt
                } else {
                    AccessorKind::FirstGuaranteed
                };

                accessors.push(Accessor {
                    name: format_ident!("get_{name}"),
                    kind,
                    target_ty: target_ty.clone(),
                });
            }
            SyntreeNodeChildrenRefExpr::Token(name, _, _, token) => {
                let kind = if compile_config.rep_passed {
                    AccessorKind::List
                } else if compile_config.either_passed
                    || compile_config.maybe_passed
                    || !compile_config.is_at_beginning
                {
                    AccessorKind::Opt
                } else {
                    AccessorKind::FirstGuaranteed
                };

                let getter_name = if let AccessorKind::List = kind {
                    format_ident!("get_{name}_list", name = name)
                } else {
                    format_ident!("get_{name}", name = name)
                };

                token_accessors.push(TokenAccessor {
                    name: getter_name,
                    token: token.clone(),
                    kind,
                    allow_empty: false,
                });
            }
            SyntreeNodeChildrenRefExpr::Repeat(_, r) => {
                r.compile_impl(
                    accessors,
                    token_accessors,
                    CompileConfig {
                        is_at_beginning: false,
                        rep_passed: true,
                        ..compile_config
                    },
                );
            }
            SyntreeNodeChildrenRefExpr::Maybe(_, r) => {
                r.compile_impl(
                    accessors,
                    token_accessors,
                    CompileConfig {
                        is_at_beginning: false,
                        maybe_passed: true,
                        ..compile_config
                    },
                );
            }
            SyntreeNodeChildrenRefExpr::ParenSet(_, s) => {
                let mut is_at_beginning = compile_config.is_at_beginning;
                for expr in &s.set {
                    expr.compile_impl(
                        accessors,
                        token_accessors,
                        CompileConfig {
                            is_at_beginning,
                            ..compile_config
                        },
                    );
                    is_at_beginning = false;
                }
            }
            SyntreeNodeChildrenRefExpr::Either(_, _, exprs) => {
                for expr in exprs {
                    expr.compile_impl(
                        accessors,
                        token_accessors,
                        CompileConfig {
                            either_passed: true,
                            ..compile_config
                        },
                    );
                }
            }
        }
    }
}

struct Accessor {
    name: Ident,
    kind: AccessorKind,
    target_ty: Ident,
}

impl Accessor {
    fn expand(&self, node_name: &Ident) -> (TokenStream, TokenStream) {
        let name = &self.name;
        let (target_ty, accessor, accessor_calls) = match self.kind {
            AccessorKind::FirstGuaranteed => {
                let target_ty = &self.target_ty;
                (
                    quote! {#target_ty},
                    quote! {get_child::<#target_ty>(&self.node)},
                    quote! {{
                        let _node = self.#name();
                        tests_data.push(
                            tests_data::ElemRef::from(self),
                            stringify!(#node_name),
                            stringify!(#name),
                            tests_data::AccessorCalledDataReturned::Returned(tests_data::ElemRef::from(&_node))
                        );
                        _node.call_accessors(tests_data);
                    }},
                )
            }
            AccessorKind::List => {
                let target_ty = &self.target_ty;
                (
                    quote! {impl Iterator<Item=#target_ty> + 'a},
                    quote! {get_children::<#target_ty>(&self.node)},
                    quote! {{
                        let _nodes = self.#name();
                        let mut list = Vec::new();
                        for _node in _nodes {
                            _node.call_accessors(tests_data);
                            list.push(tests_data::ElemRef::from(&_node));
                        }
                        tests_data.push(
                            tests_data::ElemRef::from(self),
                            stringify!(#node_name), stringify!(#name),
                            if list.is_empty() {
                                tests_data::AccessorCalledDataReturned::Nothing
                            } else {
                                tests_data::AccessorCalledDataReturned::ReturnedList(list)
                            }
                        );
                    }},
                )
            }
            AccessorKind::Opt => {
                let target_ty = &self.target_ty;
                (
                    quote! {Option<#target_ty>},
                    quote! {get_child_opt::<#target_ty>(&self.node)},
                    quote! {{
                        let _node = self.#name();
                        match _node {
                            Some(_node) => {
                                tests_data.push(
                                    tests_data::ElemRef::from(self),
                                    stringify!(#node_name),
                                    stringify!(#name),
                                    tests_data::AccessorCalledDataReturned::Returned(tests_data::ElemRef::from(&_node))
                                );
                                _node.call_accessors(tests_data);
                            }
                            None => {
                                tests_data.push(
                                    tests_data::ElemRef::from(self),
                                    stringify!(#node_name),
                                    stringify!(#name),
                                    tests_data::AccessorCalledDataReturned::Nothing
                                );
                            }
                        }
                    }},
                )
            }
        };
        (
            quote! {
                pub fn #name<'a>(&'a self) -> #target_ty {
                    #accessor
                }
            },
            accessor_calls,
        )
    }
}

enum AccessorKind {
    FirstGuaranteed,
    List,
    Opt,
}

struct TokenAccessor {
    name: Ident,
    token: TokenStream,
    kind: AccessorKind,
    allow_empty: bool,
}

impl TokenAccessor {
    fn expand(&self, node_name: &Ident) -> (TokenStream, TokenStream) {
        let name = &self.name;
        let token = &self.token;
        let allow_empty = self.allow_empty;
        let (target_ty, accessor, accessor_calls) = match self.kind {
            AccessorKind::FirstGuaranteed => (
                quote! {Token},
                quote! {get_token(&self.node, T![#token])},
                quote! {{
                    let _t = self.#name();
                    tests_data.push(
                        tests_data::ElemRef::from(self),
                        stringify!(#node_name),
                        stringify!(#name),
                        tests_data::AccessorCalledDataReturned::Returned(tests_data::ElemRef::from(&_t))
                    );
                }},
            ),
            AccessorKind::List => (
                quote! {impl Iterator<Item=Token> + 'a},
                quote! {get_token_list(&self.node, T![#token])},
                quote! {{
                    let _ts = self.#name();
                    let mut list = Vec::new();
                    for _t in _ts {
                        list.push(tests_data::ElemRef::from(&_t));
                    }
                    tests_data.push(
                        tests_data::ElemRef::from(self),
                        stringify!(#node_name),
                        stringify!(#name),
                        if list.is_empty() {
                            if #allow_empty {
                                tests_data::AccessorCalledDataReturned::EmptyAllowed
                            } else {
                                tests_data::AccessorCalledDataReturned::Nothing
                            }
                        } else {
                            tests_data::AccessorCalledDataReturned::ReturnedList(list)
                        }
                    );
                }},
            ),
            AccessorKind::Opt => (
                quote! {Option<Token>},
                quote! {get_token_opt(&self.node, T![#token])},
                quote! {{
                    let _t = self.#name();
                    match _t {
                        Some(_t) => {
                            tests_data.push(
                                tests_data::ElemRef::from(self),
                                stringify!(#node_name),
                                stringify!(#name),
                                tests_data::AccessorCalledDataReturned::Returned(tests_data::ElemRef::from(&_t))
                            );
                        }
                        None => {
                            tests_data.push(
                                tests_data::ElemRef::from(self),
                                stringify!(#node_name),
                                stringify!(#name),
                                tests_data::AccessorCalledDataReturned::Nothing
                            );
                        }
                    }
                }},
            ),
        };
        (
            quote! {
                pub fn #name<'a>(&'a self) -> #target_ty {
                    #accessor
                }
            },
            accessor_calls,
        )
    }
}

#[proc_macro]
pub fn syntree_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syn::parse_macro_input!(input as SyntreeEnumDef)
        .expand()
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

struct SyntreeEnumDef {
    name: Ident,
    eq: Token![=],
    variants: Punctuated<Ident, Token![|]>,
}

impl Parse for SyntreeEnumDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let eq = input.parse()?;
        let punct = input.parse_terminated(Ident::parse, Token![|])?;
        Ok(Self {
            name,
            eq,
            variants: punct,
        })
    }
}

impl SyntreeEnumDef {
    pub fn expand(&self) -> syn::Result<TokenStream> {
        let name = &self.name;
        let variants = &self.variants;

        let cast_expr = {
            let mut if_chain = IfChain::new();
            for name in variants {
                if_chain.add_if(parse_quote_spanned! {name.span()=>
                    if #name::can_cast_from_syntax_kind(kind) {
                        Self::#name(TreeNode::cast_from_node_raw(n))
                    }
                });
            }

            if_chain.add_else_branch_default(parse_quote_spanned! {self.eq.span()=>
                {unreachable!("invalid syntax kind: {:?}", kind);}
            });

            let mut ts = TokenStream::new();
            if_chain.compile(&mut ts);
            ts
        };

        let v_iter_1 = variants.iter();
        let v_iter_2 = variants.iter();
        let v_iter_3 = variants.iter();
        let v_iter_4 = variants.iter();

        let expanded = quote! {
            pub enum #name {
                #(#v_iter_1(#v_iter_1),)*
            }

            impl TreeNode for #name {
                unsafe fn cast_from_node_raw(n: Node) -> Self {
                    let kind = n.kind();
                    #cast_expr
                }

                fn can_cast_from_syntax_kind(kind: SyntaxKind) -> bool {
                    #(
                        #v_iter_2::can_cast_from_syntax_kind(kind)
                    )||*
                }

                fn get_node(&self) -> &Node {
                    match self {
                        #(Self::#v_iter_3(n) => n.get_node(),)*
                    }
                }
            }

            impl #name {
                pub fn call_accessors(&self, tests_data: &mut tests_data::AccessorCalledDataList) {
                    match self {
                        #(Self::#v_iter_4(n) => n.call_accessors(tests_data),)*
                    }
                }
            }
        };
        Ok(expanded)
    }
}
