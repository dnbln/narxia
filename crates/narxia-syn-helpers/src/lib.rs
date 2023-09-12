use proc_macro2::{Delimiter, Ident, TokenStream};
use proc_macro2::extra::DelimSpan;
use quote::{quote, quote_spanned, ToTokens};
use syn::{Data, DataEnum, DeriveInput, Expr, ExprCall, FnArg, MetaList, parse_quote, Token};
use syn::parse::{Parse, ParseStream};
use syn::parse::discouraged::{AnyDelimiter, Speculative};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

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
    instructions: Vec<ParserSpecInstruction>,
}

impl Parse for ParserSpecBody {
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
}

impl Parse for ParseStmtExtra {
    fn parse(input: ParseStream) -> syn::Result<Self> {
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
        } else {
            Err(syn::Error::new(ident.span(), "expected ws or eof"))
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
    let body = expand_parser_spec_body(&p, &end_expr);
    quote! {
        #[parse_fn]
        fn #name(p: &mut Parser, #args) -> CompletedMarker {
            let m = p.ev.begin();
            #body
            #end_expr
        }
    }
}

fn expand_parser_spec_body(p: &ParserSpecRule, end_expr: &TokenStream) -> TokenStream {
    let mut total = TokenStream::new();
    let recovery_check_and_return = quote! {
        if p.is_recovering() {
            return #end_expr;
        }
    };
    for instruction in &p.body.instructions {
        match instruction {
            ParserSpecInstruction::ParseCall(_, call) => {
                let mut call = call.clone();
                call.args.insert(0, parse_quote! {p});
                total.extend(quote! {
                    #call;
                    #recovery_check_and_return
                });
            }
            ParserSpecInstruction::Token(_, _, _, _, ts) => {
                total.extend(quote_spanned! {ts.span()=>p.expect(T![#ts]);#recovery_check_and_return});
            }
            ParserSpecInstruction::ParseStmtExtra(_, _, extra) => match extra {
                ParseStmtExtra::Ws(ws) => {
                    total.extend(expand_ws_extra(ws));
                }
                ParseStmtExtra::Eof(eof) => {
                    total.extend(expand_eof_behavior(eof, end_expr));
                }
                ParseStmtExtra::Dbg(ident) => {
                    total.extend(quote_spanned! {ident.span()=>
                        p.dbg();
                    });
                }
            },
        }
    }
    total
}

fn expand_ws_extra(ws_extra: &WsExtra) -> TokenStream {
    quote_spanned! {
        ws_extra.ws_ident.span()=>
        p.skip_ws();
    }
}

fn expand_eof_behavior(eof_extra: &EofExtra, end_expr: &TokenStream) -> TokenStream {
    match eof_extra.behavior.behavior {
        EofBehavior::EarlyRet => {
            let err_call = eof_extra
                .behavior
                .error
                .clone()
                .map(|(_, mut call)| {
                    quote_spanned! {
                        call.span()=>
                        {
                            use crate::parser::parse_event_handler::ParseErrorInfo::*;
                            p.#call
                        }
                    }
                })
                .into_iter();
            quote_spanned! {
                eof_extra.eof_ident.span()=>
                if p.at_eof() {
                    #(#err_call;)*
                    return #end_expr;
                }
            }
        }
    }
}
