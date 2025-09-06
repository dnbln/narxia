use std::iter;

use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::Expr;
use syn::Token;
use syn::braced;
use syn::bracketed;
use syn::parenthesized;
use syn::parse::Parse;
use syn::parse::discouraged::Speculative;
use syn::parse_quote;
use syn::punctuated::Pair;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Async;

// resolve_path!([T @ path_expr].a."b".c.d.${e});
// or
// resolve_path!(["path/to/dir" as T].a."b".c.d.${e});

pub const MAX_LEN: usize = 32;

struct CoreTyExpression {
    path: syn::Expr,
    ty: syn::Type,
}

impl Parse for CoreTyExpression {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let fork = input.fork();

        let (path, ty) = match fork.parse::<syn::Expr>() {
            Ok(expr) => match fork.is_empty() {
                true => match expr {
                    syn::Expr::Cast(expr_cast) => {
                        let expr = *expr_cast.expr;
                        let ty = *expr_cast.ty;
                        input.advance_to(&fork);
                        (expr, ty)
                    }
                    e => {
                        return Err(syn::Error::new_spanned(
                            e,
                            "expected expression to be `value as type`",
                        ));
                    }
                },
                false => {
                    if !fork.peek(Token![@]) {
                        eprintln!("expr is {}", quote! {#expr});
                        return Err(fork.error("expected 'as' or '@'"));
                    }
                    // reinterpret the expression as a type
                    let ty = input.parse::<syn::Type>()?;
                    input.parse::<Token![@]>()?;
                    let expr = input.parse::<syn::Expr>()?;
                    (expr, ty)
                }
            },
            Err(_) => {
                let ty = input.parse::<syn::Type>()?;
                input.parse::<Token![@]>()?;
                let expr = input.parse::<syn::Expr>()?;
                (expr, ty)
            }
        };

        Ok(CoreTyExpression { path, ty })
    }
}

struct ResolvePathInput {
    core: CoreTyExpression,
    segments: Punctuated<ResolveSingleSegment, Token![.]>,
}

#[expect(unused)]
impl Parse for ResolvePathInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        bracketed!(content in input);
        let core = content.parse()?;
        let dot: Token![.] = input.parse()?;
        let segments =
            Punctuated::<ResolveSingleSegment, Token![.]>::parse_separated_nonempty(input)?;

        if !input.is_empty() {
            return Err(input.error("expected end of input after path segments"));
        }

        Ok(ResolvePathInput { core, segments })
    }
}

enum ResolveSingleSegment {
    Ident(syn::Ident),
    StringLit(syn::LitStr),
    DynamicStringExpr(syn::Expr),
}

impl Parse for ResolveSingleSegment {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident) {
            Ok(ResolveSingleSegment::Ident(input.parse()?))
        } else if input.peek(syn::LitStr) {
            Ok(ResolveSingleSegment::StringLit(input.parse()?))
        } else if input.peek(Token![$]) {
            input.parse::<Token![$]>()?;
            let content;
            braced!(content in input);
            Ok(ResolveSingleSegment::DynamicStringExpr(content.parse()?))
        } else {
            Err(input.error("expected identifier or string literal"))
        }
    }
}

pub fn resolve_path(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as ResolvePathInput);

    do_resolve_path(input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn do_resolve_path(input: ResolvePathInput) -> syn::Result<TokenStream> {
    let mut current_path = input.core.ty.clone();

    let mut where_clause = syn::WhereClause {
        where_token: <Token![where]>::default(),
        predicates: Punctuated::new(),
    };

    let mut resolve = quote! {};

    for segment in &input.segments {
        match segment {
            ResolveSingleSegment::Ident(ident) => {
                let name = ident.to_string();
                let name = name.chars().collect::<Vec<_>>();
                if name.len() > MAX_LEN {
                    return Err(syn::Error::new(ident.span(), "Identifier too long"));
                }
                let name_array: [char; MAX_LEN] = name
                    .iter()
                    .cloned()
                    .chain(iter::repeat('\0'))
                    .take(MAX_LEN)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();
                where_clause.predicates.push(parse_quote! {
                    #current_path: ::dir_structure::traits::resolve::HasField<{ [#(#name_array),*] }>
                });
                resolve.extend(quote! {
                    let __current = <#current_path as ::dir_structure::traits::resolve::HasField<{ [#(#name_array),*] }>>::resolve_path(__current);
                });
                current_path = parse_quote! {
                    <#current_path as ::dir_structure::traits::resolve::HasField<{ [#(#name_array),*] }>>::Inner
                };
            }
            ResolveSingleSegment::DynamicStringExpr(expr) => {
                where_clause.predicates.push(parse_quote! {
                    #current_path: ::dir_structure::traits::resolve::DynamicHasField
                });
                resolve.extend(quote! {
                    let __current = <#current_path as ::dir_structure::traits::resolve::DynamicHasField>::resolve_path(__current, #expr);
                });
                current_path = parse_quote! {
                    <#current_path as ::dir_structure::traits::resolve::DynamicHasField>::Inner
                };
            }
            ResolveSingleSegment::StringLit(lit_str) => {
                let value = lit_str.value();
                where_clause.predicates.push(parse_quote! {
                    #current_path: ::dir_structure::traits::resolve::DynamicHasField
                });
                resolve.extend(quote! {
                    let __current = <#current_path as ::dir_structure::traits::resolve::DynamicHasField>::resolve_path(__current, #value);
                });
                current_path = parse_quote! {
                    <#current_path as ::dir_structure::traits::resolve::DynamicHasField>::Inner
                };
            }
        }
    }

    let p = input.core.path;

    Ok(quote! {{
        let __current: ::std::path::PathBuf = ::std::path::PathBuf::from(#p);
        #resolve
        __current
    }})
}

enum LoadPathAsyncVfs {
    Async(syn::Expr),
    Sync(Option<syn::Expr>),
}

struct LoadPathInput {
    core: CoreTyExpression,
    async_vfs: LoadPathAsyncVfs,
    segments: Punctuated<ResolveSingleSegment, Token![.]>,
}

impl Parse for LoadPathInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let is_async = if input.peek(Token![async]) {
            let _async: Token![async] = input.parse()?;
            true
        } else {
            false
        };

        let content;
        bracketed!(content in input);
        let core = content.parse()?;

        let async_vfs = if input.peek(Token![in]) {
            let _in: Token![in] = input.parse()?;
            let c;
            parenthesized!(c in input);
            let vfs = c.parse()?;
            if is_async {
                LoadPathAsyncVfs::Async(vfs)
            } else {
                LoadPathAsyncVfs::Sync(Some(vfs))
            }
        } else if is_async {
            return Err(input.error("expected 'in (vfs)' for async load_path"));
        } else {
            LoadPathAsyncVfs::Sync(None)
        };

        let _dot: Token![.] = input.parse()?;
        let segments =
            Punctuated::<ResolveSingleSegment, Token![.]>::parse_separated_nonempty(input)?;

        if !input.is_empty() {
            return Err(input.error("expected end of input after path segments"));
        }

        Ok(LoadPathInput {
            core,
            async_vfs,
            segments,
        })
    }
}

pub fn load_path(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as LoadPathInput);

    do_load_path(input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn do_load_path(input: LoadPathInput) -> syn::Result<TokenStream> {
    let mut current_path = input.core.ty.clone();

    let mut where_clause = syn::WhereClause {
        where_token: <Token![where]>::default(),
        predicates: Punctuated::new(),
    };

    let mut resolve = quote! {};
    let mut param_id: u32 = 0;
    let mut params = quote! {};
    let mut args = quote! {};
    let mut read_code = None::<Box<dyn Fn(bool, &Expr) -> TokenStream>>;

    for segment in input.segments.pairs() {
        let (s, last) = match segment {
            Pair::Punctuated(s, _) => (s, false),
            Pair::End(s) => (s, true),
        };

        match s {
            ResolveSingleSegment::Ident(ident) => {
                let name = ident.to_string();
                let name = name.chars().collect::<Vec<_>>();
                if name.len() > MAX_LEN {
                    return Err(syn::Error::new(ident.span(), "Identifier too long"));
                }
                let name_array: [char; MAX_LEN] = name
                    .iter()
                    .cloned()
                    .chain(iter::repeat('\0'))
                    .take(MAX_LEN)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();
                where_clause.predicates.push(parse_quote! {
                    #current_path: ::dir_structure::traits::resolve::HasField<{ [#(#name_array),*] }>
                });
                resolve.extend(quote! {
                    let __current = <#current_path as ::dir_structure::traits::resolve::HasField<{ [#(#name_array),*] }>>::resolve_path(__current);
                });
                if last {
                    let current_path = current_path.clone();
                    read_code = Some(Box::new(move |asyncness: bool, vfs: &Expr| {
                        if asyncness {
                            quote! {
                                Ok(<#current_path as ::dir_structure::traits::resolve::HasFieldMaybeNewtype<{ [#(#name_array),*] }>>::parse(
                                    <<#current_path as ::dir_structure::traits::resolve::HasFieldMaybeNewtype<{ [#(#name_array),*] }>>::ReaderType
                                        as ::dir_structure::traits::asy::ReadFromAsync<'_, _>>
                                            ::read_from_async(__current, #vfs).await?
                                ))
                            }
                        } else {
                            quote! {
                                Ok(<#current_path as ::dir_structure::traits::resolve::HasFieldMaybeNewtype<{ [#(#name_array),*] }>>::parse(
                                    <<#current_path as ::dir_structure::traits::resolve::HasFieldMaybeNewtype<{ [#(#name_array),*] }>>::ReaderType
                                        as ::dir_structure::traits::sync::ReadFrom<'_, _>>
                                            ::read_from(&__current, #vfs)?
                                ))
                            }
                        }
                    }));
                }
                current_path = parse_quote! {
                    <#current_path as ::dir_structure::traits::resolve::HasField<{ [#(#name_array),*] }>>::Inner
                };
            }
            ResolveSingleSegment::DynamicStringExpr(expr) => {
                where_clause.predicates.push(parse_quote! {
                    #current_path: ::dir_structure::traits::resolve::DynamicHasField
                });
                let param_name = format_ident!("__arg{}", param_id);
                params.extend(quote! { , #param_name: &str });
                args.extend(quote! { , #expr });
                param_id += 1;
                resolve.extend(quote! {
                    let __current = <#current_path as ::dir_structure::traits::resolve::DynamicHasField>::resolve_path(__current, #param_name);
                });
                if last {
                    let current_path = current_path.clone();
                    read_code = Some(Box::new(move |asyncness: bool, vfs: &Expr| {
                        if asyncness {
                            quote! {
                                Ok(<#current_path as ::dir_structure::traits::resolve::DynamicHasFieldMaybeNewtype>::parse(
                                    <<#current_path as ::dir_structure::traits::resolve::DynamicHasFieldMaybeNewtype>::ReaderType
                                        as ::dir_structure::traits::asy::ReadFromAsync<'_, _>>
                                            ::read_from_async(__current, #vfs).await?
                                ))
                            }
                        } else {
                            quote! {
                                Ok(<#current_path as ::dir_structure::traits::resolve::DynamicHasFieldMaybeNewtype>::parse(
                                    <<#current_path as ::dir_structure::traits::resolve::DynamicHasFieldMaybeNewtype>::ReaderType
                                        as ::dir_structure::traits::sync::ReadFrom<'_, _>>
                                            ::read_from(&__current, #vfs)?
                                ))
                            }
                        }
                    }));
                }
                current_path = parse_quote! {
                    <#current_path as ::dir_structure::traits::resolve::DynamicHasField>::Inner
                };
            }
            ResolveSingleSegment::StringLit(lit_str) => {
                let value = lit_str.value();
                where_clause.predicates.push(parse_quote! {
                    #current_path: ::dir_structure::traits::resolve::DynamicHasField
                });
                resolve.extend(quote! {
                    let __current = <#current_path as ::dir_structure::traits::resolve::DynamicHasField>::resolve_path(__current, #value);
                });
                if last {
                    let current_path = current_path.clone();
                    read_code = Some(Box::new(move |asyncness: bool, vfs: &Expr| {
                        if asyncness {
                            quote! {
                                Ok(<#current_path as ::dir_structure::traits::resolve::DynamicHasFieldMaybeNewtype>::parse(
                                    <<#current_path as ::dir_structure::traits::resolve::DynamicHasFieldMaybeNewtype>::ReaderType
                                        as ::dir_structure::traits::asy::ReadFromAsync<'_, _>>
                                            ::read_from_async(__current, #vfs).await?
                                ))
                            }
                        } else {
                            quote! {
                                Ok(<#current_path as ::dir_structure::traits::resolve::DynamicHasFieldMaybeNewtype>::parse(
                                    <<#current_path as ::dir_structure::traits::resolve::DynamicHasFieldMaybeNewtype>::ReaderType
                                        as ::dir_structure::traits::sync::ReadFrom<'_, _>>
                                            ::read_from(&__current, #vfs)?
                                ))
                            }
                        }
                    }));
                }
                current_path = parse_quote! {
                    <#current_path as ::dir_structure::traits::resolve::DynamicHasField>::Inner
                };
            }
        }
    }

    let p = input.core.path;

    let reader_code = read_code.ok_or_else(|| {
        syn::Error::new_spanned(input.core.ty, "no segments provided to load_path")
    })?;

    let (asy, read_code) = match input.async_vfs {
        LoadPathAsyncVfs::Async(vfs) => {
            (Some(Async(vfs.span())), reader_code(true, &vfs))
            // quote! {
            //     <#current_path as ::dir_structure::traits::asy::ReadFromAsync<'_, _>>::read_from_async(__current, #vfs)
            // }
        }
        LoadPathAsyncVfs::Sync(vfs) => {
            let vfs = vfs.unwrap_or_else(
                || parse_quote! { ::std::pin::Pin::new(&::dir_structure::vfs::fs_vfs::FsVfs) },
            );

            (None, reader_code(false, &vfs))
            // quote! {
            //     <#current_path as ::dir_structure::traits::sync::ReadFrom<'_, _>>::read_from(&__current, #vfs)
            // }
        }
    };

    Ok(quote! {{
        #asy fn __read_(__current: ::std::path::PathBuf #params) -> ::dir_structure::error::Result<#current_path>
            #where_clause
        {
            #resolve
            #read_code
        }
        let __current: ::std::path::PathBuf = ::std::path::PathBuf::from(#p);
        __read_(__current #args)
    }})
}
