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

use crate::dir_structure_core::DirStructureCoreInfo;
use crate::dir_structure_core::PathSpec;
use crate::dir_structure_core::compile_attrs;

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
    Async(syn::Expr, syn::Type),
    Sync(Option<(syn::Expr, syn::Type)>),
}

struct LoadPathInput {
    bound: syn::Lifetime,
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

        let bound = {
            let c = input.fork();

            match c.parse::<syn::Lifetime>() {
                Ok(l) => {
                    input.advance_to(&c);
                    l
                }
                Err(_) => parse_quote! {'static},
            }
        };

        let content;
        bracketed!(content in input);
        let core = content.parse()?;

        let async_vfs = if input.peek(Token![in]) {
            let _in: Token![in] = input.parse()?;
            let c;
            parenthesized!(c in input);
            let vfs = c.parse()?;

            let syn::Expr::Cast(syn::ExprCast { expr, ty, .. }) = vfs else {
                return Err(syn::Error::new_spanned(
                    vfs,
                    "expected expression to be `value as type`",
                ));
            };

            if is_async {
                LoadPathAsyncVfs::Async(*expr, *ty)
            } else {
                LoadPathAsyncVfs::Sync(Some((*expr, *ty)))
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
            bound,
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
                                            ::read_from(__current.as_ref(), #vfs)?
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
                                            ::read_from(__current.as_ref(), #vfs)?
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
                                            ::read_from(__current.as_ref(), #vfs)?
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

    let lt = input.bound;

    let (asy, vfs_expr, vfs_ty, read_code) = match input.async_vfs {
        LoadPathAsyncVfs::Async(vfs, ty) => {
            (
                Some(Async(vfs.span())),
                vfs,
                ty,
                reader_code(true, &parse_quote! {__vfs}),
            )
            // quote! {
            //     <#current_path as ::dir_structure::traits::asy::ReadFromAsync<'_, _>>::read_from_async(__current, #vfs)
            // }
        }
        LoadPathAsyncVfs::Sync(vfs) => {
            let (vfs, ty) = vfs.unwrap_or_else(|| {
                (
                    parse_quote! { ::std::pin::Pin::new(&::dir_structure::vfs::fs_vfs::FsVfs) },
                    parse_quote! { ::dir_structure::vfs::fs_vfs::FsVfs },
                )
            });

            (None, vfs, ty, reader_code(false, &parse_quote! {__vfs}))
            // quote! {
            //     <#current_path as ::dir_structure::traits::sync::ReadFrom<'_, _>>::read_from(&__current, #vfs)
            // }
        }
    };

    Ok(quote! {{
        #asy fn __read_<'vfs : #lt>(__current: impl Into<<<#vfs_ty as ::dir_structure::traits::vfs::VfsCore>::Path as ::dir_structure::traits::vfs::PathType>::OwnedPath>, __vfs: ::std::pin::Pin<&'vfs #vfs_ty> #params) -> ::dir_structure::error::VfsResult<#current_path, #vfs_ty>
            #where_clause
        {
            let __current = __current.into();
            #resolve
            #read_code
        }
        __read_(#p, #vfs_expr #args)
    }})
}

fn has_field_impl(
    self_path: bool,
    with_newtype: &Option<syn::Type>,
    field_name: &syn::Ident,
    field_ty: &syn::Type,
    (impl_generics, ty_name, ty_generics, where_clause): (
        &syn::ImplGenerics,
        &syn::Ident,
        &syn::TypeGenerics,
        Option<&syn::WhereClause>,
    ),
    path_param_name: &syn::Ident,
    path_pusher_for_has_field: &TokenStream,
) -> syn::Result<TokenStream> {
    if self_path {
        Ok(quote! {})
    } else {
        use std::iter;

        use crate::resolve_path::MAX_LEN;

        let field_name_str = field_name.to_string();
        if field_name_str.len() > MAX_LEN {
            return Err(syn::Error::new_spanned(
                field_name,
                format!(
                    "Field name for DirStructure must be at most {} characters long",
                    MAX_LEN
                ),
            ));
        }
        let field_name_array: [char; MAX_LEN] = field_name_str
            .chars()
            .chain(iter::repeat('\0'))
            .take(MAX_LEN)
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        let mut has_field_impl = quote! {
            #[automatically_derived]
            impl #impl_generics ::dir_structure::traits::resolve::HasField<{ [#(#field_name_array),*] }> for #ty_name #ty_generics #where_clause {
                type Inner = #field_ty;

                fn resolve_path<__P: ::dir_structure::traits::vfs::OwnedPathType>(mut #path_param_name: __P) -> __P {
                    #path_pusher_for_has_field
                    #path_param_name
                }
            }
        };

        match &with_newtype {
            Some(nt) => {
                has_field_impl.extend(quote! {
                    #[automatically_derived]
                    impl #impl_generics ::dir_structure::traits::resolve::HasFieldMaybeNewtype<{ [#(#field_name_array),*] }> for #ty_name #ty_generics #where_clause {
                        type ReaderType = #nt;

                        fn parse(read: Self::ReaderType) -> Self::Inner {
                            <#nt as ::dir_structure::traits::sync::NewtypeToInner>::into_inner(read)
                        }
                    }
                });
            }
            None => {
                has_field_impl.extend(quote! {
                    #[automatically_derived]
                    impl #impl_generics ::dir_structure::traits::resolve::HasFieldNoNewtype<{ [#(#field_name_array),*] }> for #ty_name #ty_generics #where_clause {}
                });
            }
        }

        Ok(has_field_impl)
    }
}

pub fn expand_has_field_impls(item: syn::ItemStruct) -> syn::Result<TokenStream> {
    let struct_name = &item.ident;
    let (impl_generics, ty_generics, where_clause) = item.generics.split_for_impl();

    let mut has_field_impls = quote! {};

    for field in &item.fields {
        let field_name = field
            .ident
            .as_ref()
            .ok_or_else(|| syn::Error::new_spanned(field, "Expected named fields in the struct"))?;

        let DirStructureCoreInfo {
            self_path,
            name: _,
            ty,
            newtype_ty: with_newtype,
            path,
        } = compile_attrs(field)?;

        let path_param_name = format_ident!("__path");

        let path_pusher_for_has_field = match path {
            PathSpec::Path(path) => {
                quote! {
                    #path_param_name.push_segment_str(#path);
                }
            }
            PathSpec::SelfPath => {
                // do nothing, this is a self path
                quote! {}
            }
        };

        has_field_impls.extend(has_field_impl(
            self_path,
            &with_newtype,
            field_name,
            &ty,
            (&impl_generics, struct_name, &ty_generics, where_clause),
            &path_param_name,
            &path_pusher_for_has_field,
        )?);
    }

    Ok(has_field_impls)
}
