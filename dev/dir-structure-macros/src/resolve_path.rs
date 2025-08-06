use proc_macro2::TokenStream;
use quote::quote;
use syn::Token;
use syn::parse::Parse;
use syn::parse_quote;
use syn::punctuated::Punctuated;

// resolve_path!(<path_expr @ T>.a."b".c.d);

// sync this with HAS_FIELD_MAX_LEN in dir-structure/src/lib.rs
pub const MAX_LEN: usize = 16;

struct ResolvePathInput {
    // lt: Token![<],
    path: syn::Expr,
    // at: Token![@],
    ty: syn::Type,
    // rt: Token![>],
    // dot: Token![.],
    segments: Punctuated<ResolveSingleSegment, Token![.]>,
}

#[expect(unused)]
impl Parse for ResolvePathInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lt: Token![<] = input.parse()?;
        let path: syn::Expr = input.parse()?;
        let at: Token![@] = input.parse()?;
        let ty: syn::Type = input.parse()?;
        let rt: Token![>] = input.parse()?;
        let dot: Token![.] = input.parse()?;
        let segments =
            Punctuated::<ResolveSingleSegment, Token![.]>::parse_separated_nonempty(input)?;

        if !input.is_empty() {
            return Err(input.error("expected end of input after path segments"));
        }

        Ok(ResolvePathInput {
            // lt,
            path,
            // at,
            ty,
            // rt,
            // dot,
            segments,
        })
    }
}

enum ResolveSingleSegment {
    Ident(syn::Ident),
    StringLit(syn::LitStr),
}

impl Parse for ResolveSingleSegment {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident) {
            Ok(ResolveSingleSegment::Ident(input.parse()?))
        } else if input.peek(syn::LitStr) {
            Ok(ResolveSingleSegment::StringLit(input.parse()?))
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
    let mut current_path = input.ty.clone();

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
                    .chain(std::iter::repeat('\0'))
                    .take(MAX_LEN)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();
                where_clause.predicates.push(parse_quote! {
                    #current_path: ::dir_structure::HasField<{ [#(#name_array),*] }>
                });
                resolve.extend(quote! {
                    let __current = <#current_path as ::dir_structure::HasField<{ [#(#name_array),*] }>>::resolve_path(__current);
                });
                current_path = parse_quote! {
                    <#current_path as ::dir_structure::HasField<{ [#(#name_array),*] }>>::Inner
                };
            }
            ResolveSingleSegment::StringLit(lit_str) => {
                let value = lit_str.value();
                where_clause.predicates.push(parse_quote! {
                    #current_path: ::dir_structure::DynamicHasField
                });
                resolve.extend(quote! {
                    let __current = <#current_path as ::dir_structure::DynamicHasField>::resolve_path(__current, #value);
                });
                current_path = parse_quote! {
                    <#current_path as ::dir_structure::DynamicHasField>::Inner
                };
            }
        }
    }

    let p = input.path;

    Ok(quote! {{
        fn __resolve_path(__current: ::std::path::PathBuf) -> ::std::path::PathBuf {
            #resolve
            __current
        }

        __resolve_path(#p.into())
    }})
}
