use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::Field;
use syn::ImplGenerics;
use syn::ItemStruct;
use syn::Token;
use syn::Type;
use syn::parse_quote;

#[proc_macro_derive(DirStructure, attributes(dir_structure))]
pub fn derive_dir_structure(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = syn::parse_macro_input!(item as ItemStruct);

    expand_dir_structure(item)
        // .map(|ts| {
        //     eprintln!("Expanded DirStructure for {}", ts);
        //     ts
        // })
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

struct DirStructureForField {
    read_code: TokenStream,
    async_read_code: TokenStream,
    async_read_bound: Option<syn::WherePredicate>,
    write_code: TokenStream,
    async_write_code: TokenStream,
    async_write_bound: Option<syn::WherePredicate>,
    async_write_owned_code: TokenStream,
    async_write_owned_bound: Option<syn::WherePredicate>,
    #[cfg(feature = "resolve-path")]
    has_field_impl: TokenStream,
}

fn expand_dir_structure_for_field(
    (impl_generics, ty_name, ty_generics, where_clause): (
        &ImplGenerics,
        &Ident,
        &syn::TypeGenerics,
        Option<&syn::WhereClause>,
    ),
    path_param_name: &Ident,
    field: &Field,
) -> syn::Result<DirStructureForField> {
    let field_name = field.ident.as_ref().ok_or_else(|| {
        syn::Error::new_spanned(
            field,
            "DirStructure can only be derived for structs with named fields",
        )
    })?;

    let field_ty = &field.ty;

    enum PathData {
        SelfPath,
        Path(String),
        None,
    }

    let mut path = PathData::None;
    let mut self_path = field_name == "self_path";
    let mut with_newtype = None::<Type>;

    for attr in field
        .attrs
        .iter()
        .filter(|attr| attr.meta.path().is_ident("dir_structure"))
    {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("path") {
                let _eq = meta.input.parse::<Token![=]>()?;
                if meta.input.peek(syn::LitStr) {
                    let s = meta.input.parse::<syn::LitStr>()?;
                    path = PathData::Path(s.value());
                } else if meta.input.peek(Token![self]) {
                    let _self = meta.input.parse::<Token![self]>()?;
                    path = PathData::SelfPath;
                } else {
                    return Err(syn::Error::new_spanned(
                        meta.path,
                        "Expected a string literal or `self`",
                    ));
                }
            } else if meta.path.is_ident("self_path") {
                self_path = true;
            } else if meta.path.is_ident("with_newtype") {
                let _eq = meta.input.parse::<Token![=]>()?;
                let ty = meta.input.parse::<Type>()?;
                with_newtype = Some(ty);
            } else {
                return Err(syn::Error::new_spanned(
                    meta.path,
                    "Unknown attribute for dir_structure",
                ));
            }

            Ok(())
        })?;
    }

    let (actual_path_expr, actual_path_expr_move, path_pusher_for_has_field) = match path {
        PathData::Path(p) => (
            quote! { #path_param_name.join(#p) },
            quote! { #path_param_name.join(#p) },
            quote! { #path_param_name.push(#p); },
        ),
        PathData::SelfPath => (
            quote! { #path_param_name },
            quote! { #path_param_name.clone() },
            quote! {},
        ),
        PathData::None => {
            let name = field_name.to_string();
            (
                quote! { #path_param_name.join(#name) },
                quote! { #path_param_name.join(#name) },
                quote! { #path_param_name.push(#name); },
            )
        }
    };
    let actual_field_ty_perform = with_newtype.as_ref().unwrap_or(field_ty);
    let read_code = if self_path {
        // self_path field, just use the path directly
        quote! {
            #field_ty::from(#path_param_name)
        }
    } else {
        let value_name = format_ident!("__value");
        let end_expr = match &with_newtype {
            Some(nt) => quote! {
                <#nt as ::dir_structure::NewtypeToInner>::into_inner(#value_name)
            },
            None => quote! {
                #value_name
            },
        };

        quote! {{
            let __translated_path = #actual_path_expr;
            let #value_name = <#actual_field_ty_perform as ::dir_structure::ReadFrom<Vfs>>::read_from(&__translated_path, vfs)?;
            #end_expr
        }}
    };

    let async_read_code = if self_path {
        // self_path field, just use the path directly
        quote! {
            #field_ty::from(#path_param_name)
        }
    } else {
        let value_name = format_ident!("__value");
        let end_expr = match &with_newtype {
            Some(nt) => quote! {
                <#nt as ::dir_structure::NewtypeToInner>::into_inner(#value_name)
            },
            None => quote! {
                #value_name
            },
        };

        quote! {{
            let __translated_path = #actual_path_expr_move;
            let #value_name = <#actual_field_ty_perform as ::dir_structure::ReadFromAsync<'vfs, Vfs>>::read_from_async(__translated_path, vfs).await?;
            #end_expr
        }}
    };

    let write_code = if self_path {
        // self_path does not need to write anything
        quote! {}
    } else {
        let writer = match &with_newtype {
            Some(nt) => {
                quote! { &<#nt as ::dir_structure::FromRefForWriter<'_, Vfs>>::from_ref_for_writer(&self.#field_name) }
            }
            None => quote! { &self.#field_name },
        };
        quote! {
            let __translated_path = #actual_path_expr;
            ::dir_structure::WriteTo::write_to(#writer, &__translated_path, vfs)?;
        }
    };

    let async_write_code = if self_path {
        // self_path does not need to write anything
        quote! {}
    } else {
        match &with_newtype {
            Some(nt) => {
                quote! {
                    let __translated_path = #actual_path_expr_move;
                    <<#nt as ::dir_structure::FromRefForWriterAsync<'_, Vfs>>::Wr as ::dir_structure::WriteToAsyncOwned<'_, Vfs>>::write_to_async_owned(<#nt as ::dir_structure::FromRefForWriterAsync<'_, Vfs>>::from_ref_for_writer_async(&self.#field_name), __translated_path, vfs).await?;
                }
            }
            None => quote! {
                let __translated_path = #actual_path_expr_move;
                <#actual_field_ty_perform as ::dir_structure::WriteToAsync<Vfs>>::write_to_async(&self.#field_name, __translated_path, vfs).await?;
            },
        }
    };

    let async_write_owned_code = if self_path {
        // self_path does not need to write anything
        quote! {}
    } else {
        quote! {
            let __translated_path = #actual_path_expr_move;
            ::dir_structure::WriteToAsyncOwned<'_, Vfs>::write_to_async_owned(self.#field_name, __translated_path, vfs).await?;
        }
    };

    #[cfg(feature = "resolve-path")]
    let has_field_impl = if self_path {
        quote! {}
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

        quote! {
            impl #impl_generics ::dir_structure::HasField<{ [#(#field_name_array),*] }> for #ty_name #ty_generics #where_clause {
                type Inner = #field_ty;

                fn resolve_path(mut #path_param_name: ::std::path::PathBuf) -> ::std::path::PathBuf {
                    #path_pusher_for_has_field
                    #path_param_name
                }
            }
        }
    };
    Ok(DirStructureForField {
        read_code: quote! {
            #field_name: #read_code
        },
        async_read_code: quote! {
            #field_name: #async_read_code
        },
        async_read_bound: if self_path {
            None
        } else {
            Some(parse_quote! {
                for<'trivial> #actual_field_ty_perform: ::dir_structure::ReadFromAsync<'vfs, Vfs>
            })
        },
        write_code,
        async_write_code,
        async_write_bound: if self_path {
            None
        } else {
            Some(parse_quote! {
                for<'trivial> #actual_field_ty_perform: ::dir_structure::WriteToAsync<Vfs>
            })
        },
        async_write_owned_code,
        async_write_owned_bound: if self_path {
            None
        } else {
            Some(parse_quote! {
                for<'trivial> #actual_field_ty_perform: ::dir_structure::WriteToAsyncOwned<'_, Vfs>
            })
        },
        #[cfg(feature = "resolve-path")]
        has_field_impl,
    })
}

fn expand_dir_structure(st: ItemStruct) -> syn::Result<TokenStream> {
    let name = &st.ident;
    let path_param_name = format_ident!("__dir_structure_path");
    let mut generics_for_read_write_impl = st.generics.clone();
    if !generics_for_read_write_impl.params.iter().any(|p| match p {
        syn::GenericParam::Lifetime(lt) => lt.lifetime.ident == "vfs",
        syn::GenericParam::Const(_) | syn::GenericParam::Type(_) => false,
    }) {
        generics_for_read_write_impl
            .params
            .insert(0, parse_quote! { 'vfs });
    }
    if let Some(v) = generics_for_read_write_impl
        .params
        .iter_mut()
        .find_map(|p| match p {
            syn::GenericParam::Lifetime(_) | syn::GenericParam::Const(_) => None,
            syn::GenericParam::Type(type_param) => {
                (type_param.ident == "Vfs").then_some(type_param)
            }
        })
    {
        let bounds = &v.bounds;
        if bounds.is_empty() {
            v.bounds = parse_quote! { ::dir_structure::Vfs + 'static };
        } else {
            v.bounds.push(parse_quote! { ::dir_structure::Vfs });
            v.bounds.push(parse_quote! { 'static });
        }
    } else {
        generics_for_read_write_impl
            .params
            .push(parse_quote! { Vfs: ::dir_structure::Vfs + 'static });
    }
    let (read_write_impl_generics, _, _) = generics_for_read_write_impl.split_for_impl();

    let mut generics_for_read_write_async_impl = st.generics.clone();
    if !generics_for_read_write_async_impl
        .params
        .iter()
        .any(|p| match p {
            syn::GenericParam::Lifetime(lt) => lt.lifetime.ident == "vfs",
            syn::GenericParam::Const(_) | syn::GenericParam::Type(_) => false,
        })
    {
        generics_for_read_write_async_impl
            .params
            .insert(0, parse_quote! { 'vfs });
    }
    let mut read_async_impl_generics = generics_for_read_write_async_impl.clone();

    if let Some(v) = read_async_impl_generics
        .params
        .iter_mut()
        .find_map(|p| match p {
            syn::GenericParam::Lifetime(_) | syn::GenericParam::Const(_) => None,
            syn::GenericParam::Type(type_param) => {
                (type_param.ident == "Vfs").then_some(type_param)
            }
        })
    {
        let bounds = &v.bounds;
        if bounds.is_empty() {
            v.bounds = parse_quote! { ::dir_structure::VfsAsync + 'static };
        } else {
            v.bounds.push(parse_quote! { ::dir_structure::VfsAsync });
            v.bounds.push(parse_quote! { 'static });
        }
    } else {
        read_async_impl_generics
            .params
            .push(parse_quote! { Vfs: ::dir_structure::VfsAsync + 'static });
    }

    let (read_async_impl_generics, _, _) = read_async_impl_generics.split_for_impl();

    if let Some(v) = generics_for_read_write_async_impl
        .params
        .iter_mut()
        .find_map(|p| match p {
            syn::GenericParam::Lifetime(_) | syn::GenericParam::Const(_) => None,
            syn::GenericParam::Type(type_param) => {
                (type_param.ident == "Vfs").then_some(type_param)
            }
        })
    {
        let bounds = &v.bounds;
        if bounds.is_empty() {
            v.bounds = parse_quote! { ::dir_structure::VfsAsync + 'static };
        } else {
            v.bounds.push(parse_quote! { ::dir_structure::VfsAsync });
            v.bounds.push(parse_quote! { 'static });
        }
    } else {
        generics_for_read_write_async_impl
            .params
            .push(parse_quote! { Vfs: ::dir_structure::VfsAsync + 'static });
    }

    let (read_write_async_impl_generics, _, _) =
        generics_for_read_write_async_impl.split_for_impl();

    let (impl_generics, ty_generics, where_clause) = st.generics.split_for_impl();

    let mut field_read_impls = Vec::new();
    let mut field_async_read_impls = Vec::new();
    let mut field_async_read_bounds = Vec::new();
    let mut field_write_impls = Vec::new();
    let mut field_async_write_impls = Vec::new();
    let mut field_async_write_bounds = Vec::new();
    let mut field_async_write_owned_impls = Vec::new();
    let mut field_async_write_owned_bounds = Vec::new();
    #[cfg(feature = "resolve-path")]
    let mut has_field_impls = Vec::new();

    for field in &st.fields {
        let DirStructureForField {
            read_code,
            async_read_code,
            async_read_bound,
            write_code,
            async_write_code,
            async_write_bound,
            async_write_owned_code,
            async_write_owned_bound,
            #[cfg(feature = "resolve-path")]
            has_field_impl,
        } = expand_dir_structure_for_field(
            (&impl_generics, name, &ty_generics, where_clause),
            &path_param_name,
            field,
        )?;
        field_read_impls.push(read_code);
        field_async_read_impls.push(async_read_code);
        field_async_read_bounds.extend(async_read_bound);
        field_write_impls.push(write_code);
        field_async_write_impls.push(async_write_code);
        field_async_write_bounds.extend(async_write_bound);
        field_async_write_owned_impls.push(async_write_owned_code);
        field_async_write_owned_bounds.push(async_write_owned_bound);
        #[cfg(feature = "resolve-path")]
        has_field_impls.push(has_field_impl);
    }

    #[cfg_attr(
        all(not(feature = "async"), not(feature = "resolve-path")),
        expect(unused_mut)
    )]
    let mut expanded = quote! {
        impl #read_write_impl_generics ::dir_structure::ReadFrom<'vfs, Vfs> for #name #ty_generics #where_clause {
            fn read_from(#path_param_name: &::std::path::Path, vfs:  ::std::pin::Pin<&'vfs Vfs>) -> ::dir_structure::Result<Self>
            where
                Self: Sized,
            {
                Ok(Self {
                    #(#field_read_impls,)*
                })
            }
        }
        impl #read_write_impl_generics ::dir_structure::WriteTo<Vfs> for #name #ty_generics #where_clause {
            fn write_to(&self, #path_param_name: &::std::path::Path, vfs: ::std::pin::Pin<&Vfs>) -> ::dir_structure::Result<()> {
                #(#field_write_impls)*
                Ok(())
            }
        }
        impl #impl_generics ::dir_structure::DirStructure for #name #ty_generics #where_clause {}
    };

    #[cfg(feature = "async")]
    {
        use syn::punctuated::Punctuated;

        fn merge_where_clause(
            where_clause: Option<syn::WhereClause>,
            additional_bounds: Vec<syn::WherePredicate>,
        ) -> Option<syn::WhereClause> {
            if let Some(mut where_clause) = where_clause {
                where_clause.predicates.extend(additional_bounds);
                Some(where_clause)
            } else {
                let mut where_clause = syn::WhereClause {
                    where_token: <Token![where]>::default(),
                    predicates: Punctuated::new(),
                };
                where_clause.predicates.extend(additional_bounds);
                if where_clause.predicates.is_empty() {
                    None
                } else {
                    Some(where_clause)
                }
            }
        }

        let where_clause_read_from_async =
            merge_where_clause(where_clause.cloned(), field_async_read_bounds);
        let where_clause_write_to_async =
            merge_where_clause(where_clause.cloned(), field_async_write_bounds);
        expanded.extend(quote! {
            impl #read_async_impl_generics ::dir_structure::ReadFromAsync<'vfs, Vfs> for #name #ty_generics #where_clause_read_from_async {
                type Future = //::std::pin::Pin<::std::boxed::Box<dyn
                impl ::std::future::Future<Output = ::dir_structure::Result<Self>> + ::std::marker::Send + 'vfs
                // >>
                where
                    Self: 'vfs;

                fn read_from_async(#path_param_name: ::std::path::PathBuf, vfs: ::std::pin::Pin<&'vfs Vfs>) -> Self::Future
                where
                    Self: Sized,
                {
                    // Box::pin(
                        async move {
                        Ok(Self {
                            #(#field_async_read_impls,)*
                        })
                    }
                    // )
                }
            }
            impl #read_write_async_impl_generics ::dir_structure::WriteToAsync<Vfs> for #name #ty_generics #where_clause_write_to_async {
                type Future<'a> = // ::std::pin::Pin<::std::boxed::Box<dyn
                impl ::std::future::Future<Output = ::dir_structure::Result<()>> + ::std::marker::Send + 'a
                // >>
                where
                    Self: 'a;

                fn write_to_async<'a>(&'a self, #path_param_name: ::std::path::PathBuf, vfs: ::std::pin::Pin<&'a Vfs>) -> Self::Future<'a> {
                    // Box::pin(
                        async move {
                        #(#field_async_write_impls)*
                        Ok(())
                    }
                    // )
                }
            }
        });
    }

    #[cfg(feature = "resolve-path")]
    {
        for has_field_impl in has_field_impls {
            expanded.extend(has_field_impl);
        }
    }

    Ok(expanded)
}

#[cfg(feature = "resolve-path")]
mod resolve_path;

#[cfg(feature = "resolve-path")]
#[proc_macro]
pub fn resolve_path(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    resolve_path::resolve_path(input)
}

#[cfg(feature = "resolve-path")]
#[proc_macro]
pub fn __resolve_max_len(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // This macro is used to get the maximum length of a field name for the `HasField` trait.
    // It is used in the `resolve_path` macro to ensure that field names do not exceed this length.
    let max_len = resolve_path::MAX_LEN;
    let output = quote! { #max_len };
    output.into()
}
