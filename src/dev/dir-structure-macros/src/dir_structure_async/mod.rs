use std::iter;

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::Expr;
use syn::Field;
use syn::GenericParam;
use syn::ImplGenerics;
use syn::ItemStruct;
use syn::Token;
use syn::Type;
use syn::Variant;
use syn::WherePredicate;
use syn::parse_quote;

use crate::dir_structure_core::DirStructureCoreInfo;
use crate::dir_structure_core::PathSpec;
use crate::dir_structure_core::compile_attrs;

struct FutureVariant {
    variant: Variant,
    field_ty: Type,
    actual_field_ty_perform: Type,
    path_expr: TokenStream,
    /// expr that yields Poll, will be used in Future::poll() implementation
    /// it is given the next variant of the future
    future_handler: Box<dyn Fn(Option<&FutureVariant>) -> Expr>,

    corresponding_field: Field,
    newtype: Option<Type>,
}

struct FutureEnum {
    name: Ident,
    proj_name: Ident,
    std_field_set: Vec<Field>,
    struct_field_set: Vec<Field>,
    variants: Vec<FutureVariant>,
    clauses: Vec<WherePredicate>,
    clauses_ref_vfs: bool,
}

struct DirStructureForField {
    async_read_bound: Option<Vec<WherePredicate>>,
    async_write_bound: Option<Vec<WherePredicate>>,
    async_write_ref_bound: Option<Vec<WherePredicate>>,
}

fn expand_dir_structure_for_field(
    (impl_generics, ty_name, ty_generics, where_clause): (
        &ImplGenerics,
        &Ident,
        &syn::TypeGenerics,
        Option<&syn::WhereClause>,
    ),
    path_param_name: &Ident,
    vfs_param_name: &Ident,
    field: &Field,
    async_read_future: &mut FutureEnum,
    async_write_future: &mut FutureEnum,
    async_write_owned_future: &mut FutureEnum,
) -> syn::Result<DirStructureForField> {
    let field_name = field.ident.as_ref().ok_or_else(|| {
        syn::Error::new_spanned(
            field,
            "DirStructure can only be derived for structs with named fields",
        )
    })?;

    let field_ty = &field.ty;

    let DirStructureCoreInfo {
        newtype_ty: with_newtype,
        self_path,
        path,
        ..
    } = compile_attrs(field)?;

    let (actual_path_expr, actual_path_expr_move) = match path {
        PathSpec::Path(p) => (
            quote! { #path_param_name.join(#p) },
            quote! { #path_param_name.join(#p) },
        ),
        PathSpec::SelfPath => (
            quote! { #path_param_name },
            quote! { #path_param_name.clone() },
        ),
    };
    let actual_field_ty_perform = with_newtype.as_ref().unwrap_or(field_ty);

    let async_read_bound = if self_path {
        // self_path field, just use the path directly
        None
    } else {
        let variant_name = format_ident!("Reading_{}", field_name);

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
            let #value_name = <#actual_field_ty_perform as ::dir_structure::ReadFromAsync<'vfs, Vfs>>::read_from_async(__translated_path, #vfs_param_name).await?;
            #end_expr
        }};

        let std_fields = async_read_future.std_field_set.clone().into_iter();
        let struct_fields = async_read_future.struct_field_set.clone().into_iter();
        let enum_variants = std_fields.chain(struct_fields);

        let std_fields = async_read_future
            .std_field_set
            .iter()
            .filter_map(|f| f.ident.clone())
            .collect::<Vec<_>>();
        let fields: Vec<Ident> = async_read_future
            .struct_field_set
            .iter()
            .filter_map(|f| f.ident.clone())
            .collect::<Vec<_>>();

        let ty_name_clone = ty_name.clone();
        let v_name = variant_name.clone();
        let vfs_name = vfs_param_name.clone();
        let f_name = field_name.clone();
        async_read_future.struct_field_set.push(parse_quote! {
            #field_name: #field_ty
        });
        let wnt = with_newtype.clone();
        let async_read_bound = match &wnt {
            Some(nt) => Some(vec![
                parse_quote! {
                    for<'trivial> #nt: ::dir_structure::ReadFromAsync<'vfs, Vfs>
                },
                parse_quote! {
                    for<'trivial> #nt: ::dir_structure::NewtypeToInner<Inner=#field_ty>
                },
            ]),
            None => Some(vec![parse_quote! {
                for<'trivial> #actual_field_ty_perform: ::dir_structure::ReadFromAsync<'vfs, Vfs>
            }]),
        };
        async_read_future.clauses.push(parse_quote! {
            for<'trivial> #actual_field_ty_perform: ::dir_structure::ReadFromAsync<'vfs, Vfs>
        });
        async_read_future.clauses.push(parse_quote! {
            for<'trivial> <#actual_field_ty_perform as ::dir_structure::ReadFromAsync<'vfs, Vfs>>::Future: ::std::future::Future<Output = ::dir_structure::Result<#actual_field_ty_perform>> + ::std::marker::Send + ::std::marker::Unpin + 'vfs
        });
        async_read_future.variants.push(FutureVariant {
            variant: parse_quote! {
                #variant_name {
                    #(#enum_variants,)*
                    __mut_future: <#actual_field_ty_perform as ::dir_structure::ReadFromAsync<'vfs, Vfs>>::Future,
                }
            },
            path_expr: actual_path_expr_move.clone(),
            field_ty: field_ty.clone(),
            actual_field_ty_perform: actual_field_ty_perform.clone(),
            corresponding_field: field.clone(),
            newtype: wnt.clone(),
            future_handler: Box::new(move |next_variant: Option<&FutureVariant>| {
                next_variant.map_or_else(
                    || {
                        let value_name = format_ident!("__value");
                        let end_expr = match &wnt {
                            Some(nt) => quote! {
                                <#nt as ::dir_structure::NewtypeToInner>::into_inner(#value_name)
                            },
                            None => quote! {
                                #value_name
                            },
                        };
                        parse_quote! {
                            match ::std::pin::Pin::new(&mut __mut_future).poll(cx) {
                                ::std::task::Poll::Ready(Ok(#value_name)) => {
                                    ::std::task::Poll::Ready(Ok(#ty_name_clone {
                                        #(#fields,)*
                                        #f_name: #end_expr,
                                    }))
                                }
                                ::std::task::Poll::Ready(Err(e)) => ::std::task::Poll::Ready(Err(e)),
                                ::std::task::Poll::Pending => {
                                    self.project_replace(Self::#v_name {
                                        #(#std_fields,)*
                                        #(#fields,)*
                                        __mut_future,
                                    });
                                    ::std::task::Poll::Pending
                                }
                            }
                        }
                    },
                    |v| {
                        let self_variant_name = &v_name;
                        let variant_name = &v.variant.ident;
                        let perform = &v.actual_field_ty_perform;
                        let path_expr = &v.path_expr;

                        let value_name = format_ident!("__value");
                        let end_expr = match &wnt {
                            Some(nt) => quote! {
                                <#nt as ::dir_structure::NewtypeToInner>::into_inner(#value_name)
                            },
                            None => quote! {
                                #value_name
                            },
                        };

                        parse_quote! {
                            match ::std::pin::Pin::new(&mut __mut_future).poll(cx) {
                                ::std::task::Poll::Ready(Ok(#value_name)) => {
                                    let __translated_path = #path_expr;
                                    let __mut_future = <#perform as ::dir_structure::ReadFromAsync<'vfs, Vfs>>::read_from_async(__translated_path, #vfs_name);
                                    self.project_replace(Self::#variant_name {
                                        #(#std_fields,)*
                                        #(#fields,)*
                                        #f_name: #end_expr,
                                        __mut_future,
                                    });
                                    cx.waker().wake_by_ref();
                                    ::std::task::Poll::Pending
                                },
                                ::std::task::Poll::Ready(Err(e)) => ::std::task::Poll::Ready(Err(e)),
                                ::std::task::Poll::Pending => {
                                    self.project_replace(Self::#self_variant_name {
                                        #(#std_fields,)*
                                        #(#fields,)*
                                        __mut_future,
                                    });
                                    ::std::task::Poll::Pending
                                }
                            }
                        }
                    },
                )
            }),
        });

        async_read_bound
    };

    let async_write_bound = if self_path {
        // self_path does not need to write anything
        None
    } else {
        let variant_name = format_ident!("Writing_{}", field_name);
        let std_fields = async_write_future.std_field_set.clone().into_iter();
        let struct_fields = async_write_future.struct_field_set.clone().into_iter();
        let enum_variants = std_fields.chain(struct_fields);

        let std_fields = async_write_future
            .std_field_set
            .iter()
            .filter_map(|f| f.ident.clone())
            .collect::<Vec<_>>();

        let v_name = variant_name.clone();
        let vfs_name = vfs_param_name.clone();
        let wnt = with_newtype.clone();
        let (fut_ty, mut async_write_bound) = match &wnt {
            Some(nt) => {
                let bound = vec![
                    parse_quote! {
                        for<'trivial> #nt: ::dir_structure::FromRefForWriterAsync<'fut, Vfs, Inner = #field_ty>
                    },
                    parse_quote! {
                        for<'trivial> <#nt as ::dir_structure::FromRefForWriterAsync<'fut, Vfs>>::Wr: ::dir_structure::WriteToAsync<'fut, Vfs>
                    },
                    parse_quote! {
                        for<'trivial> <<#nt as ::dir_structure::FromRefForWriterAsync<'fut, Vfs>>::Wr as ::dir_structure::WriteToAsync<'fut, Vfs>>::Future: ::std::future::Future<Output = ::dir_structure::Result<()>> + ::std::marker::Send + ::std::marker::Unpin + 'fut
                    },
                ];
                async_write_future.clauses.extend(bound.clone());

                (
                    quote! {
                        <<#nt as ::dir_structure::FromRefForWriterAsync<'fut, Vfs>>::Wr as ::dir_structure::WriteToAsync<'fut, Vfs>>::Future
                    },
                    bound,
                )
            }
            None => {
                async_write_future.clauses_ref_vfs = true;
                let bound = vec![
                    parse_quote! {
                        for<'trivial> #actual_field_ty_perform: ::dir_structure::WriteToAsyncRef<'vfs, Vfs>
                    },
                    parse_quote! {
                        for<'trivial> <#actual_field_ty_perform as ::dir_structure::WriteToAsyncRef<'vfs, Vfs>>::Future<'fut>: ::std::future::Future<Output = ::dir_structure::Result<()>> + ::std::marker::Send + ::std::marker::Unpin + 'fut
                    },
                ];
                async_write_future.clauses.extend(bound.clone());
                async_write_future.clauses.push(parse_quote! {
                    'vfs: 'fut
                });

                (
                    quote! {
                        <#actual_field_ty_perform as ::dir_structure::WriteToAsyncRef<'vfs, Vfs>>::Future<'fut>
                    },
                    bound,
                )
            }
        };
        async_write_future.variants.push(FutureVariant {
            variant: parse_quote! {
                #variant_name {
                    #(#enum_variants,)*
                    __mut_future: #fut_ty,
                }
            },
            path_expr: actual_path_expr_move.clone(),
            field_ty: field_ty.clone(),
            actual_field_ty_perform: actual_field_ty_perform.clone(),
            corresponding_field: field.clone(),
            newtype: wnt.clone(),
            future_handler: Box::new(move |next_variant: Option<&FutureVariant>| {
                next_variant.map_or_else(
                    || {
                        parse_quote! {
                            match ::std::pin::Pin::new(&mut __mut_future).poll(cx) {
                                ::std::task::Poll::Ready(Ok(())) => ::std::task::Poll::Ready(Ok(())),
                                ::std::task::Poll::Ready(Err(e)) => ::std::task::Poll::Ready(Err(e)),
                                ::std::task::Poll::Pending => {
                                    self.project_replace(Self::#v_name {
                                        #(#std_fields,)*
                                        __mut_future,
                                    });
                                    ::std::task::Poll::Pending
                                }
                            }
                        }
                    },
                    |v| {
                        let self_variant_name = &v_name;
                        let field_ty = &v.field_ty;
                        let variant_name = &v.variant.ident;
                        let perform = &v.actual_field_ty_perform;
                        let path_expr = &v.path_expr;
                        let f_name = v.corresponding_field.ident.as_ref().unwrap();

                        let fut = match &v.newtype {
                            Some(nt) => {
                                quote! {{
                                    let __translated_path = #path_expr;
                                    <<#nt as ::dir_structure::FromRefForWriterAsync<'_, Vfs>>::Wr as ::dir_structure::WriteToAsync<'_, Vfs>>::write_to_async(<#nt as ::dir_structure::FromRefForWriterAsync<'_, Vfs>>::from_ref_for_writer_async(&__this.#f_name), __translated_path, #vfs_name)
                                }}
                            }
                            None => quote! {{
                                let __translated_path = #path_expr;
                                <#perform as ::dir_structure::WriteToAsyncRef<'_, Vfs>>::write_to_async_ref(&__this.#f_name, __translated_path, #vfs_name)
                            }},
                        };

                        parse_quote! {
                            match ::std::pin::Pin::new(&mut __mut_future).poll(cx) {
                                ::std::task::Poll::Ready(Ok(())) => {
                                    let __mut_future = #fut;
                                    self.project_replace(Self::#variant_name {
                                        #(#std_fields,)*
                                        __mut_future,
                                    });
                                    cx.waker().wake_by_ref();
                                    ::std::task::Poll::Pending
                                },
                                ::std::task::Poll::Ready(Err(e)) => ::std::task::Poll::Ready(Err(e)),
                                ::std::task::Poll::Pending => {
                                    self.project_replace(Self::#self_variant_name {
                                        #(#std_fields,)*
                                        __mut_future,
                                    });
                                    ::std::task::Poll::Pending
                                }
                            }
                        }
                    },
                )
            })
        });

        for b in &mut async_write_bound {
            if let WherePredicate::Type(p) = b
                && let Some(lt) = &mut p.lifetimes
                && lt.lifetimes.iter().any(|it| match it {
                    GenericParam::Lifetime(lt) => lt.lifetime.ident == "trivial",
                    _ => false,
                })
            {
                lt.lifetimes.push(parse_quote! { 'fut });
            }
        }

        Some(async_write_bound)
    };

    let async_write_ref_code = if self_path {
        // self_path does not need to write anything
        quote! {}
    } else {
        quote! {
            let __translated_path = #actual_path_expr_move;
            ::dir_structure::WriteToAsyncRef<'vfs, Vfs>::write_to_async_ref(&self.#field_name, __translated_path, #vfs_param_name).await?;
        }
    };

    Ok(DirStructureForField {
        async_read_bound,
        async_write_bound,
        async_write_ref_bound: None,
    })
}

pub fn expand_dir_structure_async(st: ItemStruct) -> syn::Result<TokenStream> {
    let name = &st.ident;
    let path_param_name = format_ident!("__dir_structure_path");
    let vfs_param_name = format_ident!("__vfs");
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

    let mut write_async_impl_generics = generics_for_read_write_async_impl.clone();
    if let Some(v) = write_async_impl_generics
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
        write_async_impl_generics
            .params
            .push(parse_quote! { Vfs: ::dir_structure::VfsAsync + 'static });
    }

    let (write_async_impl_generics, _, _) = write_async_impl_generics.split_for_impl();

    let (impl_generics, ty_generics, where_clause) = st.generics.split_for_impl();

    let mut read_async = FutureEnum {
        name: format_ident!("__{}ReadAsyncFuture", name),
        proj_name: format_ident!("__{}ReadAsyncFutureProj", name),
        std_field_set: vec![
            parse_quote! {
                #path_param_name: ::std::path::PathBuf
            },
            parse_quote! {
                #vfs_param_name: ::std::pin::Pin<&'vfs Vfs>
            },
        ],
        struct_field_set: Vec::new(),
        variants: Vec::new(),
        clauses: Vec::new(),
        clauses_ref_vfs: false,
    };

    let mut write_async = FutureEnum {
        name: format_ident!("__{}WriteAsyncFuture", name),
        proj_name: format_ident!("__{}WriteAsyncFutureProj", name),
        std_field_set: vec![
            parse_quote! {
                #path_param_name: ::std::path::PathBuf
            },
            parse_quote! {
                #vfs_param_name: ::std::pin::Pin<&'fut Vfs>
            },
            parse_quote! {
                __this: &'fut #name #ty_generics
            },
        ],
        struct_field_set: Vec::new(),
        variants: Vec::new(),
        clauses: Vec::new(),
        clauses_ref_vfs: false,
    };

    let mut write_async_owned = FutureEnum {
        name: format_ident!("__{}WriteAsyncOwnedFuture", name),
        proj_name: format_ident!("__{}WriteAsyncOwnedFutureProj", name),
        std_field_set: vec![
            parse_quote! {
                #path_param_name: ::std::path::PathBuf
            },
            parse_quote! {
                #vfs_param_name: ::std::pin::Pin<&'vfs Vfs>
            },
        ],
        struct_field_set: Vec::new(),
        variants: Vec::new(),
        clauses: Vec::new(),
        clauses_ref_vfs: false,
    };

    let mut field_async_read_bounds = Vec::new();
    let mut field_async_write_bounds = Vec::new();
    let mut field_async_write_ref_bounds = Vec::new();

    for field in &st.fields {
        let DirStructureForField {
            async_read_bound,
            async_write_bound,
            async_write_ref_bound,
        } = expand_dir_structure_for_field(
            (&impl_generics, name, &ty_generics, where_clause),
            &path_param_name,
            &vfs_param_name,
            field,
            &mut read_async,
            &mut write_async,
            &mut write_async_owned,
        )?;
        if let Some(async_read_bound) = async_read_bound {
            field_async_read_bounds.extend(async_read_bound);
        }
        if let Some(async_write_bound) = async_write_bound {
            field_async_write_bounds.extend(async_write_bound);
        }
        if let Some(async_write_ref_bound) = async_write_ref_bound {
            field_async_write_ref_bounds.extend(async_write_ref_bound);
        }
    }

    let mut expanded = TokenStream::new();

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

    let where_clause_read_future = merge_where_clause(None, read_async.clauses.clone());

    let where_clause_write_future = merge_where_clause(None, write_async.clauses.clone());

    let read_async_impl_enum = {
        let name = &read_async.name;
        let proj_name = &read_async.proj_name;
        let variants = read_async
            .variants
            .iter()
            .map(|v| &v.variant)
            .collect::<Vec<_>>();

        let read_async_proj_name = read_async.proj_name.clone();

        let branches = read_async.variants.iter().zip(read_async.variants.iter().skip(1).map(Some).chain(iter::once(None)))
            .map(|(current, next)| {
                let current_name = &current.variant.ident;
                let e = (current.future_handler)(next);
                let (mut_fields, other_fields): (Vec<_>, Vec<_>) = current.variant.fields.iter().filter_map(|f| f.ident.as_ref()).partition(|a| a.to_string().starts_with("__mut_"));
                quote! {
                    #read_async_proj_name::#current_name { #(#other_fields,)* #(mut #mut_fields,)* } => #e,
                }
            });

        let first = read_async
            .variants
            .first()
            .expect("At least one variant is required");
        let first_name = &first.variant.ident;
        let first_path = &first.path_expr;
        let first_ty = &first.actual_field_ty_perform;
        let ty_name = &st.ident;

        quote! {
            #[allow(non_camel_case_types)]
            #[::dir_structure::pin_project::pin_project(project_replace = #proj_name)]
            enum #name<'vfs, Vfs: ::dir_structure::VfsAsync + 'static> #where_clause_read_future {
                Poison,
                Init {
                    #path_param_name: ::std::path::PathBuf,
                    #vfs_param_name: ::std::pin::Pin<&'vfs Vfs>,
                },
                #(#variants),*
            }


            impl<'vfs, Vfs: ::dir_structure::VfsAsync + 'static> ::std::future::Future for #name<'vfs, Vfs> #where_clause_read_future {
                type Output = ::dir_structure::Result<#ty_name #ty_generics>;

                fn poll(mut self: ::std::pin::Pin<&mut Self>, cx: &mut ::std::task::Context<'_>) -> ::std::task::Poll<Self::Output> {
                    let this = self.as_mut().project_replace(Self::Poison);
                    match this {
                        #read_async_proj_name::Poison => {
                            panic!("Future was polled after completion");
                        }
                        #read_async_proj_name::Init { #path_param_name, #vfs_param_name } => {
                            let __mut_future = <#first_ty as ::dir_structure::ReadFromAsync<'vfs, Vfs>>::read_from_async(#first_path, #vfs_param_name);
                            self.project_replace(Self::#first_name {
                                #path_param_name,
                                #vfs_param_name,
                                __mut_future,
                            });
                            cx.waker().wake_by_ref();
                            ::std::task::Poll::Pending
                        }
                        #(#branches)*
                    }
                }
            }
        }
    };

    let (write_async_ref_impl_enum, vfs_lifetime_header) = {
        let name = &write_async.name;
        let proj_name = &write_async.proj_name;
        let variants = write_async
            .variants
            .iter()
            .map(|v| &v.variant)
            .collect::<Vec<_>>();

        let write_async_proj_name = write_async.proj_name.clone();

        let branches = write_async.variants.iter().zip(write_async.variants.iter().skip(1).map(Some).chain(iter::once(None)))
            .map(|(current, next)| {
                let current_name = &current.variant.ident;
                let e = (current.future_handler)(next);
                let (mut_fields, other_fields): (Vec<_>, Vec<_>) = current.variant.fields.iter().filter_map(|f| f.ident.as_ref()).partition(|a| a.to_string().starts_with("__mut_"));
                quote! {
                    #write_async_proj_name::#current_name { #(#other_fields,)* #(mut #mut_fields,)* } => #e,
                }
            });

        let first = write_async
            .variants
            .first()
            .expect("At least one variant is required");
        let first_name = first
            .corresponding_field
            .ident
            .as_ref()
            .expect("First variant must have a field name");
        let first_name_variant = first.variant.ident.clone();
        let first_path = &first.path_expr;
        let first_ty = &first.actual_field_ty_perform;
        let ty_name = &st.ident;
        let first_fut = match &first.newtype {
            Some(nt) => {
                quote! {{
                    let __translated_path = #first_path;
                    <<#nt as ::dir_structure::FromRefForWriterAsync<'_, Vfs>>::Wr as ::dir_structure::WriteToAsync<'_, Vfs>>::write_to_async(<#nt as ::dir_structure::FromRefForWriterAsync<'_, Vfs>>::from_ref_for_writer_async(&__this.#first_name), __translated_path, #vfs_param_name)
                }}
            }
            None => quote! {{
                let __translated_path = #first_path;
                <#first_ty as ::dir_structure::WriteToAsyncRef<'_, Vfs>>::write_to_async_ref(&__this.#first_name, __translated_path, #vfs_param_name)
            }},
        };

        let vfs_lifetime_header = if write_async.clauses_ref_vfs {
            quote! {'vfs,}
        } else {
            quote! {}
        };

        (
            quote! {
                #[allow(non_camel_case_types)]
                #[::dir_structure::pin_project::pin_project(project_replace = #proj_name)]
                enum #name<#vfs_lifetime_header 'fut, Vfs: ::dir_structure::WriteSupportingVfsAsync + 'static> #where_clause_write_future {
                    Poison,
                    Init {
                        #path_param_name: ::std::path::PathBuf,
                        #vfs_param_name: ::std::pin::Pin<&'fut Vfs>,
                        __this: &'fut #ty_name #ty_generics,
                    },
                    #(#variants),*
                }

                impl<#vfs_lifetime_header 'fut, Vfs: ::dir_structure::WriteSupportingVfsAsync + 'static> ::std::future::Future for #name<#vfs_lifetime_header 'fut, Vfs> #where_clause_write_future {
                    type Output = ::dir_structure::Result<()>;

                    fn poll(mut self: ::std::pin::Pin<&mut Self>, cx: &mut ::std::task::Context<'_>) -> ::std::task::Poll<Self::Output> {
                        let this = self.as_mut().project_replace(Self::Poison);
                        match this {
                            #write_async_proj_name::Poison => {
                                panic!("Future was polled after completion");
                            }
                            #write_async_proj_name::Init { #path_param_name, #vfs_param_name, __this } => {
                                let __mut_future = #first_fut;
                                self.project_replace(Self::#first_name_variant {
                                    #path_param_name,
                                    #vfs_param_name,
                                    __this,
                                    __mut_future,
                                });
                                cx.waker().wake_by_ref();
                                ::std::task::Poll::Pending
                            }
                            #(#branches)*
                        }
                    }
                }
            },
            vfs_lifetime_header,
        )
    };

    let read_async_ty_name = &read_async.name;
    let write_async_ty_name = &write_async.name;

    expanded.extend(quote! {
        #read_async_impl_enum

        impl #read_async_impl_generics ::dir_structure::ReadFromAsync<'vfs, Vfs> for #name #ty_generics #where_clause_read_from_async {
            type Future = #read_async_ty_name<'vfs, Vfs>
            where
                Self: 'vfs;

            fn read_from_async(#path_param_name: ::std::path::PathBuf, #vfs_param_name: ::std::pin::Pin<&'vfs Vfs>) -> Self::Future
            where
                Self: Sized,
            {
                #read_async_ty_name::Init {
                    #path_param_name,
                    #vfs_param_name,
                }
            }
        }

        #write_async_ref_impl_enum

        impl #write_async_impl_generics ::dir_structure::WriteToAsyncRef<'vfs, Vfs> for #name #ty_generics #where_clause_write_to_async {
            type Future<'a> =  #write_async_ty_name<#vfs_lifetime_header 'a, Vfs>
            where
                Self: 'a,
                'vfs: 'a,
                Vfs: 'a;

            fn write_to_async_ref<'a>(&'a self, #path_param_name: ::std::path::PathBuf, #vfs_param_name: ::std::pin::Pin<&'a Vfs>) -> Self::Future<'a> where 'vfs: 'a {
                #write_async_ty_name::Init {
                    #path_param_name,
                    #vfs_param_name,
                    __this: self,
                }
            }
        }
    });

    Ok(expanded)
}
