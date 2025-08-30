use std::iter;

use quote::format_ident;
use quote::quote;
use syn::Field;
use syn::Ident;
use syn::ImplGenerics;
use syn::ItemStruct;
use syn::Type;
use syn::WherePredicate;
use syn::parse_quote;

use crate::dir_structure_async::FutureEnum;
use crate::dir_structure_async::FutureVariant;
use crate::dir_structure_async::merge_where_clause;
use crate::dir_structure_core::PathSpec;

pub(super) fn expand_dir_structure_for_field(
    (impl_generics, ty_name, ty_generics, where_clause): (
        &ImplGenerics,
        &Ident,
        &syn::TypeGenerics,
        Option<&syn::WhereClause>,
    ),
    path_param_name: &Ident,
    vfs_param_name: &Ident,
    field: &Field,
    newtype_ty: Option<&Type>,
    self_path: bool,
    path: &PathSpec,
    async_read_future: &mut FutureEnum,
) -> syn::Result<Option<Vec<WherePredicate>>> {
    let field_name = field.ident.as_ref().ok_or_else(|| {
        syn::Error::new_spanned(
            field,
            "DirStructure can only be derived for structs with named fields",
        )
    })?;

    let field_ty = &field.ty;

    let actual_path_expr_move = match path {
        PathSpec::Path(p) => quote! { #path_param_name.join(#p) },
        PathSpec::SelfPath => quote! { #path_param_name.clone() },
    };
    let actual_field_ty_perform = newtype_ty.unwrap_or(field_ty);

    let async_read_bound = if self_path {
        // self_path field, just use the path directly
        None
    } else {
        let variant_name = format_ident!("Reading_{}", field_name);

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
        let wnt = newtype_ty.cloned();
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

    Ok(async_read_bound)
}

pub(super) fn future_impl_enum(
    st: &ItemStruct,
    path_param_name: &Ident,
    vfs_param_name: &Ident,
    ty_generics: &syn::TypeGenerics,
    read_async: &FutureEnum,
) -> syn::Result<proc_macro2::TokenStream> {
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

    let where_clause_read_future = merge_where_clause(None, read_async.clauses.clone());

    Ok(quote! {
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
    })
}
