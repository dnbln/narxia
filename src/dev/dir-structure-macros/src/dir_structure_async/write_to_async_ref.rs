use std::iter;

use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::Field;
use syn::GenericParam;
use syn::Ident;
use syn::ImplGenerics;
use syn::ItemStruct;
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
    with_newtype: Option<&syn::Type>,
    self_path: bool,
    path: &PathSpec,
    async_write_ref_future: &mut FutureEnum,
) -> syn::Result<Option<Vec<WherePredicate>>> {
    let field_name = field.ident.as_ref().ok_or_else(|| {
        syn::Error::new_spanned(
            field,
            "DirStructure can only be derived for structs with named fields",
        )
    })?;

    let field_ty = &field.ty;

    let actual_path_expr_move = match &path {
        PathSpec::Path(p) => quote! { #path_param_name.join(#p) },
        PathSpec::SelfPath => quote! { #path_param_name.clone() },
    };
    let actual_field_ty_perform = with_newtype.unwrap_or(field_ty);

    let async_write_ref_bound = if self_path {
        // self_path does not need to write anything
        None
    } else {
        let variant_name = format_ident!("Writing_{}", field_name);
        let std_fields = async_write_ref_future.std_field_set.clone().into_iter();
        let struct_fields = async_write_ref_future.struct_field_set.clone().into_iter();
        let enum_variants = std_fields.chain(struct_fields);

        let std_fields = async_write_ref_future
            .std_field_set
            .iter()
            .filter_map(|f| f.ident.clone())
            .collect::<Vec<_>>();

        let v_name = variant_name.clone();
        let vfs_name = vfs_param_name.clone();
        let wnt = with_newtype.clone();
        let (fut_ty, mut async_write_ref_bound) = match &wnt {
            Some(nt) => {
                let bound = vec![
                    parse_quote! {
                        for<'trivial> #nt: ::dir_structure::traits::asy::FromRefForWriterAsync<'fut, Vfs, Inner = #field_ty>
                    },
                    parse_quote! {
                        for<'trivial> <#nt as ::dir_structure::traits::asy::FromRefForWriterAsync<'fut, Vfs>>::Wr: ::dir_structure::traits::asy::WriteToAsync<'fut, Vfs>
                    },
                    parse_quote! {
                        for<'trivial> <<#nt as ::dir_structure::traits::asy::FromRefForWriterAsync<'fut, Vfs>>::Wr as ::dir_structure::traits::asy::WriteToAsync<'fut, Vfs>>::Future: ::std::future::Future<Output = ::dir_structure::error::Result<()>> + ::std::marker::Send + ::std::marker::Unpin + 'fut
                    },
                ];
                async_write_ref_future.clauses.extend(bound.clone());

                (
                    quote! {
                        <<#nt as ::dir_structure::traits::asy::FromRefForWriterAsync<'fut, Vfs>>::Wr as ::dir_structure::traits::asy::WriteToAsync<'fut, Vfs>>::Future
                    },
                    bound,
                )
            }
            None => {
                async_write_ref_future.clauses_ref_vfs = true;
                let bound = vec![
                    parse_quote! {
                        for<'trivial> #actual_field_ty_perform: ::dir_structure::traits::asy::WriteToAsyncRef<'vfs, Vfs>
                    },
                    parse_quote! {
                        for<'trivial> <#actual_field_ty_perform as ::dir_structure::traits::asy::WriteToAsyncRef<'vfs, Vfs>>::Future<'fut>: ::std::future::Future<Output = ::dir_structure::error::Result<()>> + ::std::marker::Send + ::std::marker::Unpin + 'fut
                    },
                ];
                async_write_ref_future.clauses.extend(bound.clone());
                async_write_ref_future.clauses.push(parse_quote! {
                    'vfs: 'fut
                });

                (
                    quote! {
                        <#actual_field_ty_perform as ::dir_structure::traits::asy::WriteToAsyncRef<'vfs, Vfs>>::Future<'fut>
                    },
                    bound,
                )
            }
        };
        async_write_ref_future.variants.push(FutureVariant {
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
            newtype: wnt.cloned(),
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
                        let variant_name = &v.variant.ident;
                        let perform = &v.actual_field_ty_perform;
                        let path_expr = &v.path_expr;
                        let f_name = v.corresponding_field.ident.as_ref().unwrap();

                        let fut = match &v.newtype {
                            Some(nt) => {
                                quote! {{
                                    let __translated_path = #path_expr;
                                    <<#nt as ::dir_structure::traits::asy::FromRefForWriterAsync<'_, Vfs>>::Wr as ::dir_structure::traits::asy::WriteToAsync<'_, Vfs>>::write_to_async(<#nt as ::dir_structure::traits::asy::FromRefForWriterAsync<'_, Vfs>>::from_ref_for_writer_async(&__this.#f_name), __translated_path, #vfs_name)
                                }}
                            }
                            None => quote! {{
                                let __translated_path = #path_expr;
                                <#perform as ::dir_structure::traits::asy::WriteToAsyncRef<'_, Vfs>>::write_to_async_ref(&__this.#f_name, __translated_path, #vfs_name)
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

        for b in &mut async_write_ref_bound {
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

        Some(async_write_ref_bound)
    };

    Ok(async_write_ref_bound)
}

pub(super) fn future_impl_enum(
    st: &ItemStruct,
    vfs_param_name: &Ident,
    path_param_name: &Ident,
    ty_generics: &syn::TypeGenerics,
    write_async_ref: &FutureEnum,
) -> syn::Result<(TokenStream, TokenStream)> {
    let name = &write_async_ref.name;
    let proj_name = &write_async_ref.proj_name;
    let variants = write_async_ref
        .variants
        .iter()
        .map(|v| &v.variant)
        .collect::<Vec<_>>();

    let write_async_proj_name = write_async_ref.proj_name.clone();

    let branches = write_async_ref.variants.iter().zip(write_async_ref.variants.iter().skip(1).map(Some).chain(iter::once(None)))
            .map(|(current, next)| {
                let current_name = &current.variant.ident;
                let e = (current.future_handler)(next);
                let (mut_fields, other_fields): (Vec<_>, Vec<_>) = current.variant.fields.iter().filter_map(|f| f.ident.as_ref()).partition(|a| a.to_string().starts_with("__mut_"));
                quote! {
                    #write_async_proj_name::#current_name { #(#other_fields,)* #(mut #mut_fields,)* } => #e,
                }
            });

    let first = write_async_ref
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
                <<#nt as ::dir_structure::traits::asy::FromRefForWriterAsync<'_, Vfs>>::Wr as ::dir_structure::traits::asy::WriteToAsync<'_, Vfs>>::write_to_async(<#nt as ::dir_structure::traits::asy::FromRefForWriterAsync<'_, Vfs>>::from_ref_for_writer_async(&__this.#first_name), __translated_path, #vfs_param_name)
            }}
        }
        None => quote! {{
            let __translated_path = #first_path;
            <#first_ty as ::dir_structure::traits::asy::WriteToAsyncRef<'_, Vfs>>::write_to_async_ref(&__this.#first_name, __translated_path, #vfs_param_name)
        }},
    };

    let vfs_lifetime_header = if write_async_ref.clauses_ref_vfs {
        quote! {'vfs,}
    } else {
        quote! {}
    };

    let where_clause_write_future = merge_where_clause(None, write_async_ref.clauses.clone());

    Ok((
        quote! {
            #[allow(non_camel_case_types)]
            #[::dir_structure::pin_project::pin_project(project_replace = #proj_name)]
            enum #name<#vfs_lifetime_header 'fut, Vfs: ::dir_structure::traits::async_vfs::WriteSupportingVfsAsync + 'static> #where_clause_write_future {
                Poison,
                Init {
                    #path_param_name: ::std::path::PathBuf,
                    #vfs_param_name: ::std::pin::Pin<&'fut Vfs>,
                    __this: &'fut #ty_name #ty_generics,
                },
                #(#variants),*
            }

            impl<#vfs_lifetime_header 'fut, Vfs: ::dir_structure::traits::async_vfs::WriteSupportingVfsAsync + 'static> ::std::future::Future for #name<#vfs_lifetime_header 'fut, Vfs> #where_clause_write_future {
                type Output = ::dir_structure::error::Result<()>;

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
    ))
}
