use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;

#[proc_macro_derive(AssertEq, attributes(assert_eq))]
pub fn derive_assert_eq(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);

    match input.data {
        syn::Data::Struct(data) => expand_assert_eq_struct(&input.ident, &input.generics, &data)
            .unwrap_or_else(|err| err.to_compile_error())
            .into(),
        syn::Data::Enum(data) => expand_assert_eq_enum(&input.ident, &input.generics, &data)
            .unwrap_or_else(|err| err.to_compile_error())
            .into(),
        syn::Data::Union(_) => syn::Error::new_spanned(
            &input.ident,
            "AssertEq can only be derived for structs and enums",
        )
        .to_compile_error()
        .into(),
    }
}

struct FieldConfig {
    ignored: bool,
}

fn extract_field_config(field: &syn::Field) -> syn::Result<FieldConfig> {
    let mut ignored = false;

    for attr in field
        .attrs
        .iter()
        .filter(|attr| attr.meta.path().is_ident("assert_eq"))
    {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("ignore") {
                ignored = true;
            } else {
                return Err(syn::Error::new_spanned(
                    meta.path,
                    "Unknown attribute for assert_eq",
                ));
            }
            Ok(())
        })?;
    }

    Ok(FieldConfig { ignored })
}

fn expand_assert_eq_struct(
    struct_name: &syn::Ident,
    generics: &syn::Generics,
    data: &syn::DataStruct,
) -> syn::Result<proc_macro2::TokenStream> {
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let mut where_clause = where_clause.cloned();
    let field_checks = match &data.fields {
        syn::Fields::Named(n) => {
            n.named.iter().map(|field| {
                let ty = &field.ty;
                let field_name = field.ident.as_ref().unwrap();
                let FieldConfig { ignored } = extract_field_config(field)?;
                if ignored {
                    Ok(quote! {})
                } else {
                    match &mut where_clause {
                        Some(wc) => {
                            wc.predicates.push(syn::parse_quote! { for<'__trivial> #ty: ::assert_eq::AssertEq<#ty> + ::core::fmt::Debug });
                        }
                        x @ None => {
                            *x = Some(syn::parse_quote! { where for<'__trivial> #ty: ::assert_eq::AssertEq<#ty> + ::core::fmt::Debug });
                        }
                    }
                    Ok(quote! {
                        <#ty as ::assert_eq::AssertEq<#ty>>::assert_eq(&self.#field_name, &other.#field_name, &mut *path.__guard(concat!(".", stringify!(#field_name))), init_left, init_right);
                    })
                }
            }).collect::<syn::Result<TokenStream>>()?
        }
        syn::Fields::Unnamed(u) => {
            u.unnamed.iter().enumerate().map(|(idx, field)| {
                let ty = &field.ty;
                let FieldConfig { ignored } = extract_field_config(field)?;
                if ignored {
                    Ok(quote::quote! {})
                } else {
                    match &mut where_clause {
                        Some(wc) => {
                            wc.predicates.push(syn::parse_quote! { for<'__trivial> #ty: ::assert_eq::AssertEq<#ty> + ::core::fmt::Debug });
                        }
                        x @ None => {
                            *x = Some(syn::parse_quote! { where for<'__trivial> #ty: ::assert_eq::AssertEq<#ty> + ::core::fmt::Debug });
                        }
                    }

                    let id = syn::Index::from(idx);
                    Ok(quote::quote! {
                        <#ty as ::assert_eq::AssertEq<#ty>>::assert_eq(&self.#id, &other.#id, &mut *path.__guard(concat!(".", stringify!(#id))), init_left, init_right);
                    })
                }
            }).collect::<syn::Result<TokenStream>>()?
        }
        syn::Fields::Unit => {
            quote::quote! {}
        }
    };

    let expanded = quote::quote! {
        impl #impl_generics ::assert_eq::AssertEq<#struct_name #ty_generics> for #struct_name #ty_generics #where_clause {
            fn assert_eq(&self, other: &Self, path: &mut ::assert_eq::AssertPath, init_left: &impl ::core::fmt::Display, init_right: &impl ::core::fmt::Display) {
                #field_checks
            }
        }
    };

    Ok(expanded)
}

fn expand_assert_eq_enum(
    enum_name: &syn::Ident,
    generics: &syn::Generics,
    data: &syn::DataEnum,
) -> syn::Result<proc_macro2::TokenStream> {
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let mut where_clause = where_clause.cloned();

    let variant_checks = data.variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        match &variant.fields {
            syn::Fields::Named(fields) => {
                let mut field_names = Vec::new();
                let mut self_patterns = Vec::new();
                let mut other_patterns = Vec::new();
                let mut self_names = Vec::new();
                let mut other_names = Vec::new();
                let mut field_configs = Vec::new();
                for (name, field) in fields.named.iter().map(|f| (f.ident.clone().unwrap(), f)) {
                    let self_pat_name = format_ident!("__self_{}", name);
                    let other_pat_name = format_ident!("__other_{}", name);
                    self_patterns.push(quote!{ #name: #self_pat_name });
                    other_patterns.push(quote!{ #name: #other_pat_name });
                    self_names.push(self_pat_name);
                    other_names.push(other_pat_name);

                    let ty = &field.ty;
                    let f_config = extract_field_config(field)?;

                    field_names.push(name);

                    if !f_config.ignored {
                        match &mut where_clause {
                            Some(wc) => {
                                wc.predicates.push(syn::parse_quote! { for<'__trivial> #ty: ::assert_eq::AssertEq<#ty> + ::core::fmt::Debug });
                            }
                            x @ None => {
                                *x = Some(syn::parse_quote! { where for<'__trivial> #ty: ::assert_eq::AssertEq<#ty> + ::core::fmt::Debug });
                            }
                        }
                    }

                    field_configs.push(f_config);
                }

                struct EqGenerator<'a> {
                    types: &'a [syn::Type],
                    names: &'a [syn::Ident],
                    self_names: &'a [syn::Ident],
                    other_names: &'a [syn::Ident],
                    field_configs: &'a [FieldConfig],
                }

                impl<'a> EqGenerator<'a> {
                    fn generate(&self) -> proc_macro2::TokenStream {
                        let checks = self.names.iter().zip(self.self_names.iter().zip(self.other_names.iter()))
                        .zip(self.field_configs.iter())
                        .enumerate()
                        .map(|(idx, ((name, (s, o)), f_config))| {
                            let ty = &self.types[idx];
                            if f_config.ignored {
                                quote::quote! {}
                            } else {
                                quote::quote! {
                                    <#ty as ::assert_eq::AssertEq<#ty>>::assert_eq(#s, #o, &mut *__g.__guard(concat!(".", stringify!(#name))), init_left, init_right);
                                }
                            }
                        });
                        quote::quote! {
                            #(#checks)*
                        }
                    }
                }

                let self_pattern = quote::quote! { #enum_name::#variant_name { #(#self_patterns,)* } };
                let other_pattern = quote::quote! { #enum_name::#variant_name { #(#other_patterns,)* } };
                let field_checks = EqGenerator {
                    types: &fields.named.iter().map(|f| f.ty.clone()).collect::<Vec<_>>(),
                    names: &field_names,
                    self_names: &self_names,
                    other_names: &other_names,
                    field_configs: &field_configs,
                }.generate();

                Ok(quote::quote! {
                    (#self_pattern, #other_pattern) => {
                        let mut __g = path.__guard(concat!("[", stringify!(#variant_name), "]"));
                        #field_checks
                    }
                })
            }
            syn::Fields::Unnamed(fields) => {
                let (self_names, other_names) = (0..fields.unnamed.len()).map(|idx| (format_ident!("__field_self_{}", idx), format_ident!("__field_other_{}", idx))).unzip::<_, _, Vec<_>, Vec<_>>();
                let self_pattern = quote::quote! { #enum_name::#variant_name( #(#self_names),* ) };
                let other_pattern = quote::quote! { #enum_name::#variant_name( #(#other_names),* ) };
                let field_checks = self_names.iter().zip(other_names.iter()).enumerate().map(|(idx, (self_name, other_name))| {
                    let field = &fields.unnamed[idx];
                    let FieldConfig { ignored } = extract_field_config(field).unwrap();
                    if ignored {
                        quote::quote! {}
                    } else {
                        let ty = &fields.unnamed[idx].ty;
                        match &mut where_clause {
                            Some(wc) => {
                                wc.predicates.push(syn::parse_quote! { for<'__trivial> #ty: ::assert_eq::AssertEq<#ty> + ::core::fmt::Debug });
                            }
                            x @ None => {
                                *x = Some(syn::parse_quote! { where for<'__trivial> #ty: ::assert_eq::AssertEq<#ty> + ::core::fmt::Debug });
                            }
                        }
                        quote::quote! {
                            <#ty as ::assert_eq::AssertEq<#ty>>::assert_eq(#self_name, #other_name, &mut *__g.__guard(concat!(".", stringify!(#idx))), init_left, init_right);
                        }
                    }
                });
                Ok(quote::quote! {
                    (#self_pattern, #other_pattern) => {
                        let mut __g = path.__guard(concat!("[", stringify!(#variant_name), "]"));
                        #(#field_checks)*
                    }
                })
            }
            syn::Fields::Unit => {
                Ok(quote::quote! {
                    (#enum_name::#variant_name, #enum_name::#variant_name) => {}
                })
            }
        }
    }).collect::<syn::Result<Vec<_>>>()?;

    let expanded = quote::quote! {
        impl #impl_generics ::assert_eq::AssertEq<#enum_name #ty_generics> for #enum_name #ty_generics #where_clause {
            fn assert_eq(&self, other: &Self, path: &mut ::assert_eq::AssertPath, init_left: &impl ::core::fmt::Display, init_right: &impl ::core::fmt::Display) {
                match (self, other) {
                    #(#variant_checks),*,
                    _ => panic!("Enum variants do not match, at {path:?}:\n  left: {self:?}\n right: {other:?}"),
                }
            }
        }
    };
    Ok(expanded)
}
