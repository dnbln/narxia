use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::Field;
use syn::ImplGenerics;
use syn::ItemStruct;
use syn::parse_quote;

use crate::dir_structure_core::DirStructureCoreInfo;
use crate::dir_structure_core::PathSpec;
use crate::dir_structure_core::compile_attrs;

struct DirStructureForField {
    read_code: TokenStream,
    write_code: TokenStream,
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
    vfs_param_name: &Ident,
    field: &Field,
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

    let (actual_path_expr, path_pusher_for_has_field) = match path {
        PathSpec::Path(p) => (
            quote! { #path_param_name.join(#p) },
            quote! { #path_param_name.push(#p); },
        ),
        PathSpec::SelfPath => (quote! { #path_param_name }, quote! {}),
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
                <#nt as ::dir_structure::traits::sync::NewtypeToInner>::into_inner(#value_name)
            },
            None => quote! {
                #value_name
            },
        };

        quote! {{
            let __translated_path = #actual_path_expr;
            let #value_name = <#actual_field_ty_perform as ::dir_structure::traits::sync::ReadFrom<Vfs>>::read_from(&__translated_path, #vfs_param_name)?;
            #end_expr
        }}
    };

    let write_code = if self_path {
        // self_path does not need to write anything
        quote! {}
    } else {
        let writer = match &with_newtype {
            Some(nt) => {
                quote! { &<#nt as ::dir_structure::traits::sync::FromRefForWriter<'_, Vfs>>::from_ref_for_writer(&self.#field_name) }
            }
            None => quote! { &self.#field_name },
        };
        quote! {
            let __translated_path = #actual_path_expr;
            ::dir_structure::traits::sync::WriteTo::write_to(#writer, &__translated_path, #vfs_param_name)?;
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
            impl #impl_generics ::dir_structure::traits::resolve::HasField<{ [#(#field_name_array),*] }> for #ty_name #ty_generics #where_clause {
                type Inner = #actual_field_ty_perform;

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
        write_code,
        #[cfg(feature = "resolve-path")]
        has_field_impl,
    })
}

pub fn expand_dir_structure(st: ItemStruct) -> syn::Result<TokenStream> {
    let name = &st.ident;
    let path_param_name = format_ident!("__dir_structure_path");
    let vfs_param_name = format_ident!("__vfs");
    let mut generics_for_read_impl = st.generics.clone();
    if !generics_for_read_impl.params.iter().any(|p| match p {
        syn::GenericParam::Lifetime(lt) => lt.lifetime.ident == "vfs",
        syn::GenericParam::Const(_) | syn::GenericParam::Type(_) => false,
    }) {
        generics_for_read_impl
            .params
            .insert(0, parse_quote! { 'vfs });
    }
    let fork = generics_for_read_impl.clone();
    if let Some(v) = generics_for_read_impl
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
            v.bounds = parse_quote! { ::dir_structure::traits::vfs::Vfs + 'static };
        } else {
            v.bounds.push(parse_quote! { ::dir_structure::traits::vfs::Vfs });
            v.bounds.push(parse_quote! { 'static });
        }
    } else {
        generics_for_read_impl
            .params
            .push(parse_quote! { Vfs: ::dir_structure::traits::vfs::Vfs + 'static });
    }
    let (read_impl_generics, _, _) = generics_for_read_impl.split_for_impl();

    let mut generics_for_write_impl = fork;
    if let Some(v) = generics_for_write_impl
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
            v.bounds = parse_quote! { ::dir_structure::traits::vfs::WriteSupportingVfs + 'static };
        } else {
            v.bounds
                .push(parse_quote! { ::dir_structure::traits::vfs::WriteSupportingVfs });
            v.bounds.push(parse_quote! { 'static });
        }
    } else {
        generics_for_write_impl
            .params
            .push(parse_quote! { Vfs: ::dir_structure::traits::vfs::WriteSupportingVfs + 'static });
    }

    let (write_impl_generics, _, _) = generics_for_write_impl.split_for_impl();

    let (impl_generics, ty_generics, where_clause) = st.generics.split_for_impl();

    let mut field_read_impls = Vec::new();
    let mut field_write_impls = Vec::new();
    #[cfg(feature = "resolve-path")]
    let mut has_field_impls = Vec::new();

    for field in &st.fields {
        let DirStructureForField {
            read_code,
            write_code,
            #[cfg(feature = "resolve-path")]
            has_field_impl,
        } = expand_dir_structure_for_field(
            (&impl_generics, name, &ty_generics, where_clause),
            &path_param_name,
            &vfs_param_name,
            field,
        )?;
        field_read_impls.push(read_code);
        field_write_impls.push(write_code);
        #[cfg(feature = "resolve-path")]
        has_field_impls.push(has_field_impl);
    }

    #[cfg_attr(not(feature = "resolve-path"), expect(unused_mut))]
    let mut expanded = quote! {
        impl #read_impl_generics ::dir_structure::traits::sync::ReadFrom<'vfs, Vfs> for #name #ty_generics #where_clause {
            fn read_from(#path_param_name: &::std::path::Path, #vfs_param_name: ::std::pin::Pin<&'vfs Vfs>) -> ::dir_structure::error::Result<Self>
            where
                Self: Sized,
            {
                Ok(Self {
                    #(#field_read_impls,)*
                })
            }
        }
        impl #write_impl_generics ::dir_structure::traits::sync::WriteTo<Vfs> for #name #ty_generics #where_clause {
            fn write_to(&self, #path_param_name: &::std::path::Path, #vfs_param_name: ::std::pin::Pin<&Vfs>) -> ::dir_structure::error::Result<()> {
                #(#field_write_impls)*
                Ok(())
            }
        }
        impl #impl_generics ::dir_structure::traits::sync::DirStructure for #name #ty_generics #where_clause {}
    };

    #[cfg(feature = "resolve-path")]
    {
        for has_field_impl in has_field_impls {
            expanded.extend(has_field_impl);
        }
    }

    Ok(expanded)
}
