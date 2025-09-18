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

    let actual_path_expr = match path {
        PathSpec::Path(p) => {
            quote! { ::dir_structure::traits::vfs::PathType::join_segment_str(#path_param_name, #p).as_ref() }
        }
        PathSpec::SelfPath => quote! { #path_param_name },
    };
    let actual_field_ty_perform = with_newtype.as_ref().unwrap_or(field_ty);
    let read_code = if self_path {
        // self_path field, just use the path directly
        quote! {
            #field_ty::from(::dir_structure::traits::vfs::PathType::owned(#path_param_name))
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
            let #value_name = <#actual_field_ty_perform as ::dir_structure::traits::sync::ReadFrom<Vfs>>::read_from(#actual_path_expr, #vfs_param_name)?;
            #end_expr
        }}
    };

    let write_code = if self_path {
        // self_path does not need to write anything
        quote! {}
    } else {
        let writer = match &with_newtype {
            Some(nt) => {
                quote! { &<#nt as ::dir_structure::traits::sync::FromRefForWriter<'_, '_, Vfs>>::from_ref_for_writer(&self.#field_name) }
            }
            None => quote! { &self.#field_name },
        };
        quote! {
            ::dir_structure::traits::sync::WriteTo::write_to(#writer, #actual_path_expr, #vfs_param_name)?;
        }
    };

    Ok(DirStructureForField {
        read_code: quote! {
            #field_name: #read_code
        },
        write_code,
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
            v.bounds = parse_quote! { ::dir_structure::traits::vfs::Vfs<'vfs> + 'vfs };
        } else {
            v.bounds
                .push(parse_quote! { ::dir_structure::traits::vfs::Vfs<'vfs> });
            v.bounds.push(parse_quote! { 'vfs });
        }
    } else {
        generics_for_read_impl
            .params
            .push(parse_quote! { Vfs: ::dir_structure::traits::vfs::Vfs<'vfs> + 'vfs });
    }
    let (read_impl_generics, _, _) = generics_for_read_impl.split_for_impl();

    let mut generics_for_write_impl = fork;
    if !generics_for_write_impl.params.iter().any(|p| match p {
        syn::GenericParam::Lifetime(lt) => lt.lifetime.ident == "vfs",
        syn::GenericParam::Const(_) | syn::GenericParam::Type(_) => false,
    }) {
        generics_for_write_impl
            .params
            .insert(0, parse_quote! { 'vfs });
    }
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
            v.bounds =
                parse_quote! { ::dir_structure::traits::vfs::WriteSupportingVfs<'vfs> + 'vfs };
        } else {
            v.bounds
                .push(parse_quote! { ::dir_structure::traits::vfs::WriteSupportingVfs<'vfs> });
            v.bounds.push(parse_quote! { 'vfs });
        }
    } else {
        generics_for_write_impl.params.push(
            parse_quote! { Vfs: ::dir_structure::traits::vfs::WriteSupportingVfs<'vfs> + 'vfs },
        );
    }

    let (write_impl_generics, _, _) = generics_for_write_impl.split_for_impl();

    let (impl_generics, ty_generics, where_clause) = st.generics.split_for_impl();

    let mut field_read_impls = Vec::new();
    let mut field_write_impls = Vec::new();

    for field in &st.fields {
        let DirStructureForField {
            read_code,
            write_code,
        } = expand_dir_structure_for_field(
            (&impl_generics, name, &ty_generics, where_clause),
            &path_param_name,
            &vfs_param_name,
            field,
        )?;
        field_read_impls.push(read_code);
        field_write_impls.push(write_code);
    }

    #[cfg_attr(not(feature = "resolve-path"), expect(unused_mut))]
    let mut expanded = quote! {
        #[automatically_derived]
        impl #read_impl_generics ::dir_structure::traits::sync::ReadFrom<'vfs, Vfs> for #name #ty_generics #where_clause {
            fn read_from(#path_param_name: &Vfs::Path, #vfs_param_name: ::std::pin::Pin<&'vfs Vfs>) -> ::dir_structure::error::Result<Self, <Vfs::Path as ::dir_structure::traits::vfs::PathType>::OwnedPath>
            where
                Self: Sized,
            {
                Ok(Self {
                    #(#field_read_impls,)*
                })
            }
        }
        #[automatically_derived]
        impl #write_impl_generics ::dir_structure::traits::sync::WriteTo<'vfs, Vfs> for #name #ty_generics #where_clause {
            fn write_to(&self, #path_param_name: &Vfs::Path, #vfs_param_name: ::std::pin::Pin<&'vfs Vfs>) -> ::dir_structure::error::Result<(), <Vfs::Path as ::dir_structure::traits::vfs::PathType>::OwnedPath> {
                #(#field_write_impls)*
                Ok(())
            }
        }
        #[automatically_derived]
        impl #impl_generics ::dir_structure::traits::sync::DirStructure for #name #ty_generics #where_clause {}
    };

    Ok(expanded)
}
