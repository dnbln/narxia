use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{Field, ItemStruct, Token, Type};

#[proc_macro_derive(DirStructure, attributes(dir_structure))]
pub fn derive_dir_structure(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = syn::parse_macro_input!(item as ItemStruct);

    expand_dir_structure(item)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

struct DirStructureForField {
    read_code: TokenStream,
    write_code: TokenStream,
}

fn expand_dir_structure_for_field(
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

    let mut path = None::<String>;
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
                let s = meta.input.parse::<syn::LitStr>()?;
                path = Some(s.value());
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

    let actual_path = path.unwrap_or_else(|| field_name.to_string());
    let actual_field_ty_perform = with_newtype.as_ref().unwrap_or(field_ty);
    let read_code = if self_path {
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
            let __translated__path = #path_param_name.join(#actual_path);
            let #value_name = <#actual_field_ty_perform as ::dir_structure::ReadFrom>::read_from(&__translated__path)?;
            #end_expr
        }}
    };

    let write_code = if self_path {
        quote! {}
    } else {
        let writer = match &with_newtype {
            Some(nt) => {
                quote! { &<#nt as ::dir_structure::FromRefForWriter<'_>>::from_ref_for_writer(&self.#field_name) }
            }
            None => quote! { &self.#field_name },
        };
        quote! {
            let __translated_path = #path_param_name.join(#actual_path);
            ::dir_structure::WriteTo::write_to(#writer, &__translated_path)?;
        }
    };

    Ok(DirStructureForField {
        read_code: quote! {
            #field_name: #read_code
        },
        write_code,
    })
}

fn expand_dir_structure(st: ItemStruct) -> syn::Result<TokenStream> {
    let name = &st.ident;
    let path_param_name = format_ident!("__DIR_STRUCTURE_PATH");
    let (impl_generics, ty_generics, where_clause) = st.generics.split_for_impl();

    let mut field_read_impls = Vec::new();
    let mut field_write_impls = Vec::new();

    for field in &st.fields {
        let DirStructureForField {
            read_code,
            write_code,
        } = expand_dir_structure_for_field(&path_param_name, field)?;
        field_read_impls.push(read_code);
        field_write_impls.push(write_code);
    }

    let expanded = quote! {
        impl #impl_generics ::dir_structure::ReadFrom for #name #ty_generics #where_clause {
            fn read_from(#path_param_name: &std::path::Path) -> ::dir_structure::Result<Self>
            where
                Self: Sized,
            {
                Ok(Self {
                    #(#field_read_impls,)*
                })
            }
        }
        impl #impl_generics ::dir_structure::WriteTo for #name #ty_generics #where_clause {
            fn write_to(&self, #path_param_name: &std::path::Path) -> ::dir_structure::Result<()> {
                #(#field_write_impls)*
                Ok(())
            }
        }
        impl #impl_generics ::dir_structure::DirStructure for #name #ty_generics #where_clause {}
    };

    Ok(expanded)
}
