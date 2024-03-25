use quote::quote;
use syn::spanned::Spanned;
use syn::{Data, DataStruct, DeriveInput};

pub fn derive_hir_struct_id_check(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);

    match &input.data {
        Data::Struct(s) => derive_hir_struct_id_check_for_struct(&input, s),
        Data::Enum(e) => derive_hir_struct_id_check_for_enum(&input, e),
        _ => {
            panic!("Only struct and enum are supported");
        }
    }
}

fn derive_hir_struct_id_check_for_struct(
    input: &DeriveInput,
    s: &DataStruct,
) -> proc_macro::TokenStream {
    let name = &input.ident;
    let name_str = name.to_string();
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let mut fields = Vec::new();
    if let syn::Fields::Named(fields_named) = &s.fields {
        for field in &fields_named.named {
            let field_name = field.ident.as_ref().unwrap();
            let field_ty = &field.ty;
            fields.push((field_name, field_ty));
        }
    }
    let mut checks = Vec::new();
    for (field_name, field_ty) in fields {
        checks.push(quote! {
            <#field_ty as HirStructIdCheckTest>::check_hir_id(&self.#field_name, #name_str)?;
        });
    }
    let expanded = quote! {
        impl #impl_generics HirStructIdCheckTest for #name #ty_generics #where_clause {
            fn check_hir_id(&self, _name: &'static str) -> Result<(), HirIdUninitialized> {
                #(#checks)*

                Ok(())
            }
        }
    };
    expanded.into()
}

fn derive_hir_struct_id_check_for_enum(
    input: &DeriveInput,
    e: &syn::DataEnum,
) -> proc_macro::TokenStream {
    let name = &input.ident;
    let name_str = name.to_string();
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut check_code = quote!{};

    for variant in &e.variants {
        let variant_name = &variant.ident;
        let mut sel = quote! {
            Self::#variant_name
        };
        let mut checks = Vec::new();
        match &variant.fields {
            syn::Fields::Unit => {}
            syn::Fields::Unnamed(fields_unnamed) => {
                let mut fields = Vec::new();
                for (i, field) in fields_unnamed.unnamed.iter().enumerate() {
                    let field_ty = &field.ty;
                    let name = syn::Ident::new(&format!("_{}", i), field_ty.span());
                    checks.push(quote! {
                        <#field_ty as HirStructIdCheckTest>::check_hir_id(#name, #name_str)?;
                    });
                    fields.push(name);
                }

                sel.extend(quote! {
                    (
                        #(#fields),*
                    )
                })
            }
            syn::Fields::Named(fields_named) => {
                let mut fields = Vec::new();
                for field in &fields_named.named {
                    let field_name = field.ident.as_ref().unwrap();
                    let field_ty = &field.ty;
                    checks.push(quote! {
                        <#field_ty as HirStructIdCheckTest>::check_hir_id(#field_name, #name_str)?;
                    });
                    fields.push(field_name);
                }

                sel.extend(quote! {
                    {
                        #(#fields),*
                    }
                })
            }
        }

        check_code.extend(quote! {
            #sel => {
                #(#checks)*

                Ok(())
            },
        })
    }
    let expanded = quote! {
        impl #impl_generics HirStructIdCheckTest for #name #ty_generics #where_clause {
            fn check_hir_id(&self, _name: &'static str) -> Result<(), HirIdUninitialized> {
                match self {
                    #check_code
                }
            }
        }
    };
    expanded.into()
}
