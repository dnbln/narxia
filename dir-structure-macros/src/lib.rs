use syn::ItemStruct;
use syn::punctuated::Punctuated;

mod dir_structure;
#[cfg(feature = "async")]
mod dir_structure_async;
mod dir_structure_core;

#[proc_macro_derive(DirStructure, attributes(dir_structure))]
pub fn derive_dir_structure(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = syn::parse_macro_input!(item as ItemStruct);

    dir_structure::expand_dir_structure(item)
        // .map(|ts| {
        //     eprintln!("Expanded DirStructure for {}", ts);
        //     ts
        // })
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[cfg(feature = "async")]
#[proc_macro_derive(DirStructureAsync, attributes(dir_structure))]
pub fn derive_dir_structure_async(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = syn::parse_macro_input!(item as ItemStruct);

    dir_structure_async::expand_dir_structure_async(item)
        // .map(|ts| {
        //     eprintln!("Expanded DirStructureAsync for {}", ts);
        //     ts
        // })
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[cfg(feature = "resolve-path")]
mod resolve_path;

#[cfg(feature = "resolve-path")]
#[proc_macro]
pub fn resolve_path(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    resolve_path::resolve_path(input)
}

#[cfg(feature = "resolve-path")]
#[proc_macro_derive(HasField, attributes(dir_structure))]
pub fn derive_has_field(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = syn::parse_macro_input!(item as ItemStruct);

    resolve_path::expand_has_field_impls(item)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[cfg(feature = "resolve-path")]
#[proc_macro]
pub fn load_path(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    resolve_path::load_path(input)
}

#[cfg(feature = "resolve-path")]
#[proc_macro]
pub fn __resolve_max_len(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // This macro is used to get the maximum length of a field name for the `HasField` trait.
    // It is used in the `resolve_path` macro to ensure that field names do not exceed this length.
    let max_len = resolve_path::MAX_LEN;
    let output = quote::quote! { #max_len };
    output.into()
}

#[cfg(feature = "include_dir_vfs")]
#[proc_macro]
pub fn include_dir_patched(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::LitStr);
    quote::quote! {::dir_structure::include_dir::include_dir!(#input)}.into()
}

fn merge_where_clause(
    where_clause: Option<syn::WhereClause>,
    additional_bounds: Vec<syn::WherePredicate>,
) -> Option<syn::WhereClause> {
    if let Some(mut where_clause) = where_clause {
        where_clause.predicates.extend(additional_bounds);
        Some(where_clause)
    } else {
        let mut where_clause = syn::WhereClause {
            where_token: <syn::Token![where]>::default(),
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
