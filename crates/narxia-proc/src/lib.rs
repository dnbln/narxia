#![allow(dead_code, unused_imports, unused_variables)] // FIXME: fix

mod _syn;
mod _hir;

#[proc_macro_derive(DeriveT, attributes(T))]
pub fn derive_t(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    _syn::derive_t(item)
}

#[proc_macro_attribute]
pub fn parse_fn(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    _syn::parse_fn(attr, input)
}

#[proc_macro]
pub fn parse_fn_decl(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    _syn::parse_fn_decl(tokens)
}

#[proc_macro]
pub fn syntree_node(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    _syn::syntree_node(input)
}

#[proc_macro]
pub fn syntree_enum(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    _syn::syntree_enum(input)
}

#[proc_macro_derive(HirStructIdCheck, attributes(hir))]
pub fn derive_hir_struct_id_check(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    _hir::derive_hir_struct_id_check(item)
}