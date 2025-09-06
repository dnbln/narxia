use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use syn::Expr;
use syn::Field;
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

mod read_from_async;
mod write_to_async_ref;

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
    async_write_ref_future: &mut FutureEnum,
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

    let actual_path_expr_move = match &path {
        PathSpec::Path(p) => quote! { #path_param_name.join(#p) },
        PathSpec::SelfPath => quote! { #path_param_name.clone() },
    };
    let actual_field_ty_perform = with_newtype.as_ref().unwrap_or(field_ty);

    let async_read_bound = read_from_async::expand_dir_structure_for_field(
        (impl_generics, ty_name, ty_generics, where_clause),
        path_param_name,
        vfs_param_name,
        field,
        with_newtype.as_ref(),
        self_path,
        &path,
        async_read_future,
    )?;

    let async_write_ref_bound = write_to_async_ref::expand_dir_structure_for_field(
        (impl_generics, ty_name, ty_generics, where_clause),
        path_param_name,
        vfs_param_name,
        field,
        with_newtype.as_ref(),
        self_path,
        &path,
        async_write_ref_future,
    )?;

    let async_write_bound = if self_path {
        // self_path does not need to write anything
        None
    } else {
        quote! {
            let __translated_path = #actual_path_expr_move;
            ::dir_structure::WriteToAsyncRef<'vfs, Vfs>::write_to_async_ref(&self.#field_name, __translated_path, #vfs_param_name).await?;
        };

        Some(vec![])
    };

    Ok(DirStructureForField {
        async_read_bound,
        async_write_bound,
        async_write_ref_bound,
    })
}

pub fn expand_dir_structure_async(st: ItemStruct) -> syn::Result<TokenStream> {
    let name = &st.ident;
    let path_param_name = format_ident!("__dir_structure_path");
    let vfs_param_name = format_ident!("__vfs");

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
            v.bounds = parse_quote! { ::dir_structure::traits::async_vfs::VfsAsync + 'static };
        } else {
            v.bounds.push(parse_quote! { ::dir_structure::traits::async_vfs::VfsAsync });
            v.bounds.push(parse_quote! { 'static });
        }
    } else {
        read_async_impl_generics
            .params
            .push(parse_quote! { Vfs: ::dir_structure::traits::async_vfs::VfsAsync + 'static });
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
            v.bounds = parse_quote! { ::dir_structure::traits::async_vfs::WriteSupportingVfsAsync + 'static };
        } else {
            v.bounds
                .push(parse_quote! { ::dir_structure::traits::async_vfs::WriteSupportingVfsAsync });
            v.bounds.push(parse_quote! { 'static });
        }
    } else {
        write_async_impl_generics
            .params
            .push(parse_quote! { Vfs: ::dir_structure::traits::async_vfs::WriteSupportingVfsAsync + 'static });
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

    let mut write_async_ref = FutureEnum {
        name: format_ident!("__{}WriteAsyncRefFuture", name),
        proj_name: format_ident!("__{}WriteAsyncRefFutureProj", name),
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
            &mut write_async_ref,
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

    let where_clause_read_from_async =
        merge_where_clause(where_clause.cloned(), field_async_read_bounds);
    let where_clause_write_to_async_ref =
        merge_where_clause(where_clause.cloned(), field_async_write_ref_bounds);

    let read_async_impl_enum = read_from_async::future_impl_enum(
        &st,
        &path_param_name,
        &vfs_param_name,
        &ty_generics,
        &read_async,
    )?;

    let (write_async_ref_impl_enum, vfs_lifetime_header) = write_to_async_ref::future_impl_enum(
        &st,
        &vfs_param_name,
        &path_param_name,
        &ty_generics,
        &write_async_ref,
    )?;

    let read_async_ty_name = &read_async.name;
    let write_async_ty_name = &write_async_ref.name;

    expanded.extend(quote! {
        #read_async_impl_enum

        impl #read_async_impl_generics ::dir_structure::traits::asy::ReadFromAsync<'vfs, Vfs> for #name #ty_generics #where_clause_read_from_async {
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

        impl #write_async_impl_generics ::dir_structure::traits::asy::WriteToAsyncRef<'vfs, Vfs> for #name #ty_generics #where_clause_write_to_async_ref {
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
