use syn::Field;
use syn::Token;
use syn::Type;

pub enum PathSpec {
    Path(String),
    SelfPath,
}

pub struct DirStructureCoreInfo {
    /// Name of field.
    pub name: String,
    /// Type of field.
    pub ty: Type,
    /// The type (or newtype) that implements `ReadFrom` and `WriteTo`,
    /// which we will use in case of `with_newtype`.
    pub newtype_ty: Option<Type>,

    pub self_path: bool,
    pub path: PathSpec,
}

pub fn compile_attrs(field: &Field) -> syn::Result<DirStructureCoreInfo> {
    let name = field.ident.as_ref().unwrap().to_string();
    let mut path = PathSpec::Path(name.clone());
    let mut self_path = name == "self_path";
    let mut newtype_ty = None::<Type>;

    for attr in field
        .attrs
        .iter()
        .filter(|attr| attr.meta.path().is_ident("dir_structure"))
    {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("path") {
                let _eq = meta.input.parse::<Token![=]>()?;
                if meta.input.peek(syn::LitStr) {
                    let s = meta.input.parse::<syn::LitStr>()?;
                    path = PathSpec::Path(s.value());
                } else if meta.input.peek(Token![self]) {
                    let _self = meta.input.parse::<Token![self]>()?;
                    path = PathSpec::SelfPath;
                } else {
                    return Err(syn::Error::new_spanned(
                        meta.path,
                        "Expected a string literal or `self`",
                    ));
                }
            } else if meta.path.is_ident("self_path") {
                self_path = true;
            } else if meta.path.is_ident("with_newtype") {
                let _eq = meta.input.parse::<Token![=]>()?;
                let ty = meta.input.parse::<Type>()?;
                newtype_ty = Some(ty);
            } else {
                return Err(syn::Error::new_spanned(
                    meta.path,
                    "Unknown attribute for dir_structure",
                ));
            }

            Ok(())
        })?;
    }

    Ok(DirStructureCoreInfo {
        name,
        ty: field.ty.clone(),
        newtype_ty,
        self_path,
        path,
    })
}
