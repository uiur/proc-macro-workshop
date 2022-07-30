use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, Attribute, DeriveInput, Field, GenericParam, Generics, Ident,
    Path, Type,
};

type ParsedField = (syn::Ident, syn::Type, Option<String>);

fn parse_debug_attribute(attr: &Attribute) -> Result<Option<String>, Box<dyn std::error::Error>> {
    if !attr.path.is_ident("debug") {
        return Ok(None);
    }

    if let syn::Meta::NameValue(syn::MetaNameValue { lit, .. }) = attr.parse_meta()? {
        if let syn::Lit::Str(s) = lit {
            return Ok(Some(s.value()));
        }
    }

    Ok(None)
}

fn parse_field(field: &Field) -> Option<ParsedField> {
    let values = field
        .attrs
        .iter()
        .flat_map(parse_debug_attribute)
        .flatten()
        .collect::<Vec<_>>();
    let custom_format = values.first().map(|s| s.clone());

    field
        .ident
        .clone()
        .map(|ident| (ident, field.ty.clone(), custom_format))
}

enum ParsedType {
    GenericType(String),
    Other,
}

fn parse_path(path: &Path) -> Option<(&Ident, &Type)> {
    let ident = path.segments.first().map(|segment| &segment.ident);
    let mut inner_ty: Option<&Type> = None;

    let path_segment = path.segments.first();
    if let Some(syn::PathSegment {
        arguments: syn::PathArguments::AngleBracketed(t),
        ..
    }) = path_segment
    {
        if let Some(syn::GenericArgument::Type(ty)) = t.args.first() {
            inner_ty = Some(ty);
        }
    }

    match (ident, inner_ty) {
        (Some(ident), Some(inner_ty)) => Some((ident, inner_ty)),
        _ => None,
    }
}

fn parse_type(ty: &Type) -> ParsedType {
    match ty {
        Type::Path(syn::TypePath { path, .. }) => {
            if let Some((ident, _)) = parse_path(path) {
                ParsedType::GenericType(ident.to_string())
            } else {
                ParsedType::Other
            }
        }
        _ => ParsedType::Other,
    }
}

fn extract_type_params_from_type(ty: &Type) -> Vec<String> {
    match ty {
        Type::Path(syn::TypePath { path, .. }) => path
            .segments
            .iter()
            .flat_map(|path_segment| match &path_segment.arguments {
                syn::PathArguments::AngleBracketed(t) => {
                    let type_params: Vec<_> = t
                        .args
                        .iter()
                        .flat_map(|arg| match arg {
                            syn::GenericArgument::Type(t) => extract_type_params_from_type(t),
                            _ => vec![],
                        })
                        .collect();

                    type_params
                }
                _ => {
                    vec![path_segment.ident.to_string()]
                }
            })
            .collect::<Vec<_>>(),
        _ => vec![],
    }
}

fn add_trait_bounds(mut generics: Generics, parsed_fields: &[ParsedField]) -> Generics {
    // struct Field<T>
    // T -> T: std::fmt::Debug
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            let type_param_name = type_param.ident.to_string();
            let is_phantom_data = parsed_fields
                .iter()
                .filter(|(_, ty, _)| {
                    let type_params = extract_type_params_from_type(ty);
                    type_params.contains(&type_param_name)
                })
                .all(|(_, ty, _)| match parse_type(ty) {
                    ParsedType::GenericType(ty_name) => ty_name == "PhantomData",
                    _ => false,
                });

            if is_phantom_data {
                // where PhantomData<T>: std::fmt::Debug
                let type_param_ident = &type_param.ident;
                generics.where_clause =
                    parse_quote!(where PhantomData<#type_param_ident>: std::fmt::Debug);
            } else {
                type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }
    generics
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;

    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = &input.data
    {
        let all_parsed_fields: Vec<_> = named
            .iter()
            .cloned()
            .filter_map(|field| parse_field(&field))
            .collect();

        let generics = add_trait_bounds(input.generics, &all_parsed_fields);
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let (parsed_fields_with_custom_format, parsed_fields): (Vec<_>, Vec<_>) = all_parsed_fields
            .iter()
            .cloned()
            .partition(|parsed| parsed.2.is_some());

        let (field_idents, (_, _)): (Vec<_>, (Vec<_>, Vec<_>)) = parsed_fields
            .iter()
            .cloned()
            .map(|field| (field.0, (field.1, field.2)))
            .unzip();

        let (field_idents_with_custom_format, (_, field_custom_formats)): (
            Vec<_>,
            (Vec<_>, Vec<_>),
        ) = parsed_fields_with_custom_format
            .iter()
            .cloned()
            .map(|field| (field.0, (field.1, field.2)))
            .unzip();

        let expanded = quote! {
            impl #impl_generics std::fmt::Debug for #ident #ty_generics #where_clause {
                fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                    fmt.debug_struct(stringify!(#ident))
                        #(
                            .field(stringify!(#field_idents), &self.#field_idents)
                        )*
                        #(
                            .field(stringify!(#field_idents_with_custom_format), &format_args!(#field_custom_formats, self.#field_idents_with_custom_format))
                        )*
                        .finish()
                }
            }
        };
        // println!("{}", expanded.to_string());
        expanded.into()
    } else {
        TokenStream::new()
    }
}
