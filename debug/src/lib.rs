use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, token::Where, Attribute, DeriveInput, Field, GenericParam,
    Generics, Ident, LitStr, MetaNameValue, Path, Type, TypeParam, WherePredicate,
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
    GenericType(String, Type),
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
            if let Some((ident, ty)) = parse_path(path) {
                ParsedType::GenericType(ident.to_string(), ty.clone())
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

fn is_associated_type(ty: &Type, type_param: &TypeParam) -> bool {
    match ty {
        Type::Path(syn::TypePath { path, .. }) => {
            if let Some(first_segment) = path.segments.first() {
                first_segment.ident == type_param.ident && path.segments.len() >= 2
            } else {
                false
            }
        }
        _ => false,
    }
}

fn add_trait_bounds(
    mut generics: Generics,
    parsed_fields: &[ParsedField],
    bounds: &Vec<WherePredicate>,
) -> Generics {
    // struct Field<T>
    // T -> T: std::fmt::Debug
    let mut associated_types: Vec<Type> = vec![];
    let mut phantom_data_idents: Vec<&Ident> = vec![];

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
                    ParsedType::GenericType(ty_name, _) => ty_name == "PhantomData",
                    _ => false,
                });

            if is_phantom_data {
                // where PhantomData<T>: std::fmt::Debug
                let type_param_ident = &type_param.ident;
                phantom_data_idents.push(type_param_ident)
            } else {
                let found_associated_types = parsed_fields
                    .iter()
                    .cloned()
                    .filter(|(_, ty, _)| match parse_type(ty) {
                        ParsedType::GenericType(_, inner_ty) => {
                            is_associated_type(&inner_ty, type_param)
                        }
                        _ => is_associated_type(ty, type_param),
                    })
                    .map(|(_, ty, _)| match parse_type(&ty) {
                        ParsedType::GenericType(_, inner_ty) => inner_ty,
                        _ => ty,
                    })
                    .collect::<Vec<_>>();

                if let Some(associated_type) = found_associated_types.first() {
                    associated_types.push(associated_type.clone());
                } else if bounds.is_empty() {
                    type_param.bounds.push(parse_quote!(std::fmt::Debug));
                }
            }
        }
    }

    generics.where_clause = parse_quote!(
        where
            #(
                PhantomData<#phantom_data_idents>: std::fmt::Debug,
            )*
            #(
                #associated_types: std::fmt::Debug,
            )*
            #(
                #bounds,
            )*

    );

    generics
}

fn extract_custom_bounds(input: &DeriveInput) -> Vec<WherePredicate> {
    let bound_strs: Vec<_> = input
        .attrs
        .iter()
        .filter_map(|attr| {
            if attr.path.is_ident("debug") {
                match attr.parse_meta().ok()? {
                    syn::Meta::List(syn::MetaList { nested, .. }) => {
                        nested.iter().find_map(|meta| match meta {
                            syn::NestedMeta::Meta(syn::Meta::NameValue(MetaNameValue {
                                path,
                                lit: syn::Lit::Str(lit_str),
                                ..
                            })) => {
                                if path.is_ident("bound") {
                                    Some(lit_str.value())
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        })
                    }
                    _ => None,
                }
            } else {
                None
            }
        })
        .collect();

    let predicates: Vec<_> = bound_strs
        .iter()
        .filter_map(|bound_str| syn::parse_str::<WherePredicate>(bound_str).ok())
        .collect();

    predicates
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    let attr_bounds: Vec<WherePredicate> = extract_custom_bounds(&input);

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

        let generics = add_trait_bounds(input.generics, &all_parsed_fields, &attr_bounds);
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
        eprintln!("{}", expanded.to_string());
        expanded.into()
    } else {
        TokenStream::new()
    }
}
