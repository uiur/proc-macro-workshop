use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{parse::Parse, parse_macro_input, DeriveInput, Path, Type};

#[derive(Debug)]
enum ParsedType {
    Option(Type),
    Vec(Type),
    Other(Type),
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
    if let Type::Path(syn::TypePath { path, .. }) = ty {
        if let Some((ident, ty)) = parse_path(path) {
            match (ident.to_string().as_ref(), ty) {
                ("Option", inner_ty) => {
                    return ParsedType::Option(inner_ty.clone());
                }
                ("Vec", inner_ty) => {
                    return ParsedType::Vec(inner_ty.clone());
                }
                _ => (),
            }
        }
    }
    ParsedType::Other(ty.clone())
}

#[derive(Debug)]
enum ParsedAttribute {
    Each(String),
}

fn parse_attribute(attr: &syn::Attribute) -> Result<Option<ParsedAttribute>, syn::Error> {
    let meta = attr.parse_meta()?;
    let ident = meta
        .clone()
        .path()
        .get_ident()
        .and_then(|ident| Some(ident.to_string()))
        .unwrap_or_default();
    if ident != "builder" {
        return Ok(None);
    }

    if let syn::Meta::List(meta_list) = &meta {
        if let Some(nested_meta) = meta_list.nested.first() {
            if let syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) = nested_meta {
                let ident = name_value.path.get_ident();

                if let Some(ident) = ident {
                    if ident == "each" {
                        if let syn::Lit::Str(lit_str) = &name_value.lit {
                            let value = lit_str.value();
                            return Ok(Some(ParsedAttribute::Each(value)));
                        }
                    } else {
                        return Err(syn::Error::new(
                            meta.span(),
                            "expected `builder(each = \"...\")`",
                        ));
                    }
                }
            }
        }
    }

    return Ok(None);
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);
    let mut field_key: Vec<Ident> = vec![];
    let mut field_type: Vec<Type> = vec![];

    let mut optional_field_key: Vec<Ident> = vec![];
    let mut optional_field_type: Vec<Type> = vec![];

    let mut vec_field_key: Vec<Ident> = vec![];
    let mut vec_field_each_key: Vec<Ident> = vec![];
    let mut vec_field_type: Vec<Type> = vec![];

    if let syn::Data::Struct(s) = input.data {
        if let syn::Fields::Named(n) = s.fields {
            for field in n.named.iter() {
                if let Some(field_ident) = field.ident.clone() {
                    let parsed_attributes_result = field
                        .attrs
                        .iter()
                        .map(parse_attribute)
                        .collect::<Result<Vec<_>, _>>();

                    match parsed_attributes_result {
                        Ok(optional_parsed_attributes) => {
                            let optional_parsed_attribute =
                                optional_parsed_attributes.iter().flatten().next();
                            let parsed_type = parse_type(&field.ty);
                            match (parsed_type, optional_parsed_attribute) {
                                (ParsedType::Vec(inner_ty), Some(ParsedAttribute::Each(value))) => {
                                    vec_field_key.push(field_ident.clone());
                                    vec_field_each_key
                                        .push(Ident::new(value.as_ref(), Span::call_site()));
                                    vec_field_type.push(inner_ty.clone());
                                }
                                (ParsedType::Option(ty), _) => {
                                    optional_field_key.push(field_ident.clone());
                                    optional_field_type.push(ty.clone());
                                }
                                (_, _) => {
                                    field_key.push(field_ident.clone());
                                    field_type.push(field.ty.clone());
                                }
                            }
                        }
                        Err(e) => return e.into_compile_error().into(),
                    }
                }
            }
        }
    }

    let expanded = quote! {
        struct #builder_name {
            #(
                #field_key: Option<#field_type>,
            )*
            #(
                #optional_field_key: Option<#optional_field_type>,
            )*
            #(
                #vec_field_key: Vec<#vec_field_type>,
            )*
        }

        impl #builder_name {
            #(
                fn #field_key(&mut self, #field_key: #field_type) -> &mut #builder_name {
                    self.#field_key = Some(#field_key);
                    self
                }
            )*
            #(
                fn #optional_field_key(&mut self, #optional_field_key: #optional_field_type) -> &mut #builder_name {
                    self.#optional_field_key = Some(#optional_field_key);
                    self
                }
            )*
            #(
                fn #vec_field_each_key(&mut self, #vec_field_each_key: #vec_field_type) -> &mut #builder_name {
                    self.#vec_field_key.push(#vec_field_each_key);
                    self
                }
            )*


            fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                #(
                    if self.#field_key.is_none() {
                        return Err(format!("{} is missing", stringify!(#field_key)).into());
                    }
                )*
                Ok(#name {
                    #(#field_key: self.#field_key.clone().unwrap(),)*
                    #(#optional_field_key: self.#optional_field_key.clone(),)*
                    #(#vec_field_key: self.#vec_field_key.clone(),)*
                })
            }
        }

        impl #name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(
                        #field_key: None,
                    )*

                    #(
                        #optional_field_key: None,
                    )*

                    #(
                        #vec_field_key: vec![],
                    )*
                }
            }
        }
    };

    TokenStream::from(expanded)
}
