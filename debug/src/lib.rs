use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote, Attribute, DeriveInput, Field, GenericParam, Generics};

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

fn parse_field(field: &Field) -> Option<(syn::Ident, syn::Type, Option<String>)> {
    let values = field
        .attrs
        .iter()
        .map(parse_debug_attribute)
        .flatten()
        .flatten()
        .collect::<Vec<_>>();
    let custom_format = values.first().map(|s| s.clone());

    field
        .ident
        .clone()
        .map(|ident| (ident, field.ty.clone(), custom_format))
}

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    let generics = add_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = &input.data
    {
        let (parsed_fields_with_custom_format, parsed_fields): (Vec<_>, Vec<_>) = named
            .iter()
            .cloned()
            .filter_map(|field| parse_field(&field))
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
        expanded.into()
    } else {
        TokenStream::new()
    }
}
