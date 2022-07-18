use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());
    let mut field_key: Vec<Ident> = vec![];
    let mut field_type: Vec<Type> = vec![];

    let mut optional_field_key: Vec<Ident> = vec![];
    let mut optional_field_type: Vec<Type> = vec![];

    if let syn::Data::Struct(s) = input.data {
        if let syn::Fields::Named(n) = s.fields {
            for field in n.named.iter() {
                if let Some(field_ident) = field.ident.clone() {
                    let mut is_option = false;
                    if let Type::Path(t) = &field.ty {
                        let result = t
                            .path
                            .segments
                            .iter()
                            .find(|segment| segment.ident == "Option");

                        if let Some(segment) = result {
                            if let syn::PathArguments::AngleBracketed(t) = &segment.arguments {
                                if let Some(syn::GenericArgument::Type(ty)) = t.args.first() {
                                    is_option = true;
                                    optional_field_key.push(field_ident.clone());
                                    optional_field_type.push(ty.clone());
                                }
                            }
                        }
                    }

                    if !is_option {
                        field_key.push(field_ident.clone());
                        field_type.push(field.ty.clone());
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

            fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                #(
                    if self.#field_key.is_none() {
                        return Err(format!("{} is missing", stringify!(#field_key)).into());
                    }
                )*
                Ok(#name {
                    #(#field_key: self.#field_key.clone().unwrap(),)*
                    #(#optional_field_key: self.#optional_field_key.clone(),)*
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
                }
            }
        }
    };

    TokenStream::from(expanded)
}
