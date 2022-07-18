use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = Ident::new("CommandBuilder", Span::call_site());
    let mut field_key: Vec<Ident> = vec![];
    let mut field_type: Vec<Type> = vec![];

    match input.data {
        syn::Data::Struct(s) => match s.fields {
            syn::Fields::Named(n) => {
                for field in n.named.iter() {
                    if let Some(field_ident) = field.ident.clone() {
                        field_key.push(field_ident);
                        field_type.push(field.ty.clone());
                    }
                }
            }
            syn::Fields::Unnamed(_) => todo!(),
            syn::Fields::Unit => todo!(),
        },
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
    }

    let expanded = quote! {
        struct #builder_name {
            #(
                #field_key: Option<#field_type>,
            )*
        }
        impl #builder_name {
            #(
                fn #field_key(&mut self, #field_key: #field_type) -> &mut #builder_name {
                    self.#field_key = Some(#field_key);
                    self
                }
            )*
        }

        impl #name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(
                        #field_key: None,
                    )*
                }
            }
        }
    };

    TokenStream::from(expanded)
}
