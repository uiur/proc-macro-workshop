use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, FieldsNamed};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = &input.data
    {
        let parsed_fields = named
            .iter()
            .cloned()
            .filter_map(|field| field.ident.map(|ident| (ident, field.ty)))
            .collect::<Vec<_>>();

        let (field_idents, field_tys): (Vec<_>, Vec<_>) = parsed_fields.into_iter().unzip();

        println!("{:#?}", input);
        let expanded = quote! {
            impl std::fmt::Debug for #ident {
                fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                    fmt.debug_struct(stringify!(#ident))
                        #(
                            .field(stringify!(#field_idents), &self.#field_idents)
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
