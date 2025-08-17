use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DataEnum, DataStruct, DataUnion,
    DeriveInput, Error, Fields, FieldsNamed, FieldsUnnamed, LitStr, Meta,
};

#[proc_macro_derive(Display, attributes(display))]
pub fn display(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    impl_display(&ast)
}

fn impl_display(ast: &syn::DeriveInput) -> TokenStream {
}
