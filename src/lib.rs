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
    let ident = &ast.ident;

    let generated = match &ast.data {
        Data::Union(union_data) => parse_union(union_data),
        Data::Enum(enum_data) => parse_enum(enum_data),
        Data::Struct(struct_data) => parse_struct(struct_data),
    };

    quote! {
        impl std::fmt::Display for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #generated
            }
        }
    }
    .into()
}

fn parse_union(_union_data: &DataUnion) -> TokenStream2 {
    quote! {}
}

fn parse_enum(enum_data: &DataEnum) -> TokenStream2 {
fn parse_struct(_struct_data: &DataStruct) -> TokenStream2 {
    quote! {}
}

