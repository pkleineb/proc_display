use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DataEnum, DataStruct, DataUnion,
    DeriveInput, Error, Fields, FieldsNamed, FieldsUnnamed, LitStr, Meta,
};

macro_rules! enforce_correct_display_use {
    ($fields:expr) => {
        for field in $fields {
            for attr in &field.attrs {
                if attr.path().is_ident("display") {
                    let error = Error::new(
                        attr.span(),
                        "The #[display] attribute cannot be used on struct fields",
                    );
                    return error.to_compile_error();
                }
            }
        }
    };
}

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
    let branches: TokenStream2 = enum_data
        .variants
        .iter()
        .map(|variant| {
            let variant_name = &variant.ident;
            let message = variant
                .attrs
                .iter()
                .find(|attr| attr.path().is_ident("display"))
                .and_then(get_message_from_attribute)
                .unwrap_or("".to_string());

            match &variant.fields {
                Fields::Unit => {
                    quote! {
                        Self::#variant_name => write!(f, "{}", #message),
                    }
                }
                Fields::Named(fields) => {
                    quote! {
                    }
                }
                Fields::Unnamed(fields) => parse_unnamed_fields(fields),
            }
        })
        .collect();

    quote! {
        match self {
            #branches
        }
    }
}

fn parse_struct(_struct_data: &DataStruct) -> TokenStream2 {
    quote! {}
}

fn parse_named_fields(fields: &FieldsNamed, message: &str) -> TokenStream2 {
    enforce_correct_display_use!(&fields.named);
fn parse_unnamed_fields(fields: &FieldsUnnamed) -> TokenStream2 {
    enforce_correct_display_use!(&fields.unnamed);
    quote! {}
}

fn get_message_from_attribute(attr: &Attribute) -> Option<String> {
    if let Meta::List(meta_list) = &attr.meta {
        if let Ok(message) = meta_list.parse_args::<LitStr>() {
            return Some(message.value());
        }
    }

    None
}

}
