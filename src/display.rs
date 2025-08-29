use crate::parser;
use crate::validator;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, TokenStreamExt};
use syn::{spanned::Spanned, Data, DataEnum, Error, Fields, Ident, Index};

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

/// macro implementation handling all DataTypes the derive macro can be used on
pub fn impl_display(ast: &syn::DeriveInput) -> TokenStream {
    let ident = &ast.ident;

    let (message, attr_span) = parser::get_message_from_attrs(&ast.attrs, ast.span(), "");

    let generated = match &ast.data {
        Data::Union(_) => Ok(generate_write_call(
            &Fields::Unit,
            message.to_string(),
            quote! {},
        )),
        Data::Enum(enum_data) => parse_enum(enum_data, message),
        Data::Struct(struct_data) => parse_struct(message, ident, &struct_data.fields, attr_span),
    };

    match generated {
        Ok(generated) => quote! {
            impl std::fmt::Display for #ident {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    #generated
                }
            }
        }
        .into(),
        Err(error_token_stream) => error_token_stream.to_compile_error().into(),
    }
}

/// parses an enum into a `TokenStream` implementing the Display trait for all enum variants
fn parse_enum(enum_data: &DataEnum, default: String) -> Result<TokenStream2, Error> {
    let mut branches = quote! {};
    for variant in &enum_data.variants {
        let variant_ident = &variant.ident;

        let (mut message, attr_span) =
            parser::get_message_from_attrs(&variant.attrs, variant.span(), &default);
        let mut message_format_arguments =
            parser::get_format_args(&message, variant_ident, &variant.fields, attr_span)?;

        if let Fields::Unnamed(_) = &variant.fields {
            message = validator::normalize_message_positional_format_args(
                message,
                &mut message_format_arguments,
            );
        }

        let mut formatted_args = quote! {};
        let mut field_destructuring = quote! {};
        for argument in message_format_arguments {
            match &variant.fields {
                Fields::Named(_) => {
                    let argument_ident = Ident::new(&argument, proc_macro2::Span::call_site());
                    field_destructuring.append_all(quote! { #argument_ident, });
                }
                Fields::Unnamed(_) => {
                    let index: usize = argument.parse().expect(
                        "Argument should be numeric since we are operating on unnamed fields.",
                    );
                    let argument_ident = Ident::new(
                        &generate_unnamed_enum_positional_field_name(index),
                        proc_macro2::Span::call_site(),
                    );
                    formatted_args.append_all(quote! { #argument_ident, });
                    field_destructuring.append_all(quote! { #argument_ident, });
                }
                _ => (),
            }
        }

        let write_call = generate_write_call(&variant.fields, message, formatted_args);

        match &variant.fields {
            Fields::Unit => {
                branches.append_all(quote! {
                    Self::#variant_ident => #write_call,
                });
            }
            Fields::Named(_) => {
                branches.append_all(quote! {
                    Self::#variant_ident {#field_destructuring ..} => #write_call,
                });
            }
            Fields::Unnamed(_) => {
                branches.append_all(quote! {
                    Self::#variant_ident (#field_destructuring ..) => #write_call,
                });
            }
        }
    }

    Ok(quote! {
        match self {
            #branches
        }
    })
}

/// parses a struct into a `TokenStream` implementing the Display trait
fn parse_struct(
    mut message: String,
    struct_ident: &Ident,
    fields: &Fields,
    attr_span: Span,
) -> Result<TokenStream2, Error> {
    let mut message_format_arguments =
        parser::get_format_args(&message, struct_ident, fields, attr_span)?;

    if let Fields::Unnamed(_) = fields {
        message = validator::normalize_message_positional_format_args(
            message,
            &mut message_format_arguments,
        );
    }

    let formatted_args = message_format_arguments
        .iter()
        .map(|argument| match fields {
            Fields::Named(_) => {
                let argument_ident = Ident::new(argument, proc_macro2::Span::call_site());
                quote! { #argument_ident = self.#argument_ident, }
            }
            Fields::Unnamed(_) => {
                let index: usize = argument
                    .parse()
                    .expect("Argument should be numeric since we are operating on unnamed fields.");
                let index_literal = Index::from(index);
                quote! { self.#index_literal, }
            }
            _ => quote! {},
        })
        .collect();

    Ok(generate_write_call(fields, message, formatted_args))
}

/// utilty function to pin how the unnamed enum positional fields should be named when
/// destructuring or refering to them in the display string
fn generate_unnamed_enum_positional_field_name(index: usize) -> String {
    format!("field_{index}")
}

/// creates the write call based on what type of struct we are encountering
fn generate_write_call(
    fields: &Fields,
    message: String,
    message_format_arguments: TokenStream2,
) -> TokenStream2 {
    match fields {
        Fields::Unit => (),
        Fields::Named(fields) => enforce_correct_display_use!(&fields.named),
        Fields::Unnamed(fields) => enforce_correct_display_use!(&fields.unnamed),
    };

    quote! {
        write!(f, #message, #message_format_arguments)
    }
}
