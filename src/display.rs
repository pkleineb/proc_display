use crate::parser;
use crate::validator;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, TokenStreamExt};
use syn::Variant;
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

/// Array for reserved keywords that get replaced with general information of the annotated type.
/// keywords can be used like this:
/// ```compile_fail
/// use proc_display::Display;
///
/// #[derive(Display)]
/// #[display("I am {self.<keyword>}")]
/// struct MyStruct {}
/// ```
///
/// Keywords are:
///  - "name" which gets replaced with the types ident. If this is declared on enum variants it
///    will be the enum variants ident.
///    ```
///    use proc_display::Display;
///
///    #[derive(Display)]
///    enum MyEnum{
///        #[display("I am {self.name}")]
///        AType
///    }
///
///    assert_eq!(format!("{}", MyEnum::AType), "I am AType");
///    ```
///
pub const RESERVED_KEYWORDS: [&str; 1] = ["name"];

/// trait that helps us implemnt reserved keywords generically
trait ReplacementProvider {
    /// returns the ident of the type
    fn get_ident(&self) -> &Ident;
}

impl ReplacementProvider for Variant {
    fn get_ident(&self) -> &Ident {
        &self.ident
    }
}

impl ReplacementProvider for syn::DeriveInput {
    fn get_ident(&self) -> &Ident {
        &self.ident
    }
}

/// macro implementation handling all DataTypes the derive macro can be used on
pub fn impl_display(ast: &syn::DeriveInput) -> TokenStream {
    let ident = &ast.ident;

    let (mut message, attr_span) = parser::get_message_from_attrs(&ast.attrs, ast.span(), "");
    message = replace_reserved_keywords(message, ast);

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

        message = replace_reserved_keywords(message, variant);

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

/// replaces reserved keywords, that are prefixed with `self` to concrete strings
fn replace_reserved_keywords(mut message: String, provider: &impl ReplacementProvider) -> String {
    for keyword in RESERVED_KEYWORDS {
        let pattern = format!("{{self.{keyword}}}");

        let replacement = match keyword {
            "name" | "variant" => provider.get_ident().to_string(),
            _ => continue, // nothing to replace
        };

        message = message.replace(&pattern, &replacement);
    }

    message
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

#[cfg(test)]
mod tests {
    use syn::{parse_quote, DeriveInput};

    use super::*;

    #[test]
    fn valid_replace_reserved_keywords_variant() {
        let str = "I am {self.name}".to_string();
        let variant: Variant = parse_quote! {
            Variant
        };

        let result = replace_reserved_keywords(str, &variant);
        assert_eq!(result, "I am Variant");
    }

    #[test]
    fn valid_replace_reserved_keywords_struct() {
        let str = "I am {self.name}".to_string();
        let variant: DeriveInput = parse_quote! {
            struct Struct {}
        };

        let result = replace_reserved_keywords(str, &variant);
        assert_eq!(result, "I am Struct");
    }

    #[test]
    fn valid_replace_reserved_keywords_union() {
        let str = "I am {self.name}".to_string();
        let variant: DeriveInput = parse_quote! {
            union Union {
                field: i32
            }
        };

        let result = replace_reserved_keywords(str, &variant);
        assert_eq!(result, "I am Union");
    }

    #[test]
    fn invalid_replace_reserved_keywords() {
        let str = "I am {Self.name}".to_string();
        let variant: Variant = parse_quote! {
            Variant
        };

        let result = replace_reserved_keywords(str.clone(), &variant);
        assert_eq!(result, str);
    }

    #[test]
    fn unrecognized_reserved_keyword_gets_ignored() {
        let str = "I am {self.eiotuewoitewituerpoitu}".to_string();
        let variant: Variant = parse_quote! {
            Variant
        };

        let result = replace_reserved_keywords(str.clone(), &variant);
        assert_eq!(result, str);
    }
}
