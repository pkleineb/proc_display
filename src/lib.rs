use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DataEnum, DataStruct, DataUnion,
    DeriveInput, Error, Fields, FieldsNamed, FieldsUnnamed, Ident, LitStr, Meta,
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

#[derive(Debug, PartialEq)]
struct ParseError;

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
            let mut attr_span = variant.span();
            let message = variant
                .attrs
                .iter()
                .find(|attr| attr.path().is_ident("display"))
                .and_then(|attr| {
                    attr_span = attr.span();
                    get_message_from_attribute(attr)
                })
                .unwrap_or("".to_string());

            let Ok(message_placeholders) = parse_message(&message) else {
                return Error::new(
                    attr_span,
                    format!("The display message for {variant_name} could not be parsed correctly. Make sure that all placeholders refering to attributes are valid rust attributes."),
                ).to_compile_error();
            };

            if let Err(error_message) = placeholders_are_valid_fields(&variant.fields, message_placeholders.iter().map(|str| str.as_str()).collect::<Vec<_>>()) {
                return error_message;
            }

            match &variant.fields {
                Fields::Unit => {
                    quote! {
                        Self::#variant_name => write!(f, "{}", #message),
                    }
                }
                Fields::Named(fields) => {
                    enforce_correct_display_use!(&fields.named);
                    let write_call = parse_named_fields(message, message_placeholders);
                    let field_destructuring: TokenStream2 = fields
                        .named
                        .iter()
                        .map(|field| {
                            let field_ident = &field.ident;
                            quote! { #field_ident, }
                        })
                        .collect();

                    quote! {
                        Self::#variant_name {#field_destructuring ..} => #write_call,
                    }
                }
                Fields::Unnamed(fields) => {
                    enforce_correct_display_use!(&fields.unnamed);
                    let write_call = parse_unnamed_fields(message, message_placeholders);
                    let field_destructuring: TokenStream2 = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, _)| {
                            let field_name = format!("field_{i}");
                            let field_ident =
                                Ident::new(&field_name, proc_macro2::Span::call_site());
                            quote! { #field_ident, }
                        })
                        .collect();
                    quote! {
                        Self::#variant_name (#field_destructuring ..) => #write_call,
                    }
                }
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

fn placeholders_are_valid_fields(
    fields: &Fields,
    mut placeholders: Vec<&str>,
) -> Result<(), TokenStream2> {
    match fields {
        Fields::Unit => {
            if !placeholders.is_empty() {
                return Err(
                    Error::new(
                        fields.span(),
                        "#[display(...)] doesn't expect any placeholder variabels on a unit struct since there are no attributes."
                    ).to_compile_error()
                );
            }
        }
        Fields::Named(named_fields) => {
            for field in &named_fields.named {
                if let Some(position) = placeholders.iter().position(|placeholder| {
                    let field_ident = &field
                        .ident
                        .as_ref()
                        .expect("Expected field to be named when iterating over named fields.");
                    *placeholder == field_ident.to_string().as_str()
                }) {
                    placeholders.remove(position);
                }
            }

            if !placeholders.is_empty() {
                return Err(Error::new(
                    fields.span(),
                    format!(
                        "#[display(...)] found undeclared fields ({}) in display message.",
                        placeholders.join(", ")
                    ),
                )
                .to_compile_error());
            }
        }
        Fields::Unnamed(unnamed_fields) => {
            if unnamed_fields.unnamed.len() < placeholders.len() {
                return Err(
                    Error::new(
                        fields.span(),
                        format!(
                            "#[display(...)] expected {} positional arguments, but only {} are defined on the Tuple struct",
                            placeholders.len(),
                            unnamed_fields.unnamed.len()
                        )
                    ).to_compile_error()
                );
            }
        }
    }

    Ok(())
}

fn parse_named_fields(message: String, placeholders_to_use: Vec<String>) -> TokenStream2 {
    let arguments: TokenStream2 = placeholders_to_use
        .iter()
        .map(|field| {
            let field_ident = Ident::new(field, proc_macro2::Span::call_site());
            quote! { #field_ident = #field_ident, }
        })
        .collect();

    quote! {
        write!(f, #message, #arguments)
    }
}

fn parse_unnamed_fields(mut message: String, mut placeholders_to_use: Vec<String>) -> TokenStream2 {
    placeholders_to_use.sort();

    let arguments: TokenStream2 = placeholders_to_use
        .iter()
        .map(|i| {
            let field_name = format!("field_{i}");
            let field_ident = Ident::new(&field_name, proc_macro2::Span::call_site());
            quote! { #field_ident, }
        })
        .collect();

    for (i, placeholder) in placeholders_to_use.iter().enumerate() {
        message = message.replace(placeholder, i.to_string().as_str());
    }

    quote! {
        write!(f, #message, #arguments)
    }
}

fn get_message_from_attribute(attr: &Attribute) -> Option<String> {
    if let Meta::List(meta_list) = &attr.meta {
        if let Ok(message) = meta_list.parse_args::<LitStr>() {
            return Some(message.value());
        }
    }

    None
}

fn parse_message(message: &str) -> Result<Vec<String>, ParseError> {
    let mut chars = message.chars().peekable();
    let mut result = HashSet::new();
    let mut placeholder = "".to_string();

    while let Some(next_char) = chars.next() {
        if next_char == '{' {
            if chars.peek() == Some(&'{') {
                chars.next();
                continue;
            } else {
                placeholder.clear();
                while let Some(collectible_char) = chars.next() {
                    if collectible_char == '}' && chars.peek() != Some(&'}') {
                        result.insert(placeholder.clone());
                        placeholder.clear();
                        break;
                    }

                    if !collectible_char.is_alphanumeric() && collectible_char != '_' {
                        return Err(ParseError);
                    }

                    placeholder.push(collectible_char);
                }
            }
        }
        if next_char == '}' {
            return Err(ParseError);
        }
    }

    Ok(result.into_iter().collect::<Vec<_>>())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_one_argument_message_parse() {
        let str = "One {parse} message.";
        assert_eq!(parse_message(str), Ok(vec!["parse".to_string()]));
    }

    #[test]
    fn valid_multiple_argument_message_parse() {
        let str = "{Multiple} parseable {Se}quen{zes}.";
        assert_eq!(
            parse_message(str),
            Ok(vec![
                "Multiple".to_string(),
                "Se".to_string(),
                "zes".to_string()
            ])
        );
    }

    #[test]
    fn valid_number_argument_message_parse() {
        let str = "This should also parse {0}.";
        assert_eq!(parse_message(str), Ok(vec!["0".to_string()]));
    }

    #[test]
    fn invalid_argument_message_parse() {
        let str = "{{}";
        assert_eq!(parse_message(str), Err(ParseError));
        let str = "{}}";
        assert_eq!(parse_message(str), Err(ParseError));
        let str = "{-}";
        assert_eq!(parse_message(str), Err(ParseError));
        let str = "{\\}";
        assert_eq!(parse_message(str), Err(ParseError));
    }
}
