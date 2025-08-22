use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DataEnum, DataUnion, DeriveInput, Error,
    Fields, Ident, Index, LitStr, Meta,
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
                .unwrap_or_default();

            let Ok(message_format_arguments) = parse_message(&message) else {
                return Error::new(
                    attr_span,
                    format!("The display message for {variant_name} could not be parsed correctly. Make sure that all format arguments refering to attributes are valid rust attributes."),
                ).to_compile_error();
            };

            if let Err(error_message) = format_arguments_are_valid_fields(&variant.fields, message_format_arguments.iter().map(|str| str.as_str()).collect::<Vec<_>>()) {
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
                    let write_call = parse_named_fields(message, message_format_arguments);
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
                    let write_call = parse_unnamed_fields(message, message_format_arguments);
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

fn format_arguments_are_valid_fields(
    fields: &Fields,
    mut format_arguments: Vec<&str>,
) -> Result<(), TokenStream2> {
    match fields {
        Fields::Unit => {
            if !format_arguments.is_empty() {
                return Err(
                    Error::new(
                        fields.span(),
                        "#[display(...)] doesn't expect any format argument variabels on a unit struct since there are no attributes."
                    ).to_compile_error()
                );
            }
        }
        Fields::Named(named_fields) => {
            for field in &named_fields.named {
                if let Some(position) = format_arguments.iter().position(|format_argument| {
                    let field_ident = &field
                        .ident
                        .as_ref()
                        .expect("Expected field to be named when iterating over named fields.");
                    *format_argument == field_ident.to_string().as_str()
                }) {
                    format_arguments.remove(position);
                }
            }

            if !format_arguments.is_empty() {
                return Err(Error::new(
                    fields.span(),
                    format!(
                        "#[display(...)] found undeclared fields ({}) in display message.",
                        format_arguments.join(", ")
                    ),
                )
                .to_compile_error());
            }
        }
        Fields::Unnamed(unnamed_fields) => {
            if unnamed_fields.unnamed.len() < format_arguments.len() {
                return Err(
                    Error::new(
                        fields.span(),
                        format!(
                            "#[display(...)] expected {} positional arguments, but only {} are defined on the Tuple struct",
                            format_arguments.len(),
                            unnamed_fields.unnamed.len()
                        )
                    ).to_compile_error()
                );
            }
        }
    }

    Ok(())
}

fn parse_named_fields(message: String, format_arguments_to_use: Vec<String>) -> TokenStream2 {
    let arguments: TokenStream2 = format_arguments_to_use
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

fn parse_unnamed_fields(
    mut message: String,
    mut format_arguments_to_use: Vec<String>,
) -> TokenStream2 {
    format_arguments_to_use.sort();

    let arguments: TokenStream2 = format_arguments_to_use
        .iter()
        .map(|i| {
            let field_name = format!("field_{i}");
            let field_ident = Ident::new(&field_name, proc_macro2::Span::call_site());
            quote! { #field_ident, }
        })
        .collect();

    for (i, format_argument) in format_arguments_to_use.iter().enumerate() {
        message = message.replace(format_argument, i.to_string().as_str());
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
    let mut result = vec![];
    let mut format_argument = String::new();

    while let Some(next_char) = chars.next() {
        if next_char == '{' {
            if chars.peek() == Some(&'{') {
                chars.next();
                continue;
            } else {
                format_argument.clear();
                while let Some(collectible_char) = chars.next() {
                    if collectible_char == '}' && chars.peek() != Some(&'}') {
                        if !result.contains(&format_argument) {
                            result.push(format_argument.clone());
                        }

                        format_argument.clear();
                        break;
                    }

                    if !collectible_char.is_alphanumeric() && collectible_char != '_' {
                        return Err(ParseError);
                    }

                    format_argument.push(collectible_char);
                }
            }
        }
        if next_char == '}' {
            return Err(ParseError);
        }
    }

    Ok(result)
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
