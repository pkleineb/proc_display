use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, TokenStreamExt};
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DataEnum, DeriveInput, Error, Fields,
    Ident, Index, LitStr, Meta,
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

/// Error Struct indicating an error occured during parsing of a string and where that error is
#[derive(Debug, PartialEq)]
struct ParseError {
    pub pos: usize,
}

impl ParseError {
    /// creates a new `ParseError` with a position on where the error is
    pub fn new(pos: usize) -> Self {
        Self { pos }
    }
}

/// Intervall between two indices
#[derive(Debug)]
struct Intervall {
    pub start: usize,
    pub end: usize,
}

impl Intervall {
    /// Creates a new intervall with a start and end index
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[proc_macro_derive(Display, attributes(display))]
pub fn display(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    impl_display(&ast)
}

/// macro implementation handling all DataTypes the derive macro can be used on
fn impl_display(ast: &syn::DeriveInput) -> TokenStream {
    let ident = &ast.ident;

    let (message, attr_span) = get_message_from_attrs(&ast.attrs, ast.span(), "");

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
            get_message_from_attrs(&variant.attrs, variant.span(), &default);
        let mut message_format_arguments =
            get_format_args(&message, variant_ident, &variant.fields, attr_span)?;

        if let Fields::Unnamed(_) = &variant.fields {
            message =
                normalize_message_positional_format_args(message, &mut message_format_arguments);
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

/// utilty function to pin how the unnamed enum positional fields should be named when
/// destructuring or refering to them in the display string
fn generate_unnamed_enum_positional_field_name(index: usize) -> String {
    format!("field_{index}")
}

/// Tries to retrieve the message from a list of attributes by checking wether or not there is
/// an attribute with the name "display"
fn get_message_from_attrs(attrs: &[Attribute], span: Span, default: &str) -> (String, Span) {
    let mut attr_span = span;
    let message = attrs
        .iter()
        .find(|attr| attr.path().is_ident("display"))
        .and_then(|attr| {
            attr_span = attr.span();
            get_message_from_attribute(attr)
        })
        .unwrap_or(default.to_string());

    (message, attr_span)
}

/// Parses the formatarguments from the given message. Since the fromatarguments may not be
/// formatted correctly this might throw an error
fn get_format_args(
    message: &str,
    struct_ident: &Ident,
    fields: &Fields,
    attr_span: Span,
) -> Result<Vec<String>, Error> {
    let message_format_arguments = match parse_message(message) {
        Ok(args) => args,
        Err(parse_error) => {
            return Err(Error::new(
                attr_span,
                format!("The display message for {struct_ident} could not be parsed correctly because there was a problem at character {} in the string.\nMake sure that all format arguments refering to attributes are valid rust attributes and the format specifier is valid.", parse_error.pos),
            ));
        }
    };

    format_arguments_are_valid_fields(
        fields,
        message_format_arguments
            .iter()
            .map(|str| str.as_str())
            .collect::<Vec<_>>(),
    )?;

    Ok(message_format_arguments)
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

/// parses a struct into a `TokenStream` implementing the Display trait
fn parse_struct(
    mut message: String,
    struct_ident: &Ident,
    fields: &Fields,
    attr_span: Span,
) -> Result<TokenStream2, Error> {
    let mut message_format_arguments = get_format_args(&message, struct_ident, fields, attr_span)?;

    if let Fields::Unnamed(_) = fields {
        message = normalize_message_positional_format_args(message, &mut message_format_arguments);
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

/// normalizes the positional arguments in the message.
/// We have to do this because let's say we have a tuple struct, that has 3 fields but the user
/// only wants to use the second and the third argument in the display string like this:
/// ```
/// use proc_display::Display;
///
/// #[derive(Display)]
/// #[display("I only want to display the third: {2} and the second: {1} number.")]
/// struct TupleStruct(i32, i32, i32);
///
/// let my_struct = TupleStruct(42, 69, 21);
/// assert_eq!(format!("{my_struct}"), "I only want to display the third: 21 and the second: 69 number.".to_string());
/// ```
/// if we were to not normalize the messages positional arguments the write! macro in the Display
/// implementation would panick since it would expect 3 arguments in total, but only two will be
/// given to it
fn normalize_message_positional_format_args(
    message: String,
    message_format_arguments: &mut [String],
) -> String {
    let mut normalized_message = message;
    message_format_arguments.sort();

    for (i, arg) in message_format_arguments.iter().enumerate() {
        normalized_message =
            replace_format_argument(normalized_message, arg, i.to_string().as_str());
    }

    normalized_message
}

/// replaces the formatargument `from` to the formatargument `to` in the given string and returns
/// the replaced string
fn replace_format_argument(message: String, from: &str, to: &str) -> String {
    if from.is_empty() {
        return message;
    };

    let mut chars = message.chars().enumerate().peekable();
    let mut argument_intervalls = vec![];

    while let Some((pos, next_char)) = chars.next() {
        if next_char == '{' {
            if chars.peek().map(|(_, ch)| *ch) == Some('{') {
                chars.next();
                continue;
            }

            let start_pos = pos;
            let mut captured = String::new();
            let mut end_pos = start_pos;

            for (pos, inner_char) in chars.by_ref() {
                if inner_char == '}' {
                    end_pos = pos;
                    break;
                }
                captured.push(inner_char);
            }

            if captured == from {
                argument_intervalls.push(Intervall::new(start_pos, end_pos));
            }
        }
    }

    let mut new_message = String::new();
    let mut last_end = 0;
    for intervall in argument_intervalls {
        new_message.push_str(&message[last_end..intervall.start]);
        new_message.push_str(&format!("{{{}}}", to));
        last_end = intervall.end + 1;
    }

    new_message.push_str(&message[last_end..message.len()]);

    new_message
}

/// checks wether or not all format arguments exist on the struct.
fn format_arguments_are_valid_fields(
    fields: &Fields,
    mut format_arguments: Vec<&str>,
) -> Result<(), Error> {
    match fields {
        Fields::Unit => {
            if !format_arguments.is_empty() {
                return Err(
                    Error::new(
                        fields.span(),
                        "#[display(...)] doesn't expect any format argument variabels on a unit struct since there are no attributes."
                    )
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
                ));
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
                    )
                );
            }
        }
    }

    Ok(())
}

/// tries to get the message from a given attribute. If the attribute does not have a String
/// argument this will return None
fn get_message_from_attribute(attr: &Attribute) -> Option<String> {
    if let Meta::List(meta_list) = &attr.meta {
        if let Ok(message) = meta_list.parse_args::<LitStr>() {
            return Some(message.value());
        }
    }

    None
}

/// Tries to parse the message grabbing all format arguments and returning those
fn parse_message(message: &str) -> Result<Vec<String>, ParseError> {
    let mut chars = message.chars().enumerate().peekable();
    let mut result = vec![];

    while let Some((index, next_char)) = chars.next() {
        if next_char == '{' {
            if chars.peek().map(|(_, ch)| *ch) == Some('{') {
                chars.next();
                continue;
            } else {
                let mut format_argument = String::new();
                let mut parsing_format_specifier = false;
                let mut format_specifier = String::new();

                while let Some((index, collectible_char)) = chars.next() {
                    if collectible_char == '}' && chars.peek().map(|(_, ch)| *ch) != Some('}') {
                        if !result.contains(&format_argument) {
                            result.push(format_argument);
                        }
                        break;
                    }

                    if collectible_char == ':' {
                        // a format specifier like: "{:}" is always invalid
                        if chars.peek().map(|(_, ch)| *ch) == Some('}') {
                            return Err(ParseError::new(index));
                        }
                        parsing_format_specifier = true;
                        continue;
                    }

                    if !collectible_char.is_alphanumeric()
                        && collectible_char != '_'
                        && !parsing_format_specifier
                    {
                        return Err(ParseError::new(index));
                    }

                    if !parsing_format_specifier {
                        format_argument.push(collectible_char);
                    } else {
                        format_specifier.push(collectible_char);
                    }
                }

                if let Err(parse_error) = is_valid_format_specifier(format_specifier) {
                    return Err(ParseError::new(index + parse_error.pos));
                }
            }
        }
        if next_char == '}' {
            return Err(ParseError::new(index));
        }
    }

    Ok(result)
}

/// checks wether or not a specifier is a valid rust format specifier as declared in the [fmt
/// definition](https://doc.rust-lang.org/std/fmt/index.html)
fn is_valid_format_specifier(specifier: String) -> Result<(), ParseError> {
    if specifier.is_empty() {
        return Ok(());
    }

    let mut chars = specifier.chars().enumerate().peekable();

    if let Some(&(_, next)) = chars.peek() {
        let mut temp_chars = chars.clone();
        temp_chars.next();
        if let Some(&(_, align)) = temp_chars.peek() {
            if matches!(align, '<' | '>' | '^') {
                chars.next();
                chars.next();
            }
        } else if matches!(next, '<' | '>' | '^') {
            chars.next();
        }
    }

    if let Some(&(_, ch)) = chars.peek() {
        if matches!(ch, '+' | '-') {
            chars.next();
        }
    }

    if chars.peek().map(|(_, ch)| *ch) == Some('#') {
        chars.next();
    }

    if chars.peek().map(|(_, ch)| *ch) == Some('0') {
        chars.next();
    }

    while let Some(&(_, ch)) = chars.peek() {
        if ch.is_ascii_digit() {
            chars.next();
        } else if ch == '$' {
            chars.next();
            break;
        } else {
            break;
        }
    }

    if chars.peek().map(|(_, ch)| *ch) == Some('.') {
        chars.next();
        if chars.peek().map(|(_, ch)| *ch) == Some('$') {
            chars.next();
        } else {
            while let Some(&(_, ch)) = chars.peek() {
                if ch.is_ascii_digit() {
                    chars.next();
                } else {
                    break;
                }
            }
        }
    }

    if let Some(&(index, ch)) = chars.peek() {
        match ch {
            '?' | 'x' | 'X' | 'o' | 'b' | 'e' | 'E' | 'p' => {
                chars.next();
            }
            _ => return Err(ParseError::new(index)),
        }
    }

    if let Some((index, _)) = chars.next() {
        return Err(ParseError::new(index));
    }

    Ok(())
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
        assert_eq!(parse_message(str), Err(ParseError::new(2)));
        let str = "{}}";
        assert_eq!(parse_message(str), Err(ParseError::new(1)));
        let str = "{-}";
        assert_eq!(parse_message(str), Err(ParseError::new(1)));
        let str = "{\\}";
        assert_eq!(parse_message(str), Err(ParseError::new(1)));
    }

    #[test]
    fn replace_format_argument_correctly_in_simple_str() {
        let str = "{2}".to_string();
        assert_eq!(replace_format_argument(str, "2", "0"), "{0}".to_string());
    }

    #[test]
    fn replace_format_argument_correctly_in_complex_str() {
        let str = "Normal text here {2} around this argument".to_string();
        assert_eq!(
            replace_format_argument(str, "2", "0"),
            "Normal text here {0} around this argument".to_string()
        );
    }

    #[test]
    fn replace_format_argument_handle_escaped() {
        let str = "{{2}}".to_string();
        assert_eq!(replace_format_argument(str, "2", "0"), "{{2}}".to_string());
    }

    #[test]
    fn replace_format_argument_replace_multiple_of_same_kind() {
        let str = "{2} {2} {2}".to_string();
        assert_eq!(
            replace_format_argument(str, "2", "0"),
            "{0} {0} {0}".to_string()
        );
    }

    #[test]
    fn replace_format_argument_replace_only_same_kind() {
        let str = "{2} {1} {2}".to_string();
        assert_eq!(
            replace_format_argument(str, "2", "0"),
            "{0} {1} {0}".to_string()
        );
    }

    #[test]
    fn replace_format_argument_ignores_normal_numbers() {
        let str = "2 {2}".to_string();
        assert_eq!(replace_format_argument(str, "2", "0"), "2 {0}".to_string());
    }
}
