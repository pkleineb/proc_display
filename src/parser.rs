use proc_macro2::Span;
use syn::{spanned::Spanned, Attribute, Error, Fields, Ident, LitStr, Meta};

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

/// Tries to retrieve the message from a list of attributes by checking wether or not there is
/// an attribute with the name "display"
pub fn get_message_from_attrs(attrs: &[Attribute], span: Span, default: &str) -> (String, Span) {
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
pub fn get_format_args(
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
}
