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
pub fn normalize_message_positional_format_args(
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

#[cfg(test)]
mod tests {
    use super::*;

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
