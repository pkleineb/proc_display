//! # proc_display
//!
//! A procedural macro implementing the `Display` trait for structs, enums and unions
//!
//! ## Usage
//!
//! ```rust
//! use proc_display::Display;
//!
//! #[derive(Display)]
//! #[display("Greetings, {name}!")]
//! struct Greeting {
//!     name: String,
//! }
//! ```

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod display;
mod parser;
mod validator;

/// Automatic implementation of `Display` trait.
///
/// # Examples
///
/// ## Struct with named fields
/// ```rust
/// use proc_display::Display;
///
/// #[derive(Display)]
/// #[display("I am a Named struct: {field} {number}.")]
/// struct Named {
///     field: String,
///     number: i32,
/// }
/// ```
///
/// ## Struct with unnamed fields
/// ```rust
/// use proc_display::Display;
///
/// #[derive(Display)]
/// #[display("I am an unnamed struct: {0} {1}.")]
/// struct Point(i32, i32);
/// ```
///
/// ## Enum with different variations
/// ```rust
/// use proc_display::Display;
///
/// #[derive(Display)]
/// #[display("Default String here!")]
/// enum Status {
///     #[display("Loading...")]
///     Loading,
///
///     #[display("Status successfull with message: {message}")]
///     Success {
///         message: String,
///     },
///
///     #[display("Error: {0}")]
///     Error(String),
///
///     DefaultFallback,
/// }
/// ```
///
/// ## Unions (who uses this?)
/// ```rust
/// #[derive(Display)]
/// #[display("Fields are not supported since they are unsafe")]
/// union Union {
///     field: i32,
/// }
/// ```
#[proc_macro_derive(Display, attributes(display))]
pub fn display(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    display::impl_display(&ast)
}
