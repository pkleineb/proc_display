use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod display;
mod parser;
mod validator;

#[proc_macro_derive(Display, attributes(display))]
pub fn display(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    display::impl_display(&ast)
}
