#![deny(unconditional_recursion)]

#[macro_use]
mod utils;

pub(crate) use self::utils::*;

mod message_macro;

use proc_macro::TokenStream;
use syn::{parse_macro_input, Item};

use crate::message_macro::MessageArgs;

#[proc_macro_attribute]
pub fn message(_args: TokenStream, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as Item);

    self::message_macro::message_attribute(MessageArgs {}, item).into()
}
