mod enum_item;
mod scalars;
mod struct_item;

use derive_syn_parse::Parse;
use proc_macro2::TokenStream;
use syn::Item;

use self::struct_item::struct_item;

#[derive(Debug, Parse)]
pub(crate) struct MessageArgs {}

pub(crate) fn message_attribute(_args: MessageArgs, item: Item) -> TokenStream {
    match item {
        Item::Const(_) => todo!(),
        Item::Enum(item) => enum_item::enum_item(item),
        Item::ExternCrate(_) => todo!(),
        Item::Fn(_) => todo!(),
        Item::ForeignMod(_) => todo!(),
        Item::Impl(_) => todo!(),
        Item::Macro(_) => todo!(),
        Item::Macro2(_) => todo!(),
        Item::Mod(_) => todo!(),
        Item::Static(_) => todo!(),
        Item::Struct(item) => struct_item(item),
        Item::Trait(_) => todo!(),
        Item::TraitAlias(_) => todo!(),
        Item::Type(_) => todo!(),
        Item::Union(_) => todo!(),
        Item::Use(_) => todo!(),
        Item::Verbatim(_) => todo!(),
        Item::__TestExhaustive(_) => todo!(),
    }
}
