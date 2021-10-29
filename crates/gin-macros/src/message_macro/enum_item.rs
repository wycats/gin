use proc_macro2::{Literal, Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse_quote,
    punctuated::{Pair, Punctuated},
    token::Eq,
    Fields, ItemEnum, Variant,
};

pub(in crate::message_macro) fn enum_item(item: syn::ItemEnum) -> TokenStream {
    if let Some(span) = non_unit_field(&item) {
        return quote_spanned! { span =>
            compile_error!("Gin currently only supports enums if all variants are unit-style");
        };
    }

    let ItemEnum {
        attrs,
        vis,
        enum_token,
        ident,
        generics,
        brace_token,
        variants,
    } = item;

    let attrs = {
        let mut updated =
            vec![parse_quote! { #[derive(Debug, Copy, Clone, ::prost::Enumeration)] }];
        updated.extend(attrs);
        updated
    };

    let variants = map_punctuated(variants, add_discriminant);

    let enum_name = ident.clone();

    let item = ItemEnum {
        attrs,
        vis,
        enum_token,
        ident,
        generics,
        brace_token,
        variants,
    };

    quote! {
        #item

        impl ::prost::Message for #enum_name {
            fn encode_raw<B>(&self, buf: &mut B)
            where
                B: prost::bytes::BufMut,
                Self: Sized,
            {
                i32::from(*self).encode_raw(buf)
            }

            fn merge_field<B>(
                &mut self,
                tag: u32,
                wire_type: prost::encoding::WireType,
                buf: &mut B,
                ctx: prost::encoding::DecodeContext,
            ) -> Result<(), prost::DecodeError>
            where
                B: prost::bytes::Buf,
                Self: Sized,
            {
                i32::from(*self).merge_field(tag, wire_type, buf, ctx)
            }

            fn encoded_len(&self) -> usize {
                i32::from(*self).encoded_len()
            }

            fn clear(&mut self) {
                *self = Self::default()
            }
        }
    }
}

fn non_unit_field(item: &syn::ItemEnum) -> Option<Span> {
    item.variants
        .iter()
        .find(|v| !matches!(v.fields, Fields::Unit))
        .map(|v| syn::spanned::Spanned::span(&v))
}

/// Add the [prost::Enumeration] attribute + a discriminant, if necessary
fn add_discriminant(
    Variant {
        attrs,
        ident,
        fields,
        discriminant,
    }: Variant,
    index: usize,
) -> Variant {
    let index = Literal::usize_unsuffixed(index);
    let discriminant = match discriminant {
        Some(d) => d,
        None => (Eq(Span::call_site()), parse_quote![#index]),
    };

    Variant {
        attrs,
        ident,
        fields,
        discriminant: Some(discriminant),
    }
}

fn map_punctuated<Item, MappedItem, Punctuation>(
    punctuated: Punctuated<Item, Punctuation>,
    mapper: impl Fn(Item, usize) -> MappedItem,
) -> Punctuated<MappedItem, Punctuation> {
    punctuated
        .into_pairs()
        .enumerate()
        .map(|(i, pair)| match pair {
            Pair::Punctuated(item, punctuation) => Pair::Punctuated(mapper(item, i), punctuation),
            Pair::End(item) => Pair::End(mapper(item, i)),
        })
        .collect()
}
