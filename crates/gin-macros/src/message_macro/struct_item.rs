use proc_macro2::TokenStream;
use quote::quote;
use syn::{punctuated::Punctuated, Attribute, Field, Fields, FieldsNamed, ItemStruct};

use crate::utils::Spanned;

use super::scalars::{AssertedRustType, Assertion};

pub(in crate::message_macro) fn struct_item(item: syn::ItemStruct) -> TokenStream {
    let ItemStruct {
        attrs,
        vis,
        struct_token,
        ident,
        generics,
        fields,
        semi_token,
    }: ItemStruct = item;

    let (fields, assertions) = match annotate_fields(fields) {
        Ok(fields) => fields,
        Err(err) => {
            // TODO: Better error message including the original spans
            let err = err.to_string();
            return quote! {
                compile_error!(#err);
            };
        }
    };

    let out = quote! {
        #[derive(::prost::Message)]
        #(#attrs)*
        #vis #struct_token #ident #generics #fields #semi_token

        impl #ident {
            #vis fn parse(buf: impl AsRef<[u8]>) -> Result<Self, ::prost::DecodeError> {
                use ::prost::Message;

                Self::decode(&mut std::io::Cursor::new(buf))
            }

            #vis fn serialize(self) -> Vec<u8> {
                use ::prost::Message;

                let mut buf = Vec::new();
                buf.reserve(self.encoded_len());
                // Unwrap is safe, since we have reserved sufficient capacity in the vector.
                self.encode(&mut buf).unwrap();
                buf
            }
        }

        #(#assertions)*
    };

    out
}

fn annotate_fields(fields: Fields) -> syn::Result<(Fields, Vec<Assertion>)> {
    match fields {
        Fields::Named(fields) => {
            let (fields, assertions) = annotate_named_fields(fields)?;
            Ok((Fields::Named(fields), assertions))
        }
        Fields::Unnamed(_) => todo!("#[message] on a struct with unnamed fields"),
        Fields::Unit => todo!("#[message] on a unit struct"),
    }
}

fn annotate_named_fields(fields: FieldsNamed) -> syn::Result<(FieldsNamed, Vec<Assertion>)> {
    let FieldsNamed { brace_token, named } = fields;

    let mut punctuated = Punctuated::new();
    let mut all_assertions = vec![];

    for pair in named.into_pairs() {
        let field = pair.into_value();

        let (field, assertions) = annotate_named_field(field)?;

        all_assertions.extend(assertions);
        punctuated.push(field);
    }

    Ok((
        FieldsNamed {
            brace_token,
            named: punctuated,
        },
        all_assertions,
    ))
}

fn annotate_named_field(field: Field) -> syn::Result<(Field, Vec<Assertion>)> {
    let Field {
        attrs,
        vis,
        ident,
        colon_token,
        ty,
    } = field;

    let scalar_ty = <Spanned<AssertedRustType>>::parse_unhinted(ty.clone())?;

    let (annotation, assertions) = scalar_ty.into_annotation();

    let attrs: Vec<Attribute> = {
        let mut updated = vec![annotation];
        updated.extend(attrs);
        updated
    };

    Ok((
        Field {
            attrs,
            vis,
            ident,
            colon_token,
            ty,
        },
        assertions,
    ))
}
