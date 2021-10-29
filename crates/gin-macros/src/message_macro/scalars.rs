use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse_quote, AngleBracketedGenericArguments, Attribute, Path, PathArguments, PathSegment, Type,
    TypePath,
};

use crate::{
    Spanned, {FieldName, SpanItem},
};

/// By default, gin will use a balanced encoding for signed integers. For
/// example, the default encoding for i32 is `sint32`, which is a:
///
/// - variable-length encoding
/// - not biased towards positive numbers
///
/// You can specify an alternative encoding as a [ScalarHint].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(in crate::message_macro) enum SignedIntHint {
    /// The default encoding. Use variable-length encoding without a bias
    /// towards positive or negative numbers.
    Balanced,
    /// Use a variable encoding for this type, biased towards encoding positive
    /// numbers efficiently. This comes at the cost of encoding negative numbers
    /// very inefficiently.
    #[allow(unused)]
    PositiveBias,
    /// Use a fixed size for this type. For example, a FixedSize i32 will always
    /// encode as 32-bits.
    #[allow(unused)]
    FixedSize,
}

impl Default for SignedIntHint {
    fn default() -> Self {
        SignedIntHint::Balanced
    }
}

/// By default, gin will use a variable encoding for unsigned integers. For
/// example, the default encoding for u32 is `uint32`, which is a:
///
/// - variable-length encoding
/// - biased towards positive numbers
///
/// You can specify an alternative encoding as a [ScalarHint].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(in crate::message_macro) enum UnsignedIntHint {
    /// The default (variable-length) encoding.
    Variable,
    /// Use a fixed size for this type. For example, a FixedSize u32 will always
    /// encode as 32-bits.
    #[allow(unused)]
    FixedSize,
}

impl Default for UnsignedIntHint {
    fn default() -> Self {
        UnsignedIntHint::Variable
    }
}

pub(in crate::message_macro) enum RustType {
    Scalar(ScalarRustType),
    Vec(Box<Spanned<AssertedRustType>>),
    #[allow(unused)]
    Option(Box<Spanned<AssertedRustType>>),
    Custom(Type),
}

impl Into<RustType> for ScalarRustType {
    fn into(self) -> RustType {
        RustType::Scalar(self)
    }
}

impl Into<RustType> for Type {
    fn into(self) -> RustType {
        RustType::Custom(self)
    }
}

pub(in crate::message_macro) struct AssertedRustType {
    ty: RustType,
    assertions: Vec<Assertion>,
}

impl AssertedRustType {
    fn new(rust_type: impl Into<RustType>) -> AssertedRustType {
        AssertedRustType {
            ty: rust_type.into(),
            assertions: vec![],
        }
    }

    fn f32() -> AssertedRustType {
        AssertedRustType::new(ScalarRustType::F32)
    }

    fn f64() -> AssertedRustType {
        AssertedRustType::new(ScalarRustType::F64)
    }

    #[allow(unused)]
    fn i32() -> AssertedRustType {
        AssertedRustType::hinted_i32(SignedIntHint::default())
    }

    fn hinted_i32(hint: SignedIntHint) -> AssertedRustType {
        AssertedRustType::new(ScalarRustType::I32(hint))
    }

    #[allow(unused)]
    fn i64() -> AssertedRustType {
        AssertedRustType::hinted_i64(SignedIntHint::default())
    }

    fn hinted_i64(hint: SignedIntHint) -> AssertedRustType {
        AssertedRustType::new(ScalarRustType::I64(hint))
    }

    #[allow(unused)]
    fn u32() -> AssertedRustType {
        AssertedRustType::hinted_u32(UnsignedIntHint::default())
    }

    fn hinted_u32(hint: UnsignedIntHint) -> AssertedRustType {
        AssertedRustType::new(ScalarRustType::U32(hint))
    }

    #[allow(unused)]
    fn u64() -> AssertedRustType {
        AssertedRustType::hinted_u64(UnsignedIntHint::default())
    }

    fn hinted_u64(hint: UnsignedIntHint) -> AssertedRustType {
        AssertedRustType::new(ScalarRustType::U64(hint))
    }

    fn bool() -> AssertedRustType {
        AssertedRustType::new(ScalarRustType::Bool)
    }

    fn string() -> AssertedRustType {
        AssertedRustType::new(ScalarRustType::String)
    }

    fn bytes() -> AssertedRustType {
        AssertedRustType::new(ScalarRustType::Bytes)
    }

    fn vec(inner: Spanned<AssertedRustType>) -> Self {
        AssertedRustType::new(RustType::Vec(Box::new(inner)))
    }

    #[allow(unused)]
    fn option(inner: Spanned<AssertedRustType>) -> Self {
        AssertedRustType::new(RustType::Option(Box::new(inner)))
    }

    fn assert(mut self, assertion: impl Into<Assertion>) -> Self {
        self.assertions.push(assertion.into());
        self
    }

    #[allow(unused)]
    fn scalar(scalar: ScalarRustType) -> AssertedRustType {
        AssertedRustType::new(RustType::Scalar(scalar))
    }

    fn custom(custom: Type) -> AssertedRustType {
        AssertedRustType::new(RustType::Custom(custom))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(in crate::message_macro) enum ScalarRustType {
    F32,
    F64,
    I32(SignedIntHint),
    I64(SignedIntHint),
    U32(UnsignedIntHint),
    U64(UnsignedIntHint),
    Bool,
    String,
    Bytes,
}

#[allow(unused)]
#[derive(Debug, Clone, thiserror::Error)]
pub(in crate::message_macro) enum ScalarTypeError {
    #[error("in #[message]: invalid scalar type for {field}: {}", print_ty(.ty))]
    InvalidScalarType { field: FieldName, ty: Type },
    #[error("in #[message]: invalid signed integer hint for: {field}: {ty}")]
    InvalidSignedInt { field: Ident, ty: Ident },
    #[error("in #[message]: invalid unsigned integer hint for: {field}: {ty}")]
    InvalidUnsignedInt { field: Ident, ty: Ident },
}

impl Into<syn::Error> for ScalarTypeError {
    fn into(self) -> syn::Error {
        let error = self.to_string();
        let span = match self {
            ScalarTypeError::InvalidScalarType { ty, .. } => syn::spanned::Spanned::span(&ty),
            ScalarTypeError::InvalidSignedInt { ty, .. } => syn::spanned::Spanned::span(&ty),
            ScalarTypeError::InvalidUnsignedInt { ty, .. } => syn::spanned::Spanned::span(&ty),
        };

        syn::Error::new(span, error)
    }
}

impl ScalarTypeError {
    #[allow(unused)]
    pub(in crate::message_macro) fn invalid_type(
        field: impl Into<FieldName>,
        ty: impl Into<Type>,
    ) -> ScalarTypeError {
        ScalarTypeError::InvalidScalarType {
            field: field.into(),
            ty: ty.into(),
        }
    }
}

fn print_ty(ty: &Type) -> String {
    quote![#ty].to_string()
}

type AssertionFn = fn(/* type */ &Type, &TokenStream) -> TokenStream;

pub(crate) struct Assertion {
    assertion: AssertionFn,
    ty: Type,
    args: TokenStream,
}

impl Assertion {
    pub(crate) fn assert_impl(ty: Type, implements: Type) -> Assertion {
        Assertion {
            assertion: assert_impl,
            ty,
            args: quote![#implements],
        }
    }
}

impl ToTokens for Assertion {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend((self.assertion)(&self.ty, &self.args));
    }
}

fn assert_impl(ty: &Type, implements: &TokenStream) -> TokenStream {
    quote! {
        ::gin::macro_internals::static_assertions::assert_impl_all!(#ty: #implements);
    }
}

fn special_case(ty: &Type) -> syn::Result<Spanned<AssertedRustType>> {
    let full_span = syn::spanned::Spanned::span(ty);

    match ty.clone() {
        // Special case for Vec<...> / Option<...> [UPSTREAM]: Should Vec<T>
        // where T: Message implement Message? Should there be a Repeated<T> if
        // we don't want to implement it on Vec itself?
        Type::Path(TypePath {
            qself: None,
            path:
                Path {
                    leading_colon: None,
                    segments,
                },
        }) => {
            let mut segments = segments.clone().into_iter();
            let only = segments.next().expect("TODO: Invalid missing path segment");
            let PathSegment { ident, arguments } = only;

            if segments.next().is_none() {
                if ident == "Vec" {
                    match arguments {
                        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                            args,
                            ..
                        }) if args.len() == 1 => match args.first().unwrap() {
                            syn::GenericArgument::Type(ty) => {
                                let ty = <Spanned<AssertedRustType>>::parse_unhinted(ty.clone())?;
                                return Ok(AssertedRustType::vec(ty).spanned(full_span));
                            }
                            _ => {}
                        },
                        _ => {}
                    }
                }
            }
        }

        _ => {}
    }

    Err(syn::Error::new(
        full_span,
        "Special case not found (this error should not be seen by end users)",
    ))
}

impl Spanned<AssertedRustType> {
    pub(in crate::message_macro) fn parse_unhinted(ty: Type) -> syn::Result<Self> {
        // let prost = gin_crate("prost")?;

        if let Ok(special) = special_case(&ty) {
            return Ok(special);
        }

        let span = syn::spanned::Spanned::span(&ty);
        let string = quote![#ty].to_string();

        let scalar_type = match &string[..] {
            "f32" => AssertedRustType::f32(),
            "f64" => AssertedRustType::f64(),
            "i32" => AssertedRustType::hinted_i32(SignedIntHint::default()),
            "i64" => AssertedRustType::hinted_i64(SignedIntHint::default()),
            "u32" => AssertedRustType::hinted_u32(UnsignedIntHint::default()),
            "u64" => AssertedRustType::hinted_u64(UnsignedIntHint::default()),
            "bool" => AssertedRustType::bool(),
            "String" => AssertedRustType::string(),
            "Bytes" => AssertedRustType::bytes(),
            _ => AssertedRustType::custom(ty.clone())
                .assert(Assertion::assert_impl(ty, parse_quote![::prost::Message])),
        };

        Ok(scalar_type.spanned(span))
    }

    pub(in crate::message_macro) fn into_annotation(self) -> (Attribute, Vec<Assertion>) {
        let (prost_type, assertions) = self.into_prost_type(false);

        (
            parse_quote! {
                #[prost(#prost_type)]
            },
            assertions,
        )
    }

    #[allow(unused)]
    pub(in crate::message_macro) fn parse_signed_int(
        field: Ident,
        ident: Ident,
        hint: SignedIntHint,
    ) -> Result<Self, ScalarTypeError> {
        let string = &ident.to_string()[..];

        let scalar_type = match string {
            "i32" => AssertedRustType::hinted_i32(hint),
            "i64" => AssertedRustType::hinted_i64(hint),
            _ => return Err(ScalarTypeError::InvalidSignedInt { field, ty: ident }),
        };

        Ok(scalar_type.spanned(ident.span()))
    }

    #[allow(unused)]
    pub(in crate::message_macro) fn parse_unsigned_int(
        field: Ident,
        ident: Ident,
        hint: SignedIntHint,
    ) -> Result<Self, ScalarTypeError> {
        let string = &ident.to_string()[..];

        let scalar_type = match string {
            "u32" => AssertedRustType::hinted_i32(hint),
            "u64" => AssertedRustType::hinted_i64(hint),
            _ => return Err(ScalarTypeError::InvalidSignedInt { field, ty: ident }),
        };

        Ok(scalar_type.spanned(ident.span()))
    }

    pub(in crate::message_macro) fn into_prost_type(
        self,
        nested: bool,
    ) -> (TokenStream, Vec<Assertion>) {
        self.map_into(|item, span| {
            let scalar_type = match item.ty {
                RustType::Scalar(scalar) => match scalar {
                    ScalarRustType::F64 => "double",
                    ScalarRustType::F32 => "float",
                    ScalarRustType::I32(SignedIntHint::Balanced) => "sint32",
                    ScalarRustType::I32(SignedIntHint::PositiveBias) => "int32",
                    ScalarRustType::I32(SignedIntHint::FixedSize) => "fixed32",
                    ScalarRustType::I64(SignedIntHint::Balanced) => "sint64",
                    ScalarRustType::I64(SignedIntHint::PositiveBias) => "int64",
                    ScalarRustType::I64(SignedIntHint::FixedSize) => "fixed64",
                    ScalarRustType::U32(UnsignedIntHint::FixedSize) => "fixed32",
                    ScalarRustType::U32(UnsignedIntHint::Variable) => "uint32",
                    ScalarRustType::U64(UnsignedIntHint::FixedSize) => "fixed64",
                    ScalarRustType::U64(UnsignedIntHint::Variable) => "uint64",
                    ScalarRustType::Bool => "bool",
                    ScalarRustType::String => "string",
                    ScalarRustType::Bytes => "bytes",
                },
                RustType::Custom(_) => {
                    if nested {
                        return (quote![message], item.assertions);
                    } else {
                        return (quote![message, required], item.assertions);
                    }
                }
                RustType::Vec(ty) => {
                    let (prost_type, assertions) = ty.into_prost_type(true);
                    return (quote![#prost_type, repeated], assertions);
                }
                RustType::Option(_) => todo!(),
            };

            let ident = Ident::new(scalar_type, span);
            (quote![#ident], item.assertions)
        })
    }
}
