use std::{fmt::Display, hash::Hash, ops::Deref};

use proc_macro2::{Span, TokenStream};
use proc_macro_crate::crate_name;
use quote::quote;
use syn::Ident;

#[derive(Debug, Clone)]
pub(crate) enum FieldName {
    Named(Ident),
    Anonymous(usize),
}

impl From<Ident> for FieldName {
    fn from(ident: Ident) -> Self {
        FieldName::Named(ident)
    }
}

impl From<usize> for FieldName {
    fn from(index: usize) -> Self {
        FieldName::Anonymous(index)
    }
}

impl Display for FieldName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldName::Named(named) => write!(f, "{}", named),
            FieldName::Anonymous(index) => write!(f, "{}", index),
        }
    }
}

/// This struct exists because `Span` doesn't implement Eq, Ord or Hash. This
/// struct implements all three using `Span`'s exposed `Display`.
#[derive(Debug, Copy, Clone)]
pub struct GinSpan {
    span: Span,
}

impl From<Span> for GinSpan {
    fn from(span: Span) -> Self {
        GinSpan { span }
    }
}

impl Deref for GinSpan {
    type Target = Span;

    fn deref(&self) -> &Self::Target {
        &self.span
    }
}

impl GinSpan {
    fn repr(&self) -> String {
        format!("{:?}", self.span)
    }
}

impl PartialEq for GinSpan {
    fn eq(&self, other: &Self) -> bool {
        self.repr() == other.repr()
    }
}

impl Eq for GinSpan {}

impl PartialOrd for GinSpan {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.repr().partial_cmp(&other.repr())
    }
}

impl Ord for GinSpan {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.repr().cmp(&other.repr())
    }
}

impl Hash for GinSpan {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.repr().hash(state);
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub(crate) struct Spanned<T> {
    span: GinSpan,
    item: T,
}

impl<T> Spanned<T> {
    #[allow(unused)]
    pub(crate) fn item(&self) -> &T {
        &self.item
    }

    /// Create a new Spanned by applying a mapper function and annotating it
    /// with the Span from the current Spanned.
    #[allow(unused)]
    pub(crate) fn map<U>(&self, mapper: impl FnOnce(&T) -> U) -> Spanned<U> {
        let new_item = mapper(&self.item);
        Spanned {
            item: new_item,
            span: self.span,
        }
    }

    /// Turn the current Spanned into another data structure by passing the item
    /// and [proc_macro2::Span] to a mapper function. This is useful, for
    /// example, for turning a `Spanned<String>` into an `Ident`.
    pub(crate) fn map_into<U>(self, mapper: impl FnOnce(T, Span) -> U) -> U {
        mapper(self.item, *self.span)
    }
}

pub(crate) trait SpanItem: Sized {
    fn spanned(self, span: Span) -> Spanned<Self>;
}

impl<T> SpanItem for T {
    fn spanned(self, span: Span) -> Spanned<Self> {
        Spanned {
            span: span.into(),
            item: self,
        }
    }
}

/// This macro should take a syn::Result, and must be used in the context of a
/// function that returns a proc_macro::TokenStream or proc_macro::TokenStream2.
#[allow(unused)]
macro_rules! try_syn {
    ($expr:expr) => {
        match $expr {
            Err(err) => return err.into_compile_error(),
            Ok(value) => value,
        }
    };
}

#[allow(unused)]
pub(crate) fn import(package: impl AsRef<str>) -> syn::Result<Ident> {
    import_spanned(package, Span::call_site())
}

#[allow(unused)]
pub(crate) fn gin_crate(package: impl Display) -> syn::Result<TokenStream> {
    using("gin", [package])
}

pub(crate) fn using(
    package: impl AsRef<str>,
    path: impl IntoIterator<Item = impl Display>,
) -> syn::Result<TokenStream> {
    let first = import(package)?;
    let path = path
        .into_iter()
        .map(|segment| Ident::new(&segment.to_string(), Span::call_site()));
    Ok(quote![#first :: macro_internals #(:: #path)*])
}

pub(crate) fn import_spanned(package: impl AsRef<str>, span: Span) -> syn::Result<Ident> {
    let name = match crate_name(package.as_ref()) {
        Ok(val) => val,
        Err(err) => {
            return Err(syn::Error::new(
                span,
                format!("importing {} failed: {}", package.as_ref(), err),
            ))
        }
    };

    match name {
        proc_macro_crate::FoundCrate::Itself => Ok(Ident::new("crate", span)),
        proc_macro_crate::FoundCrate::Name(name) => Ok(Ident::new(&name, span)),
    }
}
