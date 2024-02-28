//! Utility trait to ease the loading of JSON-LD Contexts

use std::{borrow::Borrow, sync::Arc};

use json_ld::{
    syntax::{context::Value as ContextValue, Value},
    ExtractContext, RemoteDocument, RemoteDocumentReference,
};
use json_syntax::Parse;
use locspan::{Location, Span};
use sophia_iri::Iri;

use crate::vocabulary::ArcIri;

/// Type alias for the context references used by the JSON-LD options.
pub type ContextRef =
    RemoteDocumentReference<ArcIri, Location<ArcIri, Span>, ContextValue<Location<ArcIri, Span>>>;

/// Anything that can be turned into a [`ContextRef`]
pub trait IntoContextRef {
    /// Turn it into a [`ContextRef`]
    fn into_context_ref(self) -> ContextRef;
}

impl IntoContextRef for ContextRef {
    fn into_context_ref(self) -> ContextRef {
        self
    }
}

impl<T: Borrow<str>> IntoContextRef for Iri<T> {
    fn into_context_ref(self) -> ContextRef {
        RemoteDocumentReference::Iri(self.map_unchecked(|t| Arc::from(t.borrow())))
    }
}

/// Anything that can be turned into a [`ContextRef`] but may fail
pub trait TryIntoContextRef {
    /// Raised when the conversion to [`ContextRef`] fails
    type Error;
    /// Try to turn it into a [`ContextRef`]
    fn try_into_context_ref(self) -> Result<ContextRef, Self::Error>;
}

impl TryIntoContextRef for &str {
    type Error = Box<dyn std::error::Error>;

    fn try_into_context_ref(self) -> Result<ContextRef, Self::Error> {
        let iri = ArcIri::new_unchecked("x-string://".into());
        let doc = Value::parse_str(self, |span| locspan::Location::new(iri.clone(), span))?;
        let context = Value::extract_context(doc)
            .map_err(|e| format!("Could not extract @context: {}", e))?;
        let rdoc = RemoteDocument::new(None, None, context);
        Ok(RemoteDocumentReference::Loaded(rdoc))
    }
}
