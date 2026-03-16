//! Utility trait to ease the loading of JSON-LD Contexts

use std::borrow::Borrow;
use std::sync::Arc;

use json_ld::{ExtractContext, RemoteContextReference, RemoteDocument};
use json_syntax::Parse;
use sophia_iri::Iri;

use crate::vocabulary::ArcIri;

/// Type alias for the context references used by the JSON-LD options.
pub type ContextRef = RemoteContextReference<ArcIri>;

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
        ContextRef::Iri(self.map_unchecked(|t| Arc::from(t.borrow())))
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
        let (doc, _) = json_ld::syntax::Value::parse_str(self)?;
        let context = doc
            .into_ld_context()
            .map_err(|e| format!("Could not extract @context: {e}"))?;
        let rdoc = RemoteDocument::new(None, None, context);
        Ok(ContextRef::Loaded(rdoc))
    }
}
