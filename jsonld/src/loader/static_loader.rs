use std::collections::HashMap;

use iref::{Iri, IriBuf};
use json_ld::{LoadError, Loader, RemoteDocument};
use json_syntax::Value;

/// A document loader that stores a selected set of documents in memory.
/// This is useful for stable (e.g. normative) contexts that are expected to be used a lot.
///
/// See <https://www.w3.org/TR/json-ld11/#privacy>
#[derive(Clone, Debug, Default)]
pub struct StaticLoader {
    cache: HashMap<String, Value>,
}

impl StaticLoader {
    /// Creates a new [`StaticLoader`]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Extend this [`StaticLoader`] with another document
    pub fn with(mut self, url: &Iri, document: Value) -> Self {
        self.cache.insert(url.as_str().to_string(), document);
        self
    }
}

impl Loader for StaticLoader {
    async fn load(&self, url: &Iri) -> Result<RemoteDocument<IriBuf>, LoadError> {
        self.cache
            .get(url.as_str())
            .ok_or_else(|| {
                LoadError::new(
                    url.to_owned(),
                    StaticLoaderError::NotFound(url.as_str().to_string()),
                )
            })
            .map(|val| {
                RemoteDocument::new(
                    Some(url.to_owned()),
                    Some("application/ld+json".parse().unwrap()),
                    val.clone(),
                )
            })
    }
}

/// Error type raised by [`StaticLoader`]
#[derive(thiserror::Error, Debug)]
pub enum StaticLoaderError {
    /// Document not found
    #[error("Document not found {0}")]
    NotFound(String),
}
