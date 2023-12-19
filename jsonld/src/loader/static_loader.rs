use std::{collections::HashMap, fmt::Debug};

use json_ld::future::{BoxFuture, FutureExt};
use json_ld::{Loader, RemoteDocument};
use json_syntax::Value;
use locspan::{Location, Meta};
use sophia_iri::IsIri;

type JsonVal<I, S> = Value<Location<I, S>>;
type MetaVal<I, S> = Meta<JsonVal<I, S>, Location<I, S>>;

/// A document loader that stores a selected set of documents in memory.
/// This is useful for stable (e.g. normative) contexts that are expected to be used a lot.
///
/// See <https://www.w3.org/TR/json-ld11/#privacy>
///
/// NB: The type parameter `I` for IRI-indexes is bound to `IsIri`,
/// meaning that they must allow the IRI to be retrieved without resorting on a [`Vocabulary`](rdf_types::Vocabulary).
/// This restriction is required because cached values use indexes,
/// and that we can not guarantee that the same vocabulary will be used each time a cached value is returned.
#[derive(Clone, Debug)]
pub struct StaticLoader<I, S> {
    cache: HashMap<String, MetaVal<I, S>>,
}

impl<I, S> Default for StaticLoader<I, S> {
    fn default() -> Self {
        Self {
            cache: Default::default(),
        }
    }
}

impl<I: IsIri, S> StaticLoader<I, S> {
    /// Creates a new [`StaticLoader`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Extend this [`StaticLoader`] with another document
    pub fn with(mut self, url: I, document: MetaVal<I, S>) -> Self {
        self.cache.insert(url.borrow().into(), document);
        self
    }
}

impl<I, S> Loader<I, Location<I, S>> for StaticLoader<I, S>
where
    I: Clone + IsIri + Send,
    S: Clone + Send,
{
    type Output = JsonVal<I, S>;

    type Error = StaticLoaderError;

    fn load_with<'a>(
        &'a mut self,
        _vocabulary: &'a mut (impl Sync + Send + rdf_types::IriVocabularyMut<Iri = I>),
        url: I,
    ) -> BoxFuture<'a, json_ld::LoadingResult<I, Location<I, S>, Self::Output, Self::Error>>
    where
        I: 'a,
    {
        async move {
            self.cache
                .get(url.borrow())
                .ok_or_else(|| StaticLoaderError::NotFound(url.borrow().into()))
                .map(|val| {
                    RemoteDocument::new(
                        Some(url),
                        Some("application/ld+json".parse().unwrap()),
                        val.clone(),
                    )
                })
        }
        .boxed()
    }
}

/// Error type raised by [`StaticLoader`]
#[derive(thiserror::Error, Debug)]
pub enum StaticLoaderError {
    /// Document not found
    #[error("Document not found {0}")]
    NotFound(String),
}
