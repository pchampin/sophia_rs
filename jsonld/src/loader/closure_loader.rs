use super::{Arc, Iri, Value};
use json_ld::future::{BoxFuture, FutureExt};
use json_ld::{Loader, RemoteDocument};
use json_syntax::Parse;
use locspan::{Location, Meta};
use rdf_types::IriVocabulary;
use std::fmt;

/// Closure loader.
///
/// This is a special JSON-LD document loader that relies on an arbitrary function to load JSON-LD documents.
pub struct ClosureLoader<F> {
    closure: F,
}

impl<'f, F> Loader<Iri<Arc<str>>, Location<Iri<Arc<str>>>> for ClosureLoader<F>
where
    F: Send + FnMut(Iri<String>) -> BoxFuture<'f, Result<String, String>>,
{
    type Output = Value<Location<Iri<Arc<str>>>>;
    type Error = ClosureLoaderError;

    fn load_with<'a>(
        &'a mut self,
        vocabulary: &'a mut (impl Sync + Send + IriVocabulary<Iri = Iri<Arc<str>>>),
        url: Iri<Arc<str>>,
    ) -> BoxFuture<
        'a,
        Result<
            RemoteDocument<Iri<Arc<str>>, Location<Iri<Arc<str>>>, Value<Location<Iri<Arc<str>>>>>,
            Self::Error,
        >,
    >
    where
        Iri<Arc<str>>: 'a,
    {
        async move {
            let iri = vocabulary.iri(&url).unwrap();
            let url_str = iri.as_str().to_string();
            let content = (self.closure)(Iri::new_unchecked(url_str))
                .await
                .map_err(Self::Error::Internal)?;
            let doc = json_syntax::Value::parse_str(content.as_str(), |span| {
                locspan::Location::new(url.clone(), span)
            })
            .map_err(Self::Error::Parse)?;
            Ok(RemoteDocument::new(
                Some(url),
                Some("application/ld+json".parse().unwrap()),
                doc,
            ))
        }
        .boxed()
    }
}

impl<'f, F> ClosureLoader<F>
where
    F: Send + FnMut(Iri<String>) -> BoxFuture<'f, Result<String, String>>,
{
    /// Creates a new closure loader with the given closure.
    pub const fn new(f: F) -> Self {
        Self { closure: f }
    }
}

/// Loading error.
#[derive(Debug)]
pub enum ClosureLoaderError {
    /// Error raised by the inner closure
    Internal(String),

    /// Parse error.
    Parse(JsonParseError),
}

impl fmt::Display for ClosureLoaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Internal(e) => e.fmt(f),
            Self::Parse(e) => e.fmt(f),
        }
    }
}

type JsonParseError =
    Meta<json_syntax::parse::Error<Location<Iri<Arc<str>>>>, Location<Iri<Arc<str>>>>;
