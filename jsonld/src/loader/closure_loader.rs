use iref::{Iri, IriBuf};
use json_ld::{LoadError, Loader, RemoteDocument};
use json_syntax::Parse;
use std::future::Future;
use std::pin::Pin;

type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + Send + 'a>>;

/// Closure loader.
///
/// This is a special JSON-LD document loader that relies on an arbitrary function to load JSON-LD documents.
pub struct ClosureLoader<F> {
    closure: F,
}

impl<F> Loader for ClosureLoader<F>
where
    F: Send + Sync + for<'a> Fn(sophia_iri::Iri<&'a str>) -> BoxFuture<'a, Result<String, String>>,
{
    async fn load(&self, url: &Iri) -> Result<RemoteDocument<IriBuf>, LoadError> {
        let content = (self.closure)(sophia_iri::Iri::new_unchecked(url.as_str()))
            .await
            .map_err(|e| LoadError::new(url.to_owned(), ClosureLoaderError::Internal(e)))?;
        let (doc, _) = json_syntax::Value::parse_str(&content).map_err(|e| {
            LoadError::new(url.to_owned(), ClosureLoaderError::Parse(format!("{e}")))
        })?;
        Ok(RemoteDocument::new(
            Some(url.to_owned()),
            Some("application/ld+json".parse().unwrap()),
            doc,
        ))
    }
}

impl<F> ClosureLoader<F>
where
    F: Send + Sync + for<'a> Fn(sophia_iri::Iri<&'a str>) -> BoxFuture<'a, Result<String, String>>,
{
    /// Creates a new closure loader with the given closure.
    pub const fn new(f: F) -> Self {
        Self { closure: f }
    }
}

/// Loading error.
#[derive(Debug, thiserror::Error)]
pub enum ClosureLoaderError {
    /// Error raised by the inner closure
    #[error("{0}")]
    Internal(String),

    /// Parse error.
    #[error("parse error: {0}")]
    Parse(String),
}
