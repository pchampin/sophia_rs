use std::{fmt::Debug, pin::Pin, sync::Arc};

use futures_lite::{Future, FutureExt};
use iref::IriBuf;
use json_ld::{Loader, RemoteDocument};
use json_syntax::{Parse, Value};
use locspan::{Location, Meta};
use sophia_iri::{Iri, IsIri};
use surf::http::headers::HeaderValues;

type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + Send + 'a>>;
type JsonVal<I> = Value<Location<I>>;
type MetaVal<I> = Meta<JsonVal<I>, Location<I>>;

/// A document loader that loads documents directly from the web.
///
/// It implements a naive cache mechanism:
/// the N most recently loaded documents are stored in memory,
/// and retrieved from there if they are further requested.
///
/// See https://www.w3.org/TR/json-ld11/#privacy
///
/// NB: The type parameter `I` for IRI-indexes is bound to `IsIri`,
/// meaning that they must allow the IRI to be retrieved without resorting on a [`Vocabulary`](rdf_types::Vocabulary).
/// This restriction is required because cached values use indexes,
/// and that we can not guarantee that the same vocabulary will be used each time a cached value is returned.
pub struct SurfLoader<I = Iri<Arc<str>>> {
    client: surf::Client,
    cache: Vec<(String, MetaVal<I>)>,
    cache_limit: usize,
}

impl<I: Clone + IsIri> SurfLoader<I> {
    /// Creates a new [`SurfLoader`]
    pub fn new(cache_limit: usize) -> Self {
        Self {
            client: surf::Client::new().with(surf::middleware::Redirect::new(5)),
            cache: vec![],
            cache_limit,
        }
    }

    pub(crate) async fn get_with(
        &mut self,
        _vocabulary: &mut (impl Sync + Send + rdf_types::IriVocabularyMut<Iri = I>),
        iri_index: &I,
    ) -> Result<MetaVal<I>, SurfLoaderError<I>> {
        let iri_str = iri_index.borrow();
        if let Some(ret) = self.get_from_cache(iri_str) {
            return Ok(ret);
        }

        //let req = surf::get(iri)
        //    .header("accept", "application/ld+json;application/json,q=0.5;*/*,q=0.1");
        let mut resp = self
            .client
            .get(iri_str)
            .header(
                "accept",
                "application/ld+json;application/json,q=0.5;*/*,q=0.1",
            )
            .await?;
        if let Some(hvalues) = resp.header("link") {
            if let Some(alt) = find_alternate(hvalues, iri_str)? {
                resp = self.client.get(alt).await?;
            }
        }
        let status = resp.status();
        if !status.is_success() {
            return Err(status.into());
        };
        let body = resp.body_string().await?;
        match Value::parse_str(&body, |span| Location::new(iri_index.clone(), span)) {
            Ok(meta) => {
                self.store_in_cache(iri_str, &meta);
                Ok(meta)
            }
            Err(meta) => Err(meta.0.into()),
        }
    }

    fn get_from_cache(&mut self, iri: &str) -> Option<MetaVal<I>> {
        let mut pos = usize::MAX;
        let mut ret = None;
        for (i, (k, v)) in self.cache.iter().enumerate() {
            if k == iri {
                pos = i;
                ret = Some(v.clone());
                break;
            }
        }
        if ret.is_some() {
            self.cache[..=pos].rotate_right(1);
        };
        ret
    }

    fn store_in_cache(&mut self, iri: &str, meta: &MetaVal<I>) {
        self.cache.push((iri.into(), meta.clone()));
        self.cache.rotate_right(1);
        if self.cache.len() > self.cache_limit {
            self.cache.truncate(self.cache_limit);
        }
    }
}

impl<I: Clone + IsIri + Send + Sync> Loader<I, Location<I>> for SurfLoader<I> {
    type Output = JsonVal<I>;

    type Error = SurfLoaderError<I>;

    fn load_with<'a>(
        &'a mut self,
        vocabulary: &'a mut (impl Sync + Send + rdf_types::IriVocabularyMut<Iri = I>),
        url: I,
    ) -> BoxFuture<'a, json_ld::LoadingResult<I, Location<I>, Self::Output, Self::Error>>
    where
        I: 'a,
    {
        async move {
            let val = self.get_with(vocabulary, &url).await?;
            Ok(RemoteDocument::new(
                Some(url),
                Some("application/ld+json".parse().unwrap()),
                val,
            ))
        }
        .boxed()
    }
}

/// Error type raised by [`SurfLoader`]
#[derive(thiserror::Error, Debug)]
pub enum SurfLoaderError<I> {
    /// HTTP error
    #[error("HTTP error: {0}")]
    Surf(surf::Error),
    /// Error in Link header
    #[error("Error in Link header: {0}")]
    Link(#[from] http_link::ParseLinkError),
    /// JSON error
    #[error("JSON error: {0}")]
    Json(#[from] json_syntax::parse::Error<locspan::Location<I>>),
}

impl<I> From<surf::Error> for SurfLoaderError<I> {
    fn from(value: surf::Error) -> Self {
        Self::Surf(value)
    }
}

impl<I> From<surf::StatusCode> for SurfLoaderError<I> {
    fn from(value: surf::StatusCode) -> Self {
        let msg = format!("{} {}", value, value.canonical_reason());
        Self::Surf(surf::Error::from_str(value, msg))
    }
}

fn find_alternate<I>(
    hvalues: &HeaderValues,
    iri: &str,
) -> Result<Option<IriBuf>, SurfLoaderError<I>> {
    let base = url::Url::parse(iri).unwrap();
    for hvalue in hvalues {
        let links = http_link::parse_link_header(hvalue.as_str(), &base)?;
        for link in links {
            println!("{:?}", link);
            if link.rel == "alternate" {
                for param in &link.attributes {
                    if param.name == "type" {
                        if param.value == "application/ld+json" {
                            println!("=== redirecting to {}", link.target);
                            return Ok(Some(IriBuf::from_string(link.target.into()).unwrap()));
                        } else {
                            break;
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}
