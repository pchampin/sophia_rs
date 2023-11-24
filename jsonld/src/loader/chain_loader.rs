use std::fmt::Debug;

use json_ld::Loader;
use json_ld::{BoxFuture, FutureExt};
use json_syntax::Value;
use locspan::Location;

/// * [`ChainLoader`]: loads document from the first loader, otherwise falls back to the second one.
pub struct ChainLoader<L1, L2>(L1, L2);

impl<L1, L2> ChainLoader<L1, L2> {
    /// Build a new chain loader
    pub fn new(l1: L1, l2: L2) -> Self {
        ChainLoader(l1, l2)
    }
}

impl<I, L1, L2> Loader<I, Location<I>> for ChainLoader<L1, L2>
where
    I: Clone + Send + Sync,
    L1: Loader<I, Location<I>, Output = Value<Location<I>>> + Send,
    L2: Loader<I, Location<I>, Output = Value<Location<I>>> + Send,
    L1::Error: Debug + Send,
    L2::Error: Debug,
{
    type Output = Value<Location<I>>;

    type Error = ChainLoaderError<L1::Error, L2::Error>;

    fn load_with<'a>(
        &'a mut self,
        vocabulary: &'a mut (impl Sync + Send + rdf_types::IriVocabularyMut<Iri = I>),
        url: I,
    ) -> BoxFuture<'a, json_ld::LoadingResult<I, Location<I>, Self::Output, Self::Error>>
    where
        I: 'a,
    {
        async {
            match self.0.load_with(vocabulary, url.clone()).await {
                Ok(doc) => Ok(doc),
                Err(err1) => match self.1.load_with(vocabulary, url).await {
                    Ok(doc) => Ok(doc),
                    Err(err2) => Err(ChainLoaderError(err1, err2)),
                },
            }
        }
        .boxed()
    }
}

/// Error type raised by [`ChainLoader`]
#[derive(thiserror::Error, Debug)]
#[error("Document not found {0}")]
pub struct ChainLoaderError<E1, E2>(E1, E2)
where
    E1: Debug,
    E2: Debug;
