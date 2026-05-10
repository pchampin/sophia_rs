use iref::{Iri, IriBuf};
use json_ld::{LoadError, Loader, RemoteDocument};

/// * [`ChainLoader`]: loads document from the first loader, otherwise falls back to the second one.
#[derive(Clone, Debug, Default)]
pub struct ChainLoader<L1, L2>(L1, L2);

impl<L1, L2> ChainLoader<L1, L2> {
    /// Build a new chain loader
    pub const fn new(l1: L1, l2: L2) -> Self {
        Self(l1, l2)
    }
}

impl<L1, L2> Loader for ChainLoader<L1, L2>
where
    L1: Loader + Sync,
    L2: Loader + Sync,
{
    async fn load(&self, url: &Iri) -> Result<RemoteDocument<IriBuf>, LoadError> {
        match self.0.load(url).await {
            Ok(doc) => Ok(doc),
            Err(_) => self.1.load(url).await,
        }
    }
}
