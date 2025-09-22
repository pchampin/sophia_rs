use super::iso_term::{IsoTerm};
use sophia_api::quad::Quad;
use sophia_api::{
    dataset::{DTerm, Dataset},
    quad::Spog,
    source::{
        StreamError::{SinkError, SourceError},
        StreamResult,
    },
};
use sophia_c14n::hash::{HashFunction, Sha256};
use sophia_c14n::sophia::normalize;
use sophia_c14n::C14nError;
use std::collections::BTreeSet;

/// Computes whether two datasets are isomorphic.
///
/// # Error
/// If an error occurs while traversing `d1`,
/// a [`SourceError`](sophia_api::source::StreamError::SourceError`) is returned.
///
/// /// If an error occurs while traversing `d2`,
/// a [`SinkError`](sophia_api::source::StreamError::SinkError`) is returned.
pub fn isomorphic_datasets<D1, D2>(d1: &D1, d2: &D2) -> StreamResult<bool, C14nError<D1::Error>, C14nError<D2::Error>>
where
    D1: Dataset,
    D2: Dataset,
{
    let d1 = prepare_dataset(d1).map_err(SourceError)?;
    let d2 = prepare_dataset(d2).map_err(SinkError)?;

    // Datasets must have the same size
    if d1.len() != d2.len() {
        return Ok(false);
    }

    let hash1 = {
        let mut input = Sha256::initialize();
        normalize(&d1, input.as_write()).map_err(C14nError::cast).map_err(SourceError)?;
        input.finalize()
    };
    let hash2 = {
        let mut input = Sha256::initialize();
        normalize(&d2, input.as_write()).map_err(C14nError::cast).map_err(SinkError)?;
        input.finalize()
    };
    Ok(hash1 == hash2)
}

/// Ensure that we have a SetDataset (required by c14n algorithms)
fn prepare_dataset<D: Dataset>(d: &D) -> Result<PreparedDataset<'_, D>, C14nError<D::Error>> {
    d.quads()
        .map(|res| {
            res.map(|q| {
                let (spo, g) = q.to_spog();
                (spo.map(IsoTerm), g.map(IsoTerm))
            })
                .map_err(Into::into)
        })
        .collect()
}

type PreparedDataset<'a, D> = BTreeSet<Spog<IsoTerm<DTerm<'a, D>>>>;
