//! I provide a cheap-to-clone GraphNameMatcher

use std::sync::{Arc, Mutex};

use sophia_api::term::{IriRef, matcher::GraphNameMatcher};
use sophia_term::{ArcStrStash, ArcTerm};
use spargebra::algebra::QueryDataset;

#[derive(Clone, Debug)]
pub struct GraphMatcher(Arc<[Option<ArcTerm>]>);

impl GraphMatcher {
    pub fn from_with(value: &Option<QueryDataset>, mstash: &Mutex<ArcStrStash>) -> Self {
        match value {
            None => Self(Arc::from([None])),
            Some(query_dataset) => {
                let mut stash = mstash.lock().unwrap();
                Self(
                    query_dataset
                        .default
                        .iter()
                        .map(|nn| {
                            Some(ArcTerm::Iri(IriRef::new_unchecked(
                                stash.copy_str(nn.as_str()),
                            )))
                        })
                        .collect(),
                )
            }
        }
    }
}

impl From<[Option<ArcTerm>; 1]> for GraphMatcher {
    fn from(value: [Option<ArcTerm>; 1]) -> Self {
        Self(Arc::new(value))
    }
}

impl GraphNameMatcher for GraphMatcher {
    type Term = ArcTerm;

    fn matches<T2: sophia_api::prelude::Term + ?Sized>(
        &self,
        graph_name: sophia_api::term::GraphName<&T2>,
    ) -> bool {
        (&self.0[..]).matches(graph_name)
    }

    fn constant(&self) -> Option<sophia_api::term::GraphName<&Self::Term>> {
        if self.0.len() == 1 {
            Some(self.0[0].as_ref())
        } else {
            None
        }
    }
}

impl std::ops::Deref for GraphMatcher {
    type Target = [Option<ArcTerm>];

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}
