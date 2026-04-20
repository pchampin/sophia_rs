//! I provide a cheap-to-clone GraphNameMatcher

use std::sync::{Arc, Mutex};

use sophia_api::term::{IriRef, matcher::GraphNameMatcher};
use sophia_term::{ArcStrStash, ArcTerm};
use spargebra::algebra::QueryDataset;

#[derive(Clone, Debug, Default)]
pub struct GraphMatcher(Option<Arc<GraphMatcherInner>>);

impl GraphMatcher {
    pub fn from_with(value: &Option<QueryDataset>, mstash: &Mutex<ArcStrStash>) -> Self {
        match value {
            None => Self(None),
            Some(query_dataset) => {
                let mut stash = mstash.lock().unwrap();
                let default = query_dataset
                    .default
                    .iter()
                    .map(|nn| {
                        Some(ArcTerm::Iri(IriRef::new_unchecked(
                            stash.copy_str(nn.as_str()),
                        )))
                    })
                    .collect();
                let named = query_dataset
                    .named
                    .iter()
                    .flatten()
                    .map(|nn| ArcTerm::Iri(IriRef::new_unchecked(stash.copy_str(nn.as_str()))))
                    .collect();
                Self(Some(Arc::new(GraphMatcherInner { default, named })))
            }
        }
    }

    pub fn default_graphs(&self) -> &[Option<ArcTerm>] {
        match &self.0 {
            None => &[None],
            Some(inner) => &inner.default[..],
        }
    }

    pub fn named_graphs(&self) -> Option<&[ArcTerm]> {
        self.0.as_ref().map(|i| &i.named[..])
    }
}

impl From<[Option<ArcTerm>; 1]> for GraphMatcher {
    fn from(value: [Option<ArcTerm>; 1]) -> Self {
        Self(Some(Arc::new(GraphMatcherInner {
            default: value.to_vec(),
            named: vec![],
        })))
    }
}

impl GraphNameMatcher for GraphMatcher {
    type Term = ArcTerm;

    fn matches<T2: sophia_api::prelude::Term + ?Sized>(
        &self,
        graph_name: sophia_api::term::GraphName<&T2>,
    ) -> bool {
        match &self.0 {
            None => graph_name.is_none(),
            Some(inner) => (&inner.default[..]).matches(graph_name),
        }
    }

    fn constant(&self) -> Option<sophia_api::term::GraphName<&Self::Term>> {
        match &self.0 {
            None => Some(None),
            Some(inner) if inner.default.len() == 1 => Some(inner.default[0].as_ref()),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
struct GraphMatcherInner {
    default: Vec<Option<ArcTerm>>,
    named: Vec<ArcTerm>,
}
