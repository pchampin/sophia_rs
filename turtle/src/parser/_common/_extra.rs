use sophia_api::prefix::Prefix;
use sophia_iri::{Iri, resolve::BaseIriRef};

/// Extra parser attributes for T-style formats (Turte, TriG)
#[allow(clippy::type_complexity)]
#[derive(Clone, Debug)]
pub(crate) struct Extra<S> {
    /// Base IRI, if provided
    pub base: Option<BaseIriRef<Box<str>>>,
    /// Prefix map
    pub prefixes: Vec<(Prefix<Box<str>>, Iri<Box<str>>)>,
    /// Counter for blank node label generator
    pub bnode_id: usize,
    /// Stack of states
    pub state: Vec<S>,
}

impl<S> Default for Extra<S> {
    fn default() -> Self {
        Self {
            base: None,
            prefixes: vec![],
            bnode_id: 0,
            state: vec![],
        }
    }
}
