//! General-use types and traits from the Sophia API.

pub use crate::dataset::{Dataset, MutableDataset};
pub use crate::graph::{Graph, MutableGraph};
pub use crate::parser::{QuadParser, TripleParser};
pub use crate::quad::Quad;
pub use crate::serializer::{QuadSerializer, Stringifier, TripleSerializer};
pub use crate::source::{QuadSource, StreamResultExt, TripleSource};
pub use crate::sparql::{SparqlBindings, SparqlDataset};
pub use crate::term::{matcher::Any, Term, TermKind};
pub use crate::triple::Triple;

pub use sophia_iri::{Iri, IriRef};
