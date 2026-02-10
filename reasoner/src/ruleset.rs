//! I define the trait [`RuleSet`], as well as a number of implementations of that trait.
use crate::{ReasonableGraph, d_entailment::Recognized};

mod _simple;
pub use _simple::Simple;
mod _rdf;
pub use _rdf::Rdf;
mod _rdfs;
pub use _rdfs::Rdfs;

/// A [`RuleSet`] implements a specific entailment regime.
pub trait RuleSet: Sized + Send + Sync {
    /// Prepare the internal structure of `graph` before triples are inserted.
    fn prepare<D: Recognized>(graph: &mut ReasonableGraph<D, Self>);
    /// Saturare `graph` with all inferred triples.
    fn saturate<D: Recognized>(graph: &mut ReasonableGraph<D, Self>);
}
