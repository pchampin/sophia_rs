use crate::{Inconsistency, ReasonableGraph, d_entailment::Recognized, ruleset::RuleSet};

/// A [`RuleSet`] for [Simple semantics](https://www.w3.org/TR/rdf12-semantics/#simple)
pub struct Simple;

impl RuleSet for Simple {
    fn prepare<D: Recognized>(_graph: &mut ReasonableGraph<D, Self>) {}
    fn saturate<D: Recognized>(_graph: &mut ReasonableGraph<D, Self>) -> Result<(), Inconsistency> {
        Ok(())
    }
}
