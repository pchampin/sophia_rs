//! Abstractions for using terms and the various sub-types as trait-objects.
//!
//! _Note:_ This is a PoC implementation. Therefore, it covers only IRIs,
//! literals and blank nodes. If this concept gets accepted the implementation
//! should also cover variables.

use super::*;

/// Interface of RDF terms.
pub trait Term {
    /// Downcast to an IRI.
    fn as_iri(&self) -> Option<&dyn Iri> {
        None
    }
    /// Downcast to a literal.
    fn as_literal(&self) -> Option<&dyn Literal> {
        None
    }
    /// Downcast to a blank node.
    fn as_blank_node(&self) -> Option<&dyn BlankNode> {
        None
    }
}

impl<'a> PartialEq for &'a dyn Term {
    fn eq(&self, other: &Self) -> bool {
        if let (Some(siri), Some(oiri)) = (self.as_iri(), other.as_iri()) {
            siri == oiri
        } else if let (Some(slit), Some(olit)) = (self.as_literal(), other.as_literal()) {
            slit == olit
        } else if let (Some(sbn), Some(obn)) = (self.as_blank_node(), other.as_blank_node()) {
            sbn == obn
        } else {
            false
        }
    }
}

/// Interface of IRIs.
///
/// Applies to `sophia`'s IRIs that are represented as namespace and
/// (optionally) a suffix.
pub trait Iri {
    /// The namespace of the IRI.
    ///
    /// The full IRI if suffix is `None`.
    fn ns(&self) -> &str;
    /// The suffix of the IRI.
    fn suffix(&self) -> Option<&str> {
        None
    }
    /// The whole IRI.
    ///
    /// If the IRI is suffixed this may allocate a new string.
    fn whole(&self) -> MownStr<'_>;
}

impl<'a> PartialEq for &'a dyn Iri {
    fn eq(&self, other: &Self) -> bool {
        // could be improved
        self.whole() == other.whole()
    }
}

/// Interface of RDF literals.
pub trait Literal {
    /// The lexical value of the literal.
    fn txt(&self) -> &str;
    /// The datatype of this literal.
    ///
    /// If the literal is language-tagged the datatype is `rdf:langString` as
    /// defined by the spec.
    fn dt(&self) -> &dyn Iri;
    /// Returns the literals language tag if it is language-tagged.
    fn lang(&self) -> Option<&str>;
}

impl<'a> PartialEq for &'a dyn Literal {
    fn eq(&self, other: &Self) -> bool {
        if self.txt() == other.txt() {
            if let (Some(stag), Some(otag)) = (self.lang(), other.lang()) {
                stag == otag
            } else {
                self.dt() == other.dt()
            }
        } else {
            false
        }
    }
}

/// Interface of blank nodes.
pub trait BlankNode {
    /// The blank node's identifier (without the typical leading `_:`).
    fn id(&self) -> &str;
}

impl<'a> PartialEq for &'a dyn BlankNode {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}
