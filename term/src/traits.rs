//! Abstractions for using terms and the various sub-types as trait-objects.
//!
//! _Note:_ This is a PoC implementation. Therefore, it covers only IRIs,
//! literals and blank nodes. If this concept gets accepted the implementation
//! should also cover variables.

use super::*;

/// Interface of RDF terms.
pub trait Term {
    /// Downcast to an IRI.
    fn as_iri(&self) -> Option<IriView<'_>> {
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
    /// The whole IRI.
    ///
    /// If the IRI is suffixed this may allocate a new string.
    /// 
    /// Has a default implementation that builds from [`raw()`](#method.raw).
    fn value(&self) -> MownStr<'_> {
        self.raw().into()
    }
    /// Returns the raw representation of the IRI that is maybe split into
    /// namespace and suffix.
    fn raw(&self) -> RawIri<'_>;
}

impl<'a> PartialEq for &'a dyn Iri {
    fn eq(&self, other: &Self) -> bool {
        // could be improved
        self.value() == other.value()
    }
}

#[derive(Debug, Clone, Copy)]
/// An IRI split into namespace and optional a suffix.
/// 
/// This representation of an IRI might be used by some implementations.
pub struct RawIri<'a>(&'a str, Option<&'a str>);

impl<'a> From<(&'a str, Option<&'a str>)> for RawIri<'a> {
    fn from(raw: (&'a str, Option<&'a str>)) -> Self {
        RawIri(raw.0, raw.1)
    }
}

impl<'a> Into<MownStr<'a>> for RawIri<'a> {
    fn into(self) -> MownStr<'a> {
        if let Some(suffix) = self.1 {
            format!("{}{}", self.0, suffix).into()
        } else {
            self.0.into()
        }
    }
}

impl<'a> Iri for RawIri<'a> {
    fn raw(&self) -> RawIri<'_> {
        self.clone()
    }
}

/// Result from downcasting a term into an IRI.
pub struct IriView<'src>(&'src dyn Iri);

impl<'src> Iri for IriView<'src> {
    fn raw(&self) -> RawIri<'_> {
        self.0.raw()
    }
}

impl<'src, I> From<&'src I> for IriView<'src> 
where
    I: Iri,
{
    fn from(iri: &'src I) -> Self {
        IriView(iri)
    }
}

impl<'src> PartialEq for IriView<'src> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
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
    fn dt(&self) -> IriView;
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
