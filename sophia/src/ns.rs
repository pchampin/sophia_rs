//! Standard and custom namespaces.
//!
//! This module provides:
//! * the [`Namespace`](struct.Namespace.html) type for defining custom namespace;
//! * modules corresponding to the most common namespaces.
//!
//! # Example
//! ```
//! use sophia::graph::MutableGraph;
//! use sophia::graph::inmem::FastGraph;
//! use sophia::ns::{Namespace, rdf, rdfs, xsd};
//!
//!
//! let mut g = FastGraph::new();
//!
//! let schema = Namespace::new("http://schema.org/").unwrap();
//! let s_name = schema.get("name").unwrap();
//! g.insert(&s_name, &rdf::type_, &rdf::Property);
//! g.insert(&s_name, &rdfs::range, &xsd::string);
//! ```

use crate::term::{iri_rfc3987::is_valid_iri_ref, Result, Term, TermData, TermError};

/// A custom namespace.
#[derive(Clone, Debug)]
pub struct Namespace<T: TermData>(T);

impl<T: TermData> Namespace<T> {
    /// Build a custom namespace based on the given IRI.
    ///
    /// `iri` must be a valid IRI, othewise this constructor returns an error.
    pub fn new(iri: T) -> Result<Namespace<T>> {
        if is_valid_iri_ref(iri.as_ref()) {
            Ok(Namespace(iri))
        } else {
            Err(TermError::InvalidIri(iri.as_ref().to_string()))
        }
    }

    /// Build an IRI term by appending `suffix` to this namespace.
    ///
    /// Return an error if the concatenation produces an invalid IRI.
    pub fn get<U>(&self, suffix: U) -> Result<Term<T>>
    where
        T: From<U>,
    {
        Term::new_iri2(self.0.clone(), suffix)
    }
}

/// Helper for creating a "namespace module"
/// defining a set of terms within a given IRI space.
///
/// # Safety
/// This macro is conceptually unsafe,
/// as it is never checked that the prefix IRI is a valid IRI reference.
#[macro_export]
macro_rules! namespace {
    ($iri_prefix:expr, $($suffix:ident),*) => {
        pub static PREFIX:&'static str = $iri_prefix;
        $(
            $crate::ns_term!($iri_prefix, $suffix);
        )*
    }
}

/// Helper for creating a term in a "namespace module".
/// In general, you should use the [`namespace!`](macro.namespace.html) macro instead.
///
/// # Safety
/// This macro is conceptually unsafe,
/// as it is never checked that the prefix IRI is a valid IRI reference.
#[macro_export]
macro_rules! ns_term {
    ($prefix:expr, $ident:ident) => {
        $crate::ns_term!($prefix, $ident, stringify!($ident));
    };
    ($prefix:expr, $ident:ident, $suffix:expr) => {
        #[allow(non_upper_case_globals)]
        pub static $ident: $crate::term::StaticTerm = $crate::term::Term::Iri(unsafe {
            $crate::term::IriData::from_raw_parts($prefix, Some($suffix), true)
        });
    };
}

//pub static $ident:term::Term<'static> = term::Term::Iri(term::IriData{ns:$prefix, suffix:$suffix});

/// The standard `rdf:` namespace.
///
/// NB: since `type` is a reserved keyword in Rust,
/// the term `rdf:type` spells `rdf::type_` (with a trailing underscore).
///
pub mod rdf {
    namespace!(
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        // classes
        Alt,
        Bad,
        List,
        PlainLiteral,
        Property,
        Seq,
        Statement,
        // datatypes
        HTML,
        langString,
        XMLLiteral,
        // properties
        first,
        object,
        predicate,
        rest,
        subject,
        value,
        // individuals
        nil,
        // core syntax terms
        RDF,
        ID,
        Description,
        about,
        parseType,
        resource,
        li,
        nodeID,
        datatype,
        bagID,
        aboutEach,
        aboutEachPrefix
    );
    ns_term!("http://www.w3.org/1999/02/22-rdf-syntax-ns#", type_, "type");
}

/// The standard `xsd:` namespace.
#[rustfmt::skip]
pub mod xsd {
    namespace!(
        "http://www.w3.org/2001/XMLSchema#",
        anyType,
        anySimpleType,
            duration,
            dateTime,
            time,
            date,
            gYearMonth,
            gYear,
            gMonthDay,
            gDay,
            gMonth,
            boolean,
            base64Binary,
            hexBinary,
            float,
            double,
            anyURI,
            QName,
            NOTATION,
            string,
                normalizedString,
                    token,
                        language,
                        Name,
                            NCName,
                                ID,
                                IDREF,
                                    IDREFS,
                                ENTITY,
                                    ENTITIES,
                        NMTOKEN,
                        NMTOKENS,
            decimal,
                integer,
                    nonPositiveInteger,
                        negativeInteger,
                    long,
                        int,
                            short,
                                byte,
                    nonNegativeInteger,
                        unsignedLong,
                            unsignedInt,
                                unsignedShort,
                                    unsignedByte,
                        positiveInteger
    );
}

/// The standard `rdfs:` namespace.
pub mod rdfs {
    namespace!(
        "http://www.w3.org/2000/01/rdf-schema#",
        // types
        Class,
        Container,
        ContainerMembershipProperty,
        Datatype,
        Literal,
        Resource,
        // semantic properties
        domain,
        range,
        subClassOf,
        subPropertyOf,
        // documentation properties
        comment,
        isDefinedBy,
        label,
        member,
        seeAlso
    );
}

/// The standard `xml:` namespace
pub mod xml {
    namespace!(
        "http://www.w3.org/XML/1998/namespace#",
        lang,
        space,
        base,
        id,
        // Jon Bosak
        Father
    );
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_same_term() {
        let ns1 = Namespace::new("http://schema.org/").unwrap();
        let ns2 = Namespace::new(Rc::from("http://schema.org/")).unwrap();

        assert_eq!(ns1.get("name").unwrap(), ns1.get("name").unwrap());
        assert_eq!(ns2.get("name").unwrap(), ns2.get("name").unwrap());
        assert_eq!(ns1.get("name").unwrap(), ns2.get("name").unwrap());
    }

    #[test]
    fn test_different_terms() {
        let ns1 = Namespace::new("http://schema.org/").unwrap();
        assert_ne!(ns1.get("name").unwrap(), ns1.get("nam").unwrap());
    }

    #[test]
    fn test_invalid_namespace() {
        assert!(Namespace::new("http://schema.org ").is_err());
    }

    #[test]
    fn test_invalid_suffix() {
        let ns1 = Namespace::new("http://schema.org/").unwrap();
        assert!(ns1.get("name ").is_err());
    }
}
