//! Standard and custom namespaces.
//!
//! This module provides:
//! * the [`Namespace`](struct.Namespace.html) type for defining custom dynamic namespace;
//! * the [`namespace`](crate::namespace) macro, for defning custom static namespaces;
//! * modules corresponding to the most common namespaces
//!   (generated via the [`namespace`](crate::namespace) macro).
//!
//! # Example use
//! ```
//! use sophia_api::ns::{Namespace, rdf, rdfs, xsd};
//!
//! let schema = Namespace::new("http://schema.org/").unwrap();
//! let s_name = schema.get("name").unwrap();
//!
//! // you can now populate a graph like this:
//! let mut g = vec![];
//! g.push([&s_name, &rdf::type_, &rdf::Property]);
//! g.push([&s_name, &rdfs::range, &xsd::string]);
//! ```
//!
//! # Datatyped literals
//!
//! Note also that the terms generated via the [`namespace`](crate::namespace) macro
//! can be used to easily produce datatyped literals,
//! by simply "multiplying" a string by its datatype:
//!
//! ```
//! # use sophia_api::{term::Term, ns::xsd};
//! let date = "2023-11-15" * xsd::date ;
//! assert!(date.is_literal());
//! assert_eq!(date.lexical_form().unwrap(), "2023-11-15");
//! assert_eq!(date.datatype().unwrap(), xsd::date.iri().unwrap());
//! ```
use mownstr::MownStr;
use sophia_iri::InvalidIri;
use std::borrow::Borrow;
use std::fmt;

// rexport is necessary to ensure that the macros work.
pub use sophia_iri::IriRef;

#[macro_use]
mod _macro;
mod _namespace;
pub use _namespace::*;
mod _term;
pub use _term::*;

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
        Bag,
        List,
        PlainLiteral,
        Property,
        Seq,
        Statement,
        // datatypes
        HTML,
        JSON,
        langString,
        dirLangString,
        XMLLiteral,
        // properties
        direction,
        first,
        language,
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
        aboutEachPrefix;
        // 'type' is a Rust keyword, so we use 'type_' instead
        type_, "type"
    );
}

/// The standard `xsd:` namespace.
#[rustfmt::skip]
pub mod xsd {
    namespace!(
        "http://www.w3.org/2001/XMLSchema#",
        anyType,
        anySimpleType,
            anyAtomicType,
                anyURI,
                base64Binary,
                boolean,
                date,
                dateTime,
                    dateTimeStamp,
                decimal,
                    integer,
                        long,
                            int,
                                short,
                                    byte,
                        nonNegativeInteger,
                            positiveInteger,
                            unsignedLong,
                                unsignedInt,
                                    unsignedShort,
                                        unsignedByte,
                        nonPositiveInteger,
                            negativeInteger,
                double,
                duration,
                    dayTimeDuration,
                    yearMonthDuration,
                float,
                gDay,
                gMonth,
                gMonthDay,
                gYear,
                gYearMonth,
                hexBinary,
                NOTATION,
                QName,
                string,
                    normalizedString,
                        token,
                            language,
                            Name,
                                NCName,
                                    ENTITY,
                                    ID,
                                    IDREF,
                            NMTOKEN,
                time,
            ENTITIES,
            IDREFS,
            NMTOKENS
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

/// The standard `skos:` namespace.
pub mod skos {
    namespace!(
        "http://www.w3.org/2004/02/skos/core#",
        // annotations
        altLabel,
        changeNote,
        definition,
        editorialNote,
        example,
        hiddenLabel,
        historyNote,
        prefLabel,
        scopeNote,
        note,
        // object properties
        exactMatch,
        member,
        memberList,
        relatedMatch,
        broadMatch,
        closeMatch,
        hasTopConcept,
        inScheme,
        narrowMatch,
        related,
        topConceptOf,
        broader,
        broaderTransitive,
        narrower,
        narrowerTransitive,
        mappingRelation,
        semanticRelation,
        // data properties
        notation,
        // classes
        OrderedCollection,
        Collection,
        ConceptScheme,
        Concept
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

/// The standard `owl:` namespace
pub mod owl {
    namespace!(
        "http://www.w3.org/2002/07/owl#",
        Nothing,
        Thing,
        // Classes
        AllDifferent,
        AllDisjointClasses,
        AnnotationProperty,
        Class,
        DatatypeProperty,
        FunctionalProperty,
        InverseFunctionalProperty,
        IrreflexiveProperty,
        ObjectProperty,
        SymmetricProperty,
        TransitiveProperty,
        // Properties
        allValuesFrom,
        assertionProperty,
        complementOf,
        differentFrom,
        disjointWith,
        distinctMembers,
        equivalentClass,
        equivalentProperty,
        intersectionOf,
        inverseOf,
        maxCardinality,
        maxQualifiedCardinality,
        members,
        onClass,
        oneOf,
        onProperty,
        propertyChainAxiom,
        propertyDisjointWith,
        sameAs,
        someValuesFrom,
        sourceIndividual,
        targetIndividual,
        targetValue,
        unionOf
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

        assert_eq!(
            ns1.get("name").unwrap().to_string(),
            ns1.get("name").unwrap().to_string()
        );
        assert_eq!(
            ns2.get("name").unwrap().to_string(),
            ns2.get("name").unwrap().to_string()
        );
        assert_eq!(
            ns1.get("name").unwrap().to_string(),
            ns2.get("name").unwrap().to_string()
        );
    }

    #[test]
    fn test_different_terms() {
        let ns1 = Namespace::new("http://schema.org/").unwrap();
        assert_ne!(
            ns1.get("name").unwrap().to_string(),
            ns1.get("givenName").unwrap().to_string()
        );
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
