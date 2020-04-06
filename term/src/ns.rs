//! # Standard and custom namespaces
//!
//! This module provides:
//! * exported macros for defining custom namespace;
//! * and modules using these macros to define the most common namespaces.
//!
//! If you want to create new IRIs based on a namespace you can use [`Iri`]
//! directly. An example is given at the [`with_suffix()`] method.
//!
//! [`Iri`]: (../iri/struct.Iri.html)
//! [`with_suffix()`]: (../iri/struct.Iri.html#method.with_suffix)

/// Helper for creating a "namespace module"
/// defining a set of terms within a given IRI space.
///
/// # Safety
/// This macro is conceptually unsafe,
/// as it is never checked that the prefix IRI is a valid IRI reference.
#[macro_export]
macro_rules! namespace {
    ($iri_prefix:expr, $($suffix:ident),*; $($r_id:ident, $r_sf:expr),*) => {
        /// Prefix used in this namespace.
        pub static PREFIX:&'static str = $iri_prefix;
        $(
            $crate::ns_term!($iri_prefix, $suffix);
        )*
        $(
            $crate::ns_term!($iri_prefix, $r_id, $r_sf);
        )*

        /// Version of the terms in this namespace as `Iri`s.
        pub mod iri {
            $(
                $crate::ns_iri!($iri_prefix, $suffix);
            )*
            $(
                $crate::ns_iri!($iri_prefix, $r_id, $r_sf);
            )*
        }
    };
    ($iri_prefix:expr, $($suffix:ident),*) => {
        /// Prefix used in this namespace.
        pub static PREFIX:&'static str = $iri_prefix;
        $(
            $crate::ns_term!($iri_prefix, $suffix);
        )*

        /// Version of the terms in this namespace as `Iri`s.
        pub mod iri {
            $(
                $crate::ns_iri!($iri_prefix, $suffix);
            )*
        }
    };
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
        /// Generated term.
        #[allow(non_upper_case_globals)]
        pub static $ident: $crate::StaticTerm = $crate::Term::Iri(
            $crate::iri::Iri::from_raw_parts_unchecked($prefix, Some($suffix), true),
        );
    };
}

/// Helper for creating a term in a "namespace module".
/// In general, you should use the [`namespace!`](macro.namespace.html) macro instead.
///
/// # Safety
/// This macro is conceptually unsafe,
/// as it is never checked that the prefix IRI is a valid IRI reference.
#[macro_export]
macro_rules! ns_iri {
    ($prefix:expr, $ident:ident) => {
        $crate::ns_iri!($prefix, $ident, stringify!($ident));
    };
    ($prefix:expr, $ident:ident, $suffix:expr) => {
        /// Generated IRI.
        #[allow(non_upper_case_globals)]
        pub static $ident: $crate::iri::Iri<&'static str> =
            $crate::iri::Iri::from_raw_parts_unchecked($prefix, Some($suffix), true);
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
        Bag,
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
        aboutEachPrefix;
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
}
