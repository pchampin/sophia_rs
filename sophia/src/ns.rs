//! This module defines a number of standard namespaces.
macro_rules! namespace {
    ($prefix:expr, $($suffix:ident),*) => {
        pub static PREFIX:&'static str = $prefix;
        $(
            ns_term!($prefix, $suffix);
        )*
    };
}

macro_rules! ns_term {
    ($prefix:expr, $ident:ident) => {
        ns_term!($prefix, $ident, stringify!($ident));
    };
    ($prefix:expr, $ident:ident, $suffix:expr) => {
        #[allow(non_upper_case_globals)]
        pub static $ident:term::StaticTerm =
            term::Term::Iri(
                term::IriData{
                    ns: $prefix,
                    suffix: Some($suffix),
                    absolute: true,
            });
    }
}

//pub static $ident:term::Term<'static> = term::Term::Iri(term::IriData{ns:$prefix, suffix:$suffix});

#[allow(non_upper_case_globals)]
pub mod rdf {
    use ::term;

    namespace!("http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        // classes
        Alt, Bad, List, PlainLiteral, Property, Seq, Statement,
        // datatypes
        HTML, langString, XMLLiteral,
        // properties
        first, object, predicate, rest, subject, value,
        // individuals
        nil
    );
    ns_term!("http://www.w3.org/1999/02/22-rdf-syntax-ns#", type_, "type");
}

#[allow(non_upper_case_globals)]
pub mod xsd {
    use ::term;

    namespace!("http://www.w3.org/2001/XMLSchema#",
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

#[allow(non_upper_case_globals)]
pub mod rdfs {
    use ::term;

    namespace!("http://www.w3.org/2000/01/rdf-schema#",
        Class, Container, ContainerMembershipProperty, Datatype, Literal, Resource,
        domain, range, subClassOf, subPropertyOf,
        comment, isDefinedBy, label, member, seeAlso
    );
}
