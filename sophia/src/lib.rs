//! This meta-crate aims to provide a comprehensive toolkit
//! for working with [RDF] and [Linked Data] in Rust.
//!
//! It provides a unified access to a number of smaller crates,
//! that make the Sophia toolkit:
//!
//! * [`api`]
//! * [`c14n`]
//! * [`inmem`]
//! * [`iri`]
//! * [`isomorphism`]
//! * [`jsonld`] (with the `jsonld` feature enabled)
//! * [`resource`]
//! * [`turtle`]
//! * [`term`]
//! * [`xml`] (with the `xml` feature enabled)
//!
//! # Getting Started
//!
//! See the [Sophia book](https://pchampin.github.io/sophia_rs/ch01_getting_started.html)
//!
//!
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/
#![deny(missing_docs)]

#[doc(inline)]
pub use sophia_api as api;
#[doc(inline)]
pub use sophia_c14n as c14n;
#[doc(inline)]
pub use sophia_inmem as inmem;
#[doc(inline)]
pub use sophia_iri as iri;
#[doc(inline)]
pub use sophia_isomorphism as isomorphism;
#[cfg(feature = "jsonld")]
#[doc(inline)]
pub use sophia_jsonld as jsonld;
#[doc(inline)]
pub use sophia_resource as resource;
#[doc(inline)]
pub use sophia_term as term;
#[doc(inline)]
pub use sophia_turtle as turtle;
#[cfg(feature = "xml")]
#[doc(inline)]
pub use sophia_xml as xml;

/// Including tests from all code snippets in the book
/// from https://github.com/rust-lang/mdBook/issues/706#issuecomment-1085137304
#[cfg(doctest)]
mod booktest {
    macro_rules! booktest {
        ($i:ident) => {
            #[doc = include_str!(concat!("../../book/src/", stringify!($i), ".md"))]
            mod $i {}
        };
    }
    booktest!(ch01_getting_started);
    booktest!(ch02_rdf_terms);
    booktest!(ch03_rdf_statements);
    booktest!(ch04_rdf_graphs);
    booktest!(ch05_term_matchers);
    booktest!(ch06_rdf_datasets);
    booktest!(ch07_parsing_and_serializing);
    booktest!(ch90_changes_since_07);
}
