//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! Parser and serializer for the [RDF/XML] concrete syntax,
//! based on [`rio_xml`].
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf12-primer/
//! [Linked Data]: http://linkeddata.org/
//! [RDF/XML]: https://www.w3.org/TR/rdf11-xml/
#![deny(missing_docs)]

pub mod parser;
pub mod serializer;
