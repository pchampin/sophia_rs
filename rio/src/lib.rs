//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! This crate is not usable alone,
//! but contains common code required by
//! [`sophia_turtle`](https://docs.rs/sophia_turtle/)
//! and
//! [`sophia_xml`](https://docs.rs/sophia_xml/).
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/
#![deny(missing_docs)]

pub mod model;
pub mod parser;
pub mod serializer;
