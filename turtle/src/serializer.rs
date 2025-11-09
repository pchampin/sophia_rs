//! Serializers for the [Turtle]-family of [RDF] 1.2 concrete syntaxes.
//!
//! [RDF]: https://www.w3.org/TR/rdf12-primer/
//! [Turtle]: https://www.w3.org/TR/rdf12-turtle/
mod _common;
mod _pretty;
mod _streaming;

pub mod nq;
pub mod nt;
pub mod trig;
pub mod turtle;
