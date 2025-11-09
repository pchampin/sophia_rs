//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! Parsers and serializers for the [Turtle]-family of [RDF] 1.2 concrete syntaxes.
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf12-primer/
//! [Linked Data]: http://linkeddata.org/
//! [Turtle]: https://www.w3.org/TR/rdf12-turtle/
#![deny(missing_docs)]

pub mod parser;
pub mod serializer;

mod _stash;
mod _term;

#[cfg(test)]
mod test;

macro_rules! lazy_regex {
    ($v:vis $id:ident = $regex:literal) => {
        $v static $id: std::sync::LazyLock<regex::Regex> = std::sync::LazyLock::new(|| regex::Regex::new($regex).unwrap());
    };
    ($v:vis $id:ident = [ $($regex:literal),+ ]) => {
        $v static $id: std::sync::LazyLock<regex::RegexSet> = std::sync::LazyLock::new(|| regex::RegexSet::new(&[$($regex),+]).unwrap());
    }
}
pub(crate) use lazy_regex;
