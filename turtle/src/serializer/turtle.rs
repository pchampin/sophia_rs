//! Serializer for the [Turtle] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [Turtle]: https://www.w3.org/TR/turtle/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use rio_turtle::TurtleFormatter;
use sophia_api::prefix::{Prefix, PrefixMap, PrefixMapPair};
use sophia_api::serializer::{Stringifier, TripleSerializer};
use sophia_api::source::{SinkError, SourceError, StreamResult, TripleSource};
use sophia_api::term::{CmpTerm, SimpleTerm, Term};
use sophia_api::triple::Triple;
use sophia_iri::Iri;
use sophia_rio::serializer::rio_format_triples;
use std::io;

pub(super) use super::_pretty::*;

/// Turtle serializer configuration.
#[derive(Clone, Debug)]
pub struct TurtleConfig {
    pub(super) pretty: bool,
    pub(super) prefix_map: Vec<PrefixMapPair>,
    pub(super) indentation: String,
}

impl TurtleConfig {
    /// Should the parser make extra effort to produce pretty Turtle.
    ///
    /// If false (default), the triples will be serialized in streaming mode.
    /// Subject and predicate "factorization" will only occur based on the previous triple(s)
    /// in the stream. The collection syntax for `rdf:List`s will not be used.
    ///
    /// If true, extra effort will be made to group related triples together,
    /// and to use the collection syntax whenever possible.
    /// This requires storing the whole graph in memory.
    pub fn pretty(&self) -> bool {
        self.pretty
    }

    /// [`PrefixMap`] to use in serialization.
    /// (defaults to a map containing rdf:, rdfs: and xsd:)
    ///
    /// NB: currently, only used if [`pretty`][`TurtleConfig::pretty`] is `true`.
    pub fn prefix_map(&self) -> &[PrefixMapPair] {
        &self.prefix_map
    }

    /// Indentation to use in serialization.
    /// (defaults to `"  "`, can only contain ASCII whitespaces)
    ///
    /// NB: currently, only used if [`pretty`][`TurtleConfig::pretty`] is `true`.
    pub fn indentation(&self) -> &str {
        &self.indentation
    }

    /// Build a new default [`TurtleConfig`].
    pub fn new() -> Self {
        let pretty = false;
        let prefix_map = Self::default_prefix_map();
        let indentation = "  ".to_string();
        TurtleConfig {
            pretty,
            prefix_map,
            indentation,
        }
    }

    /// Transform a [`TurtleConfig`] by setting the [`pretty`][`TurtleConfig::pretty`] flag.
    pub fn with_pretty(mut self, b: bool) -> Self {
        self.pretty = b;
        self
    }

    /// Transform a [`TurtleConfig`] by setting the [`prefix_map`][`TurtleConfig::prefix_map`] flag
    /// (copying `pm` using [`PrefixMap::to_vec`]).
    pub fn with_prefix_map<P: PrefixMap + ?Sized>(self, pm: &P) -> Self {
        self.with_own_prefix_map(pm.to_vec())
    }

    /// Transform a [`TurtleConfig`] by setting the [`prefix_map`][`TurtleConfig::prefix_map`] flag.
    pub fn with_own_prefix_map(mut self, pm: Vec<PrefixMapPair>) -> Self {
        self.prefix_map = pm;
        self
    }

    /// Transform a [`TurtleConfig`] by setting the [`indentation`][`TurtleConfig::indentation`] flag.
    ///
    /// # Precondition
    /// `indentation` must only contain ASCII whitespaces, otherwise this method will panic.
    pub fn with_indentation<T: ToString>(mut self, indentation: T) -> Self {
        let indentation = indentation.to_string();
        assert!(indentation.chars().all(char::is_whitespace));
        self.indentation = indentation;
        self
    }

    /// Return the prefix map that is used when none is provided
    pub fn default_prefix_map() -> Vec<PrefixMapPair> {
        vec![
            (
                Prefix::new_unchecked("rdf".into()),
                Iri::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#".into()),
            ),
            (
                Prefix::new_unchecked("rdfs".into()),
                Iri::new_unchecked("http://www.w3.org/2000/01/rdf-schema#".into()),
            ),
            (
                Prefix::new_unchecked("xsd".into()),
                Iri::new_unchecked("http://www.w3.org/2001/XMLSchema#".into()),
            ),
        ]
    }
}

impl Default for TurtleConfig {
    fn default() -> Self {
        TurtleConfig::new()
    }
}

/// Turtle serializer.
pub struct TurtleSerializer<W> {
    pub(super) config: TurtleConfig,
    pub(super) write: W,
}

impl<W> TurtleSerializer<W>
where
    W: io::Write,
{
    /// Build a new Turtle serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> TurtleSerializer<W> {
        Self::new_with_config(write, TurtleConfig::default())
    }

    /// Build a new Turtle serializer writing to `write`, with the given config.
    pub fn new_with_config(write: W, config: TurtleConfig) -> TurtleSerializer<W> {
        TurtleSerializer { config, write }
    }

    /// Borrow this serializer's configuration.
    pub fn config(&self) -> &TurtleConfig {
        &self.config
    }
}

impl<W> TripleSerializer for TurtleSerializer<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn serialize_triples<TS>(
        &mut self,
        mut source: TS,
    ) -> StreamResult<&mut Self, TS::Error, Self::Error>
    where
        TS: TripleSource,
    {
        if self.config.pretty {
            let mut dataset = PrettifiableDataset::new();
            let default = None as Option<CmpTerm<SimpleTerm>>;
            source
                .for_each_triple(|t| {
                    let spo = t.spo().map(|t| CmpTerm(t.into_term()));
                    dataset.insert((default.clone(), spo));
                })
                .map_err(SourceError)?;
            prettify(dataset, &mut self.write, &self.config, "").map_err(SinkError)?;
        } else {
            let mut tf = TurtleFormatter::new(&mut self.write);
            rio_format_triples(&mut tf, source)?;
            tf.finish().map_err(SinkError)?;
        }
        Ok(self)
    }
}

impl TurtleSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    pub fn new_stringifier() -> Self {
        TurtleSerializer::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    pub fn new_stringifier_with_config(config: TurtleConfig) -> Self {
        TurtleSerializer::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for TurtleSerializer<Vec<u8>> {
    fn as_utf8(&self) -> &[u8] {
        &self.write[..]
    }
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use sophia_api::graph::Graph;
    use sophia_isomorphism::isomorphic_graphs;
    use std::error::Error;

    const TESTS: &[&str] = &[
        "#empty ttl",
        r#"# simple triple
            PREFIX : <http://example.org/ns/>
            :alice a :Person; :name "Alice"; :age 42.
            :bob a :Person, :Man; :nick "bob"@fr, "bobby"@en; :admin true.
        "#,
        r#"# lists
            <tag:alice> <tag:likes> ( 1 2 ( 3 4 ) 5 6 ), ("a" "b").
        "#,
        r#"# subject lists
            (1 2 3) a <tag:List>.
        "#,
        r#"# malformed list
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            _:a rdf:first 42, 43; rdf:rest (44 45).
            _:b rdf:first 42; rdf:rest (43), (44).
        "#,
        r#"# bnode cycles
            PREFIX : <http://example.org/ns/>
            _:a :n "a"; :p [ :q [ :r _:a ]].
            _:b :n "b"; :s [ :s _:b ].
            _:c :b "c"; :t _:c.
        "#,
        r#"# quoted triples
            PREFIX : <http://example.org/ns/>
            << :s :p :o1 >> :a :b.
            :s :p :o2 {| :c :d |}.
        "#,
    ];

    #[test]
    fn roundtrip_not_pretty() -> Result<(), Box<dyn std::error::Error>> {
        for ttl in TESTS {
            println!("==========\n{}\n----------", ttl);
            let g1: Vec<[SimpleTerm; 3]> =
                crate::parser::turtle::parse_str(ttl).collect_triples()?;

            let out = TurtleSerializer::new_stringifier()
                .serialize_triples(g1.triples())?
                .to_string();
            println!("{}", &out);

            let g2: Vec<[SimpleTerm; 3]> =
                crate::parser::turtle::parse_str(&out).collect_triples()?;

            assert!(isomorphic_graphs(&g1, &g2)?);
        }
        Ok(())
    }

    #[test]
    fn roundtrip_pretty() -> Result<(), Box<dyn Error>> {
        for ttl in TESTS {
            println!("==========\n{}\n----------", ttl);
            let g1: Vec<[SimpleTerm; 3]> =
                crate::parser::turtle::parse_str(ttl).collect_triples()?;
            let ugly = TurtleSerializer::new_stringifier()
                .serialize_triples(g1.triples())?
                .to_string();
            println!("\n>>> DEBUG ugly\n{}", &ugly);

            let mut prefix_map = TurtleConfig::default_prefix_map();
            prefix_map.push((
                Prefix::new_unchecked("".into()),
                Iri::new_unchecked("http://example.org/ns/".into()),
            ));
            let config = TurtleConfig::new()
                .with_pretty(true)
                .with_own_prefix_map(prefix_map);
            let pretty = TurtleSerializer::new_stringifier_with_config(config)
                .serialize_triples(g1.triples())?
                .to_string();
            println!("\n>>> DEBUG pretty\n{}", &pretty);

            let g2: Vec<[SimpleTerm; 3]> =
                crate::parser::turtle::parse_str(&pretty).collect_triples()?;
            let ugly = TurtleSerializer::new_stringifier()
                .serialize_triples(g2.triples())?
                .to_string();
            println!("\n>>> DEBUG pretty→ugly\n{}", &ugly);

            assert!(isomorphic_graphs(&g1, &g2)?);
        }
        Ok(())
    }
}
