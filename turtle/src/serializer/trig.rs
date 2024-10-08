//! Serializer for the [TriG] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [TriG]: https://www.w3.org/TR/trig/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use rio_turtle::TriGFormatter;
use sophia_api::quad::Quad;
use sophia_api::serializer::{QuadSerializer, Stringifier};
use sophia_api::source::{QuadSource, SinkError, SourceError, StreamResult};
use sophia_rio::serializer::rio_format_quads;
use std::io;

pub(super) use super::_pretty::*;

/// Trig serializer configuration.
pub type TrigConfig = super::turtle::TurtleConfig;

/// Trig serializer.
pub struct TrigSerializer<W> {
    pub(super) config: TrigConfig,
    pub(super) write: W,
}

impl<W> TrigSerializer<W>
where
    W: io::Write,
{
    /// Build a new Trig serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> Self {
        Self::new_with_config(write, TrigConfig::default())
    }

    /// Build a new Trig serializer writing to `write`, with the given config.
    pub const fn new_with_config(write: W, config: TrigConfig) -> Self {
        Self { config, write }
    }

    /// Borrow this serializer's configuration.
    pub const fn config(&self) -> &TrigConfig {
        &self.config
    }
}

impl<W> QuadSerializer for TrigSerializer<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn serialize_quads<TS>(
        &mut self,
        mut source: TS,
    ) -> StreamResult<&mut Self, TS::Error, Self::Error>
    where
        TS: QuadSource,
    {
        if self.config.pretty {
            let mut dataset = PrettifiableDataset::new();
            source
                .for_each_quad(|t| {
                    let (spo, g) = t.spog();
                    let spo = spo.map(sophia_api::prelude::Term::into_term);
                    let g = g.map(sophia_api::prelude::Term::into_term);
                    dataset.insert((g, spo));
                })
                .map_err(SourceError)?;
            prettify(dataset, &mut self.write, &self.config, "").map_err(SinkError)?;
        } else {
            let mut tf = TriGFormatter::new(&mut self.write);
            rio_format_quads(&mut tf, source)?;
            tf.finish().map_err(SinkError)?;
        }
        Ok(self)
    }
}

impl TrigSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    #[must_use]
    pub fn new_stringifier() -> Self {
        Self::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    #[must_use]
    pub const fn new_stringifier_with_config(config: TrigConfig) -> Self {
        Self::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for TrigSerializer<Vec<u8>> {
    fn as_utf8(&self) -> &[u8] {
        &self.write[..]
    }
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub(crate) mod test {
    use std::error::Error;

    use super::*;
    use sophia_api::term::SimpleTerm;

    use sophia_api::{dataset::Dataset, quad::Spog};
    use sophia_isomorphism::isomorphic_datasets;

    const TESTS: &[&str] = &[
        "#empty trig",
        r#"# simple quads
            PREFIX : <http://example.org/ns/>
            :alice a :Person; :name "Alice"; :age 42.

            GRAPH :g {
                :bob a :Person, :Man; :nick "bob"@fr, "bobby"@en; :admin true.
            }
        "#,
        r#"# lists
            GRAPH <tag:g> { <tag:alice> <tag:likes> ( 1 2 ( 3 4 ) 5 6 ), ("a" "b"). }
        "#,
        r"# subject lists
            GRAPH <tag:g> { (1 2 3) a <tag:List>. }
        ",
        r"# malformed list
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            GRAPH <tag:g> {
                _:a rdf:first 42, 43; rdf:rest (44 45).
                _:b rdf:first 42; rdf:rest (43), (44).
            }
        ",
        r#"# bnode cycles
            PREFIX : <http://example.org/ns/>
            GRAPH <tag:g> {
                _:a :n "a"; :p [ :q [ :r _:a ]].
                _:b :n "b"; :s [ :s _:b ].
                _:c :b "c"; :t _:c.
            }
        "#,
        r"# quoted triples
            PREFIX : <http://example.org/ns/>
            GRAPH <tag:g> {
                << :s :p :o1 >> :a :b.
                :s :p :o2 {| :c :d |}.
            }
        ",
        r"# blank node graph name
            PREFIX : <http://example.org/ns/>
            :lois :believes _:b.
            GRAPH _:b1 { :clark a :Human }
        ",
        r#"# blank node sharred across graphs
            PREFIX : <http://example.org/ns/>
            _:a :name "alice".
            GRAPH <tag:g> { _:a a :Person }
        "#,
        r"# list split over different graphs
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            _:a rdf:first 42; rdf:rest _:b.

            GRAPH [] {
                _:b rdf:first 43; rdf:rest ().
            }
        ",
        r"# issue 149
            PREFIX : <https://example.org/>
            :s :p :o .
            GRAPH :g { _:b :p2 :o2 }
        ",
    ];

    #[test]
    fn roundtrip_not_pretty() -> Result<(), Box<dyn Error>> {
        for ttl in TESTS {
            println!("==========\n{ttl}\n----------");
            let g1: Vec<Spog<SimpleTerm>> = crate::parser::trig::parse_str(ttl).collect_quads()?;

            let out = TrigSerializer::new_stringifier()
                .serialize_quads(g1.quads())?
                .to_string();
            println!("{}", &out);

            let g2: Vec<Spog<SimpleTerm>> = crate::parser::trig::parse_str(&out).collect_quads()?;

            assert!(isomorphic_datasets(&g1, &g2)?);
        }
        Ok(())
    }

    #[test]
    fn roundtrip_pretty() -> Result<(), Box<dyn Error>> {
        for ttl in TESTS {
            println!("==========\n{ttl}\n----------");
            let g1: Vec<Spog<SimpleTerm>> = crate::parser::trig::parse_str(ttl).collect_quads()?;

            let config = TrigConfig::new().with_pretty(true);
            let out = TrigSerializer::new_stringifier_with_config(config)
                .serialize_quads(g1.quads())?
                .to_string();
            println!("{}", &out);

            let g2: Vec<Spog<SimpleTerm>> = crate::parser::trig::parse_str(&out).collect_quads()?;

            assert!(isomorphic_datasets(&g1, &g2)?);
        }
        Ok(())
    }
}
