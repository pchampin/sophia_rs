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

use regex::Regex;
use rio_turtle::TurtleFormatter;
use sophia_api::dataset::{Dataset, MutableDataset};
use sophia_api::ns::{rdf, xsd};
use sophia_api::prefix::{PrefixBox, PrefixMap};
use sophia_api::serializer::*;
use sophia_api::term::{TTerm, TermKind::*};
use sophia_api::triple::stream::{SinkError, SourceError, StreamResult, TripleSource};
use sophia_api::triple::Triple;
use sophia_inmem::dataset::FastDataset;
use sophia_iri::IriBox;
use sophia_rio::serializer::rio_format_triples;
use sophia_term::RcTerm;
use std::io;

mod _pretty;
pub(super) use _pretty::prettify;

/// Turtle serializer configuration.
#[derive(Clone, Debug)]
pub struct TurtleConfig {
    pub(super) pretty: bool,
    pub(super) prefix_map: Vec<(PrefixBox, IriBox)>,
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
    pub fn prefix_map(&self) -> &[(PrefixBox, IriBox)] {
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
        let prefix_map = vec![
            (
                PrefixBox::new_unchecked("rdf".into()),
                IriBox::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#".into()),
            ),
            (
                PrefixBox::new_unchecked("rdfs".into()),
                IriBox::new_unchecked("http://www.w3.org/2000/01/rdf-schema#".into()),
            ),
            (
                PrefixBox::new_unchecked("xsd".into()),
                IriBox::new_unchecked("http://www.w3.org/2001/XMLSchema#".into()),
            ),
        ];
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
    pub fn with_own_prefix_map(mut self, pm: Vec<(PrefixBox, IriBox)>) -> Self {
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
    /// Build a new N-Triples serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> TurtleSerializer<W> {
        Self::new_with_config(write, TurtleConfig::default())
    }

    /// Build a new N-Triples serializer writing to `write`, with the given config.
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
            let mut dataset = FastDataset::new();
            source
                .for_each_triple(|t| {
                    dataset
                        .insert(t.s(), t.p(), t.o(), None as Option<&RcTerm>)
                        .unwrap();
                })
                .map_err(SourceError)?;
            let graph = dataset.graph(None); // get the default graph
            let blacklist = Default::default(); // no blacklist required for Turtle

            write_prefixes(&mut self.write, &self.config.prefix_map[..]).map_err(SinkError)?;
            prettify(graph, &mut self.write, &self.config, &blacklist, "").map_err(SinkError)?;
            self.write.flush().map_err(SinkError)?;
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
//                                      inners
// ---------------------------------------------------------------------------------

lazy_static::lazy_static! {
    /// Match an absolute IRI reference.
    pub(crate) static ref PN_LOCAL: Regex = Regex::new(r"(?x)^
        #(PN_CHARS_U | ':' | [0-9] | PLX)
        (
            [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_:0-9]
            # | PLX
            | \\ [_~.!$&'()*+,;=/?\#@%-]
            | % [0-9A-Fa-f]{2}
        )
        # ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
        (
            (
                [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}.:-]
                | \\ [_~.!$&'()*+,;=/?\#@%-]
                | % [0-9A-Fa-f]{2}
            )*
            (
                [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}:-]
                | \\ [_~.!$&'()*+,;=/?\#@%-]
                | % [0-9A-Fa-f]{2}
            )
        )?
    $").unwrap();
    pub(crate) static ref INTEGER: Regex = Regex::new(r"^[+-]?[0-9]+$").unwrap();
    pub(crate) static ref DECIMAL: Regex = Regex::new(r"^[+-]?[0-9]*.[0-9]+$").unwrap();
    pub(crate) static ref DOUBLE: Regex = Regex::new(r"(?x)^
      [+-]? ( [0-9]+ ( . [0-9]* )? | . [0-9]+ ) [eE] [+-]? [0-9]+
    $").unwrap();
    pub(crate) static ref BOOLEAN: Regex = Regex::new(r"^(true|false)$").unwrap();
}

/// write the prefix declaration of the given prefix_map, using SPARQL style.
pub fn write_prefixes<W, P>(mut write: W, prefix_map: &P) -> io::Result<()>
where
    W: io::Write,
    P: PrefixMap + ?Sized,
{
    for (pre, iri) in prefix_map.iter() {
        writeln!(&mut write, "PREFIX {}: <{}>", pre.as_ref(), iri.as_ref())?;
    }
    Ok(())
}

/// write the given term in Turtle syntax, using the prefix map from `config` if appropriate.
///
/// If `allow_anon` is true and `term` is a blank node, it will be written as `[]`
/// instead of using it's label.
pub fn write_term<W: io::Write, T: TTerm>(
    mut write: W,
    term: &T,
    config: &TurtleConfig,
    allow_anon: bool,
) -> io::Result<()> {
    match term.kind() {
        Iri => write_iri(write, term, config),
        BlankNode => {
            if allow_anon {
                write.write_all(b"[]")
            } else {
                write!(&mut write, "_:{}", term.value_raw().0)
            }
        }
        Literal => write_literal(write, term, config),
        Variable => {
            write!(&mut write, "?{}", term.value_raw().0)
        }
    }
}

fn write_iri<W: io::Write, T: TTerm>(
    mut write: W,
    iri: &T,
    config: &TurtleConfig,
) -> io::Result<()> {
    debug_assert!(iri.kind() == Iri);
    if &rdf::nil == iri {
        return write.write_all(b"()");
    }
    match config
        .prefix_map
        .get_checked_prefixed_pair(iri, |txt| PN_LOCAL.is_match(txt))
    {
        Some((pre, suf)) => {
            write!(write, "{}:{}", pre.as_ref(), suf)
        }
        None => {
            let raw = iri.value_raw();
            write!(write, "<{}{}>", raw.0, raw.1.unwrap_or(""))
        }
    }
}

fn write_literal<W: io::Write, T: TTerm>(
    mut write: W,
    lit: &T,
    config: &TurtleConfig,
) -> io::Result<()> {
    debug_assert!(lit.kind() == Literal);
    let datatype = lit.datatype().unwrap();
    let value = lit.value_raw().0;
    if datatype == xsd::integer && INTEGER.is_match(value)
        || datatype == xsd::decimal && DECIMAL.is_match(value)
        || datatype == xsd::double && DOUBLE.is_match(value)
        || datatype == xsd::boolean && BOOLEAN.is_match(value)
    {
        write.write_all(value.as_bytes())
    } else {
        write.write_all(b"\"")?;
        super::nt::quoted_string(&mut write, value.as_bytes())?;
        write.write_all(b"\"")?;
        if let Some(tag) = lit.language() {
            write!(write, "@{}", tag)
        } else {
            if datatype != xsd::string {
                write.write_all(b"^^")?;
                write_iri(write, &datatype, config)?;
            }
            Ok(())
        }
    }
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use sophia_api::graph::{isomorphic_graphs, Graph};
    use sophia_inmem::graph::FastGraph;
    use sophia_term::*;
    use std::error::Error;

    #[test]
    fn pn_local() {
        for positive in [
            "a",
            "aBc",
            "éàïsophia_api::graph::",
            ":::",
            "123",
            "%20%21%22",
            "\\%\\?\\&",
        ] {
            assert!(PN_LOCAL.is_match(positive), "{}", positive);
        }
        for negative in [" ", ".a", "a."] {
            assert!(!PN_LOCAL.is_match(negative), "{}", negative);
        }
    }

    #[test]
    fn double() {
        for positive in [
            "3.14e0",
            "+3.14e0",
            "-3.14e0",
            "3.14e+0",
            "3.14e-0",
            "0000e0000",
            ".1E0",
            "1.e+3",
            "1E-3",
        ] {
            assert!(DOUBLE.is_match(positive), "{}", positive);
        }
    }

    const TESTS: &'static [&str] = &[
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
        /* broken due to a bug in Rio
        r#"# bnode cycles
        PREFIX : <http://example.org/ns/>
        _:a :n "a"; :p [ :q [ :r _:a ]].
        _:b :n "b"; :s [ :s _:b ].
        "#,
        */
    ];

    #[test]
    fn roundtrip_not_pretty() -> Result<(), Box<dyn std::error::Error>> {
        for ttl in TESTS {
            println!("==========\n{}\n----------", ttl);
            let g1: FastGraph = crate::parser::turtle::parse_str(ttl).collect_triples()?;

            let out = TurtleSerializer::new_stringifier()
                .serialize_triples(g1.triples())?
                .to_string();
            println!("{}", &out);

            let g2: FastGraph = crate::parser::turtle::parse_str(&out).collect_triples()?;

            assert!(isomorphic_graphs(&g1, &g2)?);
        }
        Ok(())
    }

    #[test]
    fn roundtrip_pretty() -> Result<(), Box<dyn Error>> {
        for ttl in TESTS {
            println!("==========\n{}\n----------", ttl);
            let g1: FastGraph = crate::parser::turtle::parse_str(ttl).collect_triples()?;
            let out2 = TurtleSerializer::new_stringifier()
                .serialize_triples(g1.triples())?
                .to_string();
            println!("\n>>> DEBUG\n{}", &out2);

            let config = TurtleConfig::new().with_pretty(true);
            let out = TurtleSerializer::new_stringifier_with_config(config)
                .serialize_triples(g1.triples())?
                .to_string();
            println!("{}", &out);

            let g2: FastGraph = crate::parser::turtle::parse_str(&out).collect_triples()?;

            assert!(isomorphic_graphs(&g1, &g2)?);
        }
        Ok(())
    }
}
