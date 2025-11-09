//! Serializer for the [N-Triples] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [N-Triples]: https://www.w3.org/TR/rdf12-n-triples/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use sophia_api::ns::xsd;
use sophia_api::serializer::{Stringifier, TripleSerializer};
use sophia_api::source::{StreamResult, TripleSource};
use sophia_api::term::{Term, TermKind};
use sophia_api::triple::Triple;
use std::io;

use super::_common::quoted_string;

/// N-Triples serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct NTriplesConfig {
    pub(super) ascii: bool,
    pub(super) canonical: bool,
}

/// Type alias of `NTriplesConfig` for backward compatibility
#[deprecated(since = "0.10.0", note = "please use NTriplesConfig instead")]
pub type NtConfig = NTriplesConfig;

impl NTriplesConfig {
    /// Construct a default configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Indicate whether all non-ascii characters should be encoded (using `\uxxxx` or `\Uxxxxxxxx`).
    ///
    /// Defaults to false.
    #[must_use]
    pub fn with_ascii(self, ascii: bool) -> Self {
        NTriplesConfig { ascii, ..self }
    }

    /// Indicates whether [canonical N-triples] should be generated.
    ///
    /// Defaults to false.
    /// The default mode uses tabulations to separate the subject, predicate and object of asserted triples,
    /// effectively producing data that is also [TSV],
    /// making it easier to process with veadsheet applications,
    /// as well Unix-style command line tools (e.g. `sort`, `cut`, `column`...).
    ///
    /// [canonical N-triples]: https://www.w3.org/TR/rdf12-n-triples/#canonical-ntriples
    /// [TSV]: https://en.wikipedia.org/wiki/Tab-separated_values
    #[must_use]
    pub fn with_canonical(self, canonical: bool) -> Self {
        NTriplesConfig { canonical, ..self }
    }
}

/// N-Triples serializer.
pub struct NTriplesSerializer<W> {
    config: NTriplesConfig,
    write: W,
}

/// Type alias of `NTriplesSerializer` for backward compatibility
#[deprecated(since = "0.10.0", note = "please use NTriplesSerializer instead")]
pub type NtSerializer<W> = NTriplesSerializer<W>;

impl<W> NTriplesSerializer<W>
where
    W: io::Write,
{
    /// Build a new N-Triples serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> Self {
        Self::new_with_config(write, NTriplesConfig::default())
    }

    /// Build a new N-Triples serializer writing to `write`, with the given config.
    #[inline]
    pub const fn new_with_config(write: W, config: NTriplesConfig) -> Self {
        Self { config, write }
    }

    /// Borrow this serializer's configuration.
    #[inline]
    pub const fn config(&self) -> &NTriplesConfig {
        &self.config
    }
}

impl<W> TripleSerializer for NTriplesSerializer<W>
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
        if self.config.ascii {
            todo!("Pure-ASCII N-Triples is not implemented yet")
        }
        if self.config.canonical {
            source.try_for_each_triple(|t| {
                {
                    let w = &mut self.write;
                    write_triple::<_, _, true>(w, t, b' ')?;
                    w.write_all(b" .\n")
                }
                .map_err(io::Error::other)
            })
        } else {
            source.try_for_each_triple(|t| {
                {
                    let w = &mut self.write;
                    write_triple::<_, _, false>(w, t, b'\t')?;
                    w.write_all(b"\t.\n")
                }
                .map_err(io::Error::other)
            })
        }
        .map(|()| self)
    }
}

impl NTriplesSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    pub fn new_stringifier() -> Self {
        Self::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    pub const fn new_stringifier_with_config(config: NTriplesConfig) -> Self {
        Self::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for NTriplesSerializer<Vec<u8>> {
    fn as_utf8(&self) -> &[u8] {
        &self.write[..]
    }
}

/// Write the given term into the given write in the N-Triples format.
pub fn write_triple<W, T, const C: bool>(w: &mut W, t: T, separator: u8) -> io::Result<()>
where
    W: io::Write,
    T: Triple,
{
    write_term::<_, _, C>(w, t.s())?;
    w.write_all(&[separator])?;
    write_term::<_, _, C>(w, t.p())?;
    w.write_all(&[separator])?;
    write_term::<_, _, C>(w, t.o())?;
    Ok(())
}

/// Write the given term into the given write in the N-Triples format.
///
/// Generic parameter C determines whether the canonical form of N-Triples/N-Quads should be enforced,
/// which only impacts the case of language tags in this function.
pub fn write_term<W, T, const C: bool>(w: &mut W, t: T) -> io::Result<()>
where
    W: io::Write,
    T: Term,
{
    use TermKind::{BlankNode, Iri, Literal, Triple, Variable};
    match t.kind() {
        Iri => {
            w.write_all(b"<")?;
            w.write_all(t.iri().unwrap().as_bytes())?;
            w.write_all(b">")?;
        }
        BlankNode => {
            w.write_all(b"_:")?;
            w.write_all(t.bnode_id().unwrap().as_bytes())?;
        }
        Literal => {
            w.write_all(b"\"")?;
            quoted_string(w, &t.lexical_form().unwrap())?;
            if let Some(tag) = t.language_tag() {
                w.write_all(b"\"@")?;
                if C {
                    w.write_all(tag.to_ascii_lowercase().as_bytes())?;
                } else {
                    w.write_all(tag.as_bytes())?;
                }
                if let Some(dir) = t.base_direction() {
                    write!(w, "--{dir}")?;
                }
            } else {
                let dt = t.datatype().unwrap();
                if xsd::string != dt {
                    w.write_all(b"\"^^<")?;
                    w.write_all(dt.as_bytes())?;
                    w.write_all(b">")?;
                } else {
                    w.write_all(b"\"")?;
                }
            }
        }
        Triple => {
            w.write_all(b"<<( ")?;
            write_triple::<_, _, C>(w, t.to_triple().unwrap(), b' ')?;
            w.write_all(b" )>>")?;
        }
        Variable => {
            w.write_all(b"?")?;
            w.write_all(t.variable().unwrap().as_bytes())?;
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub(crate) mod test {
    use std::error::Error;

    use crate::test::{LazyMap, nt_samples};

    use super::*;
    use sophia_api::graph::{Graph, MutableGraph};
    use sophia_api::ns::*;
    use sophia_api::term::{BnodeId, LanguageTag, SimpleTerm, VarName};
    use sophia_iri::Iri;
    use sophia_isomorphism::isomorphic_graphs;
    use test_case::test_case;

    #[test]
    fn graph() -> Result<(), Box<dyn std::error::Error>> {
        let me = BnodeId::new_unchecked("me");
        let mut g: Vec<[SimpleTerm<'static>; 3]> = vec![];
        MutableGraph::insert(
            &mut g,
            me,
            rdf::type_,
            Iri::new_unchecked("http://schema.org/Person"),
        )?;
        MutableGraph::insert(
            &mut g,
            me,
            Iri::new_unchecked("http://schema.org/name"),
            "Pierre-Antoine",
        )?;
        MutableGraph::insert(
            &mut g,
            me,
            Iri::new_unchecked("http://example.org/value"),
            42,
        )?;
        MutableGraph::insert(
            &mut g,
            me,
            Iri::new_unchecked("http://example.org/message"),
            SimpleTerm::LiteralLanguage(
                "hello\nworld".into(),
                LanguageTag::new_unchecked("en".into()),
                None,
            ),
        )?;
        let r = BnodeId::new_unchecked("r");
        let tr = g[0].clone();
        MutableGraph::insert(&mut g, r, rdf::reifies, SimpleTerm::Triple(Box::new(tr)))?;
        MutableGraph::insert(
            &mut g,
            r,
            Iri::new_unchecked("http://schema.org/creator"),
            VarName::new_unchecked("x"),
        )?;

        let exp = r#"_:me	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>	<http://schema.org/Person>	.
_:me	<http://schema.org/name>	"Pierre-Antoine"	.
_:me	<http://example.org/value>	"42"^^<http://www.w3.org/2001/XMLSchema#integer>	.
_:me	<http://example.org/message>	"hello\nworld"@en	.
_:r	<http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies>	<<( _:me <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> )>>	.
_:r	<http://schema.org/creator>	?x	.
"#;

        let s1 = NTriplesSerializer::new_stringifier()
            .serialize_graph(&g)
            .unwrap()
            .to_string();
        assert_eq!(&s1, exp);

        let s2 = NTriplesSerializer::new_stringifier_with_config(
            NTriplesConfig::new().with_canonical(true),
        )
        .serialize_graph(&g)
        .unwrap()
        .to_string();
        assert_eq!(&s2, &exp.replace("\t", " "));

        Ok(())
    }

    #[test_case("empty")]
    #[test_case("comment")]
    #[test_case("version")]
    #[test_case("triple i i i")]
    #[test_case("triple b i i")]
    #[test_case("triple i i b")]
    #[test_case("triple b i b")]
    #[test_case("triple i i l")]
    #[test_case("triple b i l")]
    #[test_case("triple i i ld")]
    #[test_case("triple b i ld")]
    #[test_case("triple i i ll")]
    #[test_case("triple b i ll")]
    #[test_case("triple i i lb")]
    #[test_case("triple b i lb")]
    #[test_case("triple i i t")]
    #[test_case("triple b i t")]
    #[test_case("escape")]
    #[test_case("escape useless")]
    fn roundtrip(key: &str) -> Result<(), Box<dyn Error>> {
        static TESTS: LazyMap = nt_samples();
        let (nt, _) = TESTS.get(key).unwrap();
        let g1: Vec<[SimpleTerm; 3]> = crate::parser::nt::parse_str(nt).collect_triples()?;

        let config = NTriplesConfig::new();
        let out = NTriplesSerializer::new_stringifier_with_config(config)
            .serialize_triples(g1.triples())?
            .to_string();
        println!("\n>>> DEBUG\n{}", &out);

        let g2: Vec<[SimpleTerm; 3]> = crate::parser::nt::parse_str(&out).collect_triples()?;

        assert!(isomorphic_graphs(&g1, &g2)?, "G1 = \n{g1:#?}\nG2 = {g2:#?}");
        Ok(())
    }

    #[test_case("empty")]
    #[test_case("comment")]
    #[test_case("version")]
    #[test_case("triple i i i")]
    #[test_case("triple b i i")]
    #[test_case("triple i i b")]
    #[test_case("triple b i b")]
    #[test_case("triple i i l")]
    #[test_case("triple b i l")]
    #[test_case("triple i i ld")]
    #[test_case("triple b i ld")]
    #[test_case("triple i i ll")]
    #[test_case("triple b i ll")]
    #[test_case("triple i i lb")]
    #[test_case("triple b i lb")]
    #[test_case("triple i i t")]
    #[test_case("triple b i t")]
    #[test_case("escape")]
    #[test_case("escape useless")]
    fn roundtrip_canonical(key: &str) -> Result<(), Box<dyn Error>> {
        static TESTS: LazyMap = nt_samples();
        let (nt, _) = TESTS.get(key).unwrap();
        let g1: Vec<[SimpleTerm; 3]> = crate::parser::nt::parse_str(nt).collect_triples()?;

        let config = NTriplesConfig::new().with_canonical(true);
        let out = NTriplesSerializer::new_stringifier_with_config(config)
            .serialize_triples(g1.triples())?
            .to_string();
        println!("\n>>> DEBUG\n{}", &out);

        let g2: Vec<[SimpleTerm; 3]> = crate::parser::nt::parse_str(&out).collect_triples()?;

        assert!(isomorphic_graphs(&g1, &g2)?, "G1 = \n{g1:#?}\nG2 = {g2:#?}");
        Ok(())
    }
}
