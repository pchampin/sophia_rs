//! Serializer for the [N-Quads] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [N-Quads]: https://www.w3.org/TR/rdf12-n-quads/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use super::nt::{write_term, write_triple};
use sophia_api::quad::Quad;
use sophia_api::serializer::{QuadSerializer, Stringifier};
use sophia_api::source::{QuadSource, StreamResult};
use std::io;

/// N-Quads serializer configuration.
pub type NQuadsConfig = super::nt::NTriplesConfig;

/// Type alias of `NQuadsConfig` for backward compatibility
#[deprecated(since = "0.10.0", note = "please use NQuadsConfig instead")]
pub type NqConfig = NQuadsConfig;

/// N-Quads serializer.
pub struct NQuadsSerializer<W> {
    config: NQuadsConfig,
    write: W,
}

/// Type alias of `NQuadsSerializer` for backward compatibility
#[deprecated(since = "0.10.0", note = "please use NQuadsSerializer instead")]
pub type NqSerializer<W> = NQuadsSerializer<W>;

impl<W> NQuadsSerializer<W>
where
    W: io::Write,
{
    /// Build a new N-Quads serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> Self {
        Self::new_with_config(write, NQuadsConfig::default())
    }

    /// Build a new N-Quads serializer writing to `write`, with the given config.
    #[inline]
    pub const fn new_with_config(write: W, config: NQuadsConfig) -> Self {
        Self { config, write }
    }

    /// Borrow this serializer's configuration.
    #[inline]
    pub const fn config(&self) -> &NQuadsConfig {
        &self.config
    }
}

impl<W> QuadSerializer for NQuadsSerializer<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn serialize_quads<QS>(
        &mut self,
        mut source: QS,
    ) -> StreamResult<&mut Self, QS::Error, Self::Error>
    where
        QS: QuadSource,
    {
        if self.config.ascii {
            todo!("Pure-ASCII N-Quads is not implemented yet")
        }
        if self.config.canonical {
            source.try_for_each_quad(|q| {
                {
                    let w = &mut self.write;
                    let (tr, gn) = q.spog();
                    write_triple::<_, _, true>(w, tr, b' ')?;
                    if let Some(t) = gn {
                        w.write_all(b" ")?;
                        write_term::<_, _, true>(w, t)?;
                    }
                    w.write_all(b" .\n")
                }
                .map_err(io::Error::other)
            })
        } else {
            source.try_for_each_quad(|q| {
                {
                    let w = &mut self.write;
                    let (tr, gn) = q.spog();
                    write_triple::<_, _, false>(w, tr, b'\t')?;
                    w.write_all(b"\t")?;
                    if let Some(t) = gn {
                        write_term::<_, _, false>(w, t)?;
                    }
                    w.write_all(b"\t.\n")
                }
                .map_err(io::Error::other)
            })
        }
        .map(|()| self)
    }
}

impl NQuadsSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    pub fn new_stringifier() -> Self {
        Self::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    pub const fn new_stringifier_with_config(config: NQuadsConfig) -> Self {
        Self::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for NQuadsSerializer<Vec<u8>> {
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

    use crate::test::{LazyMap, nq_samples};

    use super::*;
    use sophia_api::dataset::MutableDataset;
    use sophia_api::ns::*;
    use sophia_api::prelude::Dataset;
    use sophia_api::quad::Spog;
    use sophia_api::term::{BnodeId, LanguageTag, SimpleTerm, VarName};
    use sophia_iri::Iri;
    use sophia_isomorphism::isomorphic_datasets;
    use test_case::test_case;

    #[test]
    fn graph() -> Result<(), Box<dyn std::error::Error>> {
        let me = BnodeId::new_unchecked("me");
        let mut d: Vec<Spog<SimpleTerm<'static>>> = vec![];
        MutableDataset::insert(
            &mut d,
            me,
            rdf::type_,
            Iri::new_unchecked("http://schema.org/Person"),
            None as Option<i32>,
        )?;
        MutableDataset::insert(
            &mut d,
            me,
            Iri::new_unchecked("http://schema.org/name"),
            "Pierre-Antoine",
            Some(me),
        )?;
        MutableDataset::insert(
            &mut d,
            me,
            Iri::new_unchecked("http://example.org/value"),
            42,
            Some(me),
        )?;
        MutableDataset::insert(
            &mut d,
            me,
            Iri::new_unchecked("http://example.org/message"),
            SimpleTerm::LiteralLanguage(
                "hello\nworld".into(),
                LanguageTag::new_unchecked("en".into()),
                None,
            ),
            Some(Iri::new_unchecked("tag:g1")),
        )?;
        let r = BnodeId::new_unchecked("r");
        let tr = d[0].spog().0.map(Clone::clone);
        MutableDataset::insert(
            &mut d,
            r,
            rdf::reifies,
            SimpleTerm::Triple(Box::new(tr)),
            None as Option<i32>,
        )?;
        MutableDataset::insert(
            &mut d,
            r,
            Iri::new_unchecked("http://schema.org/creator"),
            VarName::new_unchecked("x"),
            None as Option<i32>,
        )?;

        let exp = r#"_:me	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>	<http://schema.org/Person>		.
_:me	<http://schema.org/name>	"Pierre-Antoine"	_:me	.
_:me	<http://example.org/value>	"42"^^<http://www.w3.org/2001/XMLSchema#integer>	_:me	.
_:me	<http://example.org/message>	"hello\nworld"@en	<tag:g1>	.
_:r	<http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies>	<<( _:me <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> )>>		.
_:r	<http://schema.org/creator>	?x		.
"#;

        let s1 = NQuadsSerializer::new_stringifier()
            .serialize_dataset(&d)
            .unwrap()
            .to_string();
        assert_eq!(&s1, exp);

        let s2 =
            NQuadsSerializer::new_stringifier_with_config(NQuadsConfig::new().with_canonical(true))
                .serialize_dataset(&d)
                .unwrap()
                .to_string();
        assert_eq!(&s2, &exp.replace("\t\t.", " .").replace("\t", " "));

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
    #[test_case("quad i i i i")]
    #[test_case("quad b i i i")]
    #[test_case("quad i i b i")]
    #[test_case("quad b i b i")]
    #[test_case("quad i i l i")]
    #[test_case("quad b i l i")]
    #[test_case("quad i i ld i")]
    #[test_case("quad b i ld i")]
    #[test_case("quad i i ll i")]
    #[test_case("quad b i ll i")]
    #[test_case("quad i i lb i")]
    #[test_case("quad b i lb i")]
    #[test_case("quad i i t i")]
    #[test_case("quad b i t i")]
    #[test_case("quad i i i b")]
    #[test_case("quad b i i b")]
    #[test_case("quad i i b b")]
    #[test_case("quad b i b b")]
    #[test_case("quad i i l b")]
    #[test_case("quad b i l b")]
    #[test_case("quad i i ld b")]
    #[test_case("quad b i ld b")]
    #[test_case("quad i i ll b")]
    #[test_case("quad b i ll b")]
    #[test_case("quad i i lb b")]
    #[test_case("quad b i lb b")]
    #[test_case("quad i i t b")]
    #[test_case("quad b i t b")]
    fn roundtrip(key: &str) -> Result<(), Box<dyn Error>> {
        static TESTS: LazyMap = nq_samples();
        let (nq, _) = TESTS.get(key).unwrap();
        let d1: Vec<Spog<SimpleTerm>> = crate::parser::nq::parse_str(nq).collect_quads()?;

        let config = NQuadsConfig::new();
        let out = NQuadsSerializer::new_stringifier_with_config(config)
            .serialize_quads(d1.quads())?
            .to_string();
        println!("\n>>> DEBUG\n{}", &out);

        let d2: Vec<Spog<SimpleTerm>> = crate::parser::nq::parse_str(&out).collect_quads()?;

        assert!(isomorphic_datasets(&d1, &d2)?);
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
    #[test_case("quad i i i i")]
    #[test_case("quad b i i i")]
    #[test_case("quad i i b i")]
    #[test_case("quad b i b i")]
    #[test_case("quad i i l i")]
    #[test_case("quad b i l i")]
    #[test_case("quad i i ld i")]
    #[test_case("quad b i ld i")]
    #[test_case("quad i i ll i")]
    #[test_case("quad b i ll i")]
    #[test_case("quad i i lb i")]
    #[test_case("quad b i lb i")]
    #[test_case("quad i i t i")]
    #[test_case("quad b i t i")]
    #[test_case("quad i i i b")]
    #[test_case("quad b i i b")]
    #[test_case("quad i i b b")]
    #[test_case("quad b i b b")]
    #[test_case("quad i i l b")]
    #[test_case("quad b i l b")]
    #[test_case("quad i i ld b")]
    #[test_case("quad b i ld b")]
    #[test_case("quad i i ll b")]
    #[test_case("quad b i ll b")]
    #[test_case("quad i i lb b")]
    #[test_case("quad b i lb b")]
    #[test_case("quad i i t b")]
    #[test_case("quad b i t b")]
    fn roundtrip_canonical(key: &str) -> Result<(), Box<dyn Error>> {
        static TESTS: LazyMap = nq_samples();
        let (nq, _) = TESTS.get(key).unwrap();
        let d1: Vec<Spog<SimpleTerm>> = crate::parser::nq::parse_str(nq).collect_quads()?;

        let config = NQuadsConfig::new().with_canonical(true);
        let out = NQuadsSerializer::new_stringifier_with_config(config)
            .serialize_quads(d1.quads())?
            .to_string();
        println!("\n>>> DEBUG\n{}", &out);

        let d2: Vec<Spog<SimpleTerm>> = crate::parser::nq::parse_str(&out).collect_quads()?;

        assert!(isomorphic_datasets(&d1, &d2)?);
        Ok(())
    }
}
