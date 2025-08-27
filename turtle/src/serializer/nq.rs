//! Serializer for the [N-Quads] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [N-Quads]: https://www.w3.org/TR/n-quads/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use super::nt::{write_term, write_triple};
use sophia_api::quad::Quad;
use sophia_api::serializer::{QuadSerializer, Stringifier};
use sophia_api::source::{QuadSource, StreamResult};
use std::io;

/// N-Quads serializer configuration.
pub type NqConfig = super::nt::NtConfig;

/// N-Quads serializer.
pub struct NqSerializer<W> {
    config: NqConfig,
    write: W,
}

impl<W> NqSerializer<W>
where
    W: io::Write,
{
    /// Build a new N-Quads serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> Self {
        Self::new_with_config(write, NqConfig::default())
    }

    /// Build a new N-Quads serializer writing to `write`, with the given config.
    pub const fn new_with_config(write: W, config: NqConfig) -> Self {
        Self { config, write }
    }

    /// Borrow this serializer's configuration.
    pub const fn config(&self) -> &NqConfig {
        &self.config
    }
}

impl<W> QuadSerializer for NqSerializer<W>
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
        source
            .try_for_each_quad(|q| {
                {
                    let w = &mut self.write;
                    let (tr, gn) = q.spog();
                    write_triple(w, tr)?;
                    match gn {
                        None => w.write_all(b".\n"),
                        Some(t) => {
                            w.write_all(b" ")?;
                            write_term(w, t)?;
                            w.write_all(b".\n")
                        }
                    }
                }
                .map_err(io::Error::other)
            })
            .map(|()| self)
    }
}

impl NqSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    #[must_use]
    pub fn new_stringifier() -> Self {
        Self::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    #[must_use]
    pub const fn new_stringifier_with_config(config: NqConfig) -> Self {
        Self::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for NqSerializer<Vec<u8>> {
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
    use sophia_api::dataset::MutableDataset;
    use sophia_api::ns::*;
    use sophia_api::quad::Spog;
    use sophia_api::term::{BnodeId, LanguageTag, SimpleTerm, VarName};
    use sophia_iri::Iri;

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
        let tr = d[0].spog().0.map(Clone::clone);
        MutableDataset::insert(
            &mut d,
            SimpleTerm::Triple(Box::new(tr)),
            Iri::new_unchecked("http://schema.org/creator"),
            VarName::new_unchecked("x"),
            None as Option<i32>,
        )?;

        let s = NqSerializer::new_stringifier()
            .serialize_dataset(&d)
            .unwrap()
            .to_string();
        assert_eq!(
            &s,
            r#"_:me <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>.
_:me <http://schema.org/name> "Pierre-Antoine" _:me.
_:me <http://example.org/value> "42"^^<http://www.w3.org/2001/XMLSchema#integer> _:me.
_:me <http://example.org/message> "hello\nworld"@en <tag:g1>.
<<_:me <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>>> <http://schema.org/creator> ?x.
"#
        );
        Ok(())
    }
}
