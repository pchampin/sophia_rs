//! Serializer for the [N-Triples] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [N-Triples]: https://www.w3.org/TR/n-triples/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use sophia_api::ns::xsd;
use sophia_api::serializer::{Stringifier, TripleSerializer};
use sophia_api::source::{StreamResult, TripleSource};
use sophia_api::term::{Term, TermKind};
use sophia_api::triple::Triple;
use std::io;

/// N-Triples serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct NtConfig {
    pub(super) ascii: bool,
}

impl NtConfig {
    /// Set the ascii configuration.
    pub fn set_ascii(&mut self, ascii: bool) -> &mut Self {
        self.ascii = ascii;
        self
    }
}

/// N-Triples serializer.
pub struct NtSerializer<W> {
    config: NtConfig,
    write: W,
}

impl<W> NtSerializer<W>
where
    W: io::Write,
{
    /// Build a new N-Triples serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> Self {
        Self::new_with_config(write, NtConfig::default())
    }

    /// Build a new N-Triples serializer writing to `write`, with the given config.
    pub const fn new_with_config(write: W, config: NtConfig) -> Self {
        Self { config, write }
    }

    /// Borrow this serializer's configuration.
    pub const fn config(&self) -> &NtConfig {
        &self.config
    }
}

impl<W> TripleSerializer for NtSerializer<W>
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
        source
            .try_for_each_triple(|t| {
                {
                    let w = &mut self.write;
                    write_triple(w, t)?;
                    w.write_all(b".\n")
                }
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
            })
            .map(|()| self)
    }
}

impl NtSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    #[must_use] pub fn new_stringifier() -> Self {
        Self::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    #[must_use] pub const fn new_stringifier_with_config(config: NtConfig) -> Self {
        Self::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for NtSerializer<Vec<u8>> {
    fn as_utf8(&self) -> &[u8] {
        &self.write[..]
    }
}

/// Write the given term into the given write in the N-Triples format.
pub fn write_triple<W, T>(w: &mut W, t: T) -> io::Result<()>
where
    W: io::Write,
    T: Triple,
{
    write_term(w, t.s())?;
    w.write_all(b" ")?;
    write_term(w, t.p())?;
    w.write_all(b" ")?;
    write_term(w, t.o())?;
    Ok(())
}

/// Write the given term into the given write in the N-Triples format.
pub fn write_term<W, T>(w: &mut W, t: T) -> io::Result<()>
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
            quoted_string(w, t.lexical_form().unwrap().as_bytes())?;
            if let Some(tag) = t.language_tag() {
                w.write_all(b"\"@")?;
                w.write_all(tag.as_bytes())?;
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
            w.write_all(b"<<")?;
            write_triple(w, t.to_triple().unwrap())?;
            w.write_all(b">>")?;
        }
        Variable => {
            w.write_all(b"?")?;
            w.write_all(t.variable().unwrap().as_bytes())?;
        }
    }
    Ok(())
}

pub(crate) fn quoted_string<W: io::Write>(w: &mut W, txt: &[u8]) -> io::Result<()> {
    let mut cut = txt.len();
    let mut cutchar = b'\0';
    for (pos, chr) in txt.iter().enumerate() {
        let chr = *chr;
        if chr <= b'\\' && (chr == b'\n' || chr == b'\r' || chr == b'\\' || chr == b'"') {
            cut = pos;
            cutchar = chr;
            break;
        }
    }
    w.write_all(&txt[..cut])?;
    if cut < txt.len() {
        match cutchar {
            b'\n' => {
                w.write_all(b"\\n")?;
            }
            b'\r' => {
                w.write_all(b"\\r")?;
            }
            b'"' => {
                w.write_all(b"\\\"")?;
            }
            b'\\' => {
                w.write_all(b"\\\\")?;
            }
            _ => unreachable!(),
        }
    };
    if cut + 1 >= txt.len() {
        Ok(())
    } else {
        quoted_string(w, &txt[cut + 1..])
    }
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use sophia_api::graph::MutableGraph;
    use sophia_api::ns::*;
    use sophia_api::term::{BnodeId, LanguageTag, SimpleTerm, VarName};
    use sophia_iri::Iri;

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
            ),
        )?;
        let tr = g[0].clone();
        MutableGraph::insert(
            &mut g,
            SimpleTerm::Triple(Box::new(tr)),
            Iri::new_unchecked("http://schema.org/creator"),
            VarName::new_unchecked("x"),
        )?;

        let s = NtSerializer::new_stringifier()
            .serialize_graph(&g)
            .unwrap()
            .to_string();
        assert_eq!(
            &s,
            r#"_:me <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>.
_:me <http://schema.org/name> "Pierre-Antoine".
_:me <http://example.org/value> "42"^^<http://www.w3.org/2001/XMLSchema#integer>.
_:me <http://example.org/message> "hello\nworld"@en.
<<_:me <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>>> <http://schema.org/creator> ?x.
"#
        );
        Ok(())
    }
}
