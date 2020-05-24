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
use sophia_api::term::{TTerm, TermKind};
use std::io;

use crate::triple::stream::*;

use super::*;

/// N-Triples serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct NtConfig {
    ascii: bool,
}

impl NtConfig {
    pub fn set_ascii(&mut self, ascii: bool) -> &mut Self {
        self.ascii = ascii;
        self
    }
}

// N-Triples serializer.
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
    pub fn new(write: W) -> NtSerializer<W> {
        Self::new_with_config(write, NtConfig::default())
    }

    /// Build a new N-Triples serializer writing to `write`, with the given config.
    pub fn new_with_config(write: W, config: NtConfig) -> NtSerializer<W> {
        NtSerializer { write, config }
    }

    /// Borrow this serializer's configuration.
    pub fn config(&self) -> &NtConfig {
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
                    write_term(w, t.s())?;
                    w.write_all(b" ")?;
                    write_term(w, t.p())?;
                    w.write_all(b" ")?;
                    write_term(w, t.o())?;
                    w.write_all(b".\n")
                }
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
            })
            .map(|_| self)
    }
}

impl NtSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    pub fn new_stringifier() -> Self {
        NtSerializer::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    pub fn new_stringifier_with_config(config: NtConfig) -> Self {
        NtSerializer::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for NtSerializer<Vec<u8>> {
    fn as_utf8(&self) -> &[u8] {
        &self.write[..]
    }
}

/// Write the given term into the given write in the N-Triples format.
pub fn write_term<W, T>(w: &mut W, t: &T) -> io::Result<()>
where
    W: io::Write,
    T: TTerm + ?Sized,
{
    use TermKind::*;
    match t.kind() {
        Iri => {
            w.write_all(b"<")?;
            let v = t.value_raw();
            w.write_all(v.0.as_bytes())?;
            if let Some(suffix) = v.1 {
                w.write_all(suffix.as_bytes())?;
            }
            w.write_all(b">")
        }
        Literal => {
            w.write_all(b"\"")?;
            quoted_string(w, t.value_raw().0.as_bytes())?;
            match t.language() {
                Some(tag) => {
                    w.write_all(b"\"@")?;
                    w.write_all(tag.as_bytes())
                }
                None => {
                    let dt = t.datatype().unwrap();
                    if xsd::string != dt {
                        w.write_all(b"\"^^")?;
                        write_term(w, &dt)
                    } else {
                        w.write_all(b"\"")
                    }
                }
            }
        }
        BlankNode => {
            w.write_all(b"_:")?;
            w.write_all(t.value_raw().0.as_bytes())
        }
        Variable => {
            w.write_all(b"?")?;
            w.write_all(t.value_raw().0.as_bytes())
        }
    }
}

fn quoted_string<W: io::Write>(w: &mut W, txt: &[u8]) -> io::Result<()> {
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
    use sophia_api::ns::*;
    use sophia_term::literal::convert::AsLiteral;
    use sophia_term::*;

    #[test]
    fn graph() {
        let me = StaticTerm::new_iri("http://champin.net/#pa").unwrap();
        let g = vec![
            [
                me,
                rdf::type_.into(),
                StaticTerm::new_iri("http://schema.org/Person").unwrap(),
            ],
            [
                me,
                StaticTerm::new_iri("http://schema.org/name").unwrap(),
                "Pierre-Antoine".as_literal().into(),
            ],
        ];
        let s = NtSerializer::new_stringifier()
            .serialize_graph(&g)
            .unwrap()
            .to_string();
        assert_eq!(
            &s,
            r#"<http://champin.net/#pa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>.
<http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine".
"#
        );
    }
}
