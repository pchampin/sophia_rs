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

use std::io;

use crate::quad::{stream::*, Quad};

use super::nt::write_term;
use super::*;

/// N-Quads serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct NqConfig {
    ascii: bool,
}

impl NqConfig {
    pub fn set_ascii(&mut self, ascii: bool) -> &mut Self {
        self.ascii = ascii;
        self
    }
}

// N-Quads serializer.
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
    pub fn new(write: W) -> NqSerializer<W> {
        Self::new_with_config(write, NqConfig::default())
    }

    /// Build a new N-Quads serializer writing to `write`, with the given config.
    pub fn new_with_config(write: W, config: NqConfig) -> NqSerializer<W> {
        NqSerializer { write, config }
    }

    /// Borrow this serializer's configuration.
    pub fn config(&self) -> &NqConfig {
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
                    write_term(w, q.s())?;
                    w.write_all(b" ")?;
                    write_term(w, q.p())?;
                    w.write_all(b" ")?;
                    write_term(w, q.o())?;
                    if let Some(n) = q.g() {
                        w.write_all(b" ")?;
                        write_term(w, n)?;
                    }
                    w.write_all(b".\n")
                }
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
            })
            .map(|_| self)
    }
}

impl NqSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    pub fn new_stringifier() -> Self {
        NqSerializer::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    pub fn new_stringifier_with_config(config: NqConfig) -> Self {
        NqSerializer::new_with_config(Vec::new(), config)
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
    use sophia_api::ns::*;
    use sophia_term::literal::convert::AsLiteral;
    use sophia_term::*;

    #[test]
    fn dataset() {
        let me = StaticTerm::new_iri("http://champin.net/#pa").unwrap();
        let d = vec![
            (
                [
                    me,
                    rdf::type_.into(),
                    StaticTerm::new_iri("http://schema.org/Person").unwrap(),
                ],
                None,
            ),
            (
                [
                    me,
                    StaticTerm::new_iri("http://schema.org/name").unwrap(),
                    "Pierre-Antoine".as_literal().into(),
                ],
                Some(StaticTerm::new_iri("http://champin.net/").unwrap()),
            ),
        ];
        let s = NqSerializer::new_stringifier()
            .serialize_dataset(&d)
            .unwrap()
            .to_string();
        assert_eq!(
            &s,
            r#"<http://champin.net/#pa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>.
<http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine" <http://champin.net/>.
"#
        );
    }
}
