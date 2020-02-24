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

use super::*;

/// N-Quads serializer configuration.
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

// N-Quads serializer.
pub struct NtSerializer<W> {
    config: NtConfig,
    write: W,
}

impl<W> NtSerializer<W>
where
    W: io::Write,
{
    /// Build a new N-Quads serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> NtSerializer<W> {
        Self::new_with_config(write, NtConfig::default())
    }

    /// Build a new N-Quads serializer writing to `write`, with the given config.
    pub fn new_with_config(write: W, config: NtConfig) -> NtSerializer<W> {
        NtSerializer { write, config }
    }

    /// Borrow this serializer's configuration.
    pub fn config(&self) -> &NtConfig {
        &self.config
    }
}

impl<W> QuadSerializer for NtSerializer<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn serialize_quads<QS>(
        &mut self,
        source: &mut QS,
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
                    write!(w, "{} {} {} ", q.s(), q.p(), q.o())?;
                    if let Some(g) = q.g() {
                        write!(w, "{} ", g)?;
                    }
                    w.write_all(b".\n")
                }
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
            })
            .map(|_| self)
    }
}

type NtStringifier = NtSerializer<Vec<u8>>;

impl NtStringifier {
    #[inline]
    pub fn new_stringifier() -> NtStringifier {
        NtSerializer::new(Vec::new())
    }

    #[inline]
    pub fn new_stringifier_with_config(config: NtConfig) -> NtStringifier {
        NtSerializer::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for NtStringifier {
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
    use crate::ns::*;
    use crate::term::*;

    #[test]
    fn dataset() {
        let me = StaticTerm::new_iri("http://champin.net/#pa").unwrap();
        let d = vec![
            (
                [
                    me,
                    rdf::type_,
                    StaticTerm::new_iri("http://schema.org/Person").unwrap(),
                ],
                None,
            ),
            (
                [
                    me,
                    StaticTerm::new_iri("http://schema.org/name").unwrap(),
                    StaticTerm::new_literal_dt("Pierre-Antoine", xsd::string).unwrap(),
                ],
                Some(StaticTerm::new_iri("http://champin.net/").unwrap()),
            ),
        ];
        let s = NtSerializer::new_stringifier()
            .serialize_dataset(&d)
            .unwrap()
            .to_string();
        assert_eq!(
            &s,
            r#"<http://champin.net/#pa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine" <http://champin.net/> .
"#
        );
    }
}
