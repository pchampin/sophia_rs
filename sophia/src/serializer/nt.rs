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
        source: &mut TS,
    ) -> StreamResult<&mut Self, TS::Error, Self::Error>
    where
        TS: TripleSource,
    {
        if self.config.ascii {
            todo!("Pure-ASCII N-Triples is not implemented yet")
        }
        source
            .try_for_each_triple(|t| {
                let w = &mut self.write;
                writeln!(w, "{} {} {} .", t.s(), t.p(), t.o())
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

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use crate::ns::*;
    use crate::term::*;

    #[test]
    fn graph() {
        let me = StaticTerm::new_iri("http://champin.net/#pa").unwrap();
        let g = vec![
            [
                me,
                rdf::type_,
                StaticTerm::new_iri("http://schema.org/Person").unwrap(),
            ],
            [
                me,
                StaticTerm::new_iri("http://schema.org/name").unwrap(),
                "Pierre-Antoine".into(),
            ],
        ];
        let s = NtSerializer::new_stringifier()
            .serialize_graph(&g)
            .unwrap()
            .to_string();
        assert_eq!(
            &s,
            r#"<http://champin.net/#pa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine" .
"#
        );
    }
}
