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

use crate::quad::stream::*;
use crate::quad::Quad;
use crate::serializer::nt::write_term;

use super::*;

/// N-Quads serializer.
#[derive(Clone, Debug, Default)]
pub struct NqSerializer {
    ascii: bool,
    strict: bool,
}

impl<W: io::Write> QuadSerializer<W> for NqSerializer {
    type Sink = NqSink<W>;

    fn to(&self, write: W) -> Self::Sink {
        NqSink::new(self, write)
    }
}

derive_quad_stringifier!(NqSerializer);

def_mod_functions_for_write_quad_serializer!(NqSerializer);

/// The [`QuadSink`] type returned by [`NqSerializer::sink`]
///
/// [`QuadSink`]: ../../quad/stream/trait.QuadSink.html
/// [`NqSerialier::sink`]: struct.Config.html#method.sink
pub struct NqSink<W> {
    write: W,
}

impl<W: io::Write> NqSink<W> {
    fn new(serializer: &NqSerializer, write: W) -> Self {
        if serializer.ascii {
            unimplemented!()
        }
        // TODO if ascii is true,
        // wrap write in a dedicated type that will rewrite non-ascii characters
        if serializer.strict {
            unimplemented!()
        }
        // TODO if strict is true,
        // ensure that non-RDF quads are rejected
        NqSink { write }
    }
}

impl<W: io::Write> QuadSink for NqSink<W> {
    type Outcome = ();
    type Error = io::Error;

    fn feed<T: Quad>(&mut self, t: &T) -> Result<(), Self::Error> {
        let w = &mut self.write;

        write_term(w, t.s())?;
        w.write_all(b" ")?;
        write_term(w, t.p())?;
        w.write_all(b" ")?;
        write_term(w, t.o())?;
        if let Some(g) = t.g() {
            w.write_all(b" ")?;
            write_term(w, g)?;
        }
        w.write_all(b" .\n")
    }

    fn finish(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<W: io::Write> QuadSerializingSink for NqSink<W> {}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::ns::*;
    use crate::term::*;

    #[test]
    fn dataset() {
        let me = StaticTerm::new_iri("http://champin.net/#pa").unwrap();
        let quads = vec![
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
                Some(StaticTerm::new_iri("http://example.org/graph").unwrap()),
            ),
        ];
        let s = stringify_dataset(&quads).unwrap();
        assert_eq!(
            s,
            r#"<http://champin.net/#pa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine" <http://example.org/graph> .
"#
        );
    }
}
