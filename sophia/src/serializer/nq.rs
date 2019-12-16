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
use std::mem::swap;
use std::result::Result as StdResult;

use crate::error::*;
use crate::quad::stream::*;
use crate::quad::Quad;
use crate::term::{LiteralKind, Term, TermData};

use super::*;

/// N-Quads serializer configuration.
///
/// For more information,
/// see the [uniform interface] of serializers.
///
/// [uniform interface]: ../index.html#uniform-interface
///
#[derive(Clone, Debug, Default)]
pub struct Config {
    ascii: bool,
}

impl Config {
    pub fn writer<W: io::Write>(&self, write: W) -> Writer<W> {
        Writer::new(write, self.clone())
    }

    pub fn stringifier(&self) -> Stringifier {
        Stringifier::new(self.clone())
    }
}

def_default_serializer_api!();

/// A [`QuadSink`] returned by [`Config::writer`].
///
/// [`QuadSink`]: ../../quad/stream/trait.QuadSink.html
/// [`Config::writer`]: struct.Config.html#method.writer
pub struct Writer<W: io::Write> {
    write: W,
}

impl<W: io::Write> QuadWriter<W> for Writer<W> {
    type Config = Config;

    fn new(write: W, config: Self::Config) -> Self {
        if config.ascii {
            unimplemented!()
        }
        // TODO if ascii is true,
        // wrap write in a dedicated type that will rewrite non-ascii characters
        Writer { write }
    }
}

impl<W: io::Write> QuadSink for Writer<W> {
    type Outcome = ();
    type Error = Error;

    fn feed<'a, T: Quad<'a>>(&mut self, t: &T) -> StdResult<(), Self::Error> {
        let w = &mut self.write;

        (|| {
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
        })()
        .chain_err(|| ErrorKind::SerializerError("N-Quads serializer".into()))
    }

    fn finish(&mut self) -> StdResult<(), Self::Error> {
        Ok(())
    }
}

def_quad_stringifier!();

/// Write a single RDF term into `w` using the N-Quads syntax.
pub fn write_term<T, W>(w: &mut W, t: &Term<T>) -> io::Result<()>
where
    T: TermData,
    W: io::Write,
{
    use self::LiteralKind::*;
    use self::Term::*;
    match t {
        Iri(iri) => {
            w.write_all(b"<")?;
            iri.write_to(w)?;
            w.write_all(b">")?;
        }
        BNode(ident) => {
            w.write_all(b"_:")?;
            if ident.is_n3() {
                w.write_all((ident.as_ref()).as_bytes())?;
            } else {
                write_non_n3_bnode_id(w, ident.as_ref())?;
            }
        }
        Literal(value, Lang(tag)) => {
            w.write_all(b"\"")?;
            write_quoted_string(w, value.as_ref())?;
            w.write_all(b"\"@")?;
            w.write_all(tag.as_ref().as_bytes())?;
        }
        Literal(value, Datatype(iri)) => {
            w.write_all(b"\"")?;
            write_quoted_string(w, value.as_ref())?;
            w.write_all(b"\"")?;
            if iri != &"http://www.w3.org/2001/XMLSchema#string" {
                w.write_all(b"^^<")?;
                iri.write_to(w)?;
                w.write_all(b">")?;
            }
        }
        Variable(name) => {
            w.write_all(b"?")?;
            w.write_all(name.as_ref().as_bytes())?;
        }
    };
    Ok(())
}

/// Stringifies a single RDF term using the N-Quads syntax.
pub fn stringify_term<T>(t: &Term<T>) -> String
where
    T: TermData,
{
    let mut v = Vec::new();
    write_term(&mut v, t).unwrap();
    unsafe { String::from_utf8_unchecked(v) }
}

pub(crate) fn write_quoted_string(w: &mut impl io::Write, txt: &str) -> io::Result<()> {
    let mut cut = txt.len();
    let mut cutchar = '\0';
    for (pos, chr) in txt.char_indices() {
        if chr <= '\\' && (chr == '\n' || chr == '\r' || chr == '\\' || chr == '"') {
            cut = pos;
            cutchar = chr;
            break;
        }
    }
    w.write_all(txt[..cut].as_bytes())?;
    if cut < txt.len() {
        match cutchar {
            '\n' => {
                w.write_all(b"\\n")?;
            }
            '\r' => {
                w.write_all(b"\\r")?;
            }
            '"' => {
                w.write_all(b"\\\"")?;
            }
            '\\' => {
                w.write_all(b"\\\\")?;
            }
            _ => unreachable!(),
        }
    };
    if cut + 1 >= txt.len() {
        return Ok(());
    } // else
    write_quoted_string(w, &txt[cut + 1..])
}

pub(crate) fn write_non_n3_bnode_id(w: &mut impl io::Write, id: &str) -> io::Result<()> {
    fn halfbyte_to_hex(val: u8) -> u8 {
        if val < 10 {
            b'0' + val
        } else {
            b'a' + val
        }
    }
    w.write_all(b"_")?;
    for b in id.as_bytes() {
        w.write_all(&[halfbyte_to_hex(b / 16), halfbyte_to_hex(b % 16)])?;
    }
    w.write_all(b"_:_")?;
    Ok(())
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::super::nt::test::NT_TERMS;
    use super::*;
    use crate::ns::*;
    use crate::term::*;

    #[test]
    fn terms() {
        for (term, expected) in NT_TERMS.iter() {
            let got = stringify_term(term);
            assert_eq!(got, *expected);
        }
    }

    #[test]
    fn graph() {
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
        let mut quads = quads.into_iter().as_quad_source();
        let s = quads.in_sink(&mut stringifier()).unwrap();
        assert_eq!(s, r#"<http://champin.net/#pa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine" <http://example.org/graph> .
"#);
    }
}
