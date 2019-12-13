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
use std::mem::swap;

use crate::term::{LiteralKind, Term, TermData};
use crate::triple::stream::*;
use crate::triple::Triple;

use super::*;

/// N-Triples serializer configuration.
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

/// A [`TripleSink`] returned by [`Config::writer`].
///
/// [`TripleSink`]: ../../triple/stream/trait.TripleSink.html
/// [`Config::writer`]: struct.Config.html#method.writer
pub struct Writer<W: io::Write> {
    write: W,
}

impl<W: io::Write> TripleWriter<W> for Writer<W> {
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

impl<W: io::Write> TripleSink for Writer<W> {
    type Outcome = ();
    type Error = SerializationError;

    fn feed<'a, T: Triple<'a>>(&mut self, t: &T) -> Result<(), Self::Error> {
        let w = &mut self.write;

        write_term(w, t.s())?;
        w.write_all(b" ")?;
        write_term(w, t.p())?;
        w.write_all(b" ")?;
        write_term(w, t.o())?;
        w.write_all(b" .\n")?;
        Ok(())
    }

    fn finish(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

def_triple_stringifier!();

/// Write a single RDF term into `w` using the N-Triples syntax.
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

/// Stringifies a single RDF term using the N-Triples syntax.
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
pub(crate) mod test {
    use super::*;
    use crate::ns::*;
    use crate::term::*;

    lazy_static! {
        pub(crate) static ref NT_TERMS: Vec<(StaticTerm, &'static str)> = vec![
            (
                StaticTerm::new_iri("http://example.org/foo/bar").unwrap(),
                r"<http://example.org/foo/bar>",
            ),
            (
                StaticTerm::new_iri2("http://example.org/foo/", "bar").unwrap(),
                r"<http://example.org/foo/bar>",
            ),
            (
                // IRI with non ascii term
                StaticTerm::new_iri("http://example.org/hé/\u{10000}/").unwrap(),
                "<http://example.org/hé/\u{10000}/>",
            ),
            (
                // BNode nice
                StaticTerm::new_bnode("foo_bar.baz").unwrap(),
                r"_:foo_bar.baz",
            ),
            (
                // BNode naughty
                StaticTerm::new_bnode("foo bar").unwrap(),
                r"_:_666p6p20626172_:_",
            ),
            (
                StaticTerm::new_literal_lang("chat", "fr-FR").unwrap(),
                r#""chat"@fr-FR"#,
            ),
            (
                StaticTerm::new_literal_dt("chat", xsd::string).unwrap(),
                r#""chat""#,
            ),
            (
                StaticTerm::new_literal_dt("42", xsd::integer).unwrap(),
                r#""42"^^<http://www.w3.org/2001/XMLSchema#integer>"#,
            ),
            (
                StaticTerm::new_literal_dt(" \n \r \\ \" hello world", xsd::string).unwrap(),
                r#"" \n \r \\ \" hello world""#,
            ),
            (
                // Literal with non-ascii characteres
                StaticTerm::new_literal_dt("é \u{10000}", xsd::string).unwrap(),
                // in canonical form, non-ascii characters are NOT escaped in literals
                "\"é \u{10000}\"",
            )
        ];
    }

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
        let triples = vec![
            [
                me,
                rdf::type_,
                StaticTerm::new_iri("http://schema.org/Person").unwrap(),
            ],
            [
                me,
                StaticTerm::new_iri("http://schema.org/name").unwrap(),
                StaticTerm::new_literal_dt("Pierre-Antoine", xsd::string).unwrap(),
            ],
        ];
        let triples = triples.into_iter().as_triple_source();
        let s = stringifier().feed_all_and_finish(triples).unwrap();
        assert_eq!(s, r#"<http://champin.net/#pa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine" .
"#);
    }
}
