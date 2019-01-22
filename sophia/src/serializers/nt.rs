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

use std::borrow::Borrow;
use std::io;
use std::mem::swap;

use ::streams::*;
use ::term::{LiteralKind,Term};
use ::triple::Triple;

use super::*;


/// NT serializer configuration.
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
/// [`TripleSink`]: ../../streams/trait.TripleSink.html
/// [`Config::writer`]: struct.Config.html#method.writer
pub struct Writer<W: io::Write> {
    write: W,
}

impl<W: io::Write> WriteSerializer<W> for Writer<W> {
    type Config = Config;

    fn new(write: W, config: Self::Config) -> Self {
        if config.ascii { unimplemented!() }
        // TODO if ascii is true,
        // wrap write in a dedicated type that will rewrite non-ascii characters
        Writer{ write }
    }
}

impl<W: io::Write> TripleSink for Writer<W> {
    type Outcome = ();
    type Error = Error;

    fn feed<'a, T: Triple<'a>>(&mut self, t: &T) -> Result<(), Self::Error> {
        let w = &mut self.write;

        (|| {
            write_term(w, t.s())?;
            w.write_all(" ".as_bytes())?;
            write_term(w, t.p())?;
            w.write_all(" ".as_bytes())?;
            write_term(w, t.o())?;
            w.write_all(" .\n".as_bytes())
        })()
        .chain_err(||
            ErrorKind::SerializerError("NT serializer".into())
        )
    }

    fn finish(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

def_stringifier!();


/// Write a single RDF term into `w` using the NT syntax.
pub fn write_term<T,W> (w: &mut W, t: &Term<T>) -> io::Result<()> where
    T: Borrow<str>,
    W: io::Write,
{
    use self::Term::*;
    use self::LiteralKind::*;
    match t {
        Iri(iri) => {
            w.write_all("<".as_bytes())?;
            iri.write_to(w)?;
            w.write_all(">".as_bytes())?;
        }
        BNode(ident) => {
            w.write_all("_:".as_bytes())?;
            if ident.is_n3() {
                w.write_all((ident.borrow() as &str).as_bytes())?;
            } else {
                write_non_n3_bnode_id(w, ident.borrow())?;
            }
        }
        Literal(value, Lang(tag)) => {
            w.write_all("\"".as_bytes())?;
            write_quoted_string(w, value.borrow())?;
            w.write_all("\"@".as_bytes())?;
            w.write_all(tag.borrow().as_bytes())?;
        }
        Literal(value, Datatype(iri)) => {
            w.write_all("\"".as_bytes())?;
            write_quoted_string(w, value.borrow())?;
            w.write_all("\"".as_bytes())?;
            if iri != &"http://www.w3.org/2001/XMLSchema#string" {
                w.write_all("^^<".as_bytes())?;
                iri.write_to(w)?;
                w.write_all(">".as_bytes())?;
            }
        }
        Variable(name) => {
            w.write_all("?".as_bytes())?;
            w.write_all(name.borrow().as_bytes())?;
        }
    };
    Ok(())
}

/// Stringifies a single RDF term using the NT syntax.
pub fn stringify_term<T> (t: &Term<T>) -> String where
    T: Borrow<str>,
{
    let mut v = Vec::new();
    write_term(&mut v, t).unwrap();
    unsafe { String::from_utf8_unchecked(v) }
}


pub(crate) fn write_quoted_string(w: &mut impl io::Write, txt: &str) -> io::Result<()> {
    let mut cut = txt.len();
    let mut cutchar = '\0';
    for (pos, chr) in txt.char_indices() {
        if chr<='\\' && (chr=='\n' || chr=='\r' || chr=='\\' || chr=='"') {
            cut = pos;
            cutchar = chr;
            break;
        }
    }
    w.write_all(txt[..cut].as_bytes())?;
    if cut < txt.len() {
        match cutchar {
            '\n' => { w.write_all(r"\n".as_bytes())?; }
            '\r' => { w.write_all(r"\r".as_bytes())?; }
            '"'  => { w.write_all("\\\"".as_bytes())?; }
            '\\' => { w.write_all(r"\\".as_bytes())?; }
            _    => unreachable!()
         }
    };
    if cut+1 >= txt.len() { return Ok(()); } // else
    write_quoted_string(w, &txt[cut+1..])
}

pub(crate) fn write_non_n3_bnode_id(w: &mut impl io::Write, id: &str) -> io::Result<()> {
    fn halfbyte_to_hex(val: u8) -> u8 {
        if val < 10 { ('0' as u8) + val }
        else        { ('a' as u8) + val }
    }
    w.write_all("_".as_bytes())?;
    for b in id.as_bytes() {
        w.write_all(&[
            halfbyte_to_hex(b/16),
            halfbyte_to_hex(b%16),
        ])?;
    }
    w.write_all("_:_".as_bytes())?;
    Ok(())
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use ::ns::*;
    use ::term::*;
    use super::*;

    #[test]
    fn iri_() {
        let t = StaticTerm::new_iri("http://example.org/foo/bar").unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, r"<http://example.org/foo/bar>");
    }

    #[test]
    fn iri2_() {
        let t = StaticTerm::new_iri2("http://example.org/foo/", "bar").unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, r"<http://example.org/foo/bar>");
    }

    #[test]
    fn iri_with_non_ascii() {
        // in canonical form, non-ascii characters are NOT escaped in IRIs
        let t = StaticTerm::new_iri("http://example.org/hé/\u{10000}/").unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, "<http://example.org/hé/\u{10000}/>");
    }

    #[test]
    fn bnode_nice() {
        let t = StaticTerm::new_bnode("foo_bar.baz").unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, r"_:foo_bar.baz");
    }

    #[test]
    fn bnode_naughty() {
        let t = StaticTerm::new_bnode("foo bar").unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, r"_:_666p6p20626172_:_");
    }

    #[test]
    fn literal_lang_() {
        let t = StaticTerm::new_literal_lang("chat", "fr-FR").unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, r#""chat"@fr-FR"#);
    }

    #[test]
    fn literal_string() {
        let t = StaticTerm::new_literal_dt("chat", xsd::string).unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, r#""chat""#);
    }

    #[test]
    fn literal_integer() {
        let t = StaticTerm::new_literal_dt("42", xsd::integer).unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, r#""42"^^<http://www.w3.org/2001/XMLSchema#integer>"#);
    }

    #[test]
    fn literal_with_escapes() {
        let t = StaticTerm::new_literal_dt(" \n \r \\ \" hello world", xsd::string).unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, r#"" \n \r \\ \" hello world""#)
    }

    #[test]
    fn literal_with_non_ascii() {
        // in canonical form, non-ascii characters are NOT escaped in literals
        let t = StaticTerm::new_literal_dt("é \u{10000}", xsd::string).unwrap();
        let s = stringify_term(&t);
        assert_eq!(s, "\"é \u{10000}\"")
    }

    #[test]
    fn graph() {
        let me = StaticTerm::new_iri("http://champin.net/#pa").unwrap();
        let triples = vec![
            [ me,
              rdf::type_,
              StaticTerm::new_iri("http://schema.org/Person").unwrap()
            ],
            [ me,
              StaticTerm::new_iri("http://schema.org/name").unwrap(),
              StaticTerm::new_literal_dt("Pierre-Antoine", xsd::string).unwrap()
            ],
        ];
        let mut triples = triples.into_iter().as_triple_source();
        let s = triples.in_sink(&mut stringifier()).unwrap();
        assert_eq!(s, r#"<http://champin.net/#pa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine" .
"#);
    }
}
