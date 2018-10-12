// TODO proper documentation (using rust specific comments)

// TODO document the fact that
// all functions in this module accept any `Write`,
// but are not trying to optimize the number of io::Write operations,
// so in most cases, they should be passed a `BufWriter`

// TODO by default, this serializer produces canonical (i.e. UTF-8) N-Triples.
// There should be an `ascii<>` sub-modules providing an equivalent API,
// but producing pure-ascii N-Triples.
// This could be done by post-processing the canonical form,
// taking care of *not* double-escaping `\n`, `\r`, `\\` and `"`.

use std::borrow::Borrow;
use std::fmt::Debug;
use std::io;

use ::graph::Graph;
use ::term::{LiteralKind,Term};
use ::triple::Triple;

/*
*/
pub fn write_graph<G> (w: &mut impl io::Write, g: &G) -> io::Result<()>  where
    G: Graph,
{
    write_triples(w, &mut g.iter())
}

pub fn stringify_graph<G> (g: &G) -> String where
    G: Graph,
{
    stringify_triples(&mut g.iter())
}

pub fn write_triples<I, W, T, E> (w: &mut W, triples: &mut I) -> io::Result<()>  where
    W: io::Write,
    I: Iterator<Item=Result<T, E>>,
    T: Triple,
    E: Debug,
{
    for ftriple in triples {
        let triple = ftriple.unwrap();
        write_triple(w, &triple)?; // TODO handle error of triples
    }
    Ok(())
}

pub fn stringify_triples<I, T, E> (triples: &mut I) -> String where
    I: Iterator<Item=Result<T, E>>,
    T: Triple,
    E: Debug,
{
    let mut v = Vec::new();
    write_triples(&mut v, triples).unwrap();
    unsafe { String::from_utf8_unchecked(v) }
}

pub fn write_triple<T> (w: &mut impl io::Write, t: &T) -> io::Result<()> where
    T: Triple,
{
    write_term(w, t.s())?;
    w.write_all(" ".as_bytes())?;
    write_term(w, t.p())?;
    w.write_all(" ".as_bytes())?;
    write_term(w, t.o())?;
    w.write_all(" .\n".as_bytes())?;
    Ok(())
}

pub fn stringify_triple<T> (t: &T) -> String where
    T: Triple,
{
    let mut v = Vec::new();
    write_triple(&mut v, t).unwrap();
    unsafe { String::from_utf8_unchecked(v) }
}

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
            ( me,
              rdf::type_,
              StaticTerm::new_iri("http://schema.org/Person").unwrap()
            ),
            ( me,
              StaticTerm::new_iri("http://schema.org/name").unwrap(),
              StaticTerm::new_literal_dt("Pierre-Antoine", xsd::string).unwrap()
            ),
        ];
        let s = stringify_triples(&mut triples.iter());
        assert_eq!(s, r#"<http://champin.net/#pa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine" .
"#);
    }
}
