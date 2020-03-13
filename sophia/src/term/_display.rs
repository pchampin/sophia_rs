// this module is transparently re-exported by its parent `term`
//
// Implement the Display trait for Term, using the Turtle family of syntax.

use std::fmt;

use crate::term::*;

impl<T> fmt::Display for Term<T>
where
    T: TermData,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_term(f, self)
    }
}

/// Write a single RDF term into `w` using the N-Triples syntax.
fn write_term<T, W>(w: &mut W, t: &Term<T>) -> fmt::Result
where
    T: TermData,
    W: fmt::Write,
{
    use self::LiteralKind::*;
    use self::Term::*;
    match t {
        Iri(iri) => {
            iri.write_fmt(w)?;
        }
        BNode(bn) => {
            bn.write_fmt(w)?;
        }
        Literal(value, Lang(tag)) => {
            w.write_char('"')?;
            write_quoted_string(w, value.as_ref())?;
            w.write_str("\"@")?;
            w.write_str(tag.as_ref())?;
        }
        Literal(value, Datatype(iri)) => {
            w.write_char('"')?;
            write_quoted_string(w, value.as_ref())?;
            w.write_char('"')?;
            if iri != "http://www.w3.org/2001/XMLSchema#string" {
                w.write_str("^^")?;
                iri.write_fmt(w)?;
            }
        }
        Variable(var) => {
            var.write_fmt(w)?;
        }
    };
    Ok(())
}

fn write_quoted_string(w: &mut impl fmt::Write, txt: &str) -> fmt::Result {
    let mut cut = txt.len();
    let mut cutchar = '\0';
    for (pos, chr) in txt.char_indices() {
        if chr <= '\\' && (chr == '\n' || chr == '\r' || chr == '\\' || chr == '"') {
            cut = pos;
            cutchar = chr;
            break;
        }
    }
    w.write_str(&txt[..cut])?;
    if cut < txt.len() {
        match cutchar {
            '\n' => {
                w.write_str("\\n")?;
            }
            '\r' => {
                w.write_str("\\r")?;
            }
            '"' => {
                w.write_str("\\\"")?;
            }
            '\\' => {
                w.write_str("\\\\")?;
            }
            _ => unreachable!(),
        }
    };
    if cut + 1 >= txt.len() {
        return Ok(());
    } // else
    write_quoted_string(w, &txt[cut + 1..])
}

#[cfg(test)]
pub(crate) mod test {
    use crate::ns::*;
    use crate::term::*;
    use lazy_static::lazy_static;

    lazy_static! {
        pub(crate) static ref NT_TERMS: Vec<(StaticTerm, &'static str)> = vec![
            (
                StaticTerm::new_iri("http://example.org/foo/bar").unwrap(),
                r"<http://example.org/foo/bar>",
            ),
            (
                StaticTerm::new_iri_suffixed("http://example.org/foo/", "bar").unwrap(),
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
                // Literal with non-ascii characters
                StaticTerm::new_literal_dt("é \u{10000}", xsd::string).unwrap(),
                // in canonical form, non-ascii characters are NOT escaped in literals
                "\"é \u{10000}\"",
            )
        ];
    }

    #[test]
    fn terms() {
        for (term, expected) in NT_TERMS.iter() {
            let got = format!("{}", term);
            assert_eq!(&got, expected);
        }
    }
}
