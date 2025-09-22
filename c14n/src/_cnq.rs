//! Canonical N-Quads

use sophia_api::{
    ns::xsd,
    term::{Term, TermKind},
};

/// Serialize a term in canonical n-quads
pub fn nq<T: Term, W: std::fmt::Write>(term: T, write: &mut W) -> std::fmt::Result {
    match term.kind() {
        TermKind::Iri => {
            write.write_char('<')?;
            write.write_str(&term.iri().unwrap())?;
            write.write_char('>')?;
        }
        TermKind::Literal => {
            write.write_char('"')?;
            for c in term.lexical_form().unwrap().chars() {
                match c {
                    '"' => write.write_str("\\\"")?,
                    '\\' => write.write_str("\\\\")?,
                    '\n' => write.write_str("\\n")?,
                    '\r' => write.write_str("\\r")?,
                    '\t' => write.write_str("\\t")?,
                    '\x08' => write.write_str("\\b")?,
                    '\x0c' => write.write_str("\\f")?,
                    '\x7f' => write.write_str("\\u007F")?,
                    c if c <= '\x1f' => write.write_str(&format!("\\u{:04X}", c as u8))?,
                    _ => write.write_char(c)?,
                }
            }
            write.write_char('"')?;
            if let Some(tag) = term.language_tag() {
                write.write_char('@')?;
                write.write_str(&tag)?;
                if let Some(dir) = term.base_direction() {
                    write.write_str("--")?;
                    write!(write, "{dir}").unwrap();
                }
            } else {
                let datatype = term.datatype().unwrap();
                if !Term::eq(&datatype, xsd::string) {
                    write.write_str("^^")?;
                    nq(term.datatype().unwrap(), write).unwrap();
                }
            }
        }
        TermKind::BlankNode => {
            write.write_str("_:")?;
            write.write_str(&term.bnode_id().unwrap())?;
        }
        TermKind::Triple => {
            write.write_str("<<( ")?;
            for subterm in term.triple().unwrap() {
                nq(subterm, write).unwrap();
                write.write_char(' ')?;
            }
            write.write_str(")>>")?;
        }
        TermKind::Variable => {
            write.write_char('?')?;
            write.write_str(&term.variable().unwrap())?;
        }
    }
    Ok(())
}
