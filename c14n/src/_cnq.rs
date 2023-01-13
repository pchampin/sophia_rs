//! Canonical N-Quads

use sophia_api::term::{Term, TermKind};

/// Serialize a term in canonical n-quads
pub fn nq<T: Term>(term: T, buffer: &mut String) {
    match term.kind() {
        TermKind::Iri => {
            buffer.push('<');
            buffer.push_str(&term.iri().unwrap());
            buffer.push('>');
        }
        TermKind::Literal => {
            buffer.push('"');
            for c in term.lexical_form().unwrap().chars() {
                match c {
                    '"' => buffer.push_str("\\\""),
                    '\\' => buffer.push_str("\\\\"),
                    '\n' => buffer.push_str("\\n"),
                    '\r' => buffer.push_str("\\r"),
                    _ => buffer.push(c),
                }
            }
            buffer.push('"');
            if let Some(tag) = term.language_tag() {
                buffer.push('@');
                buffer.push_str(&tag);
            } else {
                buffer.push_str("^^");
                nq(term.datatype().unwrap(), buffer);
                buffer.pop(); // remove spurious space after datatype
            }
        }
        TermKind::BlankNode => {
            buffer.push_str("_:");
            buffer.push_str(&term.bnode_id().unwrap());
        }
        TermKind::Triple => {
            buffer.push_str("<< ");
            for subterm in term.triple().unwrap() {
                nq(subterm, buffer);
            }
            buffer.push_str(">>");
        }
        TermKind::Variable => {
            buffer.push('?');
            buffer.push_str(&term.variable().unwrap());
        }
    }
    buffer.push(' ');
}
