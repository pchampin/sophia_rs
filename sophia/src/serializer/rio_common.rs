//! Serializer for the [RDF/XML] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [RDF/XML]: https://www.w3.org/TR/rdf-syntax-grammar/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use rio_api::formatter::TriplesFormatter;
use rio_api::model::{BlankNode, Literal, NamedNode, Triple as RioTriple};
use sophia_api::ns::xsd;
use sophia_api::term::{TTerm, TermKind};
use sophia_api::triple::stream::{StreamResult, TripleSource};
use sophia_api::triple::Triple;

/// Format each triple of `triples` using `tf`.
pub fn rio_format_triples<TF, TS>(
    tf: &mut TF,
    mut triples: TS,
) -> StreamResult<(), TS::Error, TF::Error>
where
    TF: TriplesFormatter,
    TS: TripleSource,
{
    triples.try_for_each_triple(|t| {
        let bufs;
        let bufp;
        let bufo;
        let bufd;
        let subject = {
            let term = t.s();
            match term.kind() {
                TermKind::Iri => {
                    bufs = term.value();
                    NamedNode { iri: &bufs }.into()
                }
                TermKind::BlankNode => BlankNode {
                    id: &term.value_raw().0,
                }
                .into(),
                _ => return Ok(()), // non standard subject, skip this triple
            }
        };
        let predicate = {
            let term = t.p();
            match term.kind() {
                TermKind::Iri => {
                    bufp = term.value();
                    NamedNode { iri: &bufp }
                }
                _ => return Ok(()), // non standard predicate, skip this triple
            }
        };
        let object = {
            let term = t.o();
            match term.kind() {
                TermKind::Iri => {
                    bufo = term.value();
                    NamedNode { iri: &bufo }.into()
                }
                TermKind::BlankNode => BlankNode {
                    id: &term.value_raw().0,
                }
                .into(),
                TermKind::Literal => match term.language() {
                    None => {
                        let datatype = term.datatype().unwrap();
                        if datatype == xsd::string {
                            Literal::Simple {
                                value: &term.value_raw().0,
                            }
                            .into()
                        } else {
                            bufd = datatype.value().to_string();
                            Literal::Typed {
                                value: &term.value_raw().0,
                                datatype: NamedNode { iri: &bufd },
                            }
                            .into()
                        }
                    }
                    Some(tag) => Literal::LanguageTaggedString {
                        value: &term.value_raw().0,
                        language: tag,
                    }
                    .into(),
                },
                _ => return Ok(()), // non standard object, skip this triple
            }
        };
        let rt = RioTriple {
            subject,
            predicate,
            object,
        };
        tf.format(&rt)?;
        Ok(())
    })
}
