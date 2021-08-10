//! Common implementations for adapting
//! [RIO](https://docs.rs/rio_api/) serializers.

use rio_api::formatter::{QuadsFormatter, TriplesFormatter};
use rio_api::model::{BlankNode, Literal, NamedNode, Quad as RioQuad, Triple as RioTriple};
use sophia_api::ns::xsd;
use sophia_api::quad::stream::QuadSource;
use sophia_api::quad::Quad;
use sophia_api::term::{TTerm, TermKind};
use sophia_api::triple::stream::{StreamResult, TripleSource};
use sophia_api::triple::Triple;

/// Format all standard RDF triples of `triples` using `tf`.
///
/// NB: non-standard (generalized) RDF triples will be silently ignored.
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
                    id: term.value_raw().0,
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
                    id: term.value_raw().0,
                }
                .into(),
                TermKind::Literal => match term.language() {
                    None => {
                        let datatype = term.datatype().unwrap();
                        if datatype == xsd::string {
                            Literal::Simple {
                                value: term.value_raw().0,
                            }
                            .into()
                        } else {
                            bufd = datatype.value().to_string();
                            Literal::Typed {
                                value: term.value_raw().0,
                                datatype: NamedNode { iri: &bufd },
                            }
                            .into()
                        }
                    }
                    Some(tag) => Literal::LanguageTaggedString {
                        value: term.value_raw().0,
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

/// Format all standard RDF quads of `quads` using `qf`.
///
/// NB: non-standard (generalized) RDF quads will be silently ignored.
pub fn rio_format_quads<QF, QS>(
    qf: &mut QF,
    mut quads: QS,
) -> StreamResult<(), QS::Error, QF::Error>
where
    QF: QuadsFormatter,
    QS: QuadSource,
{
    quads.try_for_each_quad(|q| {
        let bufs;
        let bufp;
        let bufo;
        let bufd;
        let bufg;
        let subject = {
            let term = q.s();
            match term.kind() {
                TermKind::Iri => {
                    bufs = term.value();
                    NamedNode { iri: &bufs }.into()
                }
                TermKind::BlankNode => BlankNode {
                    id: term.value_raw().0,
                }
                .into(),
                _ => return Ok(()), // non standard subject, skip this triple
            }
        };
        let predicate = {
            let term = q.p();
            match term.kind() {
                TermKind::Iri => {
                    bufp = term.value();
                    NamedNode { iri: &bufp }
                }
                _ => return Ok(()), // non standard predicate, skip this triple
            }
        };
        let object = {
            let term = q.o();
            match term.kind() {
                TermKind::Iri => {
                    bufo = term.value();
                    NamedNode { iri: &bufo }.into()
                }
                TermKind::BlankNode => BlankNode {
                    id: term.value_raw().0,
                }
                .into(),
                TermKind::Literal => match term.language() {
                    None => {
                        let datatype = term.datatype().unwrap();
                        if datatype == xsd::string {
                            Literal::Simple {
                                value: term.value_raw().0,
                            }
                            .into()
                        } else {
                            bufd = datatype.value().to_string();
                            Literal::Typed {
                                value: term.value_raw().0,
                                datatype: NamedNode { iri: &bufd },
                            }
                            .into()
                        }
                    }
                    Some(tag) => Literal::LanguageTaggedString {
                        value: term.value_raw().0,
                        language: tag,
                    }
                    .into(),
                },
                _ => return Ok(()), // non standard object, skip this triple
            }
        };
        let graph_name = match q.g() {
            None => None,
            Some(term) => match term.kind() {
                TermKind::Iri => {
                    bufg = term.value();
                    Some(NamedNode { iri: &bufg }.into())
                }
                TermKind::BlankNode => Some(
                    BlankNode {
                        id: term.value_raw().0,
                    }
                    .into(),
                ),
                _ => return Ok(()), // non standard subject, skip this triple
            },
        };
        let rq = RioQuad {
            subject,
            predicate,
            object,
            graph_name,
        };
        qf.format(&rq)?;
        Ok(())
    })
}
