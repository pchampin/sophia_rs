//! Common implementations for adapting
//! [RIO](https://docs.rs/rio_api/) serializers.

use rio_api::formatter::{QuadsFormatter, TriplesFormatter};
use rio_api::model::{
    BlankNode, GraphName as RioGraphName, Literal, NamedNode, Quad as RioQuad, Subject,
    Term as RioTerm, Triple as RioTriple,
};
use sophia_api::ns::xsd;
use sophia_api::quad::Quad;
use sophia_api::source::{QuadSource, StreamResult, TripleSource};
use sophia_api::term::{FromTerm, SimpleTerm};
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
    TF::Error: std::error::Error + Send + Sync + 'static,
    TS: TripleSource,
{
    triples.try_for_each_triple(|t| {
        let t = t.to_spo().map(SimpleTerm::from_term);
        match convert_triple(&t, Empty).head() {
            None => Ok(()),
            Some(head) => tf.format(head),
        }
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
    QF::Error: std::error::Error + Send + Sync + 'static,
    QS: QuadSource,
{
    quads.try_for_each_quad(|q| {
        let (t, g) = q.to_spog();
        let t = t.map(SimpleTerm::from_term);
        let g = g.map(SimpleTerm::from_term);
        let graph_name: Option<RioGraphName> = match &g {
            None => None,
            Some(SimpleTerm::Iri(iri)) => Some(NamedNode { iri }.into()),
            Some(SimpleTerm::BlankNode(id)) => Some(BlankNode { id }.into()),
            _ => {
                return Ok(());
            }
        };
        match convert_triple(&t, Empty).head() {
            None => Ok(()),
            Some(RioTriple {
                subject,
                predicate,
                object,
            }) => {
                let q = RioQuad {
                    subject: *subject,
                    predicate: *predicate,
                    object: *object,
                    graph_name,
                };
                qf.format(&q)
            }
        }
    })
}

/// Convert this triple of SimpleTerms to a RioTriple if possible
/// (i.e. if it is a strict RDF triple)
fn convert_triple<'a>(
    t: &'a [SimpleTerm<'a>; 3],
    mut stack: Stack<RioTriple<'a>>,
) -> Stack<RioTriple<'a>> {
    let subject = match t.s() {
        SimpleTerm::Iri(iri) => NamedNode { iri }.into(),
        SimpleTerm::BlankNode(id) => BlankNode { id }.into(),
        SimpleTerm::Triple(triple) => {
            stack = convert_triple(triple, stack);
            // Safety: the line below is safe because the triple will then be inserted in the same stack
            match unsafe { stack.head2() } {
                Some(t) => Subject::Triple(t),
                None => {
                    return Empty;
                }
            }
        }
        _ => {
            return Empty;
        }
    };
    let predicate = match t.p() {
        SimpleTerm::Iri(iri) => NamedNode { iri },
        _ => {
            return Empty;
        }
    };
    let object = match t.o() {
        SimpleTerm::Iri(iri) => NamedNode { iri }.into(),
        SimpleTerm::BlankNode(id) => BlankNode { id }.into(),
        SimpleTerm::LiteralDatatype(value, iri) if xsd::string == iri => {
            Literal::Simple { value }.into()
        }
        SimpleTerm::LiteralDatatype(value, iri) => Literal::Typed {
            value,
            datatype: NamedNode { iri },
        }
        .into(),
        SimpleTerm::LiteralLanguage(value, language) => {
            Literal::LanguageTaggedString { value, language }.into()
        }
        SimpleTerm::Triple(triple) => {
            stack = convert_triple(triple, stack);
            // Safety: the line below is safe because the triple will then be inserted in the same stack
            match unsafe { stack.head2() } {
                Some(t) => RioTerm::Triple(t),
                None => {
                    return Empty;
                }
            }
        }
        _ => {
            return Empty;
        }
    };
    Stack::Node(Box::new((
        RioTriple {
            subject,
            predicate,
            object,
        },
        stack,
    )))
}

enum Stack<T> {
    Empty,
    Node(Box<(T, Stack<T>)>),
}
use Stack::*;
impl<T> Stack<T> {
    /// Get the triple at the head of the stack.
    fn head(&self) -> Option<&T> {
        match self {
            Empty => None,
            Node(b) => Some(&b.0),
        }
    }
    /// Get the triple at the head of the stack,
    /// *for an arbitrary lifetime*.
    /// The last part obviously makes it unsafe.
    /// Therefore, this method must only be called when the output lifetime 'a
    /// is known to be safe.
    ///
    /// Good examples are:
    /// * when the returned triple will be used as a constituent of another triple in the same stack
    ///   (the constituent will stay in the stack longer than the nested triple)
    /// * when the returned triple will not live as long as the stack itself
    unsafe fn head2<'a>(&self) -> Option<&'a T>
    where
        Self: 'a,
        T: 'a,
    {
        self.head().map(|h| std::mem::transmute(h))
    }
}
