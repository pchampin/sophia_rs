//! Common implementations for adapting
//! [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs) parsers.
//!
//! NB: Rio provides its own adapter for Sophia's traits (using the `sophia` features).
//! However,
//! published versions of Rio will always depend on the previously published version of Sophia,
//! which makes it impossible for Sophia itself to rely on that feature.

use std::error::Error;
use std::result::Result as StdResult;

use rio_api::model::*;
use rio_api::parser::*;

use sophia_api::ns::{rdf, xsd};
use sophia_api::quad::stream::*;
use sophia_api::quad::streaming_mode::StreamedQuad;
use sophia_api::term::*;
use sophia_api::triple::stream::*;
use sophia_api::triple::streaming_mode::StreamedTriple;

/// TripleSource / QuadSource adapter for RIO TripleParser / QuadParser.
pub enum StrictRioSource<T, E> {
    /// A RIO TripleParser / QuadParser.
    Parser(T),
    /// An error in acquiring the RIO TripleParser / QuadParser.
    Error(Option<E>),
}

impl<T, E> From<StdResult<T, E>> for StrictRioSource<T, E> {
    fn from(res: StdResult<T, E>) -> Self {
        match res {
            Ok(parser) => StrictRioSource::Parser(parser),
            Err(error) => StrictRioSource::Error(Some(error)),
        }
    }
}

// This intermediate type is required,
// because Rio requires that the error type of triple_handler/quad_handler
// implement From<TurtleError> (or whatever Rio-specific error returned by the parser).
//
// This is costless, though,
// because MyStreamError's internal representation is identical to StreamError,
// so the final type conversion performed by into_stream_error is actually
// just for pleasing the compiler.
enum MyStreamError<E1, E2> {
    Source(E1),
    Sink(E2),
}
impl<E1, E2> MyStreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    fn from_sink_error(err: E2) -> Self {
        MyStreamError::Sink(err)
    }
    fn into_stream_error(self) -> StreamError<E1, E2> {
        match self {
            MyStreamError::Source(err) => SourceError(err),
            MyStreamError::Sink(err) => SinkError(err),
        }
    }
}
impl<E1, E2> From<E1> for MyStreamError<E1, E2>
where
    E1: Error + 'static,
    E2: Error + 'static,
{
    fn from(other: E1) -> Self {
        MyStreamError::Source(other)
    }
}

/// A triple produced by a RIO source.
pub type RioSourceTriple<'a> = [RioTermWrapper<'a>; 3];
sophia_api::make_scoped_triple_streaming_mode!(
    /// A scoped RIO source triple.
    ScopedRioSourceTriple,
    RioSourceTriple
);

impl<T, E> TripleSource for StrictRioSource<T, E>
where
    T: TriplesParser<Error = E>,
    E: Error + 'static,
{
    type Error = E;
    //type Triple = crate::triple::streaming_mode::ByValue<RioSourceTriple<'static>>;
    type Triple = ScopedRioSourceTriple;

    fn try_for_some_triple<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
    where
        F: FnMut(StreamedTriple<Self::Triple>) -> Result<(), EF>,
        EF: Error,
    {
        match self {
            StrictRioSource::Error(opt) => Err(SourceError(consume_err(opt))),
            StrictRioSource::Parser(parser) => {
                if parser.is_end() {
                    return Ok(false);
                }
                parser
                    .parse_step(&mut |t| -> StdResult<(), MyStreamError<E, EF>> {
                        f(StreamedTriple::scoped([
                            RioTermWrapper(t.subject.into()),
                            RioTermWrapper(t.predicate.into()),
                            RioTermWrapper(t.object),
                        ]))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())
                    .and(Ok(true))
            }
        }
    }
}

/// A RIO source quad.
pub type RioSourceQuad<'a> = ([RioTermWrapper<'a>; 3], Option<RioTermWrapper<'a>>);
sophia_api::make_scoped_quad_streaming_mode!(
    /// A scoped RIO source quad.
    ScopedRioSourceQuad,
    RioSourceQuad
);

impl<T, E> QuadSource for StrictRioSource<T, E>
where
    T: QuadsParser<Error = E>,
    E: Error + 'static,
{
    type Error = E;
    type Quad = ScopedRioSourceQuad;

    fn try_for_some_quad<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> Result<(), EF>,
        EF: Error,
    {
        match self {
            StrictRioSource::Error(opt) => Err(SourceError(consume_err(opt))),
            StrictRioSource::Parser(parser) => {
                if parser.is_end() {
                    return Ok(false);
                }
                parser
                    .parse_step(&mut |q| -> StdResult<(), MyStreamError<E, EF>> {
                        f(StreamedQuad::scoped((
                            [
                                RioTermWrapper(q.subject.into()),
                                RioTermWrapper(q.predicate.into()),
                                RioTermWrapper(q.object),
                            ],
                            q.graph_name.map(|g| RioTermWrapper(g.into())),
                        )))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())
                    .and(Ok(true))
            }
        }
    }
}

/// A Generalized RIO source quad.
pub type GRioSourceQuad<'a> = ([GRioTermWrapper<'a>; 3], Option<GRioTermWrapper<'a>>);
sophia_api::make_scoped_quad_streaming_mode!(
    /// A scoped RIO source quad.
    ScopedGRioSourceQuad,
    GRioSourceQuad
);

/// QuadSource adapter for RIO GeneralizedQuadParser.
pub enum GeneralizedRioSource<T, E> {
    /// A RIO GeneralizedQuadParser.
    Parser(T),
    /// An error in acquiring the RIO GeneralizedQuadParser.
    Error(Option<E>),
}

impl<T, E> From<StdResult<T, E>> for GeneralizedRioSource<T, E> {
    fn from(res: StdResult<T, E>) -> Self {
        match res {
            Ok(parser) => GeneralizedRioSource::Parser(parser),
            Err(error) => GeneralizedRioSource::Error(Some(error)),
        }
    }
}

impl<T, E> QuadSource for GeneralizedRioSource<T, E>
where
    T: GeneralizedQuadsParser<Error = E>,
    E: Error + 'static,
{
    type Error = E;
    type Quad = ScopedGRioSourceQuad;

    fn try_for_some_quad<F, EF>(&mut self, f: &mut F) -> StreamResult<bool, E, EF>
    where
        F: FnMut(StreamedQuad<Self::Quad>) -> Result<(), EF>,
        EF: Error,
    {
        match self {
            GeneralizedRioSource::Error(opt) => Err(SourceError(consume_err(opt))),
            GeneralizedRioSource::Parser(parser) => {
                if parser.is_end() {
                    return Ok(false);
                }
                parser
                    .parse_step(&mut |q| -> StdResult<(), MyStreamError<E, EF>> {
                        f(StreamedQuad::scoped((
                            [
                                GRioTermWrapper(q.subject),
                                GRioTermWrapper(q.predicate),
                                GRioTermWrapper(q.object),
                            ],
                            q.graph_name.map(GRioTermWrapper),
                        )))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())
                    .and(Ok(true))
            }
        }
    }
}

/// Consume inner error and convert it to Error
fn consume_err<E>(opt: &mut Option<E>) -> E {
    opt.take().unwrap_or_else(|| {
        panic!("This parser has failed previously, and can not be used anymore");
    })
}

/// TTerm wrapper for Rio Term
pub struct RioTermWrapper<'a>(Term<'a>);

impl<'a> TTerm for RioTermWrapper<'a> {
    /// Returns the kind of this term (IRI, literal, blank node, variable).
    fn kind(&self) -> TermKind {
        match self.0 {
            Term::BlankNode(_) => TermKind::BlankNode,
            Term::Literal(_) => TermKind::Literal,
            Term::NamedNode(_) => TermKind::Iri,
        }
    }

    fn datatype(&self) -> Option<SimpleIri> {
        if let Term::Literal(lit) = self.0 {
            Some(match lit {
                Literal::Simple { .. } => xsd::string,
                Literal::LanguageTaggedString { .. } => rdf::langString,
                Literal::Typed { datatype, .. } => SimpleIri::new_unchecked(datatype.iri, None),
            })
        } else {
            None
        }
    }

    fn language(&self) -> Option<&str> {
        if let Term::Literal(Literal::LanguageTaggedString { language, .. }) = self.0 {
            Some(language)
        } else {
            None
        }
    }

    fn value_raw(&self) -> RawValue {
        use Literal::*;
        match self.0 {
            Term::BlankNode(node) => node.id.into(),
            Term::Literal(Simple { value }) => value.into(),
            Term::Literal(LanguageTaggedString { value, .. }) => value.into(),
            Term::Literal(Typed { value, .. }) => value.into(),
            Term::NamedNode(node) => node.iri.into(),
        }
    }

    fn is_absolute(&self) -> bool {
        // Rio standard terms are always absolute
        true
    }

    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

/// TTerm wrapper for Rio Generalized Term
pub struct GRioTermWrapper<'a>(GeneralizedTerm<'a>);

impl<'a> TTerm for GRioTermWrapper<'a> {
    /// Returns the kind of this term (IRI, literal, blank node, variable).
    fn kind(&self) -> TermKind {
        match self.0 {
            GeneralizedTerm::BlankNode(_) => TermKind::BlankNode,
            GeneralizedTerm::Literal(_) => TermKind::Literal,
            GeneralizedTerm::NamedNode(_) => TermKind::Iri,
            GeneralizedTerm::Variable(_) => TermKind::Variable,
        }
    }

    fn datatype(&self) -> Option<SimpleIri> {
        if let GeneralizedTerm::Literal(lit) = self.0 {
            Some(match lit {
                Literal::Simple { .. } => xsd::string,
                Literal::LanguageTaggedString { .. } => rdf::langString,
                Literal::Typed { datatype, .. } => SimpleIri::new_unchecked(datatype.iri, None),
            })
        } else {
            None
        }
    }

    fn language(&self) -> Option<&str> {
        if let GeneralizedTerm::Literal(Literal::LanguageTaggedString { language, .. }) = self.0 {
            Some(language)
        } else {
            None
        }
    }

    fn value_raw(&self) -> RawValue {
        use Literal::*;
        match self.0 {
            GeneralizedTerm::BlankNode(node) => node.id.into(),
            GeneralizedTerm::Literal(Simple { value }) => value.into(),
            GeneralizedTerm::Literal(LanguageTaggedString { value, .. }) => value.into(),
            GeneralizedTerm::Literal(Typed { value, .. }) => value.into(),
            GeneralizedTerm::NamedNode(node) => node.iri.into(),
            GeneralizedTerm::Variable(var) => var.name.into(),
        }
    }

    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}
