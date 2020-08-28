//! Common implementations for adapting [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs) parsers.

use std::error::Error;
use std::result::Result as StdResult;

use rio_api::model::*;
use rio_api::parser::*;

use sophia_api::quad::stream::*;
use sophia_api::quad::streaming_mode::StreamedQuad;
use sophia_api::triple::stream::*;
use sophia_api::triple::streaming_mode::StreamedTriple;
use sophia_term::literal::convert::AsLiteral;
use sophia_term::{BoxTerm, RefTerm};

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
pub type RioSourceTriple<'a> = [RefTerm<'a>; 3];
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
                            rio2refterm(t.subject.into()),
                            rio2refterm(t.predicate.into()),
                            rio2refterm(t.object.into()),
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
pub type RioSourceQuad<'a> = ([RefTerm<'a>; 3], Option<RefTerm<'a>>);
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
                                rio2refterm(q.subject.into()),
                                rio2refterm(q.predicate.into()),
                                rio2refterm(q.object.into()),
                            ],
                            q.graph_name.map(|g| rio2refterm(g.into())),
                        )))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())
                    .and(Ok(true))
            }
        }
    }
}

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
    type Quad = ScopedRioSourceQuad;

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
                                rio2refterm(q.subject),
                                rio2refterm(q.predicate),
                                rio2refterm(q.object),
                            ],
                            q.graph_name.map(rio2refterm),
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

/// Convert RIO term to Sophia term
pub fn rio2refterm(t: GeneralizedTerm) -> RefTerm {
    use Literal::*;

    match t {
        GeneralizedTerm::BlankNode(b) => RefTerm::new_bnode(b.id).unwrap(),
        GeneralizedTerm::NamedNode(n) => {
            RefTerm::new_iri(n.iri).expect("Already checked by parser but determine if absolute.")
        }
        GeneralizedTerm::Literal(Simple { value }) => value.as_literal().into(),
        GeneralizedTerm::Literal(LanguageTaggedString { value, language }) => {
            RefTerm::new_literal_lang_unchecked(value, language)
        }
        GeneralizedTerm::Literal(Typed { value, datatype }) => RefTerm::new_literal_dt_unchecked(
            value,
            RefTerm::new_iri(datatype.iri)
                .expect("Already checked by parser but determine if absolute."),
        ),
        GeneralizedTerm::Variable(v) => RefTerm::new_variable_unchecked(v.name),
    }
}

/// Convert RIO term to Sophia term
pub fn rio2boxterm(t: GeneralizedTerm) -> BoxTerm {
    rio2refterm(t).map_into()
}
