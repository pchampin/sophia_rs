//! Common implementations for adapting [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs) parsers.

use std::error::Error;
use std::result::Result as StdResult;

use rio_api::model::*;
use rio_api::parser::*;

use crate::ns::xsd;
use crate::quad::stream::*;
use crate::quad::streaming_mode::StreamedQuad;
use crate::term::{BoxTerm, RefTerm};
use crate::triple::stream::*;
use crate::triple::streaming_mode::StreamedTriple;

/// TripleSource / QuadSource adapter for RIO TripleParser / QuadParser
pub enum StrictRioSource<T, E> {
    Parser(T),
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

impl<T, E> TripleSource for StrictRioSource<T, E>
where
    T: TriplesParser<Error = E>,
    E: Error + 'static,
{
    type Error = E;
    type Triple = crate::triple::streaming_mode::ByRefTerms;

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
                        f(StreamedTriple::by_ref_terms(
                            rio2refterm(t.subject.into()),
                            rio2refterm(t.predicate.into()),
                            rio2refterm(t.object.into()),
                        ))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())
                    .and(Ok(true))
            }
        }
    }
}

impl<T, E> QuadSource for StrictRioSource<T, E>
where
    T: QuadsParser<Error = E>,
    E: Error + 'static,
{
    type Error = E;
    type Quad = crate::quad::streaming_mode::ByRefTerms;

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
                        f(StreamedQuad::by_ref_terms(
                            rio2refterm(q.subject.into()),
                            rio2refterm(q.predicate.into()),
                            rio2refterm(q.object.into()),
                            q.graph_name.map(|g| rio2refterm(g.into())),
                        ))
                        .map_err(MyStreamError::from_sink_error)
                    })
                    .map_err(|e| e.into_stream_error())
                    .and(Ok(true))
            }
        }
    }
}

/// QuadSource adapter for RIO GeneralizedQuadParser
pub enum GeneralizedRioSource<T, E> {
    Parser(T),
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
    type Quad = crate::quad::streaming_mode::ByRefTerms;

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
                        f(StreamedQuad::by_ref_terms(
                            rio2refterm(q.subject),
                            rio2refterm(q.predicate),
                            rio2refterm(q.object),
                            q.graph_name.map(rio2refterm),
                        ))
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

    unsafe {
        match t {
            GeneralizedTerm::BlankNode(b) => RefTerm::new_bnode(b.id).unwrap(),
            GeneralizedTerm::NamedNode(n) => RefTerm::new_iri_unchecked(n.iri, None),
            GeneralizedTerm::Literal(Simple { value }) => {
                RefTerm::new_literal_dt_unchecked(value, xsd::string)
            }
            GeneralizedTerm::Literal(LanguageTaggedString { value, language }) => {
                RefTerm::new_literal_lang_unchecked(value, language)
            }
            GeneralizedTerm::Literal(Typed { value, datatype }) => {
                RefTerm::new_literal_dt_unchecked(
                    value,
                    RefTerm::new_iri_unchecked(datatype.iri, None),
                )
            }
            GeneralizedTerm::Variable(v) => RefTerm::new_variable_unchecked(v.name),
        }
    }
}

/// Convert RIO term to Sophia term
pub fn rio2boxterm(t: GeneralizedTerm) -> BoxTerm {
    BoxTerm::from_with(&rio2refterm(t), Box::from)
}
