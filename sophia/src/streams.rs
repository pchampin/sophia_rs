//! `TripleSource` and `TripleSink`,
//! are pervasive traits for streaming triples from one object to another.
//! 
//! See [`TripleSource`]'s and [`TripleSink`]'s documentation for more detail.
//! 
//! # Why not simply use iterators of [`Triple`]s?
//! 
//! Iterators are well suited in the following cases:
//! * the items are owned by a container,
//!   and the iterator yields references to them;
//! * each item is produced during an iteration,
//!   and "given" by the iterator to the consuming code.
//! 
//! Some situations do not fit those categories.
//! For example, a parser will get lines of text from an IO stream,
//! and produce triples that borrow text from these lines.
//! We do not want the parser to store all the lines
//! (there could be too many of them),
//! so it can not own the triples undefinitely.
//! But it can not give them away either,
//! as they borrow from the ephemeral lines.
//! 
//! 
//! [`TripleSource`] provides an abstraction that can handle that situation.
//! As it is more general than [`Iterator`],
//! any iterator of (results wrapping) triples implements [`TripleSource`].
//! 
//! [`TripleSource`]: trait.TripleSource.html
//! [`TripleSink`]: trait.TripleSink.html
//! [`Triple`]: ../triple/trait.Triple.html
//! [`Iterator`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html

use std::iter::Map;

use ::error::*;
use ::graph::*;
use ::triple::*;

/// A triple source is anything that yields or produces [triples](../triple/trait.Triple.html),
/// and may also fail in the process.
/// 
/// Typical triple sources are
/// *fallible triple iterators*
/// (i.e. iterators yielding [results] whose `Ok` values are [triples]).
/// 
/// See also [`TripleSink`].
/// 
/// [results]: https://doc.rust-lang.org/std/result/enum.Result.html
/// [triples]: ../triple/trait.Triple.html
/// [`TripleSink`]: trait.TripleSink.html
/// 
pub trait TripleSource: Sized {
    /// Feed all triples from this source into the given [sink](trait.TripleSink.html).
    /// 
    /// Stop on the first error (in the source or the sink).
    fn into_sink<TS: TripleSink>(self, sink: &mut TS) -> Result<TS::Outcome>;

    /// Insert all triples from this source into the given [graph](../graph/trait.MutableGraph.html).
    /// 
    /// Stop on the first error (in the source or in the graph).
    fn into_graph<G: MutableGraph>(self, graph: &mut G) -> Result<usize> {
        self.into_sink(&mut graph.inserter())
    }
}

impl<'a, I, T> TripleSource for I
where
    I: Iterator<Item=Result<T>>,
    T: Triple<'a>,
{
    fn into_sink<TS: TripleSink>(self, sink: &mut TS) -> Result<TS::Outcome> {
        for tr in self {
            let t = tr?;
            sink.feed(&t)?;
        }
        return sink.finish()
    }
}


/// A utility extension trait for converting standard iterators
/// into result iterators.
/// Useful for converting any [`Triple`] iterator into a valid [`TripleSource`].
/// 
/// [`TripleSource`]: trait.TripleSource.html
/// [`Triple`]: ../triple/trait.Triple.html
pub trait WrapAsOks<T>: Sized {
    /// Map all items of this iterator into an Ok result.
    fn wrap_as_oks(self) -> Map<Self, fn(T) -> Result<T>>;
}

impl<T, I> WrapAsOks<T> for I
    where I: Iterator<Item=T> + Sized,
{
    fn wrap_as_oks(self) -> Map<Self, fn(T) -> Result<T>> {
        self.map(Ok)
    }
}



/// A triple sink is anything that consumes [triples](../triple/trait.Triple.html),
/// produces a result,
/// and may also fail in the process.
/// 
/// Typical triple sinks are [serializers]
/// or graphs' [inserters] and [removers].
/// 
/// See also [`TripleSource`].
/// 
/// [serializers]: ../serializers/index.html
/// [inserters]: ../graph/trait.MutableGraph.html#method.inserter
/// [removers]: ../graph/trait.MutableGraph.html#method.remover
/// [`TripleSource`]: trait.TripleSource.html
/// 
pub trait TripleSink {
    /// The type of the result produced by this triple sink.
    /// 
    /// See [`finish`](#tymethod.finish).
    type Outcome;

    /// Feed one triple in this sink.
    fn feed<'a, T: Triple<'a>>(&mut self, t: &T) -> Result<()>;

    /// Produce the result once all triples were fed.
    /// 
    /// NB: the behaviour of a triple sink after `finish` is called is unspecified by this trait.
    fn finish(&mut self) -> Result<Self::Outcome>;
}

/// [`()`](https://doc.rust-lang.org/std/primitive.unit.html) acts as a "black hole",
/// consuming all triples without erring, and producing no result.
/// 
/// Useful for benchmarking triple sources.
impl TripleSink for () {
    type Outcome = ();

    fn feed<'a, T: Triple<'a>>(&mut self, _: &T) -> Result<()> { Ok(()) }
    fn finish(&mut self) -> Result<Self::Outcome> { Ok(()) }
}



#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the parsers/serializers modules).
}