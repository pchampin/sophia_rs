// this module is transparently re-exported by its parent `graph`

use std::borrow::Borrow;

use resiter::filter::*;
use resiter::map::*;

use crate::error::*;
use crate::graph::sinks::*;
use crate::streams::*;
use crate::term::*;
use crate::term::matcher::TermMatcher;
use crate::triple::*;

/// Type alias for results iterators produced by a graph.
pub type GResult<'a, G, T> = std::result::Result<T, <G as Graph<'a>>::Error>;
/// Type alias for fallible triple iterators produced by a graph.
pub type GTripleSource<'a, G> =
    Box<Iterator<Item=GResult<'a, G, <G as Graph<'a>>::Triple>>+'a>;


/// Generic trait for RDF graphs.
/// 
/// For convenience, this trait is implemented
/// by [standard collections of triples](#foreign-impls).
/// 
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`](trait.SetGraph.html).
/// 
pub trait Graph<'a> {
    /// The type of [`Triple`](../triple/trait.Triple.html)s
    /// that the methods of this graph will yield.
    type Triple: Triple<'a>;
    /// The error type that this graph may raise.
    ///
    /// Must be either [`Never`](../error/enum.Never.html) (for infallible graphs)
    /// or [`Error`](../error/struct.Error.html).
    type Error: CoercibleWith<Error> + CoercibleWith<Never>;

    /// An iterator visiting all triples of this graph in arbitrary order.
    /// 
    /// This iterator is fallible:
    /// its items are `Result`s,
    /// an error may occur at any time during the iteration.
    fn triples(&'a self) -> GTripleSource<'a, Self>;

    /// An iterator visiting all triples with the given subject.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn triples_with_s<T> (&'a self, s: &'a Term<T>) -> GTripleSource<'a, Self> where
        T: Borrow<str>,
    {
        Box::new(
            self.triples().filter_ok(move |t| t.s()==s)
        )
    }
    /// An iterator visiting all triples with the given predicate.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn triples_with_p<T> (&'a self, p: &'a Term<T>) -> GTripleSource<'a, Self> where
        T: Borrow<str>,
    {
        Box::new(
            self.triples().filter_ok(move |t| t.p()==p)
        )
    }
    /// An iterator visiting all triples with the given object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn triples_with_o<T> (&'a self, o: &'a Term<T>) -> GTripleSource<'a, Self> where
        T: Borrow<str>,
    {
        Box::new(
            self.triples().filter_ok(move |t| t.o()==o)
        )
    }
    /// An iterator visiting all triples with the given subject and predicate.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn triples_with_sp<T, U> (&'a self, s: &'a Term<T>, p: &'a Term<U>) -> GTripleSource<'a, Self> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Box::new(
            self.triples_with_s(s).filter_ok(move |t| t.p()==p)
        )
    }
    /// An iterator visiting all triples with the given subject and object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn triples_with_so<T, U> (&'a self, s: &'a Term<T>, o: &'a Term<U>) -> GTripleSource<'a, Self> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Box::new(
            self.triples_with_s(s).filter_ok(move |t| t.o()==o)
        )
    }
    /// An iterator visiting all triples with the given predicate and object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn triples_with_po<T, U> (&'a self, p: &'a Term<T>, o: &'a Term<U>) -> GTripleSource<'a, Self> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Box::new(
            self.triples_with_p(p).filter_ok(move |t| t.o()==o)
        )
    }
    /// An iterator visiting all triples with the given subject, predicate and object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn triples_with_spo<T, U, V> (&'a self, s: &'a Term<T>, p: &'a Term<U>, o: &'a Term<V>) -> GTripleSource<'a, Self> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        Box::new(
            self.triples_with_sp(s,p).filter_ok(move |t| t.o()==o)
        )
    }

    /// Return `true` if this graph contains the given triple.
    fn contains(&'a self, s: &'a RefTerm, p: &'a RefTerm, o: &'a RefTerm) -> GResult<'a, Self, bool> {
        match self.triples_with_spo(s, p, o).next() {
            None           => Ok(false),
            Some(Ok(_))    => Ok(true),
            Some(Err(err)) => Err(err),
        }
    }

    /// An iterator visiting all triples matching the given subject, predicate and object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn triples_matching<S, P, O> (&'a self, ms: &'a S, mp: &'a P, mo: &'a O) -> GTripleSource<'a, Self> where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
    {
        match (&ms.constant(), &mp.constant(), &mo.constant()) {
            (None,    None,    None   )    => Box::from(
                self.triples().filter_ok(move |t| ms.matches(t.s()) && mp.matches(t.p()) && mo.matches(t.o()))),
            (Some(s), None,    None   )    => Box::from(
                self.triples_with_s(s).filter_ok(move |t| mp.matches(t.p()) && mo.matches(t.o()))),
            (None,    Some(p), None   )    => Box::from(
                self.triples_with_p(p).filter_ok(move |t| ms.matches(t.s()) && mo.matches(t.o()))),
            (None,    None,    Some(o))    => Box::from(
                self.triples_with_o(o).filter_ok(move |t| ms.matches(t.s()) && mp.matches(t.p()))),
            (Some(s), Some(p), None   )    => Box::from(
                self.triples_with_sp(s, p).filter_ok(move |t| mo.matches(t.o()))),
            (Some(s), None,    Some(o))    => Box::from(
                self.triples_with_so(s, o).filter_ok(move |t| mp.matches(t.p()))),
            (None,    Some(p), Some(o))    => Box::from(
                self.triples_with_po(p, o).filter_ok(move |t| ms.matches(t.s()))),
            (Some(s), Some(p), Some(o))    => Box::from(
                self.triples_with_spo(s, p, o))
        }
    }
}

/// Type alias for results iterators produced by a graph.
pub type MGResult<G, T> = std::result::Result<T, <G as MutableGraph>::MutationError>;

/// Generic trait for mutable RDF graphs.
/// 
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`](trait.SetGraph.html).
///
pub trait MutableGraph: for<'x> Graph<'x> {
    /// The error type that this graph may raise during mutations.
    ///
    /// Must be either [`Never`](../error/enum.Never.html) (for infallible graphs)
    /// or [`Error`](../error/struct.Error.html).
    type MutationError: CoercibleWith<Error> + CoercibleWith<Never>;

    /// Insert the given triple in this graph.
    /// 
    /// Return `true` iff the triple was actually inserted.
    /// 
    /// NB: unless this graph also implements [`SetGraph`](trait.SetGraph.html),
    /// a return value of `true` does *not* mean that the triple was not already in the graph,
    /// only that the graph now has one more occurence of it.
    /// 
    fn insert<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    ;

    /// Insert the given triple in this graph.
    /// 
    /// Return `true` iff the triple was actually removed.
    /// 
    /// NB: unless this graph also implements [`SetGraph`](trait.SetGraph.html),
    /// a return value of `true` does *not* mean that the triple is not still contained in the graph,
    /// only that the graph now has one less occurence of it.
    /// 
    fn remove<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult< Self, bool> where
        T: Borrow<str>,
        U: Borrow<str>,
        V:Borrow<str>,
    ;

    /// Return a [`TripleSink`](../streams/trait.TripleSink.html)
    /// that will insert into this graph all the triples it receives.
    #[inline]
    fn inserter(&mut self) -> Inserter<Self> {
        Inserter::new(self)
    }

    /// Insert into this graph all triples from the given source.
    #[inline]
    fn insert_all<'a, TS>(&mut self, src: &mut TS) -> CoercedResult<usize, TS::Error, <Self as MutableGraph>::MutationError>
    where
        TS: TripleSource<'a>,
        TS::Error: CoercibleWith<<Self as MutableGraph>::MutationError>,
    {
        src.in_sink(&mut self.inserter())
    }

    /// Return a [`TripleSink`](../streams/trait.TripleSink.html)
    /// that will remove from this graph all the triples it receives.
    #[inline]
    fn remover(&mut self) -> Remover<Self> {
        Remover::new(self)
    }

    /// Remove from this graph all triples from the given source.
    #[inline]
    fn remove_all<'a, TS>(&mut self, src: &mut TS) -> CoercedResult<usize, TS::Error, <Self as MutableGraph>::MutationError>
    where
        TS: TripleSource<'a>,
        TS::Error: CoercibleWith<<Self as MutableGraph>::MutationError>,
    {
        src.in_sink(&mut self.remover())
    }

    /// Remove all triples matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    fn remove_matching<S, P, O> (&mut self, ms: &S, mp: &P, mo: &O) -> MGResult<Self, usize>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
        // The following trait bound means that Self::Error must convert to Self::MutationError;
        // it is always satisfied when both of them are either Error or Never;
        // it is required to raise an error when building to_remove
        for <'a> <Self as Graph<'a>>::Error: Into<Self::MutationError>,
        // The following trait bound is required by remove_all,
        // who coerces to_remove::Error (always Never) with self::MutationError.
        Never: CoercibleWith<Self::MutationError>,
        // The following trait is trivially verified in all cases,
        // (acatually T is always EQUAL to CoercedError<Never, T>)
        // but unfortunetaly the compiler can not see that.
        Self::MutationError: From<CoercedError<Never, Self::MutationError>>,
    {
        let to_remove: Vec<_> =
            self.triples_matching(ms, mp, mo)
            .map_ok(|t| {
                [BoxTerm::from(t.s()), BoxTerm::from(t.p()), BoxTerm::from(t.o())]
            })
            .collect::<std::result::Result<_,_>>()
            .map_err(|err| err.into())?;
        let mut to_remove = to_remove.into_iter().as_triple_source();
        Ok(self.remove_all(&mut to_remove)?)
    }

    /// Keep only the triples matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    ///
    fn retain<S, P, O> (&mut self, ms: &S, mp: &P, mo: &O) -> MGResult<Self, ()>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
        // The following trait bound means that Self::Error must convert to Self::MutationError;
        // it is always satisfied when both of them are either Error or Never;
        // it is required to raise an error when building to_remove
        for <'a> <Self as Graph<'a>>::Error: Into<Self::MutationError>,
        // The following trait bound is required by remove_all,
        // who coerces to_remove::Error (always Never) with self::MutationError.
        Never: CoercibleWith<Self::MutationError>,
        // The following trait is trivially verified in all cases,
        // (acatually T is always EQUAL to CoercedError<Never, T>)
        // but unfortunetaly the compiler can not see that.
        Self::MutationError: From<CoercedError<Never, Self::MutationError>>,
    {
        let to_remove: Vec<_> =
            self.triples()
            .filter_ok(|t| {
                !(ms.matches(t.s()) && mp.matches(t.p()) && mo.matches(t.o()))
            })
            .map_ok(|t| {
                [BoxTerm::from(t.s()), BoxTerm::from(t.p()), BoxTerm::from(t.o())]
            })
            .collect::<std::result::Result<_,_>>()
            .map_err(|err| err.into())?;
        let mut to_remove = to_remove.into_iter().as_triple_source();
        self.remove_all(&mut to_remove)?;
        Ok(())
    }

}

/// Marker trait constraining the semantics of
/// [`Graph`](trait.Graph.html) and [`MutableGraph`](trait.MutableGraph.html),
/// by guaranteeing that triples will never be returned / stored multiple times.
pub trait SetGraph {
}


#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the macro test_graph_impl!).
}