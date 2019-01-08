// this module is transparently re-exported by its parent `graph`

use std::borrow::Borrow;

use resiter::filter_x::*;
use resiter::map_x::*;

use ::graph::sinks::*;
use ::streams::*;
use ::term::*;
use ::term::matcher::TermMatcher;
use ::triple::*;

/// Type alias for fallible triple iterators produced by a graph.
pub type GFallibleTripleIterator<'a, G> =
    Box<Iterator<Item=Result<<G as Graph<'a>>::Triple, <G as Graph<'a>>::Error>>+'a>;

/// Generic trait for RDF graphs.
/// 
/// For convenience, this trait is implemented
/// by [standard collections of triples](#foreign-impls).
/// 
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`](trait.SetGraph.html).
/// 
pub trait Graph<'a>
{
    /// The type of [`Triple`](../triple/trait.Triple.html)s
    /// that the methods of this graph will yield.
    type Triple: Triple<'a>;
    /// The error type that this graph may raise.
    /// 
    /// Must be [`Never`](../error/enum.Never.html) for infallible graphs.
    type Error: ::std::error::Error;

    /// An iterator visiting all triples of this graph in arbitrary order.
    /// 
    /// This iterator is fallible:
    /// its items are `Result`s,
    /// an error may occur at any time during the iteration.
    fn iter(&'a self) -> GFallibleTripleIterator<'a, Self>;

    /// An iterator visiting all triples with the given subject.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn iter_for_s<T> (&'a self, s: &'a Term<T>) -> GFallibleTripleIterator<'a, Self> where
        T: Borrow<str>,
    {
        Box::new(
            self.iter().filter_ok(move |t| t.s()==s)
        )
    }
    /// An iterator visiting all triples with the given predicate.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn iter_for_p<T> (&'a self, p: &'a Term<T>) -> GFallibleTripleIterator<'a, Self> where
        T: Borrow<str>,
    {
        Box::new(
            self.iter().filter_ok(move |t| t.p()==p)
        )
    }
    /// An iterator visiting all triples with the given object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn iter_for_o<T> (&'a self, o: &'a Term<T>) -> GFallibleTripleIterator<'a, Self> where
        T: Borrow<str>,
    {
        Box::new(
            self.iter().filter_ok(move |t| t.o()==o)
        )
    }
    /// An iterator visiting all triples with the given subject and predicate.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn iter_for_sp<T, U> (&'a self, s: &'a Term<T>, p: &'a Term<U>) -> GFallibleTripleIterator<'a, Self> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Box::new(
            self.iter_for_s(s).filter_ok(move |t| t.p()==p)
        )
    }
    /// An iterator visiting all triples with the given subject and object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn iter_for_so<T, U> (&'a self, s: &'a Term<T>, o: &'a Term<U>) -> GFallibleTripleIterator<'a, Self> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Box::new(
            self.iter_for_s(s).filter_ok(move |t| t.o()==o)
        )
    }
    /// An iterator visiting all triples with the given predicate and object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn iter_for_po<T, U> (&'a self, p: &'a Term<T>, o: &'a Term<U>) -> GFallibleTripleIterator<'a, Self> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Box::new(
            self.iter_for_p(p).filter_ok(move |t| t.o()==o)
        )
    }
    /// An iterator visiting all triples with the given subject, predicate and object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn iter_for_spo<T, U, V> (&'a self, s: &'a Term<T>, p: &'a Term<U>, o: &'a Term<V>) -> GFallibleTripleIterator<'a, Self> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        Box::new(
            self.iter_for_sp(s,p).filter_ok(move |t| t.o()==o)
        )
    }

    /// Return `true` if this graph contains the given triple.
    fn contains(&'a self, s: &'a RefTerm, p: &'a RefTerm, o: &'a RefTerm) -> Result<bool, Self::Error> {
        match self.iter_for_spo(s, p, o).next() {
            None           => Ok(false),
            Some(Ok(_))    => Ok(true),
            Some(Err(err)) => Err(err),
        }
    }

    /// An iterator visiting all triples matching the given subject, predicate and object.
    /// 
    /// See also [`iter`](#tymethod.iter).
    fn iter_matching<S, P, O> (&'a self, ms: &'a S, mp: &'a P, mo: &'a O) -> GFallibleTripleIterator<'a, Self> where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
    {
        match (&ms.constant(), &mp.constant(), &mo.constant()) {
            (None,    None,    None   )    => Box::from(
                self.iter().filter_ok(move |t| ms.try(t.s()) && mp.try(t.p()) && mo.try(t.o()))),
            (Some(s), None,    None   )    => Box::from(
                self.iter_for_s(s).filter_ok(move |t| mp.try(t.p()) && mo.try(t.o()))),
            (None,    Some(p), None   )    => Box::from(
                self.iter_for_p(p).filter_ok(move |t| ms.try(t.s()) && mo.try(t.o()))),
            (None,    None,    Some(o))    => Box::from(
                self.iter_for_o(o).filter_ok(move |t| ms.try(t.s()) && mp.try(t.p()))),
            (Some(s), Some(p), None   )    => Box::from(
                self.iter_for_sp(s, p).filter_ok(move |t| mo.try(t.o()))),
            (Some(s), None,    Some(o))    => Box::from(
                self.iter_for_so(s, o).filter_ok(move |t| mp.try(t.p()))),
            (None,    Some(p), Some(o))    => Box::from(
                self.iter_for_po(p, o).filter_ok(move |t| ms.try(t.s()))),
            (Some(s), Some(p), Some(o))    => Box::from(
                self.iter_for_spo(s, p, o))
        }
    }
}

/// Generic trait for mutable RDF graphs.
/// 
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`](trait.SetGraph.html).
///
pub trait MutableGraph: for<'x> Graph<'x> {

    /// The error type that this mutable graph may raise.
    /// 
    /// Must be [`Never`](../error/enum.Never.html) for infallible graphs.
    type MutationError: std::error::Error;

    /// Insert the given triple in this graph.
    /// 
    /// Return `true` iff the triple was actually inserted.
    /// 
    /// NB: unless this graph also implements [`SetGraph`](trait.SetGraph.html),
    /// a return value of `true` does *not* mean that the triple was not already in the graph,
    /// only that the graph now has one more occurence of it.
    /// 
    fn insert<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Result<bool, Self::MutationError> where
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
    fn remove<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Result<bool, Self::MutationError> where
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
    fn insert_all<TS: TripleSource>(&mut self, src: TS)
    -> Result<usize, WhereFrom<TS::Error, Self::MutationError>> {
        src.into_sink(&mut self.inserter())
    }

    /// Return a [`TripleSink`](../streams/trait.TripleSink.html)
    /// that will remove from this graph all the triples it receives.
    #[inline]
    fn remover(&mut self) -> Remover<Self> {
        Remover::new(self)
    }

    /// Remove from this graph all triples from the given source.
    #[inline]
    fn remove_all<TS: TripleSource>(&mut self, src: TS)
    -> Result<usize, WhereFrom<TS::Error, Self::MutationError>> {
        src.into_sink(&mut self.remover())
    }

    /// Remove all triples matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    fn remove_matching<S, P, O> (&mut self, ms: &S, mp: &P, mo: &O) -> Result<usize, Self::MutationError> where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
    {
        let to_remove =
            self.iter_matching(ms, mp, mo)
            .map_ok(|t| {
                [BoxTerm::from(t.s()), BoxTerm::from(t.p()), BoxTerm::from(t.o())]
            })
            .collect::<Result<Vec<_>, _>>().unwrap();
            // TODO instead of unwrapping the result above,
            // lift the error as the *cause* of another error
        let mut to_remove = to_remove.into_iter().wrap_as_oks();
        self.remove_all(&mut to_remove).unwrap_upstream()
    }

    /// Keep only the triples matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    ///
    fn retain<S, P, O> (&mut self, ms: &S, mp: &P, mo: &O) -> Result<(), Self::MutationError> where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
    {
        let to_remove =
            self.iter()
            .filter_ok(|t| {
                !(ms.try(t.s()) && mp.try(t.p()) && mo.try(t.o()))
            })
            .map_ok(|t| {
                [BoxTerm::from(t.s()), BoxTerm::from(t.p()), BoxTerm::from(t.o())]
            })
            .collect::<Result<Vec<_>, _>>().unwrap();
            // TODO instead of unwrapping the result above,
            // lift the error as the *cause* of another error
        let mut to_remove = to_remove.into_iter().wrap_as_oks();
        self.remove_all(&mut to_remove).unwrap_upstream()?;
        Ok(())
    }
}

/// This trait constrains the semantics of
/// [`Graph`](trait.Graph.html) and [`MutableGraph`](trait.MutableGraph.html),
/// by guaranteeing that triples will never be returned / stored multiple times.
pub trait SetGraph {
}



#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the macro test_graph_impl!).
}