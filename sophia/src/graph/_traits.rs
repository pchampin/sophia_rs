// this module is transparently re-exported by its parent `graph`

use std::collections::HashSet;
use std::hash::Hash;
use std::marker::PhantomData;

use resiter::filter::*;
use resiter::map::*;

use crate::graph::adapter::GraphAsDataset;
use crate::graph::{Inserter, Remover};
use crate::term::matcher::TermMatcher;
use crate::term::*;
use crate::triple::stream::*;
use crate::triple::streaming_mode::*;
use crate::triple::*;

use std::convert::Infallible;
use std::error::Error;

/// Type alias for the terms returned by a graph.
pub type GTerm<G> =
    Term<<<<G as Graph>::Triple as TripleStreamingMode>::UnsafeTriple as UnsafeTriple>::TermData>;
/// Type alias for the triples returned by a graph.
pub type GTriple<'a, G> = StreamedTriple<'a, <G as Graph>::Triple>;
/// Type alias for results produced by a graph.
pub type GResult<G, T> = Result<T, <G as Graph>::Error>;
/// Type alias for fallible triple iterators produced by a graph.
pub type GTripleSource<'a, G> = Box<dyn Iterator<Item = GResult<G, GTriple<'a, G>>> + 'a>;
/// Type alias for fallible hashets of terms produced by a graph.
pub type GResultTermSet<G> = GResult<G, HashSet<GTerm<G>>>;

/// Generic trait for RDF graphs.
///
/// For convenience, this trait is implemented
/// by [standard collections of triples](#foreign-impls).
///
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`](trait.SetGraph.html).
///
pub trait Graph {
    /// Determine the type of [`Triple`](../triple/trait.Triple.html)s
    /// that the methods of this graph will yield
    /// (see [`streaming_mode`](../triple/streaming_mode/index.html)
    type Triple: TripleStreamingMode;
    /// The error type that this graph may raise.
    type Error: 'static + Error;

    /// An iterator visiting all triples of this graph in arbitrary order.
    ///
    /// This iterator is fallible:
    /// its items are `Result`s,
    /// an error may occur at any time during the iteration.
    fn triples(&self) -> GTripleSource<Self>;

    /// An iterator visiting all triples with the given subject.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_s<'s, T>(&'s self, s: &'s Term<T>) -> GTripleSource<'s, Self>
    where
        T: TermData,
    {
        Box::new(self.triples().filter_ok(move |t| t.s() == s))
    }
    /// An iterator visiting all triples with the given predicate.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_p<'s, T>(&'s self, p: &'s Term<T>) -> GTripleSource<'s, Self>
    where
        T: TermData,
    {
        Box::new(self.triples().filter_ok(move |t| t.p() == p))
    }
    /// An iterator visiting all triples with the given object.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_o<'s, T>(&'s self, o: &'s Term<T>) -> GTripleSource<'s, Self>
    where
        T: TermData,
    {
        Box::new(self.triples().filter_ok(move |t| t.o() == o))
    }
    /// An iterator visiting all triples with the given subject and predicate.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_sp<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
    ) -> GTripleSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.triples_with_s(s).filter_ok(move |t| t.p() == p))
    }
    /// An iterator visiting all triples with the given subject and object.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_so<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        o: &'s Term<U>,
    ) -> GTripleSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.triples_with_s(s).filter_ok(move |t| t.o() == o))
    }
    /// An iterator visiting all triples with the given predicate and object.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_po<'s, T, U>(
        &'s self,
        p: &'s Term<T>,
        o: &'s Term<U>,
    ) -> GTripleSource<'s, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.triples_with_p(p).filter_ok(move |t| t.o() == o))
    }
    /// An iterator visiting all triples with the given subject, predicate and object.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_spo<'s, T, U, V>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
        o: &'s Term<V>,
    ) -> GTripleSource<'s, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(self.triples_with_sp(s, p).filter_ok(move |t| t.o() == o))
    }

    /// Return `true` if this graph contains the given triple.
    fn contains<T, U, V>(&self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> GResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        match self.triples_with_spo(s, p, o).next() {
            None => Ok(false),
            Some(Ok(_)) => Ok(true),
            Some(Err(err)) => Err(err),
        }
    }

    /// An iterator visiting all triples matching the given subject, predicate and object.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_matching<'s, S, P, O>(
        &'s self,
        ms: &'s S,
        mp: &'s P,
        mo: &'s O,
    ) -> GTripleSource<'s, Self>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
    {
        match (&ms.constant(), &mp.constant(), &mo.constant()) {
            (None, None, None) => {
                Box::from(self.triples().filter_ok(move |t| {
                    ms.matches(t.s()) && mp.matches(t.p()) && mo.matches(t.o())
                }))
            }
            (Some(s), None, None) => Box::from(
                self.triples_with_s(s)
                    .filter_ok(move |t| mp.matches(t.p()) && mo.matches(t.o())),
            ),
            (None, Some(p), None) => Box::from(
                self.triples_with_p(p)
                    .filter_ok(move |t| ms.matches(t.s()) && mo.matches(t.o())),
            ),
            (None, None, Some(o)) => Box::from(
                self.triples_with_o(o)
                    .filter_ok(move |t| ms.matches(t.s()) && mp.matches(t.p())),
            ),
            (Some(s), Some(p), None) => Box::from(
                self.triples_with_sp(s, p)
                    .filter_ok(move |t| mo.matches(t.o())),
            ),
            (Some(s), None, Some(o)) => Box::from(
                self.triples_with_so(s, o)
                    .filter_ok(move |t| mp.matches(t.p())),
            ),
            (None, Some(p), Some(o)) => Box::from(
                self.triples_with_po(p, o)
                    .filter_ok(move |t| ms.matches(t.s())),
            ),
            (Some(s), Some(p), Some(o)) => self.triples_with_spo(s, p, o),
        }
    }

    /// Build a Hashset of all the terms used as subject in this Graph.
    fn subjects(&self) -> GResultTermSet<Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            insert_if_absent(&mut res, t?.s());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as predicate in this Graph.
    fn predicates(&self) -> GResultTermSet<Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            insert_if_absent(&mut res, t?.p());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as object in this Graph.
    fn objects(&self) -> GResultTermSet<Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            insert_if_absent(&mut res, t?.o());
        }
        Ok(res)
    }

    /// Build a Hashset of all the IRIs used in this Graph.
    fn iris(&self) -> GResultTermSet<Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            let t = t?;
            let (s, p, o) = (t.s(), t.p(), t.o());
            if let Iri(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let Iri(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let Iri(_) = o {
                insert_if_absent(&mut res, o)
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the BNodes used in this Graph.
    fn bnodes(&self) -> GResultTermSet<Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            let t = t?;
            let (s, p, o) = (t.s(), t.p(), t.o());
            if let BNode(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let BNode(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let BNode(_) = o {
                insert_if_absent(&mut res, o)
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the Literals used in this Graph.
    fn literals(&self) -> GResultTermSet<Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            let t = t?;
            let (s, p, o) = (t.s(), t.p(), t.o());
            if let Literal(_, _) = s {
                insert_if_absent(&mut res, s)
            }
            if let Literal(_, _) = p {
                insert_if_absent(&mut res, p)
            }
            if let Literal(_, _) = o {
                insert_if_absent(&mut res, o)
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the variables used in this Graph.
    fn variables(&self) -> GResultTermSet<Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            let t = t?;
            let (s, p, o) = (t.s(), t.p(), t.o());
            if let Variable(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let Variable(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let Variable(_) = o {
                insert_if_absent(&mut res, o)
            }
        }
        Ok(res)
    }

    /// [`Dataset`](../dataset/trait.Dataset.html) adapter borrowing this graph
    fn borrow_as_dataset(&self) -> GraphAsDataset<Self, &Self> {
        GraphAsDataset(self, PhantomData)
    }

    /// [`Dataset`](../dataset/trait.Dataset.html) adapter borrowing this graph mutably
    fn borrow_mut_as_dataset(&mut self) -> GraphAsDataset<Self, &mut Self> {
        GraphAsDataset(self, PhantomData)
    }

    /// [`Dataset`](../dataset/trait.Dataset.html) adapter taking ownership of this graph
    fn as_dataset(self) -> GraphAsDataset<Self, Self>
    where
        Self: Sized,
    {
        GraphAsDataset(self, PhantomData)
    }
}

/// Type alias for results produced by a mutable graph.
pub type MGResult<G, T> = std::result::Result<T, <G as MutableGraph>::MutationError>;

/// Generic trait for mutable RDF graphs.
///
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`](trait.SetGraph.html).
///
pub trait MutableGraph: Graph {
    /// The error type that this graph may raise during mutations.
    type MutationError: 'static + Error;

    /// Insert the given triple in this graph.
    ///
    /// Return `true` iff the triple was actually inserted.
    ///
    /// NB: unless this graph also implements [`SetGraph`](trait.SetGraph.html),
    /// a return value of `true` does *not* mean that the triple was not already in the graph,
    /// only that the graph now has one more occurence of it.
    ///
    fn insert<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData;

    /// Insert the given triple in this graph.
    ///
    /// Return `true` iff the triple was actually removed.
    ///
    /// NB: unless this graph also implements [`SetGraph`](trait.SetGraph.html),
    /// a return value of `true` does *not* mean that the triple is not still contained in the graph,
    /// only that the graph now has one less occurence of it.
    ///
    fn remove<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData;

    /// Return a [`TripleSink`](../triple/stream/trait.TripleSink.html)
    /// that will insert into this graph all the triples it receives.
    #[inline]
    fn inserter(&mut self) -> Inserter<Self> {
        Inserter::new(self)
    }

    /// Insert into this graph all triples from the given source.
    #[inline]
    fn insert_all<TS>(
        &mut self,
        src: &mut TS,
    ) -> StreamResult<usize, TS::Error, <Self as MutableGraph>::MutationError>
    where
        TS: TripleSource,
    {
        src.in_sink(&mut self.inserter())
    }

    /// Return a [`TripleSink`](../triple/stream/trait.TripleSink.html)
    /// that will remove from this graph all the triples it receives.
    #[inline]
    fn remover(&mut self) -> Remover<Self> {
        Remover::new(self)
    }

    /// Remove from this graph all triples from the given source.
    #[inline]
    fn remove_all<TS>(
        &mut self,
        src: &mut TS,
    ) -> StreamResult<usize, TS::Error, <Self as MutableGraph>::MutationError>
    where
        TS: TripleSource,
    {
        src.in_sink(&mut self.remover())
    }

    /// Remove all triples matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    fn remove_matching<S, P, O>(
        &mut self,
        ms: &S,
        mp: &P,
        mo: &O,
    ) -> Result<usize, Self::MutationError>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
        <Self as Graph>::Error: Into<Self::MutationError>,
        Infallible: Into<Self::MutationError>,
    {
        let to_remove = self
            .triples_matching(ms, mp, mo)
            .map_ok(|t| {
                [
                    BoxTerm::from(t.s()),
                    BoxTerm::from(t.p()),
                    BoxTerm::from(t.o()),
                ]
            })
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(Into::into)?;
        let mut to_remove = to_remove.into_iter().as_triple_source();
        Ok(self
            .remove_all(&mut to_remove)
            .map_err(|err| err.inner_into())?)
    }

    /// Keep only the triples matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    ///
    fn retain_matching<S, P, O>(
        &mut self,
        ms: &S,
        mp: &P,
        mo: &O,
    ) -> Result<(), Self::MutationError>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
        <Self as Graph>::Error: Into<Self::MutationError>,
        Infallible: Into<Self::MutationError>,
    {
        let to_remove = self
            .triples()
            .filter_ok(|t| !(ms.matches(t.s()) && mp.matches(t.p()) && mo.matches(t.o())))
            .map_ok(|t| {
                [
                    BoxTerm::from(t.s()),
                    BoxTerm::from(t.p()),
                    BoxTerm::from(t.o()),
                ]
            })
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(Into::into)?;
        let mut to_remove = to_remove.into_iter().as_triple_source();
        self.remove_all(&mut to_remove)
            .map_err(|err| err.inner_into())?;
        Ok(())
    }
}

/// Marker trait constraining the semantics of
/// [`Graph`](trait.Graph.html) and [`MutableGraph`](trait.MutableGraph.html),
/// by guaranteeing that triples will never be returned / stored multiple times.
pub trait SetGraph {}

#[inline]
pub(crate) fn insert_if_absent<T: Clone + Eq + Hash>(set: &mut HashSet<T>, val: &T) {
    if !set.contains(val) {
        set.insert(val.clone());
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the macro test_graph_impl!).
}
