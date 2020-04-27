// this module is transparently re-exported by its parent `graph`

use std::collections::HashSet;
use std::hash::Hash;

use resiter::filter::*;
use resiter::map::*;

use crate::dataset::adapter::GraphAsDataset;
use crate::triple::stream::*;
use crate::triple::streaming_mode::*;
use crate::triple::*;
use sophia_term::matcher::TermMatcher;
use sophia_term::*;

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
///
/// See [`Graph::triples`](./trait.Graph.html#tymethod.triples)
/// for more information about how to use it.
pub type GTripleSource<'a, G> = Box<dyn Iterator<Item = GResult<G, GTriple<'a, G>>> + 'a>;
/// Type alias for fallible hashsets of terms produced by a graph.
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
    ///
    /// # Examples
    ///
    /// The result of this method is an iterator,
    /// so it can be used in a `for` loop:
    /// ```
    /// # use sophia::graph::Graph;
    /// # use sophia::term::BoxTerm;
    /// # fn foo() -> Result<(), std::convert::Infallible> {
    /// # let graph = Vec::<[BoxTerm;3]>::new();
    /// for t in graph.triples() {
    ///     let t = t?; // rethrow error if any
    ///     // do something with t
    /// }
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Another way is to use the specific methods provided by
    /// [`TripleSource`](../triple/stream/trait.TripleSource.html),
    /// for example:
    /// ```
    /// # use sophia::graph::Graph;
    /// # use sophia::term::BoxTerm;
    /// # use sophia::triple::stream::TripleSource;
    /// # fn foo() -> Result<(), std::convert::Infallible> {
    /// # let graph = Vec::<[BoxTerm;3]>::new();
    /// graph.triples().for_each_triple(|t| {
    ///     // do something with t
    /// })?; // rethrow error if any
    /// # Ok(())
    /// # }
    /// ```
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
    ///
    /// # Usage
    ///
    /// ```
    /// # use sophia::graph::{*, inmem::LightGraph};
    /// # use sophia::triple::Triple;
    /// use sophia_term::ns::{Namespace, rdf};
    /// use sophia_term::matcher::{ANY, TermMatcher};
    ///
    /// # let mut graph = LightGraph::new();
    /// let s = Namespace::new("http://schema.org/").unwrap();
    /// let city = s.get("City").unwrap();
    /// let country = s.get("Country").unwrap();
    ///
    /// for t in graph.triples_matching(&ANY, &rdf::type_, &[city, country]) {
    ///     let t = t.unwrap();
    ///     println!("{} was found", t.s());
    /// }
    /// ```
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
            if let Term::Iri(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let Term::Iri(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let Term::Iri(_) = o {
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
            if let Term::BNode(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let Term::BNode(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let Term::BNode(_) = o {
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
            if let Term::Literal(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let Term::Literal(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let Term::Literal(_) = o {
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
            if let Term::Variable(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let Term::Variable(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let Term::Variable(_) = o {
                insert_if_absent(&mut res, o)
            }
        }
        Ok(res)
    }

    /// [`Dataset`](../dataset/trait.Dataset.html) adapter borrowing this graph
    fn as_dataset(&self) -> GraphAsDataset<Self, &Self> {
        GraphAsDataset::new(self)
    }

    /// [`Dataset`](../dataset/trait.Dataset.html) adapter borrowing this graph mutably
    fn as_dataset_mut(&mut self) -> GraphAsDataset<Self, &mut Self> {
        GraphAsDataset::new(self)
    }

    /// [`Dataset`](../dataset/trait.Dataset.html) adapter taking ownership of this graph
    fn into_dataset(self) -> GraphAsDataset<Self>
    where
        Self: Sized,
    {
        GraphAsDataset::new(self)
    }
}

/// A graph that can be constructed from a
/// [`TripleSource`](../triple/stream/trait.TripleSource.html)
pub trait CollectibleGraph: Graph + Sized {
    fn from_triple_source<TS: TripleSource>(
        triples: TS,
    ) -> StreamResult<Self, TS::Error, Self::Error>;
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
    /// # Return value
    /// The `bool` value returned in case of success is
    /// **not significant unless** this graph also implements [`SetGraph`].
    ///
    /// If it does,
    /// `true` is returned iff the insertion actually changed the graph.
    /// In other words,
    /// a return value of `false` means that the graph was not changed,
    /// because the triple was already present in this [`SetGraph`].
    ///
    /// # Usage
    /// ```
    /// # use sophia_term::BoxTerm;
    /// # use sophia_term::ns::{Namespace, rdf, rdfs, xsd};
    /// # use sophia::graph::{MutableGraph, MGResult};
    /// # use std::collections::HashSet;
    ///
    /// # fn populate<G: MutableGraph>(graph: &mut G) -> MGResult<G, ()> {
    /// let schema = Namespace::new("http://schema.org/").unwrap();
    /// let s_name = schema.get("name").unwrap();
    ///
    /// graph.insert(&s_name, &rdf::type_, &rdf::Property)?;
    /// graph.insert(&s_name, &rdfs::range, &xsd::string)?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// [`SetGraph`]: trait.SetGraph.html
    fn insert<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData;

    /// Remove the given triple from this graph.
    ///
    /// # Return value
    /// The `bool` value returned in case of success is
    /// **not significant unless** this graph also implements [`SetGraph`].
    ///
    /// If it does,
    /// `true` is returned iff the removal actually changed the graph.
    /// In other words,
    /// a return value of `false` means that the graph was not changed,
    /// because the triple was already absent from this [`SetGraph`].
    ///
    /// [`SetGraph`]: trait.SetGraph.html
    fn remove<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> MGResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData;

    /// Insert into this graph all triples from the given source.
    ///
    /// # Blank node scope
    /// The blank nodes contained in the triple source will be inserted as is.
    /// If they happen to have the same identifier as blank nodes already present,
    /// they will be considered equal.
    /// This might *not* be what you want,
    /// especially if the graph contains data from a file,
    /// and you are inserting data from a different file.
    /// In that case, you should first transform the triple source,
    /// in order to get fresh blank node identifiers.
    ///
    /// # Return value
    /// The `usize` value returned in case of success is
    /// **not significant unless** this graph also implements [`SetGraph`].
    ///
    /// If it does,
    /// the number of triples that were *actually* inserted
    /// (i.e. that were not already present in this [`SetGraph`])
    /// is returned.
    ///
    /// [`SetGraph`]: trait.SetGraph.html
    #[inline]
    fn insert_all<TS>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, <Self as MutableGraph>::MutationError>
    where
        TS: TripleSource,
    {
        let mut src = src;
        let mut c = 0;
        src.try_for_each_triple(|t| -> MGResult<Self, ()> {
            if self.insert(t.s(), t.p(), t.o())? {
                c += 1;
            }
            Ok(())
        })
        .and(Ok(c))
    }

    /// Remove from this graph all triples from the given source.
    ///
    /// # Return value
    /// The `usize` value returned in case of success is
    /// **not significant unless** this graph also implements [`SetGraph`].
    ///
    /// If it does,
    /// the number of triples that were *actually* removed
    /// (i.e. that were not already absent from this [`SetGraph`])
    /// is returned.
    ///
    /// [`SetGraph`]: trait.SetGraph.html
    #[inline]
    fn remove_all<TS>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, <Self as MutableGraph>::MutationError>
    where
        TS: TripleSource,
    {
        let mut src = src;
        let mut c = 0;
        src.try_for_each_triple(|t| -> MGResult<Self, ()> {
            if self.remove(t.s(), t.p(), t.o())? {
                c += 1;
            }
            Ok(())
        })
        .and(Ok(c))
    }

    /// Remove all triples matching the given matchers.
    ///
    /// # Return value
    /// The `usize` value returned in case of success is
    /// **not significant unless** this graph also implements [`SetGraph`].
    ///
    /// If it does,
    /// the number of triples that were *actually* removed
    /// (i.e. that were not already absent from this [`SetGraph`])
    /// is returned.
    ///
    /// # Note to implementors
    /// The default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    ///
    /// [`SetGraph`]: trait.SetGraph.html
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
                    t.s().clone_into::<Box<str>>(),
                    t.p().clone_into::<Box<str>>(),
                    t.o().clone_into::<Box<str>>(),
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
    /// # Note to implementors
    /// The default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
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
                    t.s().clone_into::<Box<str>>(),
                    t.p().clone_into::<Box<str>>(),
                    t.o().clone_into::<Box<str>>(),
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
/// [`Graph`] and [`MutableGraph`].
///
/// It guarantees that
/// (1) triples will never be returned / stored multiple times.
///
/// If the type also implements [`MutableGraph`],
/// it must also ensure that
/// (2) the `bool` or `usize` values returned by [`MutableGraph`]
/// methods accurately describe how many triples were actually added/removed.
///
/// # Note to implementors
/// A type implementing both [`Graph`] and [`MutableGraph`],
/// enforcing (1) but failing to enforce (2)
/// *must not* implement this trait.
///
/// [`Graph`]: trait.Graph.html
/// [`MutableGraph`]: trait.MutableGraph.html

pub trait SetGraph: Graph {}

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
