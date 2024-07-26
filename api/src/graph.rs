//! An RDF graph, the central notion of the RDF data model,
//! is a collection of [triples](super::triple).
//!
//! This module provides [reusable abstractions](#traits)
//! for different kinds of graph,
//! as well as a few implementations for them.

use crate::dataset::adapter::GraphAsDataset;
use crate::source::{IntoSource, StreamResult, TripleSource};
use crate::term::{matcher::TermMatcher, SimpleTerm, Term};
use crate::triple::Triple;

use resiter::{filter::*, flat_map::*, map::*};

mod _foreign_impl;
pub mod adapter;
#[cfg(any(test, feature = "test_macro"))]
#[macro_use]
pub mod test;

/// Type alias for results produced by a graph.
pub type GResult<G, T> = Result<T, <G as Graph>::Error>;
/// Type alias for fallible triple iterators produced by a graph.
///
/// See [`Graph::triples`] for more information about how to use it.
#[deprecated(
    since = "0.8.1",
    note = "prototypes of `triples` and `triples_matching` have changed"
)]
pub type GTripleSource<'a, G> = Box<dyn Iterator<Item = GResult<G, <G as Graph>::Triple<'a>>> + 'a>;
/// Type alias for terms produced by a graph.
pub type GTerm<'a, G> = <<G as Graph>::Triple<'a> as Triple>::Term;
/// Type alias for fallible term iterators produced by a graph.
///
/// See [`Graph::subjects`] for more information about how to use it.
#[deprecated(
    since = "0.8.1",
    note = "prototypes of term-yielding methods have changed"
)]
pub type GTermSource<'a, G> = Box<dyn Iterator<Item = GResult<G, GTerm<'a, G>>> + 'a>;

/// Generic trait for RDF graphs.
///
/// For convenience, this trait is implemented
/// by [standard collections of triples](#foreign-impls).
///
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`].
pub trait Graph {
    /// Determine the type of [`Triple`]s
    /// that the methods of this graph will yield.
    type Triple<'x>: Triple
    where
        Self: 'x;
    /// The error type that this graph may raise.
    type Error: Error + 'static;

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
    /// # fn test() -> Result<(), Box<dyn std::error::Error>> {
    /// # use sophia_api::graph::Graph;
    /// # use sophia_api::term::SimpleTerm;
    /// # let graph = Vec::<[SimpleTerm;3]>::new();
    /// #
    /// for t in graph.triples() {
    ///     let t = t?; // rethrow error if any
    ///     // do something with t
    /// }
    /// #
    /// # Ok(())
    /// # }
    /// ```
    /// Another way is to use the specific methods provided by [`TripleSource`],
    /// for example:
    /// ```
    /// # use sophia_api::graph::Graph;
    /// # use sophia_api::term::SimpleTerm;
    /// # use sophia_api::source::TripleSource;
    /// # fn test() -> Result<(), Box<dyn std::error::Error>> {
    /// # let graph = Vec::<[SimpleTerm;3]>::new();
    /// #
    /// graph.triples().for_each_triple(|t| {
    ///     // do something with t
    /// })?; // rethrow error if any
    /// #
    /// # Ok(())
    /// # }
    /// ```
    fn triples(&self) -> impl Iterator<Item = GResult<Self, Self::Triple<'_>>> + '_;

    /// An iterator visiting all triples matching the given subject, predicate and object.
    /// See [`crate::term::matcher`].
    ///
    /// See also [`triples`](Graph::triples).
    ///
    /// # Usage
    ///
    /// Typical implementations of [`TermMatcher`] include arrays/slices of [`Term`]s,
    /// closure accepting a [`SimpleTerm`], or the special matcher [`Any`].
    ///
    /// [`Term`]: crate::term::Term
    /// [`SimpleTerm`]: crate::term::SimpleTerm
    /// [`Any`]: crate::term::matcher::Any
    /// ```
    /// # use sophia_api::prelude::*;
    /// # use sophia_api::ns::{Namespace, rdf};
    /// #
    /// # fn test<G: Graph>(graph: &G) -> Result<(), Box<dyn std::error::Error>>
    /// # where
    /// #     G: Graph,
    /// # {
    /// #
    /// let s = Namespace::new("http://schema.org/")?;
    /// let city = s.get("City")?;
    /// let country = s.get("Country")?;
    ///
    /// for t in graph.triples_matching(Any, [&rdf::type_], [city, country]) {
    ///     println!("{:?} was found", t?.s());
    /// }
    /// #
    /// # Ok(()) }
    /// ```
    ///
    /// Here is another example using a closure as a [`TermMatcher`].
    ///
    /// ```
    /// # use sophia_api::prelude::*;
    /// # use sophia_api::ns::rdfs;
    /// # use sophia_api::term::SimpleTerm;
    /// #
    /// # fn test<G>(graph: &G) -> Result<(), Box<dyn std::error::Error>>
    /// # where
    /// #     G: Graph,
    /// # {
    /// #
    /// for t in graph.triples_matching(
    ///     Any,
    ///     [&rdfs::label],
    ///     |t: SimpleTerm| t.lexical_form().map(|v| v.contains("needle")).unwrap_or(false),
    /// ) {
    ///     println!("{:?} was found", t?.s());
    /// }
    /// #
    /// # Ok(()) }
    /// ```
    fn triples_matching<'s, S, P, O>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
    ) -> impl Iterator<Item = GResult<Self, Self::Triple<'s>>> + 's
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
        O: TermMatcher + 's,
    {
        self.triples()
            .filter_ok(move |t| t.matched_by(sm.matcher_ref(), pm.matcher_ref(), om.matcher_ref()))
    }

    /// Return `true` if this graph contains the given triple.
    fn contains<TS, TP, TO>(&self, s: TS, p: TP, o: TO) -> GResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        self.triples_matching([s], [p], [o])
            .next()
            .transpose()
            .map(|o| o.is_some())
    }

    /// Build a fallible iterator of all the terms used as subject in this Graph.
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn subjects(&self) -> impl Iterator<Item = GResult<Self, GTerm<'_, Self>>> + '_ {
        self.triples().map_ok(Triple::to_s)
    }

    /// Build a fallible iterator of all the terms used as predicate in this Graph.
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn predicates(&self) -> impl Iterator<Item = GResult<Self, GTerm<'_, Self>>> + '_ {
        self.triples().map_ok(Triple::to_p)
    }

    /// Build a fallible iterator of all the terms used as object in this Graph.
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn objects(&self) -> impl Iterator<Item = GResult<Self, GTerm<'_, Self>>> + '_ {
        self.triples().map_ok(Triple::to_o)
    }

    /// Build a fallible iterator of all the IRIs used in this Graph
    /// (including those used inside quoted triples, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn iris(&self) -> impl Iterator<Item = GResult<Self, GTerm<'_, Self>>> + '_ {
        self.triples()
            .flat_map_ok(Triple::to_spo)
            .flat_map_ok(Term::to_atoms)
            .filter_ok(Term::is_iri)
    }

    /// Build a fallible iterator of all the blank nodes used in this Graph
    /// (including those used inside quoted triples, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn blank_nodes(&self) -> impl Iterator<Item = GResult<Self, GTerm<'_, Self>>> + '_ {
        self.triples()
            .flat_map_ok(Triple::to_spo)
            .flat_map_ok(Term::to_atoms)
            .filter_ok(Term::is_blank_node)
    }

    /// Build a fallible iterator of all the literals used in this Graph
    /// (including those used inside quoted triples, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn literals(&self) -> impl Iterator<Item = GResult<Self, GTerm<'_, Self>>> + '_ {
        self.triples()
            .flat_map_ok(Triple::to_spo)
            .flat_map_ok(Term::to_atoms)
            .filter_ok(Term::is_literal)
    }

    /// Build a fallible iterator of all the quoted triples used in this Graph
    /// (including those used inside quoted triples, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn quoted_triples<'s>(&'s self) -> Box<dyn Iterator<Item = GResult<Self, GTerm<'_, Self>>> + '_>
    where
        GTerm<'s, Self>: Clone,
    {
        Box::new(
            self.triples()
                .flat_map_ok(Triple::to_spo)
                .flat_map_ok(Term::to_constituents)
                .filter_ok(Term::is_triple),
        )
    }

    /// Build a fallible iterator of all the variables used in this Graph
    /// (including those used inside quoted triples, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn variables(&self) -> impl Iterator<Item = GResult<Self, GTerm<'_, Self>>> + '_ {
        self.triples()
            .flat_map_ok(Triple::to_spo)
            .flat_map_ok(Term::to_atoms)
            .filter_ok(Term::is_variable)
    }

    /// [`Dataset`](crate::dataset::Dataset) adapter borrowing this graph
    fn as_dataset(&self) -> GraphAsDataset<&Self> {
        GraphAsDataset::new(self)
    }

    /// [`Dataset`](crate::dataset::Dataset) adapter borrowing this graph mutably
    fn as_dataset_mut(&mut self) -> GraphAsDataset<&mut Self> {
        GraphAsDataset::new(self)
    }

    /// [`Dataset`](crate::dataset::Dataset) adapter taking ownership of this graph
    fn into_dataset(self) -> GraphAsDataset<Self>
    where
        Self: Sized,
    {
        GraphAsDataset::new(self)
    }
}

/// A [`Graph`] that can be constructed from a [`TripleSource`]
pub trait CollectibleGraph: Graph + Sized {
    /// Construct a graph from the given source
    fn from_triple_source<TS: TripleSource>(
        triples: TS,
    ) -> StreamResult<Self, TS::Error, Self::Error>;
}

/// Type alias for results produced by a mutable graph.
pub type MgResult<G, T> = std::result::Result<T, <G as MutableGraph>::MutationError>;

/// Generic trait for mutable RDF graphs.
///
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`].
pub trait MutableGraph: Graph {
    /// The error type that this graph may raise during mutations.
    type MutationError: Error + 'static;

    /// Insert in this graph a triple made of the the given terms.
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
    /// See also [`insert_triple`](MutableGraph::insert_triple)
    ///
    /// # Usage
    /// ```
    /// # use sophia_api::graph::{MutableGraph, MgResult};
    /// # use sophia_api::ns::{Namespace, rdf, rdfs, xsd};
    /// # fn populate<G: MutableGraph>(graph: &mut G) -> MgResult<G, ()> {
    /// #
    /// let schema = Namespace::new("http://schema.org/").unwrap();
    /// let s_name = schema.get("name").unwrap();
    ///
    /// graph.insert(&s_name, &rdf::type_, &rdf::Property)?;
    /// graph.insert(&s_name, &rdfs::range, &xsd::string)?;
    /// graph.insert(&s_name, &rdfs::comment, "The name of the item.")?;
    /// #
    /// # Ok(())
    /// # }
    /// ```
    fn insert<TS, TP, TO>(&mut self, s: TS, p: TP, p: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term;

    /// Insert in this graph the given triple.
    ///
    /// NB: if you want to insert a triple `t` while keeping its ownership,
    /// you can still pass [`t.spo()`](Triple::spo).
    ///
    /// See also [`MutableGraph::insert`]
    fn insert_triple<T>(&mut self, triple: T) -> MgResult<Self, bool>
    where
        T: Triple,
    {
        let [s, p, o] = triple.to_spo();
        self.insert(s, p, o)
    }

    /// Remove from this graph the triple made of the the given terms.
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
    fn remove<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term;

    /// Remoe from this graph the given triple.
    ///
    /// NB: if you want to remove a triple `t` while keeping its ownership,
    /// you can still pass [`t.spo()`](Triple::spo).
    ///
    /// See also [MutableGraph::remove]
    fn remove_triple<T>(&mut self, triple: T) -> MgResult<Self, bool>
    where
        T: Triple,
    {
        let [s, p, o] = triple.to_spo();
        self.remove(s, p, o)
    }

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
    #[inline]
    fn insert_all<TS: TripleSource>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, <Self as MutableGraph>::MutationError> {
        let mut src = src;
        let mut c = 0;
        src.try_for_each_triple(|t| -> MgResult<Self, ()> {
            if self.insert_triple(t.spo())? {
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
    #[inline]
    fn remove_all<TS: TripleSource>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, <Self as MutableGraph>::MutationError> {
        let mut src = src;
        let mut c = 0;
        src.try_for_each_triple(|t| -> MgResult<Self, ()> {
            if self.remove_triple(t.spo())? {
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
    fn remove_matching<S, P, O>(
        &mut self,
        ms: S,
        mp: P,
        mo: O,
    ) -> Result<usize, Self::MutationError>
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
        Self::MutationError: From<Self::Error>,
    {
        let to_remove: Result<Vec<[SimpleTerm; 3]>, _> = self
            .triples_matching(ms, mp, mo)
            .map_ok(|t| t.spo().map(Term::into_term))
            .collect();
        self.remove_all(to_remove?.into_iter().into_source())
            .map_err(|err| err.unwrap_sink_error())
    }

    /// Keep only the triples matching the given matchers.
    ///
    /// # Note to implementors
    /// The default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    fn retain_matching<S, P, O>(&mut self, ms: S, mp: P, mo: O) -> Result<(), Self::MutationError>
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
        Self::MutationError: From<Self::Error>,
    {
        let to_remove: Result<Vec<[SimpleTerm; 3]>, _> = self
            .triples()
            .filter_ok(|t| !t.matched_by(ms.matcher_ref(), mp.matcher_ref(), mo.matcher_ref()))
            .map_ok(|t| t.spo().map(Term::into_term))
            .collect();
        self.remove_all(to_remove?.into_iter().into_source())
            .map_err(|err| err.unwrap_sink_error())?;
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
pub trait SetGraph: Graph {}

#[cfg(test)]
mod check_implementability {
    /// This is a naive implementation of an RDF-star graph,
    /// where the graph maintains
    /// - a list of terms (either atoms or index of triple)
    /// - a list of triples (SPO indexes, plus an 'asserted' flag)
    ///
    /// This avoids the need to store arbitrarily nested triples.
    use super::*;
    use crate::term::SimpleTerm;

    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(dead_code)] // testing implementability
    enum MyInternalTerm {
        Atom(SimpleTerm<'static>),
        QuotedTriple(usize),
    }
    use MyInternalTerm::*;

    #[derive(Clone, Debug, Eq, PartialEq)]
    struct MyInternalTriple {
        asserted: bool,
        spo: [usize; 3],
    }

    #[derive(Clone, Debug)]
    struct MyGraph {
        terms: Vec<MyInternalTerm>,
        triples: Vec<MyInternalTriple>,
    }

    impl MyGraph {
        fn make_term(&self, i: usize) -> SimpleTerm<'_> {
            match &self.terms[i] {
                Atom(t) => t.as_simple(),
                QuotedTriple(j) => {
                    SimpleTerm::Triple(Box::new(self.make_triple(self.triples[*j].spo)))
                }
            }
        }

        fn make_triple(&self, spo: [usize; 3]) -> [SimpleTerm<'_>; 3] {
            spo.map(|j| self.make_term(j))
        }
    }

    impl Graph for MyGraph {
        type Triple<'x> = [SimpleTerm<'x>; 3] where Self: 'x;
        type Error = std::convert::Infallible;

        fn triples(&self) -> impl Iterator<Item = GResult<Self, Self::Triple<'_>>> + '_ {
            self.triples
                .iter()
                .filter(|t| t.asserted)
                .map(|t| Ok(self.make_triple(t.spo)))
        }
    }
}

#[cfg(test)]
mod check_implementability_lazy_term {
    /// This implementation is internally similar to the one above,
    /// but using dedicated lazy implementations of Term
    /// (lazy because it avoids allocating nested triples until forced)
    use super::*;
    use crate::term::{SimpleTerm, TermKind};

    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(dead_code)] // testing implementability
    enum MyInternalTerm {
        Atom(SimpleTerm<'static>),
        QuotedTriple(usize),
    }
    use MyInternalTerm::*;

    #[derive(Clone, Debug, Eq, PartialEq)]
    struct MyInternalTriple {
        asserted: bool,
        spo: [usize; 3],
    }

    #[derive(Clone, Debug)]
    struct MyGraph {
        terms: Vec<MyInternalTerm>,
        triples: Vec<MyInternalTriple>,
    }

    #[derive(Clone, Copy, Debug)]
    struct MyTerm<'a> {
        graph: &'a MyGraph,
        index: usize,
    }

    impl<'a> Term for MyTerm<'a> {
        type BorrowTerm<'x> = MyTerm<'x> where Self: 'x;

        fn kind(&self) -> crate::term::TermKind {
            if let Atom(t) = &self.graph.terms[self.index] {
                t.kind()
            } else {
                TermKind::Triple
            }
        }

        fn iri(&self) -> Option<crate::term::IriRef<mownstr::MownStr>> {
            if let Atom(t) = &self.graph.terms[self.index] {
                t.iri()
            } else {
                None
            }
        }

        fn bnode_id(&self) -> Option<crate::term::BnodeId<mownstr::MownStr>> {
            if let Atom(t) = &self.graph.terms[self.index] {
                t.bnode_id()
            } else {
                None
            }
        }

        fn lexical_form(&self) -> Option<mownstr::MownStr> {
            if let Atom(t) = &self.graph.terms[self.index] {
                t.lexical_form()
            } else {
                None
            }
        }

        fn datatype(&self) -> Option<crate::term::IriRef<mownstr::MownStr>> {
            if let Atom(t) = &self.graph.terms[self.index] {
                t.datatype()
            } else {
                None
            }
        }

        fn language_tag(&self) -> Option<crate::term::LanguageTag<mownstr::MownStr>> {
            if let Atom(t) = &self.graph.terms[self.index] {
                t.language_tag()
            } else {
                None
            }
        }

        fn variable(&self) -> Option<crate::term::VarName<mownstr::MownStr>> {
            if let Atom(t) = &self.graph.terms[self.index] {
                t.variable()
            } else {
                None
            }
        }

        fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
            self.to_triple()
        }

        fn to_triple(self) -> Option<[Self; 3]> {
            if let QuotedTriple(i) = &self.graph.terms[self.index] {
                Some(self.graph.triples[*i].spo.map(|t| MyTerm {
                    graph: self.graph,
                    index: t,
                }))
            } else {
                None
            }
        }

        fn borrow_term(&self) -> Self::BorrowTerm<'_> {
            *self
        }
    }

    impl Graph for MyGraph {
        type Triple<'x> = [MyTerm<'x>; 3] where Self: 'x;

        type Error = std::convert::Infallible;

        fn triples(&self) -> impl Iterator<Item = GResult<Self, Self::Triple<'_>>> + '_ {
            self.triples.iter().filter(|t| t.asserted).map(|t| {
                Ok(t.spo.map(|i| MyTerm {
                    graph: self,
                    index: i,
                }))
            })
        }
    }
}
