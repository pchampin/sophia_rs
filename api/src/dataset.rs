//! An RDF dataset is composed of a default [dataset](crate::dataset),
//! and zero or more named graphs, each associated with a dataset name.
//!
//! Another way to look at it is as a collection of [quad](crate::quad)s.
//!
//! This module provides [reusable abstractions](#traits)
//! for different kinds of datasets,
//! as well as a few implementations for them.

use crate::graph::adapter::{DatasetGraph, PartialUnionGraph, UnionGraph};
use crate::quad::{iter_spog, Quad};
use crate::source::{IntoSource, QuadSource, StreamResult};
use crate::term::matcher::{GraphNameMatcher, TermMatcher};
use crate::term::{GraphName, SimpleTerm, Term};
use crate::Error;
use resiter::{filter::*, filter_map::*, flat_map::*, map::*};

mod _foreign_impl;
pub mod adapter;
#[cfg(any(test, feature = "test_macro"))]
#[macro_use]
pub mod test;

/// Type alias for results produced by a dataset.
pub type DResult<D, T> = Result<T, <D as Dataset>::Error>;
/// Type alias for fallible quad iterators produced by a dataset.
///
/// See [`Dataset::quads`] for more information about how to use it.
#[deprecated(
    since = "0.8.1",
    note = "prototypes of `quads` and `quads_matching` have changed"
)]
pub type DQuadSource<'a, D> = Box<dyn Iterator<Item = DResult<D, <D as Dataset>::Quad<'a>>> + 'a>;
/// Type alias for terms produced by a dataset.
pub type DTerm<'a, D> = <<D as Dataset>::Quad<'a> as Quad>::Term;
/// Type alias for fallible term iterators produced by a dataset.
///
/// See [`Dataset::subjects`] for more information about how to use it.
#[deprecated(
    since = "0.8.1",
    note = "prototypes of term-yielding methods have changed"
)]
pub type DTermSource<'a, D> = Box<dyn Iterator<Item = DResult<D, DTerm<'a, D>>> + 'a>;

/// Generic trait for RDF datasets.
///
/// For convenience, this trait is implemented
/// by [standard collections of quads](#foreign-impls).
///
/// NB: the semantics of this trait allows a dataset to contain duplicate quads;
/// see also [`SetDataset`].
pub trait Dataset {
    /// Determine the type of [`Quad`]s
    /// that the methods of this dataset will yield.
    type Quad<'x>: Quad
    where
        Self: 'x;
    /// The error type that this dataset may raise.
    type Error: Error + 'static;

    /// An iterator visiting all quads of this dataset in arbitrary order.
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
    /// # fn test() -> Result<(), Box<dyn sophia_api::Error>> {
    /// # use sophia_api::dataset::Dataset;
    /// # use sophia_api::term::SimpleTerm;
    /// # let dataset = Vec::<[SimpleTerm;4]>::new();
    /// #
    /// for q in dataset.quads() {
    ///     let q = q?; // rethrow error if any
    ///     // do something with q
    /// }
    /// #
    /// # Ok(())
    /// # }
    /// ```
    /// Another way is to use the specific methods provided by [`QuadSource`],
    /// for example:
    /// ```
    /// # use sophia_api::dataset::Dataset;
    /// # use sophia_api::term::SimpleTerm;
    /// # use sophia_api::source::QuadSource;
    /// # fn test() -> Result<(), Box<dyn sophia_api::Error>> {
    /// # let dataset = Vec::<[SimpleTerm;4]>::new();
    /// #
    /// dataset.quads().for_each_quad(|q| {
    ///     // do something with q
    /// })?; // rethrow error if any
    /// #
    /// # Ok(())
    /// # }
    /// ```
    fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_;

    /// An iterator visiting all quads matching the given subject, predicate and object.
    /// See [`crate::term::matcher`]
    ///
    /// See also [`quads`](Dataset::quads).
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
    /// # fn test<G: Dataset>(dataset: &G) -> Result<(), Box<dyn sophia_api::Error>>
    /// # where
    /// #     G: Dataset,
    /// # {
    /// #
    /// let s = Namespace::new("http://schema.org/")?;
    /// let city = s.get("City")?;
    /// let country = s.get("Country")?;
    ///
    /// for q in dataset.quads_matching(Any, [&rdf::type_], [city, country], Any) {
    ///     println!("{:?} was found", q?.s());
    /// }
    /// #
    /// # Ok(()) }
    /// ```
    ///
    /// Here is another example using a closure as a [`TermMatcher`].
    ///
    /// ```
    /// # use sophia_api::dataset::Dataset;
    /// # use sophia_api::term::{SimpleTerm, Term};
    /// # use sophia_api::quad::Quad;
    /// # use sophia_api::ns::rdfs;
    /// #
    /// # fn test<G>(dataset: &G) -> Result<(), Box<dyn sophia_api::Error>>
    /// # where
    /// #     G: Dataset,
    /// # {
    /// #
    /// use sophia_api::term::matcher::Any;
    ///
    /// for q in dataset.quads_matching(
    ///     Any,
    ///     [&rdfs::label],
    ///     |t: SimpleTerm| t.lexical_form().map(|v| v.contains("needle")).unwrap_or(false),
    ///     Any,
    /// ) {
    ///     println!("{:?} was found", q?.s());
    /// }
    /// #
    /// # Ok(()) }
    /// ```
    fn quads_matching<'s, S, P, O, G>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
        gm: G,
    ) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
        O: TermMatcher + 's,
        G: GraphNameMatcher + 's,
    {
        self.quads().filter_ok(move |q| {
            q.matched_by(
                sm.matcher_ref(),
                pm.matcher_ref(),
                om.matcher_ref(),
                gm.matcher_ref(),
            )
        })
    }

    /// Return `true` if this dataset contains the given quad.
    fn contains<TS, TP, TO, TG>(&self, s: TS, p: TP, o: TO, g: GraphName<TG>) -> DResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        self.quads_matching([s], [p], [o], [g])
            .next()
            .transpose()
            .map(|o| o.is_some())
    }

    /// Build a fallible iterator of all the terms used as subject in this Dataset.
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn subjects(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        self.quads().map_ok(Quad::to_s)
    }

    /// Build a fallible iterator of all the terms used as predicate in this Dataset.
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn predicates(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        self.quads().map_ok(Quad::to_p)
    }

    /// Build a fallible iterator of all the terms used as object in this Dataset.
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn objects(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        self.quads().map_ok(Quad::to_o)
    }

    /// Build a fallible iterator of all the terms used as graph name in this Dataset.
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn graph_names(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        self.quads().filter_map_ok(Quad::to_g)
    }

    /// Build a fallible iterator of all the IRIs used in this Dataset
    /// (including those used inside quoted quads, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn iris(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        self.quads()
            .flat_map_ok(iter_spog)
            .flat_map_ok(Term::to_atoms)
            .filter_ok(Term::is_iri)
    }

    /// Build a fallible iterator of all the blank nodes used in this Dataset
    /// (including those used inside quoted quads, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn blank_nodes(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        self.quads()
            .flat_map_ok(iter_spog)
            .flat_map_ok(Term::to_atoms)
            .filter_ok(Term::is_blank_node)
    }

    /// Build a fallible iterator of all the literals used in this Dataset
    /// (including those used inside quoted quads, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn literals(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        self.quads()
            .flat_map_ok(iter_spog)
            .flat_map_ok(Term::to_atoms)
            .filter_ok(Term::is_literal)
    }

    /// Build a fallible iterator of all the quoted triples used in this Dataset
    /// (including those used inside quoted triples, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn quoted_triples<'s>(&'s self) -> Box<dyn Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_>
    where
        DTerm<'s, Self>: Clone,
    {
        Box::new(
            self.quads()
                .flat_map_ok(iter_spog)
                .flat_map_ok(Term::to_constituents)
                .filter_ok(Term::is_triple),
        )
    }

    /// Build a fallible iterator of all the variables used in this Dataset
    /// (including those used inside quoted quads, if any).
    ///
    /// NB: implementations SHOULD avoid yielding the same term multiple times, but MAY do so.
    /// Users MUST therefore be prepared to deal with duplicates.
    fn variables(&self) -> impl Iterator<Item = DResult<Self, DTerm<'_, Self>>> + '_ {
        self.quads()
            .flat_map_ok(iter_spog)
            .flat_map_ok(Term::to_atoms)
            .filter_ok(Term::is_variable)
    }

    /// Borrows one of the graphs of this dataset
    fn graph<T>(&self, graph_name: GraphName<T>) -> DatasetGraph<&Self, T>
    where
        T: for<'x> Term<BorrowTerm<'x> = DTerm<'x, Self>> + 'static,
    {
        DatasetGraph::new(self, graph_name)
    }

    /// Borrows mutably one of the graphs of this dataset
    fn graph_mut<T>(&mut self, graph_name: GraphName<T>) -> DatasetGraph<&mut Self, T>
    where
        T: for<'x> Term<BorrowTerm<'x> = DTerm<'x, Self>> + 'static,
    {
        DatasetGraph::new(self, graph_name)
    }

    /// Borrows a graph that is the union of some of this dataset's graphs
    fn partial_union_graph<M>(&self, selector: M) -> PartialUnionGraph<&Self, M>
    where
        M: GraphNameMatcher + Copy,
    {
        PartialUnionGraph::new(self, selector)
    }

    /// Borrows a graph that is the union of all this dataset's graphs (default and named)
    fn union_graph(&self) -> UnionGraph<&Self> {
        UnionGraph::new(self)
    }

    /// Convert into a graph that is the union of all this dataset's graphs (default and named)
    fn into_union_graph(self) -> UnionGraph<Self>
    where
        Self: Sized,
    {
        UnionGraph::new(self)
    }
}

/// A [`Dataset`] that can be constructed from a [`QuadSource`]
pub trait CollectibleDataset: Dataset + Sized {
    /// Construct a dataset from the given source
    fn from_quad_source<TS: QuadSource>(quads: TS) -> StreamResult<Self, TS::Error, Self::Error>;
}

/// Type alias for results produced by a mutable dataset.
pub type MdResult<D, T> = std::result::Result<T, <D as MutableDataset>::MutationError>;

/// Generic trait for mutable RDF datasets.
///
/// NB: the semantics of this trait allows a dataset to contain duplicate quads;
/// see also [`SetDataset`].
pub trait MutableDataset: Dataset {
    /// The error type that this dataset may raise during mutations.
    type MutationError: Error + 'static;

    /// Insert the given quad in this dataset.
    ///
    /// # Return value
    /// The `bool` value returned in case of success is
    /// **not significant unless** this dataset also implements [`SetDataset`].
    ///
    /// If it does,
    /// `true` is returned iff the insertion actually changed the dataset.
    /// In other words,
    /// a return value of `false` means that the dataset was not changed,
    /// because the quad was already present in this [`SetDataset`].
    ///
    /// See also [`MutableDataset::insert_quad`]
    ///
    /// # Usage
    /// ```
    /// # use sophia_api::dataset::{MutableDataset, MdResult};
    /// # use sophia_api::ns::{Namespace, rdf, rdfs, xsd};
    /// # use sophia_api::term::SimpleTerm;
    /// # fn populate<D: MutableDataset>(dataset: &mut D) -> MdResult<D, ()> {
    /// #
    /// let schema = Namespace::new("http://schema.org/").unwrap();
    /// let s_name = schema.get("name").unwrap();
    /// let default_graph: Option<&'static SimpleTerm<'static>> = None;
    ///
    /// dataset.insert(&s_name, &rdf::type_, &rdf::Property, default_graph)?;
    /// dataset.insert(&s_name, &rdfs::range, &xsd::string, default_graph)?;
    /// dataset.insert(&s_name, &rdfs::comment, "The name of the item.", Some(&rdfs::comment))?;
    /// #
    /// # Ok(())
    /// # }
    /// ```
    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term;

    /// Insert in this graph the given quad.
    ///
    /// NB: if you want to insert a quad `q` while keeping its ownership,
    /// you can still pass [`q.spog()`](Quad::spog).
    ///
    /// See also [`MutableDataset::insert`]
    fn insert_quad<T>(&mut self, quad: T) -> MdResult<Self, bool>
    where
        T: Quad,
    {
        let ([s, p, o], g) = quad.to_spog();
        self.insert(s, p, o, g)
    }

    /// Remove the given quad from this dataset.
    ///
    /// # Return value
    /// The `bool` value returned in case of success is
    /// **not significant unless** this dataset also implements [`SetDataset`].
    ///
    /// If it does,
    /// `true` is returned iff the removal actually changed the dataset.
    /// In other words,
    /// a return value of `false` means that the dataset was not changed,
    /// because the quad was already absent from this [`SetDataset`].
    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term;

    /// Remove from this graph a the given quad.
    ///
    /// NB: if you want to remove a quad `q` while keeping its ownership,
    /// you can still pass [`q.spog()`](Quad::spog).
    ///
    /// See also [MutableDataset::remove]
    fn remove_quad<T>(&mut self, quad: T) -> MdResult<Self, bool>
    where
        T: Quad,
    {
        let ([s, p, o], g) = quad.to_spog();
        self.remove(s, p, o, g)
    }

    /// Insert into this dataset all quads from the given source.
    ///
    /// # Blank node scope
    /// The blank nodes contained in the quad source will be inserted as is.
    /// If they happen to have the same identifier as blank nodes already present,
    /// they will be considered equal.
    /// This might *not* be what you want,
    /// especially if the dataset contains data from a file,
    /// and you are inserting data from a different file.
    /// In that case, you should first transform the quad source,
    /// in order to get fresh blank node identifiers.
    ///
    /// # Return value
    /// The `usize` value returned in case of success is
    /// **not significant unless** this dataset also implements [`SetDataset`].
    ///
    /// If it does,
    /// the number of quads that were *actually* inserted
    /// (i.e. that were not already present in this [`SetDataset`])
    /// is returned.
    #[inline]
    fn insert_all<TS: QuadSource>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, <Self as MutableDataset>::MutationError> {
        let mut src = src;
        let mut c = 0;
        src.try_for_each_quad(|q| -> MdResult<Self, ()> {
            if self.insert_quad(q.spog())? {
                c += 1;
            }
            Ok(())
        })
        .and(Ok(c))
    }

    /// Remove from this dataset all quads from the given source.
    ///
    /// # Return value
    /// The `usize` value returned in case of success is
    /// **not significant unless** this dataset also implements [`SetDataset`].
    ///
    /// If it does,
    /// the number of quads that were *actually* removed
    /// (i.e. that were not already absent from this [`SetDataset`])
    /// is returned.
    #[inline]
    fn remove_all<TS: QuadSource>(
        &mut self,
        src: TS,
    ) -> StreamResult<usize, TS::Error, <Self as MutableDataset>::MutationError> {
        let mut src = src;
        let mut c = 0;
        src.try_for_each_quad(|q| -> MdResult<Self, ()> {
            if self.remove_quad(q.spog())? {
                c += 1;
            }
            Ok(())
        })
        .and(Ok(c))
    }

    /// Remove all quads matching the given matchers.
    ///
    /// # Return value
    /// The `usize` value returned in case of success is
    /// **not significant unless** this dataset also implements [`SetDataset`].
    ///
    /// If it does,
    /// the number of quads that were *actually* removed
    /// (i.e. that were not already absent from this [`SetDataset`])
    /// is returned.
    ///
    /// # Note to implementors
    /// The default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    fn remove_matching<S, P, O, G>(
        &mut self,
        ms: S,
        mp: P,
        mo: O,
        mg: G,
    ) -> Result<usize, Self::MutationError>
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
        G: GraphNameMatcher,
        Self::MutationError: From<Self::Error>,
    {
        let to_remove: Result<Vec<([SimpleTerm; 3], GraphName<SimpleTerm>)>, _> = self
            .quads_matching(ms, mp, mo, mg)
            .map_ok(|q| {
                let (spo, g) = q.spog();
                (spo.map(Term::into_term), g.map(Term::into_term))
            })
            .collect();
        self.remove_all(to_remove?.into_iter().into_source())
            .map_err(|err| err.unwrap_sink_error())
    }

    /// Keep only the quads matching the given matchers.
    ///
    /// # Note to implementors
    /// The default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    fn retain_matching<S, P, O, G>(
        &mut self,
        ms: S,
        mp: P,
        mo: O,
        mg: G,
    ) -> Result<(), Self::MutationError>
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
        G: GraphNameMatcher,
        Self::MutationError: From<Self::Error>,
    {
        let to_remove: Result<Vec<([SimpleTerm; 3], GraphName<SimpleTerm>)>, _> = self
            .quads()
            .filter_ok(|q| {
                !q.matched_by(
                    ms.matcher_ref(),
                    mp.matcher_ref(),
                    mo.matcher_ref(),
                    mg.matcher_ref(),
                )
            })
            .map_ok(|q| {
                let (spo, g) = q.spog();
                (spo.map(Term::into_term), g.map(Term::into_term))
            })
            .collect();
        self.remove_all(to_remove?.into_iter().into_source())
            .map_err(|err| err.unwrap_sink_error())?;
        Ok(())
    }
}

/// Marker trait constraining the semantics of
/// [`Dataset`] and [`MutableDataset`].
///
/// It guarantees that
/// (1) quads will never be returned / stored multiple times.
///
/// If the type also implements [`MutableDataset`],
/// it must also ensure that
/// (2) the `bool` or `usize` values returned by [`MutableDataset`]
/// methods accurately describe how many quads were actually added/removed.
///
/// # Note to implementors
/// A type implementing both [`Dataset`] and [`MutableDataset`],
/// enforcing (1) but failing to enforce (2)
/// *must not* implement this trait.
pub trait SetDataset: Dataset {}

mod check_implementability {
    /// This is a naive implementation of an RDF-star dataset,
    /// where the dataset maintains
    /// - a list of terms (either atoms or index of quad)
    /// - a list of triples (SPO indexes)
    /// - a list of named graphs associated the triple indexes contained in the graph
    /// This avoids the need to store arbitrarily nested triples.
    /// NB: unasserted triples are not used in any quoted graph.
    use super::*;
    use crate::term::SimpleTerm;
    use std::collections::HashMap;

    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(dead_code)] // testing implementability
    enum MyInternalTerm {
        Atom(SimpleTerm<'static>),
        QuotedTriple(usize),
    }
    use MyInternalTerm::*;

    #[derive(Clone, Debug)]
    struct MyDataset {
        terms: Vec<MyInternalTerm>,
        triples: Vec<[usize; 3]>,
        graphs: HashMap<usize, Vec<usize>>,
    }

    impl MyDataset {
        fn make_term(&self, i: usize) -> SimpleTerm<'_> {
            match &self.terms[i] {
                Atom(t) => t.as_simple(),
                QuotedTriple(j) => {
                    SimpleTerm::Triple(Box::new(self.triples[*j].map(|k| self.make_term(k))))
                }
            }
        }
    }

    impl Dataset for MyDataset {
        type Quad<'x> = [SimpleTerm<'x>; 4] where Self: 'x;
        type Error = std::convert::Infallible;

        fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
            self.graphs.iter().flat_map(move |(gi, tis)| {
                let g = self.make_term(*gi);
                tis.iter().copied().map(move |ti| {
                    let [s, p, o] = self.triples[ti].map(|j| self.make_term(j));
                    Ok([s, p, o, g.clone()])
                })
            })
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
    use std::collections::HashMap;

    #[derive(Clone, Debug, Eq, PartialEq)]
    #[allow(dead_code)] // testing implementability
    enum MyInternalTerm {
        Atom(SimpleTerm<'static>),
        QuotedTriple(usize),
    }
    use MyInternalTerm::*;

    #[derive(Clone, Debug)]
    struct MyDataset {
        terms: Vec<MyInternalTerm>,
        triples: Vec<[usize; 3]>,
        graphs: HashMap<usize, Vec<usize>>,
    }

    #[derive(Clone, Copy, Debug)]
    struct MyTerm<'a> {
        dataset: &'a MyDataset,
        index: usize,
    }

    impl<'a> Term for MyTerm<'a> {
        type BorrowTerm<'x> = MyTerm<'x> where Self: 'x;

        fn kind(&self) -> crate::term::TermKind {
            if let Atom(t) = &self.dataset.terms[self.index] {
                t.kind()
            } else {
                TermKind::Triple
            }
        }

        fn iri(&self) -> Option<crate::term::IriRef<mownstr::MownStr>> {
            if let Atom(t) = &self.dataset.terms[self.index] {
                t.iri()
            } else {
                None
            }
        }

        fn bnode_id(&self) -> Option<crate::term::BnodeId<mownstr::MownStr>> {
            if let Atom(t) = &self.dataset.terms[self.index] {
                t.bnode_id()
            } else {
                None
            }
        }

        fn lexical_form(&self) -> Option<mownstr::MownStr> {
            if let Atom(t) = &self.dataset.terms[self.index] {
                t.lexical_form()
            } else {
                None
            }
        }

        fn datatype(&self) -> Option<crate::term::IriRef<mownstr::MownStr>> {
            if let Atom(t) = &self.dataset.terms[self.index] {
                t.datatype()
            } else {
                None
            }
        }

        fn language_tag(&self) -> Option<crate::term::LanguageTag<mownstr::MownStr>> {
            if let Atom(t) = &self.dataset.terms[self.index] {
                t.language_tag()
            } else {
                None
            }
        }

        fn variable(&self) -> Option<crate::term::VarName<mownstr::MownStr>> {
            if let Atom(t) = &self.dataset.terms[self.index] {
                t.variable()
            } else {
                None
            }
        }

        fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
            self.to_triple()
        }

        fn to_triple(self) -> Option<[Self; 3]> {
            if let QuotedTriple(i) = &self.dataset.terms[self.index] {
                Some(self.dataset.triples[*i].map(|t| MyTerm {
                    dataset: self.dataset,
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

    impl Dataset for MyDataset {
        type Quad<'x> = [MyTerm<'x>; 4] where Self: 'x;
        type Error = std::convert::Infallible;

        fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
            self.graphs.iter().flat_map(move |(gi, tis)| {
                let g = MyTerm {
                    dataset: self,
                    index: *gi,
                };
                tis.iter().copied().map(move |ti| {
                    let [s, p, o] = self.triples[ti].map(|j| MyTerm {
                        dataset: self,
                        index: j,
                    });
                    Ok([s, p, o, g])
                })
            })
        }
    }
}
