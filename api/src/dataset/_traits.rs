// this module is transparently re-exported by its parent `dataset`

use std::collections::HashSet;
use std::error::Error;
use std::hash::Hash;

use resiter::filter::*;
use resiter::map::*;

use crate::dataset::adapter::DatasetGraph;
use crate::quad::stream::*;
use crate::quad::streaming_mode::*;
use crate::quad::*;
use crate::term::matcher::*;
use crate::term::{same_graph_name, term_eq, TTerm, TermKind};
use crate::triple::stream::StreamResult;

use crate::graph::insert_if_absent;

/// Type alias for the terms returned by a dataset.
pub type DTerm<D> = <<<D as Dataset>::Quad as QuadStreamingMode>::UnsafeQuad as UnsafeQuad>::Term;
/// Type alias for the quads returned by a dataset.
pub type DQuad<'a, D> = StreamedQuad<'a, <D as Dataset>::Quad>;
/// Type alias for results iterators produced by a dataset.
pub type DResult<D, T> = Result<T, <D as Dataset>::Error>;
/// Type alias for fallible quad iterators produced by a dataset.
pub type DQuadSource<'a, D> = Box<dyn Iterator<Item = DResult<D, DQuad<'a, D>>> + 'a>;
/// Type alias for fallible hashsets of terms produced by a dataset.
///
/// See [`Dataset::quads`](./trait.Dataset.html#tymethod.quads)
/// for more information about how to use it.
pub type DResultTermSet<D> = DResult<D, HashSet<DTerm<D>>>;

/// Generic trait for RDF datasets.
///
/// For convenience, this trait is implemented
/// by [standard collections of quads](#foreign-impls).
///
/// NB: the semantics of this trait allows a dataset to contain duplicate quads;
/// see also [`SetDataset`](trait.SetDataset.html).
pub trait Dataset {
    /// Determine the type of [`Quad`](../quad/trait.Quad.html)s
    /// that the methods of this dataset will yield
    /// (see [`streaming_mode`](../quad/streaming_mode/index.html)
    type Quad: QuadStreamingMode;
    /// The error type that this dataset may raise.
    type Error: 'static + Error;

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
    /// # use sophia_api::dataset::Dataset;
    /// # use sophia_api::term::simple_iri::SimpleIri;
    /// # fn foo() -> Result<(), std::convert::Infallible> {
    /// # let dataset = Vec::<[SimpleIri;4]>::new();
    /// for q in dataset.quads() {
    ///     let q = q?; // rethrow error if any
    ///     // do something with q
    /// }
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Another way is to use the specific methods provided by
    /// [`QuadSource`](../quad/stream/trait.QuadSource.html),
    /// for example:
    /// ```
    /// # use sophia_api::dataset::Dataset;
    /// # use sophia_api::quad::stream::QuadSource;
    /// # use sophia_api::term::simple_iri::SimpleIri;
    /// # fn foo() -> Result<(), std::convert::Infallible> {
    /// # let dataset = Vec::<[SimpleIri;4]>::new();
    /// dataset.quads().for_each_quad(|q| {
    ///     // do something with q
    /// })?; // rethrow error if any
    /// # Ok(())
    /// # }
    /// ```
    fn quads(&self) -> DQuadSource<Self>;

    /// An iterator visiting all quads with the given subject.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_s<'s, TS>(&'s self, s: &'s TS) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
    {
        Box::new(self.quads().filter_ok(move |q| term_eq(q.s(), s)))
    }
    /// An iterator visiting all quads with the given predicate.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_p<'s, TP>(&'s self, p: &'s TP) -> DQuadSource<'s, Self>
    where
        TP: TTerm + ?Sized,
    {
        Box::new(self.quads().filter_ok(move |q| term_eq(q.p(), p)))
    }
    /// An iterator visiting add quads with the given object.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_o<'s, TS>(&'s self, o: &'s TS) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
    {
        Box::new(self.quads().filter_ok(move |q| term_eq(q.o(), o)))
    }
    /// An iterator visiting add quads with the given graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_g<'s, TS>(&'s self, g: Option<&'s TS>) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
    {
        Box::new(self.quads().filter_ok(move |q| same_graph_name(q.g(), g)))
    }
    /// An iterator visiting add quads with the given subject and predicate.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_sp<'s, TS, TP>(&'s self, s: &'s TS, p: &'s TP) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
    {
        Box::new(self.quads_with_s(s).filter_ok(move |q| term_eq(q.p(), p)))
    }
    /// An iterator visiting add quads with the given subject and object.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_so<'s, TS, TO>(&'s self, s: &'s TS, o: &'s TO) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        Box::new(self.quads_with_s(s).filter_ok(move |q| term_eq(q.o(), o)))
    }
    /// An iterator visiting add quads with the given subject and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_sg<'s, TS, TG>(&'s self, s: &'s TS, g: Option<&'s TG>) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        Box::new(self.quads_with_g(g).filter_ok(move |q| term_eq(q.s(), s)))
    }
    /// An iterator visiting add quads with the given predicate and object.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_po<'s, TP, TO>(&'s self, p: &'s TP, o: &'s TO) -> DQuadSource<'s, Self>
    where
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        Box::new(self.quads_with_p(p).filter_ok(move |q| term_eq(q.o(), o)))
    }
    /// An iterator visiting add quads with the given predicate and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_pg<'s, TP, TG>(&'s self, p: &'s TP, g: Option<&'s TG>) -> DQuadSource<'s, Self>
    where
        TP: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        Box::new(self.quads_with_g(g).filter_ok(move |q| term_eq(q.p(), p)))
    }
    /// An iterator visiting add quads with the given object and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_og<'s, TO, TG>(&'s self, o: &'s TO, g: Option<&'s TG>) -> DQuadSource<'s, Self>
    where
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        Box::new(self.quads_with_g(g).filter_ok(move |q| term_eq(q.o(), o)))
    }
    /// An iterator visiting add quads with the given subject, predicate and object.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_spo<'s, TS, TP, TO>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        o: &'s TO,
    ) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        Box::new(
            self.quads_with_sp(s, p)
                .filter_ok(move |q| term_eq(q.o(), o)),
        )
    }
    /// An iterator visiting add quads with the given subject, predicate and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_spg<'s, TS, TP, TG>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        Box::new(
            self.quads_with_sg(s, g)
                .filter_ok(move |q| term_eq(q.p(), p)),
        )
    }
    /// An iterator visiting add quads with the given subject, object and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_sog<'s, TS, TO, TG>(
        &'s self,
        s: &'s TS,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        Box::new(
            self.quads_with_sg(s, g)
                .filter_ok(move |q| term_eq(q.o(), o)),
        )
    }
    /// An iterator visiting add quads with the given predicate, object and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_pog<'s, TP, TO, TG>(
        &'s self,
        p: &'s TP,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self>
    where
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        Box::new(
            self.quads_with_pg(p, g)
                .filter_ok(move |q| term_eq(q.o(), o)),
        )
    }
    /// An iterator visiting add quads with the given subject, predicate, object and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_spog<'s, TS, TP, TO, TG>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DQuadSource<'s, Self>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        Box::new(
            self.quads_with_spg(s, p, g)
                .filter_ok(move |q| term_eq(q.o(), o)),
        )
    }

    /// Return `true` if this dataset contains the given quad.
    fn contains<'s, TS, TP, TO, TG>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        o: &'s TO,
        g: Option<&'s TG>,
    ) -> DResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized,
    {
        match self.quads_with_spog(s, p, o, g).next() {
            None => Ok(false),
            Some(Ok(_)) => Ok(true),
            Some(Err(err)) => Err(err),
        }
    }

    /// An iterator visiting add quads matching the given subject, predicate, object and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_matching<'s, S, P, O, G>(
        &'s self,
        ms: &'s S,
        mp: &'s P,
        mo: &'s O,
        mg: &'s G,
    ) -> DQuadSource<'s, Self>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
        G: GraphNameMatcher + ?Sized,
    {
        match (ms.constant(), mp.constant(), mo.constant(), mg.constant()) {
            (None, None, None, None) => Box::from(self.quads().filter_ok(move |q| {
                ms.matches(q.s()) && mp.matches(q.p()) && mo.matches(q.o()) && mg.matches(q.g())
            })),
            (Some(s), None, None, None) => {
                Box::from(self.quads_with_s(s).filter_ok(move |q| {
                    mp.matches(q.p()) && mo.matches(q.o()) && mg.matches(q.g())
                }))
            }
            (None, Some(p), None, None) => {
                Box::from(self.quads_with_p(p).filter_ok(move |q| {
                    ms.matches(q.s()) && mo.matches(q.o()) && mg.matches(q.g())
                }))
            }
            (None, None, Some(o), None) => {
                Box::from(self.quads_with_o(o).filter_ok(move |q| {
                    ms.matches(q.s()) && mp.matches(q.p()) && mg.matches(q.g())
                }))
            }
            (None, None, None, Some(g)) => {
                Box::from(self.quads_with_g(g).filter_ok(move |q| {
                    ms.matches(q.s()) && mp.matches(q.p()) && mo.matches(q.o())
                }))
            }
            (Some(s), Some(p), None, None) => Box::from(
                self.quads_with_sp(s, p)
                    .filter_ok(move |q| mo.matches(q.o()) && mg.matches(q.g())),
            ),
            (Some(s), None, Some(o), None) => Box::from(
                self.quads_with_so(s, o)
                    .filter_ok(move |q| mp.matches(q.p()) && mg.matches(q.g())),
            ),
            (Some(s), None, None, Some(g)) => Box::from(
                self.quads_with_sg(s, g)
                    .filter_ok(move |q| mp.matches(q.p()) && mo.matches(q.o())),
            ),
            (None, Some(p), Some(o), None) => Box::from(
                self.quads_with_po(p, o)
                    .filter_ok(move |q| ms.matches(q.s()) && mg.matches(q.g())),
            ),
            (None, Some(p), None, Some(g)) => Box::from(
                self.quads_with_pg(p, g)
                    .filter_ok(move |q| ms.matches(q.s()) && mo.matches(q.o())),
            ),
            (None, None, Some(o), Some(g)) => Box::from(
                self.quads_with_og(o, g)
                    .filter_ok(move |q| ms.matches(q.s()) && mp.matches(q.p())),
            ),
            (Some(s), Some(p), Some(o), None) => Box::from(
                self.quads_with_spo(s, p, o)
                    .filter_ok(move |q| mg.matches(q.g())),
            ),
            (Some(s), Some(p), None, Some(g)) => Box::from(
                self.quads_with_spg(s, p, g)
                    .filter_ok(move |q| mo.matches(q.o())),
            ),
            (Some(s), None, Some(o), Some(g)) => Box::from(
                self.quads_with_sog(s, o, g)
                    .filter_ok(move |q| mp.matches(q.p())),
            ),
            (None, Some(p), Some(o), Some(g)) => Box::from(
                self.quads_with_pog(p, o, g)
                    .filter_ok(move |q| ms.matches(q.s())),
            ),
            (Some(s), Some(p), Some(o), Some(g)) => self.quads_with_spog(s, p, o, g),
        }
    }

    /// Build a Hashset of all the terms used as subject in this Dataset.
    fn subjects(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            insert_if_absent(&mut res, q?.s());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as predicate in this Dataset.
    fn predicates(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            insert_if_absent(&mut res, q?.p());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as object in this Dataset.
    fn objects(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            insert_if_absent(&mut res, q?.o());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as graph names in this Dataset.
    fn graph_names(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            let q = q?;
            let name = q.g();
            if let Some(name) = name {
                insert_if_absent(&mut res, name);
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the IRIs used in this Dataset.
    fn iris(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            for i in q?.components() {
                if matches!(i.kind(), TermKind::Iri) {
                    insert_if_absent(&mut res, i)
                }
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the BNodes used in this Dataset.
    fn bnodes(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            for i in q?.components() {
                if matches!(i.kind(), TermKind::BlankNode) {
                    insert_if_absent(&mut res, i)
                }
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the Literals used in this Dataset.
    fn literals(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            for i in q?.components() {
                if matches!(i.kind(), TermKind::Literal) {
                    insert_if_absent(&mut res, i)
                }
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the variables used in this Dataset.
    fn variables(&self) -> DResultTermSet<Self>
    where
        DTerm<Self>: Clone + Eq + Hash,
    {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            for i in q?.components() {
                if matches!(i.kind(), TermKind::Variable) {
                    insert_if_absent(&mut res, i)
                }
            }
        }
        Ok(res)
    }

    /// Borrows one of the graphs of this dataset
    fn graph<'s, T>(
        &'s self,
        graph_name: Option<&'s T>,
    ) -> DatasetGraph<Self, &'s Self, Option<&'s T>>
    where
        T: TTerm + ?Sized,
    {
        DatasetGraph::new(self, graph_name)
    }

    /// Borrows mutably one of the graphs of this dataset
    fn graph_mut<'s, T>(
        &'s mut self,
        graph_name: Option<&'s T>,
    ) -> DatasetGraph<Self, &'s mut Self, Option<&'s T>>
    where
        T: TTerm + ?Sized,
    {
        DatasetGraph::new(self, graph_name)
    }

    fn union_graph<'s, T>(&'s self, gmatcher: T) -> DatasetGraph<Self, &'s Self, T>
    where
        T: GraphNameMatcher + 's,
    {
        DatasetGraph::new(self, gmatcher)
    }
}

/// A dataset that can be constructed from a
/// [`QuadSource`](../quad/stream/trait.QuadSource.html)
pub trait CollectibleDataset: Dataset + Sized {
    fn from_quad_source<QS: QuadSource>(quad: QS) -> StreamResult<Self, QS::Error, Self::Error>;
}

/// Type alias for results produced by a mutable dataset.
pub type MDResult<D, T> = std::result::Result<T, <D as MutableDataset>::MutationError>;

/// Generic trait for mutable RDF datasets.
///
/// NB: the semantics of this trait allows a dataset to contain duplicate quads;
/// see also [`SetDataset`](trait.SetDataset.html).
///
pub trait MutableDataset: Dataset {
    /// The error type that this dataset may raise during mutations.
    type MutationError: 'static + Error;

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
    /// [`SetDataset`]: trait.SetDataset.html
    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> MDResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized;

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
    ///
    /// [`SetDataset`]: trait.SetDataset.html
    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> MDResult<Self, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized;

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
    ///
    /// [`SetDataset`]: trait.SetDataset.html
    #[inline]
    fn insert_all<QS>(
        &mut self,
        src: QS,
    ) -> StreamResult<usize, QS::Error, <Self as MutableDataset>::MutationError>
    where
        QS: QuadSource,
    {
        let mut src = src;
        let mut c = 0;
        src.try_for_each_quad(|q| -> MDResult<Self, ()> {
            if self.insert(q.s(), q.p(), q.o(), q.g())? {
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
    ///
    /// [`SetDataset`]: trait.SetDataset.html
    #[inline]
    fn remove_all<QS>(
        &mut self,
        src: QS,
    ) -> StreamResult<usize, QS::Error, <Self as MutableDataset>::MutationError>
    where
        QS: QuadSource,
    {
        let mut src = src;
        let mut c = 0;
        src.try_for_each_quad(|q| -> MDResult<Self, ()> {
            if self.remove(q.s(), q.p(), q.o(), q.g())? {
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
    ///
    /// [`SetDataset`]: trait.SetDataset.html
    fn remove_matching<S, P, O, G>(
        &mut self,
        ms: &S,
        mp: &P,
        mo: &O,
        mg: &G,
    ) -> MDResult<Self, usize>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
        G: GraphNameMatcher + ?Sized,
        DTerm<Self>: Clone,
        <Self as Dataset>::Error: Into<Self::MutationError>,
    {
        let to_remove = self
            .quads_matching(ms, mp, mo, mg)
            .map_ok(|q| {
                (
                    [q.s().clone(), q.p().clone(), q.o().clone()],
                    q.g().map(Clone::clone),
                )
            })
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(Into::into)?;
        let mut to_remove = to_remove.into_iter().as_quad_source();
        Ok(self
            .remove_all(&mut to_remove)
            .map_err(|err| err.unwrap_sink_error())?)
    }

    /// Keep only the quads matching the given matchers.
    ///
    /// # Note to implementors
    /// The default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    fn retain_matching<S, P, O, G>(&mut self, ms: &S, mp: &P, mo: &O, mg: &G) -> MDResult<Self, ()>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
        G: GraphNameMatcher + ?Sized,
        DTerm<Self>: Clone,
        <Self as Dataset>::Error: Into<Self::MutationError>,
    {
        let to_remove = self
            .quads()
            .filter_ok(|q| {
                !(ms.matches(q.s()) && mp.matches(q.p()) && mo.matches(q.o()) && mg.matches(q.g()))
            })
            .map_ok(|q| {
                (
                    [q.s().clone(), q.p().clone(), q.o().clone()],
                    q.g().map(Clone::clone),
                )
            })
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(Into::into)?;
        let mut to_remove = to_remove.into_iter().as_quad_source();
        self.remove_all(&mut to_remove)
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
///
/// [`Dataset`]: trait.Dataset.html
/// [`MutableDataset`]: trait.MutableDataset.html

pub trait SetDataset: Dataset {}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the macro test_dataset_impl!).
}
