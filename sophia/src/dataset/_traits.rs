// this module is transparently re-exported by its parent `dataset`

use std::collections::HashSet;
use std::marker::PhantomData;

use resiter::filter::*;
use resiter::map::*;

use crate::dataset::adapter::DatasetGraph;
use crate::error::*;
use crate::quad::stream::*;
use crate::quad::*;
use crate::term::matcher::*;
use crate::term::*;

use super::*;
use crate::graph::insert_if_absent;

/// Type alias for the terms returned by a dataset.
pub type DTerm<'a, D> = Term<<<D as Dataset<'a>>::Quad as Quad<'a>>::TermData>;
/// Type alias for results iterators produced by a dataset.
pub type DResult<'a, D, T> = std::result::Result<T, <D as Dataset<'a>>::Error>;
/// Type alias for fallible quad iterators produced by a dataset.
pub type DQuadSource<'a, D> =
    Box<dyn Iterator<Item = DResult<'a, D, <D as Dataset<'a>>::Quad>> + 'a>;
/// Type alias for fallible hashets of terms produced by a dataset.
pub type DResultTermSet<'a, D> = DResult<'a, D, HashSet<DTerm<'a, D>>>;

/// Generic trait for RDF datasets.
///
/// For convenience, this trait is implemented
/// by [standard collections of quads](#foreign-impls).
///
/// NB: the semantics of this trait allows a dataset to contain duplicate quads;
/// see also [`SetDataset`](trait.SetDataset.html).
///
/// # How to use `Dataset` in a trait bound?
///
/// The same rules as for [`Graph`](../graph/trait.Graph.html#how-to-use-graph-in-a-trait-bound)
/// apply.
///
pub trait Dataset<'a> {
    /// The type of [`Quad`](../quad/trait.Quad.html)s
    /// that the methods of this dataset will yield.
    type Quad: Quad<'a>;
    /// The error type that this dataset may raise.
    ///
    /// Must be either [`Never`](../error/enum.Never.html) (for infallible datasets)
    /// or [`Error`](../error/struct.Error.html).
    type Error: CoercibleWith<Error> + CoercibleWith<Never>;

    /// An iterator visiting all quads of this dataset in arbitrary order.
    ///
    /// This iterator is fallible:
    /// its items are `Result`s,
    /// an error may occur at any time during the iteration.
    fn quads(&'a self) -> DQuadSource<'a, Self>;

    /// An iterator visiting all quads with the given subject.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_s<T>(&'a self, s: &'a Term<T>) -> DQuadSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.quads().filter_ok(move |q| q.s() == s))
    }
    /// An iterator visiting all quads with the given predicate.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_p<T>(&'a self, p: &'a Term<T>) -> DQuadSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.quads().filter_ok(move |q| q.p() == p))
    }
    /// An iterator visiting add quads with the given object.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_o<T>(&'a self, o: &'a Term<T>) -> DQuadSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.quads().filter_ok(move |q| q.o() == o))
    }
    /// An iterator visiting add quads with the given graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_g<T>(&'a self, g: Option<&'a Term<T>>) -> DQuadSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.quads().filter_ok(move |q| same_graph_name(q.g(), g)))
    }
    /// An iterator visiting add quads with the given subject and predicate.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_sp<T, U>(&'a self, s: &'a Term<T>, p: &'a Term<U>) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.quads_with_s(s).filter_ok(move |q| q.p() == p))
    }
    /// An iterator visiting add quads with the given subject and object.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_so<T, U>(&'a self, s: &'a Term<T>, o: &'a Term<U>) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.quads_with_s(s).filter_ok(move |q| q.o() == o))
    }
    /// An iterator visiting add quads with the given subject and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_sg<T, U>(
        &'a self,
        s: &'a Term<T>,
        g: Option<&'a Term<U>>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.quads_with_g(g).filter_ok(move |q| q.s() == s))
    }
    /// An iterator visiting add quads with the given predicate and object.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_po<T, U>(&'a self, p: &'a Term<T>, o: &'a Term<U>) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.quads_with_p(p).filter_ok(move |q| q.o() == o))
    }
    /// An iterator visiting add quads with the given predicate and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_pg<T, U>(
        &'a self,
        p: &'a Term<T>,
        g: Option<&'a Term<U>>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.quads_with_g(g).filter_ok(move |q| q.p() == p))
    }
    /// An iterator visiting add quads with the given object and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_og<T, U>(
        &'a self,
        o: &'a Term<T>,
        g: Option<&'a Term<U>>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.quads_with_g(g).filter_ok(move |q| q.o() == o))
    }
    /// An iterator visiting add quads with the given subject, predicate and object.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_spo<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(self.quads_with_sp(s, p).filter_ok(move |q| q.o() == o))
    }
    /// An iterator visiting add quads with the given subject, predicate and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_spg<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        g: Option<&'a Term<V>>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(self.quads_with_sg(s, g).filter_ok(move |q| q.p() == p))
    }
    /// An iterator visiting add quads with the given subject, object and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_sog<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        o: &'a Term<U>,
        g: Option<&'a Term<V>>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(self.quads_with_sg(s, g).filter_ok(move |q| q.o() == o))
    }
    /// An iterator visiting add quads with the given predicate, object and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_pog<T, U, V>(
        &'a self,
        p: &'a Term<T>,
        o: &'a Term<U>,
        g: Option<&'a Term<V>>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(self.quads_with_pg(p, g).filter_ok(move |q| q.o() == o))
    }
    /// An iterator visiting add quads with the given subject, predicate, object and graph name.
    ///
    /// See also [`quads`](#tymethod.quads).
    fn quads_with_spog<T, U, V, W>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
        g: Option<&'a Term<W>>,
    ) -> DQuadSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        Box::new(self.quads_with_spg(s, p, g).filter_ok(move |q| q.o() == o))
    }

    /// Return `true` if this dataset contains the given quad.
    fn contains<T, U, V, W>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
        g: Option<&'a Term<W>>,
    ) -> DResult<'a, Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
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
    fn quads_matching<S, P, O, G>(
        &'a self,
        ms: &'a S,
        mp: &'a P,
        mo: &'a O,
        mg: &'a G,
    ) -> DQuadSource<'a, Self>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
        G: GraphNameMatcher + ?Sized,
    {
        match (
            &ms.constant(),
            &mp.constant(),
            &mo.constant(),
            &mg.constant(),
        ) {
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
                Box::from(self.quads_with_g(*g).filter_ok(move |q| {
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
                self.quads_with_sg(s, *g)
                    .filter_ok(move |q| mp.matches(q.p()) && mo.matches(q.o())),
            ),
            (None, Some(p), Some(o), None) => Box::from(
                self.quads_with_po(p, o)
                    .filter_ok(move |q| ms.matches(q.s()) && mg.matches(q.g())),
            ),
            (None, Some(p), None, Some(g)) => Box::from(
                self.quads_with_pg(p, *g)
                    .filter_ok(move |q| ms.matches(q.s()) && mo.matches(q.o())),
            ),
            (None, None, Some(o), Some(g)) => Box::from(
                self.quads_with_og(o, *g)
                    .filter_ok(move |q| ms.matches(q.s()) && mp.matches(q.p())),
            ),
            (Some(s), Some(p), Some(o), None) => Box::from(
                self.quads_with_spo(s, p, o)
                    .filter_ok(move |q| mg.matches(q.g())),
            ),
            (Some(s), Some(p), None, Some(g)) => Box::from(
                self.quads_with_spg(s, p, *g)
                    .filter_ok(move |q| mo.matches(q.o())),
            ),
            (Some(s), None, Some(o), Some(g)) => Box::from(
                self.quads_with_sog(s, o, *g)
                    .filter_ok(move |q| mp.matches(q.p())),
            ),
            (None, Some(p), Some(o), Some(g)) => Box::from(
                self.quads_with_pog(p, o, *g)
                    .filter_ok(move |q| ms.matches(q.s())),
            ),
            (Some(s), Some(p), Some(o), Some(g)) => self.quads_with_spog(s, p, o, *g),
        }
    }

    /// Build a Hashset of all the terms used as subject in this Dataset.
    fn subjects(&'a self) -> DResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            insert_if_absent(&mut res, q?.s());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as predicate in this Dataset.
    fn predicates(&'a self) -> DResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            insert_if_absent(&mut res, q?.p());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as object in this Dataset.
    fn objects(&'a self) -> DResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            insert_if_absent(&mut res, q?.o());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as graph names in this Dataset.
    fn graph_names(&'a self) -> DResultTermSet<'a, Self> {
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
    fn iris(&'a self) -> DResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            let q = q?;
            let (s, p, o) = (q.s(), q.p(), q.o());
            if let Iri(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let Iri(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let Iri(_) = o {
                insert_if_absent(&mut res, o)
            }
            if let Some(gn) = q.g() {
                if let Iri(_) = gn {
                    insert_if_absent(&mut res, &gn)
                }
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the BNodes used in this Dataset.
    fn bnodes(&'a self) -> DResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            let q = q?;
            let (s, p, o) = (q.s(), q.p(), q.o());
            if let BNode(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let BNode(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let BNode(_) = o {
                insert_if_absent(&mut res, o)
            }
            if let Some(gn) = q.g() {
                if let BNode(_) = gn {
                    insert_if_absent(&mut res, &gn)
                }
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the Literals used in this Dataset.
    fn literals(&'a self) -> DResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            let q = q?;
            let (s, p, o) = (q.s(), q.p(), q.o());
            if let Literal(_, _) = s {
                insert_if_absent(&mut res, s)
            }
            if let Literal(_, _) = p {
                insert_if_absent(&mut res, p)
            }
            if let Literal(_, _) = o {
                insert_if_absent(&mut res, o)
            }
            if let Some(gn) = q.g() {
                if let Literal(_, _) = gn {
                    insert_if_absent(&mut res, &gn)
                }
            }
        }
        Ok(res)
    }

    /// Build a Hashset of all the variables used in this Dataset.
    fn variables(&'a self) -> DResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for q in self.quads() {
            let q = q?;
            let (s, p, o) = (q.s(), q.p(), q.o());
            if let Variable(_) = s {
                insert_if_absent(&mut res, s)
            }
            if let Variable(_) = p {
                insert_if_absent(&mut res, p)
            }
            if let Variable(_) = o {
                insert_if_absent(&mut res, o)
            }
            if let Some(gn) = q.g() {
                if let Variable(_) = gn {
                    insert_if_absent(&mut res, &gn)
                }
            }
        }
        Ok(res)
    }

    /// Borrows one of the graphs of this dataset
    fn graph<T>(&self, graph_name: Option<&Term<T>>) -> DatasetGraph<Self, &Self, Option<BoxTerm>>
    where
        T: TermData,
    {
        DatasetGraph {
            dataset: self,
            gmatcher: graph_name.map(|n| n.into()),
            _phantom: PhantomData,
        }
    }

    /// Borrows mutably one of the graphs of this dataset
    fn graph_mut<T>(
        &mut self,
        graph_name: Option<&Term<T>>,
    ) -> DatasetGraph<Self, &mut Self, Option<BoxTerm>>
    where
        T: TermData,
    {
        DatasetGraph {
            dataset: self,
            gmatcher: graph_name.map(|n| n.into()),
            _phantom: PhantomData,
        }
    }

    /// Borrows a graph containing the union of all graphs matched by `gmatcher`
    fn union_graph<T>(&'a self, gmatcher: T) -> DatasetGraph<Self, &Self, T>
    where
        T: GraphNameMatcher,
    {
        DatasetGraph {
            dataset: self,
            gmatcher,
            _phantom: PhantomData,
        }
    }
}

/// Type alias for results produced by a mutable dataset.
pub type MDResult<D, T> = std::result::Result<T, <D as MutableDataset>::MutationError>;

/// Generic trait for mutable RDF datasets.
///
/// NB: the semantics of this trait allows a dataset to contain duplicate quads;
/// see also [`SetDataset`](trait.SetDataset.html).
///
pub trait MutableDataset: for<'x> Dataset<'x> {
    /// The error type that this dataset may raise during mutations.
    ///
    /// Must be either [`Never`](../error/enum.Never.html) (for infallible datasets)
    /// or [`Error`](../error/struct.Error.html).
    type MutationError: CoercibleWith<Error> + CoercibleWith<Never>;

    /// Insert the given quad in this dataset.
    ///
    /// Return `true` iff the quad was actually inserted.
    ///
    /// NB: unless this dataset also implements [`SetDataset`](trait.SetDataset.html),
    /// a return value of `true` does *not* mean that the quad was not already in the dataset,
    /// only that the dataset now has one more occurence of it.
    ///
    fn insert<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData;

    /// Remove the given quad in this dataset.
    ///
    /// Return `true` iff the quad was actually removed.
    ///
    /// NB: unless this dataset also implements [`SetDataset`](trait.SetDataset.html),
    /// a return value of `true` does *not* mean that the quad is not still contained in the dataset,
    /// only that the dataset now has one less occurence of it.
    ///
    fn remove<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> MDResult<Self, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData;

    /// Return a [`QuadSink`](../quad/stream/trait.QuadSink.html)
    /// that will insert into this dataset all the quads it receives.
    #[inline]
    fn inserter(&mut self) -> Inserter<Self> {
        Inserter::new(self)
    }

    /// Insert into this dataset all quads from the given source.
    #[inline]
    fn insert_all<TS>(
        &mut self,
        src: &mut TS,
    ) -> CoercedResult<usize, TS::Error, <Self as MutableDataset>::MutationError>
    where
        TS: QuadSource,
        TS::Error: CoercibleWith<<Self as MutableDataset>::MutationError>,
    {
        src.in_sink(&mut self.inserter())
    }

    /// Return a [`QuadSink`](../quad/stream/trait.QuadSink.html)
    /// that will remove from this dataset all the quads it receives.
    #[inline]
    fn remover(&mut self) -> Remover<Self> {
        Remover::new(self)
    }

    /// Remove from this dataset all quads from the given source.
    #[inline]
    fn remove_all<TS>(
        &mut self,
        src: &mut TS,
    ) -> CoercedResult<usize, TS::Error, <Self as MutableDataset>::MutationError>
    where
        TS: QuadSource,
        TS::Error: CoercibleWith<<Self as MutableDataset>::MutationError>,
    {
        src.in_sink(&mut self.remover())
    }

    /// Remove all quads matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
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
        // The following trait bound means that Self::Error must convert to Self::MutationError;
        // it is always satisfied when both of them are either Error or Never;
        // it is required to raise an error when building to_remove
        for<'a> <Self as Dataset<'a>>::Error: Into<Self::MutationError>,
        // The following trait bound is required by remove_all,
        // who coerces to_remove::Error (always Never) with self::MutationError.
        Never: CoercibleWith<Self::MutationError>,
        // The following trait is trivially verified in all cases,
        // (acatually T is always EQUAL to CoercedError<Never, T>)
        // but unfortunetaly the compiler can not see that.
        Self::MutationError: From<CoercedError<Never, Self::MutationError>>,
    {
        let to_remove: Vec<_> = self
            .quads_matching(ms, mp, mo, mg)
            .map_ok(|q| {
                (
                    [
                        BoxTerm::from(q.s()),
                        BoxTerm::from(q.p()),
                        BoxTerm::from(q.o()),
                    ],
                    q.g().map(BoxTerm::from),
                )
            })
            .collect::<std::result::Result<_, _>>()
            .map_err(Into::into)?;
        let mut to_remove = to_remove.into_iter().as_quad_source();
        Ok(self.remove_all(&mut to_remove)?)
    }

    /// Keep only the quads matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    ///
    fn retain<S, P, O, G>(&mut self, ms: &S, mp: &P, mo: &O, mg: &G) -> MDResult<Self, ()>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
        G: GraphNameMatcher + ?Sized,
        // The following trait bound means that Self::Error must convert to Self::MutationError;
        // it is always satisfied when both of them are either Error or Never;
        // it is required to raise an error when building to_remove
        for<'a> <Self as Dataset<'a>>::Error: Into<Self::MutationError>,
        // The following trait bound is required by remove_all,
        // who coerces to_remove::Error (always Never) with self::MutationError.
        Never: CoercibleWith<Self::MutationError>,
        // The following trait is trivially verified in all cases,
        // (acatually T is always EQUAL to CoercedError<Never, T>)
        // but unfortunetaly the compiler can not see that.
        Self::MutationError: From<CoercedError<Never, Self::MutationError>>,
    {
        let to_remove: Vec<_> = self
            .quads()
            .filter_ok(|q| {
                !(ms.matches(q.s()) && mp.matches(q.p()) && mo.matches(q.o()) && mg.matches(q.g()))
            })
            .map_ok(|q| {
                (
                    [
                        BoxTerm::from(q.s()),
                        BoxTerm::from(q.p()),
                        BoxTerm::from(q.o()),
                    ],
                    q.g().map(BoxTerm::from),
                )
            })
            .collect::<std::result::Result<_, _>>()
            .map_err(Into::into)?;
        let mut to_remove = to_remove.into_iter().as_quad_source();
        self.remove_all(&mut to_remove)?;
        Ok(())
    }
}

/// Marker trait constraining the semantics of
/// [`Dataset`](trait.Dataset.html) and [`MutableDataset`](trait.MutableDataset.html),
/// by guaranteeing that quads will never be returned / stored multiple times.
pub trait SetDataset {}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the macro test_dataset_impl!).
}
