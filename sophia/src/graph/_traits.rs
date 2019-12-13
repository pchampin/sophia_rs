// this module is transparently re-exported by its parent `graph`

use std::collections::HashSet;
use std::hash::Hash;
use std::marker::PhantomData;
use resiter::filter::*;
use resiter::map::*;
use anyhow;
use crate::error::*;
use crate::graph::adapter::GraphAsDataset;
use crate::term::matcher::TermMatcher;
use crate::term::*;
use crate::triple::stream::*;
use crate::triple::*;

/// Type alias for the terms returned by a graph.
pub type GTerm<'a, G> = Term<<<G as Graph<'a>>::Triple as Triple<'a>>::TermData>;
/// Type alias for results produced by a graph.
pub type GResult<'a, G, T> = Result<T, <G as Graph<'a>>::Error>;
/// Type alias for fallible triple iterators produced by a graph.
pub type GTripleSource<'a, G> = Box<dyn Iterator<Item = GResult<'a, G, <G as Graph<'a>>::Triple>> + 'a>;
/// Type alias for fallible hashets of terms produced by a graph.
pub type GResultTermSet<'a, G> = GResult<'a, G, HashSet<GTerm<'a, G>>>;

/// Generic trait for RDF graphs.
///
/// For convenience, this trait is implemented
/// by [standard collections of triples](#foreign-impls).
///
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`](trait.SetGraph.html).
///
/// # How to use `Graph` in a trait bound?
///
/// TL;DR: If the compiler complains about lifetimes,
/// replace `G: Graph<'a>` with `G: for<'x> Graph<'x>`.
///
/// The lifetime parameter of `Graph` has a very specific semantics:
/// it is the lifetime for which the graph can be borrowed when iterating over its triples.
/// Since lifetime parameters in traits are
/// [invariant](https://doc.rust-lang.org/nightly/nomicon/subtyping.html),
/// an instance of `Graph<'a>` will only be usable with the *exact* lifetime `'a`,
/// but not with shorter lifetimes, which is counter-intuitive.
///
/// In many situations, when you write a function accepting a graph,
/// you can not define the appropriate lifetime in the function's signature.
/// For example, you may need to borrow the graph for the lifetime of a *local* variable of the function:
/// ```compile_fail
/// use sophia::error::Never;
/// use sophia::graph::Graph;
/// use sophia::term::*;
///
/// fn count_str_mentions<'a, G>(g: &G, txt: &str) -> usize where
///   G: Graph<'a, Error=Never>
/// {
///   let literal = RefTerm::from(txt);
///   g.triples_with_o(&literal).count()
///   // fails to compile because `literal` does not live as long as 'a .
/// }
/// ```
/// NB: the compilers's error messages are not very helpful here;
/// they suggest to add lifetime `'a` to the function parameters,
/// but that does not solve the problem.
///
/// In this kind of situation, the solution consists in using a
/// [Higher-Rank Trait Bound](https://doc.rust-lang.org/nomicon/hrtb.html)
/// for `G`:
/// ```
/// use std::convert::Infallible;
/// use sophia::graph::Graph;
/// use sophia::term::*;
///
/// fn count_str_mentions<G>(g: &G, txt: &str) -> usize where
///   G: for<'x> Graph<'x, Error=Infallible>  // <-- higher-rank trait bound
/// {
///   let literal = RefTerm::from(txt);
///   g.triples_with_o(&literal).count()
/// }
/// ```
/// NB:
/// The Higher-Ranked Type Bound above states
/// "`G` implements `Graph<'x>` for *every* lifetime `'x`",
/// which allows us to *not* define the lifetime in the function's signature.
/// This has its own drawback, though:
/// only implementations of `Graph` who *own* their data will match this bound
/// (fortunately, it is the case of most implementations).

pub trait Graph<'a> {
    /// The type of [`Triple`](../triple/trait.Triple.html)s
    /// that the methods of this graph will yield.
    type Triple: Triple<'a>;
    /// The error type that this graph may raise.
    type Error: SafeError;

    /// An iterator visiting all triples of this graph in arbitrary order.
    ///
    /// This iterator is fallible:
    /// its items are `Result`s,
    /// an error may occur at any time during the iteration.
    fn triples(&'a self) -> GTripleSource<'a, Self>;

    /// An iterator visiting all triples with the given subject.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_s<T>(&'a self, s: &'a Term<T>) -> GTripleSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.triples().filter_ok(move |t| t.s() == s))
    }
    /// An iterator visiting all triples with the given predicate.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_p<T>(&'a self, p: &'a Term<T>) -> GTripleSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.triples().filter_ok(move |t| t.p() == p))
    }
    /// An iterator visiting all triples with the given object.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_o<T>(&'a self, o: &'a Term<T>) -> GTripleSource<'a, Self>
    where
        T: TermData,
    {
        Box::new(self.triples().filter_ok(move |t| t.o() == o))
    }
    /// An iterator visiting all triples with the given subject and predicate.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_sp<T, U>(&'a self, s: &'a Term<T>, p: &'a Term<U>) -> GTripleSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.triples_with_s(s).filter_ok(move |t| t.p() == p))
    }
    /// An iterator visiting all triples with the given subject and object.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_so<T, U>(&'a self, s: &'a Term<T>, o: &'a Term<U>) -> GTripleSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.triples_with_s(s).filter_ok(move |t| t.o() == o))
    }
    /// An iterator visiting all triples with the given predicate and object.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_po<T, U>(&'a self, p: &'a Term<T>, o: &'a Term<U>) -> GTripleSource<'a, Self>
    where
        T: TermData,
        U: TermData,
    {
        Box::new(self.triples_with_p(p).filter_ok(move |t| t.o() == o))
    }
    /// An iterator visiting all triples with the given subject, predicate and object.
    ///
    /// See also [`triples`](#tymethod.triples).
    fn triples_with_spo<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
    ) -> GTripleSource<'a, Self>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        Box::new(self.triples_with_sp(s, p).filter_ok(move |t| t.o() == o))
    }

    /// Return `true` if this graph contains the given triple.
    fn contains<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
    ) -> GResult<'a, Self, bool>
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
    fn triples_matching<S, P, O>(
        &'a self,
        ms: &'a S,
        mp: &'a P,
        mo: &'a O,
    ) -> GTripleSource<'a, Self>
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
    fn subjects(&'a self) -> GResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            insert_if_absent(&mut res, t?.s());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as predicate in this Graph.
    fn predicates(&'a self) -> GResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            insert_if_absent(&mut res, t?.p());
        }
        Ok(res)
    }

    /// Build a Hashset of all the terms used as object in this Graph.
    fn objects(&'a self) -> GResultTermSet<'a, Self> {
        let mut res = std::collections::HashSet::new();
        for t in self.triples() {
            insert_if_absent(&mut res, t?.o());
        }
        Ok(res)
    }

    /// Build a Hashset of all the IRIs used in this Graph.
    fn iris(&'a self) -> GResultTermSet<'a, Self> {
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
    fn bnodes(&'a self) -> GResultTermSet<'a, Self> {
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
    fn literals(&'a self) -> GResultTermSet<'a, Self> {
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
    fn variables(&'a self) -> GResultTermSet<'a, Self> {
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

/// Generic trait for mutable RDF graphs.
///
/// NB: the semantics of this trait allows a graph to contain duplicate triples;
/// see also [`SetGraph`](trait.SetGraph.html).
///
pub trait MutableGraph: for<'x> Graph<'x> {
    /// The error type that this graph may raise during mutations.
    type MutationError: SafeError;

    /// Insert the given triple in this graph.
    ///
    /// Return `true` iff the triple was actually inserted.
    ///
    /// NB: unless this graph also implements [`SetGraph`](trait.SetGraph.html),
    /// a return value of `true` does *not* mean that the triple was not already in the graph,
    /// only that the graph now has one more occurence of it.
    ///
    fn insert<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Result<bool, Self::MutationError>
    where
        T: TermData,
        U: TermData,
        V: TermData;

    /// Insert into this graph all triples from the given source.
    #[inline]
    fn insert_all<'a, TS>(
        &mut self,
        src: TS,
    ) -> Result<usize, anyhow::Error>
    where
        TS: TripleSource<'a>,
    {
        let mut src = src;
        let mut cnt = 0;
        for t in src.as_iter() {
            let t = t?;
            if self.insert(t.s(), t.p(), t.o())? { cnt += 1; }
        }

        Ok(cnt)
    }

    /// Insert the given triple in this graph.
    ///
    /// Return `true` iff the triple was actually removed.
    ///
    /// NB: unless this graph also implements [`SetGraph`](trait.SetGraph.html),
    /// a return value of `true` does *not* mean that the triple is not still contained in the graph,
    /// only that the graph now has one less occurence of it.
    ///
    fn remove<T, U, V>(&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Result<bool, Self::MutationError>
    where
        T: TermData,
        U: TermData,
        V: TermData;

    /// Remove from this graph all triples from the given source.
    #[inline]
    fn remove_all<'a, TS>(
        &mut self,
        src: TS,
    ) -> Result<usize, anyhow::Error>
    where
        TS: TripleSource<'a>,
    {
        let mut src = src;
        let mut cnt = 0;
        for t in src.as_iter() {
            let t = t?;
            if self.remove(t.s(), t.p(), t.o())? { cnt += 1; }
        }

        Ok(cnt)
    }

    /// Remove all triples matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    fn remove_matching<S, P, O>(&mut self, ms: &S, mp: &P, mo: &O) -> Result<usize, anyhow::Error>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
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
            .collect::<Result<Vec<_>, _>>()?;
        let to_remove = to_remove.into_iter().as_triple_source();
        Ok(self.remove_all(to_remove)?)
    }

    /// Keep only the triples matching the given matchers.
    ///
    /// Note that the default implementation is rather naive,
    /// and could be improved in specific implementations of the trait.
    ///
    fn retain<S, P, O>(&mut self, ms: &S, mp: &P, mo: &O) -> Result<(), anyhow::Error>
    where
        S: TermMatcher + ?Sized,
        P: TermMatcher + ?Sized,
        O: TermMatcher + ?Sized,
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
            .collect::<std::result::Result<Vec<_>, _>>()?;
        let to_remove = to_remove.into_iter().as_triple_source();
        self.remove_all(to_remove)?;
        Ok(())
    }
}

/// Extension trait for fallible iterators of triples, i.e. `TripleSource`s.
/// Porvides the same semantics as `Iterator::collect` but propagates errors
/// while iterating the source or populating the graph.
pub trait CollectToGraph {
    fn collect_to_graph<MG>(self) -> Result<MG, anyhow::Error> 
    where
        MG: MutableGraph + Default,
        MG::MutationError: SafeError;
}

impl<'a, I, T, E> CollectToGraph for I
where
    I: Iterator<Item = Result<T, E>> + 'a,
    T: Triple<'a>,
    E: SafeError,
{
    fn collect_to_graph<MG>(self) -> Result<MG, anyhow::Error> 
    where
        MG: MutableGraph + Default,
        MG::MutationError: SafeError,
    {
        let mut mg = MG::default();
        mg.insert_all(self)?;
        Ok(mg)
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
