//! I define adapters for the [`graph`](super) related traits.
use super::*;
use crate::dataset::{DTerm, Dataset, MutableDataset};
use crate::quad::Quad;
use crate::term::{
    matcher::{Any, GraphNameMatcher},
    GraphName,
};

/// I wrap a [`Dataset`] as a [`Graph`]
/// corresponding to the union of all graphs (default and named)
/// from the original dataset.
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct UnionGraph<T: Dataset>(T);

impl<T: Dataset> UnionGraph<T> {
    /// Wrap the given dataset as a the union of all its graphs.
    pub fn new(wrapped: T) -> Self {
        UnionGraph(wrapped)
    }

    /// Unwrap the inner [`Dataset`].
    pub fn unwrap(self) -> T {
        self.0
    }
}

impl<T: Dataset> Graph for UnionGraph<T> {
    type Triple<'x> = [DTerm<'x, T>; 3] where Self: 'x;
    type Error = T::Error;

    fn triples(&self) -> GTripleSource<Self> {
        Box::new(
            self.0
                .quads()
                // NB: for some reason, .map_ok(...) below does not compile since 1.66 nightly
                .map(|r| r.map(Quad::into_triple)),
        )
    }

    fn triples_matching<'s, S, P, O>(&'s self, sm: S, pm: P, om: O) -> GTripleSource<'s, Self>
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
        O: TermMatcher + 's,
    {
        Box::new(
            self.0
                .quads_matching(sm, pm, om, Any)
                // NB: for some reason, .map_ok(...) below does not compile since 1.66 nightly
                .map(|r| r.map(Quad::into_triple)),
        )
    }

    fn subjects(&self) -> GTermSource<Self> {
        self.0.subjects()
    }

    fn predicates(&self) -> GTermSource<Self> {
        self.0.predicates()
    }

    fn objects(&self) -> GTermSource<Self> {
        self.0.objects()
    }

    fn iris(&self) -> GTermSource<Self> {
        self.0.iris()
    }

    fn blank_nodes(&self) -> GTermSource<Self> {
        self.0.blank_nodes()
    }

    fn literals(&self) -> GTermSource<Self> {
        self.0.literals()
    }

    fn quoted_triples<'s>(&'s self) -> GTermSource<'s, Self>
    where
        GTerm<'s, Self>: Clone,
    {
        self.0.quoted_triples()
    }

    fn variables(&self) -> GTermSource<Self> {
        self.0.variables()
    }
}

//

/// I wrap a [`Dataset`] as a [`Graph`]
/// corresponding to the union of a subset of its graphs,
/// determined by a [`GraphNameMatcher`].
#[derive(Clone, Copy, Debug)]
pub struct PartialUnionGraph<D: Dataset, M: GraphNameMatcher> {
    d: D,
    m: M,
}

impl<D: Dataset, M: GraphNameMatcher + Copy> PartialUnionGraph<D, M> {
    /// Wrap the given dataset as a single [`Graph`]
    /// (selected via the given graph name).
    pub fn new(d: D, m: M) -> Self {
        PartialUnionGraph { d, m }
    }

    /// Unwrap the inner [`GraphName`] and [`Dataset`].
    pub fn unwrap(self) -> (D, M) {
        (self.d, self.m)
    }
}

impl<D: Dataset, M: GraphNameMatcher + Copy> Graph for PartialUnionGraph<D, M> {
    type Triple<'x> = [DTerm<'x, D>; 3] where Self: 'x;
    type Error = D::Error;

    fn triples(&self) -> GTripleSource<Self> {
        Box::new(
            self.d
                .quads_matching(Any, Any, Any, self.m)
                // NB: for some reason, .map_ok(...) below does not compile since 1.66 nightly
                .map(|r| r.map(Quad::into_triple)),
        )
    }

    fn triples_matching<'s, S, P, O>(&'s self, sm: S, pm: P, om: O) -> GTripleSource<'s, Self>
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
        O: TermMatcher + 's,
    {
        Box::new(
            self.d
                .quads_matching(sm, pm, om, self.m)
                // NB: for some reason, .map_ok(...) below does not compile since 1.66 nightly
                .map(|r| r.map(Quad::into_triple)),
        )
    }
}

//

/// I wrap a [`Dataset`] as a [`Graph`]
/// corresponding to a specific graph (default or named) of the wrapped dataset.
///
/// This graph is also [mutable](`MutableGraph`) if the underlying dataset is.
#[derive(Clone, Copy, Debug)]
pub struct DatasetGraph<D: Dataset, G: Term> {
    d: D,
    g: GraphName<G>,
}

impl<D: Dataset, G: Term> DatasetGraph<D, G> {
    /// Wrap the given dataset as a single [`Graph`]
    /// (selected via the given graph name).
    pub fn new(d: D, g: GraphName<G>) -> Self {
        DatasetGraph { d, g }
    }

    /// Unwrap the inner [`GraphName`] and [`Dataset`].
    pub fn unwrap(self) -> (D, GraphName<G>) {
        (self.d, self.g)
    }

    fn g(&self) -> GraphName<G::BorrowTerm<'_>> {
        self.g.as_ref().map(|gn| gn.borrow_term())
    }

    fn gd(&mut self) -> (Option<G::BorrowTerm<'_>>, &mut D) {
        (self.g.as_ref().map(|gn| gn.borrow_term()), &mut self.d)
    }
}

impl<D: Dataset, G: Term> Graph for DatasetGraph<D, G> {
    type Triple<'x> = [DTerm<'x, D>; 3] where Self: 'x;
    type Error = D::Error;

    fn triples(&self) -> GTripleSource<Self> {
        Box::new(
            self.d
                .quads_matching(Any, Any, Any, [self.g()])
                // NB: for some reason, .map_ok(...) below does not compile since 1.66 nightly
                .map(|r| r.map(Quad::into_triple)),
        )
    }

    fn triples_matching<'s, S, P, O>(&'s self, sm: S, pm: P, om: O) -> GTripleSource<'s, Self>
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
        O: TermMatcher + 's,
    {
        Box::new(
            self.d
                .quads_matching(sm, pm, om, [self.g()])
                // NB: for some reason, .map_ok(...) below does not compile since 1.66 nightly
                .map(|r| r.map(Quad::into_triple)),
        )
    }
}

impl<D: MutableDataset, G: Term> MutableGraph for DatasetGraph<D, G> {
    type MutationError = D::MutationError;

    fn insert<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        let (g, d) = self.gd();
        d.insert(s, p, o, g)
    }

    fn remove<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        let (g, d) = self.gd();
        d.remove(s, p, o, g)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::quad::Spog;
    use crate::term::{graph_name_eq, CmpTerm, FromTerm};
    use sophia_iri::Iri;
    use std::collections::BTreeSet;

    static G1: Iri<&'static str> = Iri::new_unchecked_const("http://example.com/g1");
    static G2: Iri<&'static str> = Iri::new_unchecked_const("http://example.com/g2");
    static G3: Iri<&'static str> = Iri::new_unchecked_const("http://example.com/g3");

    type MyTerm = CmpTerm<SimpleTerm<'static>>;
    type MyQuad = Spog<MyTerm>;
    type MyDS = BTreeSet<MyQuad>;

    #[derive(Clone, Copy, Debug)]
    struct GM;
    impl GraphNameMatcher for GM {
        type Term = MyTerm;

        fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
            graph_name_eq(graph_name.map(|gn| gn.borrow_term()), Some(G1))
                || graph_name_eq(graph_name.map(|gn| gn.borrow_term()), Some(G2))
        }
    }

    type MyUG = UnionGraph<MyDS>;
    fn collect_union_graph<T: TripleSource>(mut ts: T) -> Result<MyUG, T::Error> {
        let mut ds = MyDS::new();
        ts.for_each_triple(|t| {
            let [s, p, o] = t.spo();
            ds.insert_quad(([s, p, o], Some(s))).unwrap();
        })?;
        Ok(ds.into_union_graph())
    }
    crate::test_immutable_graph_impl!(union_graph, MyUG, true, true, collect_union_graph);

    type MyPUG = PartialUnionGraph<MyDS, GM>;
    fn collect_partial_union_graph<T: TripleSource>(mut ts: T) -> Result<MyPUG, T::Error> {
        let g1: GraphName<MyTerm> = Some(G1.into_term());
        let g2: GraphName<MyTerm> = Some(G2.into_term());
        let g3: GraphName<MyTerm> = Some(G3.into_term());
        let mut ds = MyDS::new();
        let mut b = true;
        ts.for_each_triple(|t| {
            let [s, p, o] = t.spo();
            if b {
                ds.insert_quad(([s, p, o].map(MyTerm::from_term), g1.clone()))
                    .unwrap();
                ds.insert_quad(([o, p, s].map(MyTerm::from_term), g3.clone()))
                    .unwrap();
            } else {
                ds.insert_quad(([s, p, o].map(MyTerm::from_term), g2.clone()))
                    .unwrap();
            }
            b = !b;
        })?;
        Ok(PartialUnionGraph::new(ds, GM))
    }
    crate::test_immutable_graph_impl!(
        partial_union_graph,
        MyPUG,
        true,
        true,
        collect_partial_union_graph
    );

    type MyDG = DatasetGraph<MyDS, MyTerm>;
    fn collect_dataset_graph<T: TripleSource>(mut ts: T) -> Result<MyDG, T::Error> {
        let g1: GraphName<MyTerm> = Some(G1.into_term());
        let g2: GraphName<MyTerm> = Some(G2.into_term());
        let mut ds = MyDS::new();
        ts.for_each_triple(|t| {
            let [s, p, o] = t.spo();
            ds.insert_quad(([s, p, o].map(MyTerm::from_term), g1.clone()))
                .unwrap();
            ds.insert_quad(([o, p, s].map(MyTerm::from_term), g2.clone()))
                .unwrap();
        })?;
        Ok(DatasetGraph::new(ds, g1))
    }
    crate::test_graph_impl!(dataset_graph, MyDG, true, true, collect_dataset_graph);

    #[allow(dead_code)] // just check this compiles
    fn check_trait_impls() {
        let mut ds = MyDS::new();
        let g1: Option<MyTerm> = Some(G1.into_term());

        // check that Dataset::graph returns a Graph
        for _ in ds.graph(g1.clone()).triples() {}

        let mut gm = ds.graph_mut(g1);
        // check that Dataset::graph_mut returns a Graph
        for _ in gm.triples() {}
        // check that Dataset::graph_mut returns a MutableGraph
        gm.remove_triple([1, 2, 3]).unwrap();

        // check that Dataset::partial_union_graph returns a Graph
        for _ in ds.partial_union_graph(GM).triples() {}

        // check that Dataset::union_graph returns a Graph
        for _ in ds.union_graph().triples() {}

        // check that Dataset::into_union_graph returns a Graph
        for _ in ds.into_union_graph().triples() {}
    }
}
