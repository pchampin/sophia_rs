//! I define adapters for the [`dataset`](super) related traits.
use super::*;
use crate::graph::{GTerm, Graph, MutableGraph};
use crate::quad::Spog;
use crate::term::{GraphName, Term};
use crate::triple::Triple;

/// I wrap a [`Graph`] as a [`Dataset`] containing only that graph as the default graph.
#[derive(Clone, Copy, Debug)]
pub struct GraphAsDataset<T>(T);

impl<T> GraphAsDataset<T>
where
    T: Graph,
{
    /// Wrap the given graph with the given name.
    pub fn new(graph: T) -> Self {
        GraphAsDataset(graph)
    }

    /// Unwrap the inner graph and name.
    pub fn unwrap(self) -> T {
        self.0
    }
}

impl<T> Dataset for GraphAsDataset<T>
where
    T: Graph,
{
    type Quad<'x> = Spog<GTerm<'x, T>> where Self: 'x;
    type Error = T::Error;

    fn quads(&self) -> DQuadSource<Self> {
        Box::new(
            self.0
                .triples()
                // NB: for some reason, .map_ok(...) below does not compile since 1.66 nightly
                .map(|r| r.map(Triple::into_quad)),
        )
    }

    fn quads_matching<'s, S, P, O, G>(&'s self, sm: S, pm: P, om: O, gm: G) -> DQuadSource<'s, Self>
    where
        S: TermMatcher + 's,
        P: TermMatcher + 's,
        O: TermMatcher + 's,
        G: GraphNameMatcher + 's,
    {
        if gm.matches(None as GraphName<&GTerm<T>>) {
            Box::new(
                self.0
                    .triples_matching(sm, pm, om)
                    // NB: for some reason, .map_ok(...) below does not compile since 1.66 nightly
                    .map(|r| r.map(Triple::into_quad)),
            )
        } else {
            Box::new(std::iter::empty())
        }
    }

    fn contains<TS, TP, TO, TG>(&self, s: TS, p: TP, o: TO, g: GraphName<TG>) -> DResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        if g.is_none() {
            self.0.contains(s, p, o)
        } else {
            Ok(false)
        }
    }

    fn subjects(&self) -> DTermSource<Self> {
        self.0.subjects()
    }

    fn predicates(&self) -> DTermSource<Self> {
        self.0.predicates()
    }

    fn objects(&self) -> DTermSource<Self> {
        self.0.objects()
    }

    fn graph_names(&self) -> DTermSource<Self> {
        Box::new(std::iter::empty())
    }

    fn iris(&self) -> DTermSource<Self> {
        self.0.iris()
    }

    fn blank_nodes(&self) -> DTermSource<Self> {
        self.0.blank_nodes()
    }

    fn literals(&self) -> DTermSource<Self> {
        self.0.literals()
    }

    fn quoted_triples<'s>(&'s self) -> DTermSource<'s, Self>
    where
        GTerm<'s, T>: Clone,
    {
        self.0.quoted_triples()
    }

    fn variables(&self) -> DTermSource<Self> {
        self.0.variables()
    }
}

impl<T> MutableDataset for GraphAsDataset<T>
where
    T: MutableGraph,
{
    type MutationError = GraphAsDatasetMutationError<T::MutationError>;

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
        TG: Term,
    {
        if g.is_none() {
            self.0
                .insert(s, p, o)
                .map_err(GraphAsDatasetMutationError::Graph)
        } else {
            Err(GraphAsDatasetMutationError::OnlyDefaultGraph)
        }
    }

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
        TG: Term,
    {
        if g.is_none() {
            self.0
                .insert(s, p, o)
                .map_err(GraphAsDatasetMutationError::Graph)
        } else {
            Ok(false)
        }
    }
}

/// Error raised by mutating a [`GraphAsDataset`].
///
/// In addition to the errors raised by the underlying [graph][GraphAsDatasetMutationError::Graph],
/// [`GraphAsDataset`] may also raise error when quads are added [in a named graph][GraphAsDatasetMutationError::OnlyDefaultGraph].
#[derive(thiserror::Error, Debug)]
pub enum GraphAsDatasetMutationError<T: Error> {
    /// Error in the underlying graph.
    #[error("{0:?}")]
    Graph(T),
    /// Can not insert a quad in a named graph.
    #[error("This dataset only supports a default graph")]
    OnlyDefaultGraph,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::graph::adapter::DatasetGraph;
    use crate::source::{StreamError, TripleSource};
    use crate::term::CmpTerm;
    use std::collections::BTreeSet;

    type MyTerm = CmpTerm<SimpleTerm<'static>>;
    type MyGraph = BTreeSet<[MyTerm; 3]>;

    // NB: using test_dataset_impl! for testing GraphAsDataset is not convenient,
    // because GraphAsDataset is not a full-fledged dataset (it has only a default graph).
    //
    // In order to test it, we further wrap the dataset into a DatasetGraph,
    // and call test_graph_impl! on it.
    type MyGaDG = DatasetGraph<GraphAsDataset<MyGraph>, MyTerm>;
    fn collect_graph_as_dataset<T: TripleSource>(ts: T) -> Result<MyGaDG, T::Error> {
        ts.collect_triples()
            .map(|g: MyGraph| DatasetGraph::new(g.into_dataset(), None))
            .map_err(StreamError::unwrap_source_error)
    }
    crate::test_immutable_graph_impl!(
        graph_as_dataset,
        MyGaDG,
        true,
        true,
        collect_graph_as_dataset
    );

    #[allow(dead_code)] // just check this compiles
    fn check_trait_impls() {
        let mut g: Vec<[SimpleTerm; 3]> = vec![];

        // check that Graph::as_dataset implememnts Dataset
        for _ in g.as_dataset().quads() {}

        let mut gd = g.as_dataset_mut();
        // check that Graph::as_dataset_mut implememnts Dataset
        for _ in gd.quads() {}
        // check that Graph::as_dataset_mut implememnts MutableDataset
        gd.remove_quad(([1, 2, 3], None)).unwrap();

        let mut gd = g.into_dataset();
        // check that Graph::as_dataset_mut implememnts Dataset
        for _ in gd.quads() {}
        // check that Graph::as_dataset_mut implememnts MutableDataset
        gd.remove_quad(([1, 2, 3], None)).unwrap();
    }
}
