// this module is transparently re-exported by its parent `graph`

use crate::graph::_traits::*;
use crate::triple::stream::*;
use crate::triple::*;

/// The [`TripleSink`] returned by [`MutableGraph::inserter`].
///
/// [`TripleSink`]: ../triple/stream/trait.TripleSink.html
/// [`MutableGraph::inserter`]: trait.MutableGraph.html#method.inserter
///
pub struct Inserter<'a, G: ?Sized + 'a> {
    graph: &'a mut G,
    count: usize,
}

impl<'a, G: MutableGraph + ?Sized + 'a> Inserter<'a, G> {
    pub fn new(graph: &'a mut G) -> Self {
        Inserter { graph, count: 0 }
    }
}

impl<'a, G: MutableGraph + ?Sized + 'a> TripleSink for Inserter<'a, G> {
    type Outcome = usize;
    type Error = <G as MutableGraph>::MutationError;

    fn feed<T: Triple>(&mut self, t: &T) -> Result<(), Self::Error> {
        self.graph.insert(t.s(), t.p(), t.o()).map(|inserted| {
            if inserted {
                self.count += 1;
            }
        })
    }
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error> {
        Ok(self.count)
    }
}

/// The [`TripleSink`] returned by [`MutableGraph::remover`].
///
/// [`TripleSink`]: ../triple/stream/trait.TripleSink.html
/// [`MutableGraph::remover`]: trait.MutableGraph.html#method.remover
///
pub struct Remover<'a, G: ?Sized + 'a> {
    graph: &'a mut G,
    count: usize,
}

impl<'a, G: MutableGraph + ?Sized + 'a> Remover<'a, G> {
    pub fn new(graph: &'a mut G) -> Self {
        Remover { graph, count: 0 }
    }
}

impl<'a, G: MutableGraph + ?Sized + 'a> TripleSink for Remover<'a, G> {
    type Outcome = usize;
    type Error = <G as MutableGraph>::MutationError;

    fn feed<T: Triple>(&mut self, t: &T) -> Result<(), Self::Error> {
        self.graph.remove(t.s(), t.p(), t.o()).map(|removed| {
            if removed {
                self.count += 1;
            }
        })
    }
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error> {
        Ok(self.count)
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the macro test_graph_impl!).
}
