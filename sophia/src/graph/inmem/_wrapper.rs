// this module is transparently re-exported by its parent `graph::inmem`

use super::*;
use sophia_api::graph::{GResult, GResultTermSet, GTerm, GTripleSource};
use sophia_api::term::TTerm;
use std::hash::Hash;

/// A graph wrapper wraps a [`Graph`] and overrides some of its methods.
///
/// This trait mimmics the interface of the [`Graph`] trait,
/// with all methods having a default implementation
/// that delegates to the corresponding method of the wrapped graph.
/// Implementation of this trait may however expected to override
/// *some* of the methods.
///
/// Conversely, the [`impl_graph_for_wrapper!`] macro can be used to derive
/// the Graph implementation for any implementation of GraphWrapper.
///
/// [`Graph`]: ../trait.Graph.html
/// [`impl_mutable_graph_for_indexed_graph`]: ../../macro.impl_mutable_graph_for_indexed_graph.html
pub trait GraphWrapper {
    /// The type of the wrapped graph.
    type Wrapped: Graph;

    /// Borrow the wrapped graph.
    fn get_wrapped(&self) -> &Self::Wrapped;

    /// Borrow the wrapped graph mutably.
    fn get_wrapped_mut(&mut self) -> &mut Self::Wrapped;

    #[inline]
    /// Mimmic the [`iter`](../trait.Graph.html#tymethod.iter) method.
    fn gw_triples(&self) -> GTripleSource<Self::Wrapped> {
        self.get_wrapped().triples()
    }

    #[inline]
    /// Mimmic the [`triples_with_s`](../trait.Graph.html#method.triples_with_s) method.
    fn gw_triples_with_s<'s, TS>(&'s self, s: &'s TS) -> GTripleSource<'s, Self::Wrapped>
    where
        TS: TTerm + ?Sized,
    {
        self.get_wrapped().triples_with_s(s)
    }
    #[inline]
    /// Mimmic the [`triples_with_p`](../trait.Graph.html#method.triples_with_p) method.
    fn gw_triples_with_p<'s, TP>(&'s self, p: &'s TP) -> GTripleSource<'s, Self::Wrapped>
    where
        TP: TTerm + ?Sized,
    {
        self.get_wrapped().triples_with_p(p)
    }
    #[inline]
    /// Mimmic the [`triples_with_o`](../trait.Graph.html#method.triples_with_o) method.
    fn gw_triples_with_o<'s, TO>(&'s self, o: &'s TO) -> GTripleSource<'s, Self::Wrapped>
    where
        TO: TTerm + ?Sized,
    {
        self.get_wrapped().triples_with_o(o)
    }
    #[inline]
    /// Mimmic the [`triples_with_sp`](../trait.Graph.html#method.triples_with_sp) method.
    fn gw_triples_with_sp<'s, TS, TP>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
    ) -> GTripleSource<'s, Self::Wrapped>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
    {
        self.get_wrapped().triples_with_sp(s, p)
    }
    #[inline]
    /// Mimmic the [`triples_with_so`](../trait.Graph.html#method.triples_with_so) method.
    fn gw_triples_with_so<'s, TS, TO>(
        &'s self,
        s: &'s TS,
        o: &'s TO,
    ) -> GTripleSource<'s, Self::Wrapped>
    where
        TS: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        self.get_wrapped().triples_with_so(s, o)
    }
    #[inline]
    /// Mimmic the [`triples_with_po`](../trait.Graph.html#method.triples_with_po) method.
    fn gw_triples_with_po<'s, TP, TO>(
        &'s self,
        p: &'s TP,
        o: &'s TO,
    ) -> GTripleSource<'s, Self::Wrapped>
    where
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        self.get_wrapped().triples_with_po(p, o)
    }
    #[inline]
    /// Mimmic the [`triples_with_spo`](../trait.Graph.html#method.triples_with_spo) method.
    fn gw_triples_with_spo<'s, TS, TP, TO>(
        &'s self,
        s: &'s TS,
        p: &'s TP,
        o: &'s TO,
    ) -> GTripleSource<'s, Self::Wrapped>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        self.get_wrapped().triples_with_spo(s, p, o)
    }

    #[inline]
    /// Mimmic the [`contains`](../trait.Graph.html#method.contains) method.
    fn gw_contains<TS, TP, TO>(&self, s: &TS, p: &TP, o: &TO) -> GResult<Self::Wrapped, bool>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
    {
        self.get_wrapped().contains(s, p, o)
    }

    #[inline]
    /// Mimmic the [`subjects`](../trait.Graph.html#method.subjects) method.
    fn gw_subjects(&self) -> GResultTermSet<Self::Wrapped>
    where
        GTerm<Self::Wrapped>: Clone + Eq + Hash,
    {
        self.get_wrapped().subjects()
    }

    #[inline]
    /// Mimmic the [`predicates`](../trait.Graph.html#method.predicates) method.
    fn gw_predicates(&self) -> GResultTermSet<Self::Wrapped>
    where
        GTerm<Self::Wrapped>: Clone + Eq + Hash,
    {
        self.get_wrapped().predicates()
    }

    #[inline]
    /// Mimmic the [`objects`](../trait.Graph.html#method.objects) method.
    fn gw_objects(&self) -> GResultTermSet<Self::Wrapped>
    where
        GTerm<Self::Wrapped>: Clone + Eq + Hash,
    {
        self.get_wrapped().objects()
    }

    #[inline]
    /// Mimmic the [`iris`](../trait.Graph.html#method.iris) method.
    fn gw_iris(&self) -> GResultTermSet<Self::Wrapped>
    where
        GTerm<Self::Wrapped>: Clone + Eq + Hash,
    {
        self.get_wrapped().iris()
    }

    #[inline]
    /// Mimmic the [`bnodes`](../trait.Graph.html#method.bnodes) method.
    fn gw_bnodes(&self) -> GResultTermSet<Self::Wrapped>
    where
        GTerm<Self::Wrapped>: Clone + Eq + Hash,
    {
        self.get_wrapped().bnodes()
    }

    #[inline]
    /// Mimmic the [`literals`](../trait.Graph.html#method.literals) method.
    fn gw_literals(&self) -> GResultTermSet<Self::Wrapped>
    where
        GTerm<Self::Wrapped>: Clone + Eq + Hash,
    {
        self.get_wrapped().literals()
    }

    #[inline]
    /// Mimmic the [`variables`](../trait.Graph.html#method.variables) method.
    fn gw_variables(&self) -> GResultTermSet<Self::Wrapped>
    where
        GTerm<Self::Wrapped>: Clone + Eq + Hash,
    {
        self.get_wrapped().variables()
    }
}

/// Defines the implementation of [`Graph`] for [`GraphWrapper`].
///
/// [`Graph`]: graph/trait.Graph.html
/// [`GraphWrapper`]: graph/inmem/trait.GraphWrapper.html
#[macro_export]
macro_rules! impl_graph_for_wrapper {
    ($wrapper: ty) => {
        impl $crate::graph::Graph for $wrapper {
            impl_graph_for_wrapper!();
        }
    };
    () => {
        type Triple =
            <<Self as $crate::graph::inmem::GraphWrapper>::Wrapped as $crate::graph::Graph>::Triple;
        type Error =
            <<Self as $crate::graph::inmem::GraphWrapper>::Wrapped as $crate::graph::Graph>::Error;

        #[inline]
        fn triples(&self) -> $crate::graph::GTripleSource<Self> {
            $crate::graph::inmem::GraphWrapper::gw_triples(self)
        }
        #[inline]
        fn triples_with_s<'s_, TS_>(
            &'s_ self,
            s: &'s_ TS_,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            TS_: sophia_api::term::TTerm + ?Sized,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_s(self, s)
        }
        #[inline]
        fn triples_with_p<'s_, TP_>(
            &'s_ self,
            p: &'s_ TP_,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            TP_: sophia_api::term::TTerm + ?Sized,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_p(self, p)
        }
        #[inline]
        fn triples_with_o<'s_, TO_>(
            &'s_ self,
            o: &'s_ TO_,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            TO_: sophia_api::term::TTerm + ?Sized,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_o(self, o)
        }
        #[inline]
        fn triples_with_sp<'s_, TS_, TP_>(
            &'s_ self,
            s: &'s_ TS_,
            p: &'s_ TP_,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            TS_: sophia_api::term::TTerm + ?Sized,
            TP_: sophia_api::term::TTerm + ?Sized,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_sp(self, s, p)
        }
        #[inline]
        fn triples_with_so<'s_, TS_, TO_>(
            &'s_ self,
            s: &'s_ TS_,
            o: &'s_ TO_,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            TS_: sophia_api::term::TTerm + ?Sized,
            TO_: sophia_api::term::TTerm + ?Sized,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_so(self, s, o)
        }
        #[inline]
        fn triples_with_po<'s_, TP_, TO_>(
            &'s_ self,
            p: &'s_ TP_,
            o: &'s_ TO_,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            TP_: sophia_api::term::TTerm + ?Sized,
            TO_: sophia_api::term::TTerm + ?Sized,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_po(self, p, o)
        }
        #[inline]
        fn triples_with_spo<'s_, TS_, TP_, TO_>(
            &'s_ self,
            s: &'s_ TS_,
            p: &'s_ TP_,
            o: &'s_ TO_,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            TS_: sophia_api::term::TTerm + ?Sized,
            TP_: sophia_api::term::TTerm + ?Sized,
            TO_: sophia_api::term::TTerm + ?Sized,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_spo(self, s, p, o)
        }

        #[inline]
        fn contains<TS_, TP_, TO_>(
            &self,
            s: &TS_,
            p: &TP_,
            o: &TO_,
        ) -> $crate::graph::GResult<Self, bool>
        where
            TS_: sophia_api::term::TTerm + ?Sized,
            TP_: sophia_api::term::TTerm + ?Sized,
            TO_: sophia_api::term::TTerm + ?Sized,
        {
            $crate::graph::inmem::GraphWrapper::gw_contains(self, s, p, o)
        }

        #[inline]
        fn subjects(
            &self,
        ) -> $crate::graph::GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>>
        where
            $crate::graph::GTerm<Self>: Clone + Eq + std::hash::Hash,
        {
            $crate::graph::inmem::GraphWrapper::gw_subjects(self)
        }

        #[inline]
        fn predicates(
            &self,
        ) -> $crate::graph::GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>>
        where
            $crate::graph::GTerm<Self>: Clone + Eq + std::hash::Hash,
        {
            $crate::graph::inmem::GraphWrapper::gw_predicates(self)
        }

        #[inline]
        fn objects(
            &self,
        ) -> $crate::graph::GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>>
        where
            $crate::graph::GTerm<Self>: Clone + Eq + std::hash::Hash,
        {
            $crate::graph::inmem::GraphWrapper::gw_objects(self)
        }

        #[inline]
        fn iris(
            &self,
        ) -> $crate::graph::GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>>
        where
            $crate::graph::GTerm<Self>: Clone + Eq + std::hash::Hash,
        {
            $crate::graph::inmem::GraphWrapper::gw_iris(self)
        }

        #[inline]
        fn bnodes(
            &self,
        ) -> $crate::graph::GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>>
        where
            $crate::graph::GTerm<Self>: Clone + Eq + std::hash::Hash,
        {
            $crate::graph::inmem::GraphWrapper::gw_bnodes(self)
        }

        #[inline]
        fn literals(
            &self,
        ) -> $crate::graph::GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>>
        where
            $crate::graph::GTerm<Self>: Clone + Eq + std::hash::Hash,
        {
            $crate::graph::inmem::GraphWrapper::gw_literals(self)
        }

        #[inline]
        fn variables(
            &self,
        ) -> $crate::graph::GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>>
        where
            $crate::graph::GTerm<Self>: Clone + Eq + std::hash::Hash,
        {
            $crate::graph::inmem::GraphWrapper::gw_variables(self)
        }
    };
}

/// An indexed graph wrapper wraps an [`IndexedGraph`] and augments some of its methods,
/// through hooks of the forme `before_x` and `after_x`.
///
/// This trait is designed to add mutability to [`GraphWrapper`],
/// through the [`impl_indexed_graph_for_wrapper!`] macro.
///
/// [`IndexedGraph`]: ../indexed/trait.IndexedGraph.html
/// [`GraphWrapper`]: ./trait.GraphWrapper.html
/// [`impl_mutable_graph_for_indexed_graph!`]: ../../macro.impl_mutable_graph_for_indexed_graph.html
pub trait IndexedGraphWrapper<T>
where
    T: IndexedGraph,
{
    /// Wrap the given graph.
    ///
    /// # Pre-conditions
    /// This method requires that the given graph is empty.
    fn igw_wrap_empty(graph: T) -> Self;

    /// Hook to be executed at the end of
    /// [`IndexedGraph::insert_indexed`](../indexed/trait.IndexedGraph.html#tymethod.insert_indexed).
    fn igw_hook_insert_indexed(&mut self, modified: &Option<[T::Index; 3]>);

    /// Hook to be executed at the end of
    /// [`IndexedGraph::remove_indexed`](../indexed/trait.IndexedGraph.html#tymethod.remove_indexed).
    fn igw_hook_remove_indexed(&mut self, modified: &Option<[T::Index; 3]>);

    /// Hook to be executed at the end of
    /// [`IndexedGraph::shrink_to_fit`](../indexed/trait.IndexedGraph.html#tymethod.shrink_to_fit).
    fn igw_hook_shrink_to_fit(&mut self);
}

/// Defines the implementation of [`IndexedGraph`] for [`GraphWrapper`] around another [`IndexedGraph`].
///
/// [`IndexedGraph`]: graph/indexed/trait.IndexedGraph.html
/// [`GraphWrapper`]: graph/inmem/trait.GraphWrapper.html
#[macro_export]
macro_rules! impl_indexed_graph_for_wrapper {
    () => {
        type Index = T::Index;
        type TermData = T::TermData;

        #[inline]
        fn with_capacity(capacity: usize) -> Self {
            Self::igw_wrap_empty(T::with_capacity(capacity))
        }

        #[inline]
        fn shrink_to_fit(&mut self) {
            self.get_wrapped_mut().shrink_to_fit();
            self.igw_hook_shrink_to_fit();
        }

        #[inline]
        fn get_index<U_>(&self, t: &U_) -> Option<Self::Index>
        where
            U_: sophia_api::term::TTerm + ?Sized,
        {
            self.get_wrapped().get_index(t)
        }

        #[inline]
        fn get_term(&self, i: Self::Index) -> Option<&Term<Self::TermData>> {
            self.get_wrapped().get_term(i)
        }

        fn insert_indexed<TS_, TP_, TO_>(
            &mut self,
            s: &TS_,
            p: &TP_,
            o: &TO_,
        ) -> Option<[Self::Index; 3]>
        where
            TS_: sophia_api::term::TTerm + ?Sized,
            TP_: sophia_api::term::TTerm + ?Sized,
            TO_: sophia_api::term::TTerm + ?Sized,
        {
            let modified = self.get_wrapped_mut().insert_indexed(s, p, o);
            self.igw_hook_insert_indexed(&modified);
            modified
        }

        fn remove_indexed<TS_, TP_, TO_>(
            &mut self,
            s: &TS_,
            p: &TP_,
            o: &TO_,
        ) -> Option<[Self::Index; 3]>
        where
            TS_: sophia_api::term::TTerm + ?Sized,
            TP_: sophia_api::term::TTerm + ?Sized,
            TO_: sophia_api::term::TTerm + ?Sized,
        {
            let modified = self.get_wrapped_mut().remove_indexed(s, p, o);
            self.igw_hook_remove_indexed(&modified);
            modified
        }
    };
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially in ./inmem.rs).
}
