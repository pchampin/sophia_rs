// this module is transparently re-exported by its parent `graph::inmem`

use super::*;

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
    fn gw_triples_with_s<'s, T>(&'s self, s: &'s Term<T>) -> GTripleSource<'s, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().triples_with_s(s)
    }
    #[inline]
    /// Mimmic the [`triples_with_p`](../trait.Graph.html#method.triples_with_p) method.
    fn gw_triples_with_p<'s, T>(&'s self, p: &'s Term<T>) -> GTripleSource<'s, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().triples_with_p(p)
    }
    #[inline]
    /// Mimmic the [`triples_with_o`](../trait.Graph.html#method.triples_with_o) method.
    fn gw_triples_with_o<'s, T>(&'s self, o: &'s Term<T>) -> GTripleSource<'s, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().triples_with_o(o)
    }
    #[inline]
    /// Mimmic the [`triples_with_sp`](../trait.Graph.html#method.triples_with_sp) method.
    fn gw_triples_with_sp<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
    ) -> GTripleSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().triples_with_sp(s, p)
    }
    #[inline]
    /// Mimmic the [`triples_with_so`](../trait.Graph.html#method.triples_with_so) method.
    fn gw_triples_with_so<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        o: &'s Term<U>,
    ) -> GTripleSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().triples_with_so(s, o)
    }
    #[inline]
    /// Mimmic the [`triples_with_po`](../trait.Graph.html#method.triples_with_po) method.
    fn gw_triples_with_po<'s, T, U>(
        &'s self,
        p: &'s Term<T>,
        o: &'s Term<U>,
    ) -> GTripleSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().triples_with_po(p, o)
    }
    #[inline]
    /// Mimmic the [`triples_with_spo`](../trait.Graph.html#method.triples_with_spo) method.
    fn gw_triples_with_spo<'s, T, U, V>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
        o: &'s Term<V>,
    ) -> GTripleSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().triples_with_spo(s, p, o)
    }

    #[inline]
    /// Mimmic the [`contains`](../trait.Graph.html#method.contains) method.
    fn gw_contains<T, U, V>(
        &self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
    ) -> GResult<Self::Wrapped, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().contains(s, p, o)
    }

    #[inline]
    /// Mimmic the [`subjects`](../trait.Graph.html#method.subjects) method.
    fn gw_subjects(&self) -> GResultTermSet<Self::Wrapped> {
        self.get_wrapped().subjects()
    }

    #[inline]
    /// Mimmic the [`predicates`](../trait.Graph.html#method.predicates) method.
    fn gw_predicates(&self) -> GResultTermSet<Self::Wrapped> {
        self.get_wrapped().predicates()
    }

    #[inline]
    /// Mimmic the [`objects`](../trait.Graph.html#method.objects) method.
    fn gw_objects(&self) -> GResultTermSet<Self::Wrapped> {
        self.get_wrapped().objects()
    }

    #[inline]
    /// Mimmic the [`iris`](../trait.Graph.html#method.iris) method.
    fn gw_iris(&self) -> GResultTermSet<Self::Wrapped> {
        self.get_wrapped().iris()
    }

    #[inline]
    /// Mimmic the [`bnodes`](../trait.Graph.html#method.bnodes) method.
    fn gw_bnodes(&self) -> GResultTermSet<Self::Wrapped> {
        self.get_wrapped().bnodes()
    }

    #[inline]
    /// Mimmic the [`literals`](../trait.Graph.html#method.literals) method.
    fn gw_literals(&self) -> GResultTermSet<Self::Wrapped> {
        self.get_wrapped().literals()
    }

    #[inline]
    /// Mimmic the [`variables`](../trait.Graph.html#method.variables) method.
    fn gw_variables(&self) -> GResultTermSet<Self::Wrapped> {
        self.get_wrapped().variables()
    }

    #[inline]
    /// Mimic the [`len`](../trait.Graph.html#method.len) method.
    fn gw_len(&self) -> usize {
        self.get_wrapped().len()
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
        fn triples_with_s<'s_, T_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            T_: sophia_term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_s(self, s)
        }
        #[inline]
        fn triples_with_p<'s_, T_>(
            &'s_ self,
            p: &'s_ sophia_term::Term<T_>,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            T_: sophia_term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_p(self, p)
        }
        #[inline]
        fn triples_with_o<'s_, T_>(
            &'s_ self,
            o: &'s_ sophia_term::Term<T_>,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            T_: sophia_term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_o(self, o)
        }
        #[inline]
        fn triples_with_sp<'s_, T_, U_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            p: &'s_ sophia_term::Term<U_>,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_sp(self, s, p)
        }
        #[inline]
        fn triples_with_so<'s_, T_, U_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            o: &'s_ sophia_term::Term<U_>,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_so(self, s, o)
        }
        #[inline]
        fn triples_with_po<'s_, T_, U_>(
            &'s_ self,
            p: &'s_ sophia_term::Term<T_>,
            o: &'s_ sophia_term::Term<U_>,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_po(self, p, o)
        }
        #[inline]
        fn triples_with_spo<'s_, T_, U_, V_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            p: &'s_ sophia_term::Term<U_>,
            o: &'s_ sophia_term::Term<V_>,
        ) -> $crate::graph::GTripleSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
            V_: sophia_term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_spo(self, s, p, o)
        }

        #[inline]
        fn contains<T_, U_, V_>(
            &self,
            s: &sophia_term::Term<T_>,
            p: &sophia_term::Term<U_>,
            o: &sophia_term::Term<V_>,
        ) -> $crate::graph::GResult<Self, bool>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
            V_: sophia_term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_contains(self, s, p, o)
        }

        #[inline]
        fn subjects(&self) -> GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_subjects(self)
        }

        #[inline]
        fn predicates(
            &self,
        ) -> GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_predicates(self)
        }

        #[inline]
        fn objects(&self) -> GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_objects(self)
        }

        #[inline]
        fn iris(&self) -> GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_iris(self)
        }

        #[inline]
        fn bnodes(&self) -> GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_bnodes(self)
        }

        #[inline]
        fn literals(&self) -> GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_literals(self)
        }

        #[inline]
        fn variables(
            &self,
        ) -> GResult<Self, std::collections::HashSet<$crate::graph::GTerm<Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_variables(self)
        }

        #[inline]
        fn len(&self) -> usize {
            $crate::graph::inmem::GraphWrapper::gw_len(self)
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
        fn get_index<U>(&self, t: &sophia_term::Term<U>) -> Option<Self::Index>
        where
            U: sophia_term::TermData,
        {
            self.get_wrapped().get_index(t)
        }

        #[inline]
        fn get_term(&self, i: Self::Index) -> Option<&Term<Self::TermData>> {
            self.get_wrapped().get_term(i)
        }

        fn insert_indexed<U, V, W>(
            &mut self,
            s: &sophia_term::Term<U>,
            p: &sophia_term::Term<V>,
            o: &sophia_term::Term<W>,
        ) -> Option<[Self::Index; 3]>
        where
            U: sophia_term::TermData,
            V: sophia_term::TermData,
            W: sophia_term::TermData,
        {
            let modified = self.get_wrapped_mut().insert_indexed(s, p, o);
            self.igw_hook_insert_indexed(&modified);
            modified
        }

        fn remove_indexed<U, V, W>(
            &mut self,
            s: &sophia_term::Term<U>,
            p: &sophia_term::Term<V>,
            o: &sophia_term::Term<W>,
        ) -> Option<[Self::Index; 3]>
        where
            U: sophia_term::TermData,
            V: sophia_term::TermData,
            W: sophia_term::TermData,
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
