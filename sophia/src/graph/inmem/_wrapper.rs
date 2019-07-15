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
/// Conversely, the `impl_graph_for_wrapper!` macro can be used to derive
/// the Graph implementation for any implementation of GraphWrapper.
///
/// [`Graph`]: ../trait.Graph.html
pub trait GraphWrapper<'a> {
    /// The type of the wrapped graph.
    type Wrapped: Graph<'a>;

    /// Borrow the wrapped graph.
    fn get_wrapped(&'a self) -> &'a Self::Wrapped;

    /// Borrow the wrapped graph mutably.
    fn get_wrapped_mut(&'a mut self) -> &'a mut Self::Wrapped;

    #[inline]
    /// Mimmic the [`iter`](../trait.Graph.html#tymethod.iter) method.
    fn gw_triples(&'a self) -> GTripleSource<'a, Self::Wrapped> {
        self.get_wrapped().triples()
    }

    #[inline]
    /// Mimmic the [`triples_with_s`](../trait.Graph.html#method.triples_with_s) method.
    fn gw_triples_with_s<T>(&'a self, s: &'a Term<T>) -> GTripleSource<'a, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().triples_with_s(s)
    }
    #[inline]
    /// Mimmic the [`triples_with_p`](../trait.Graph.html#method.triples_with_p) method.
    fn gw_triples_with_p<T>(&'a self, p: &'a Term<T>) -> GTripleSource<'a, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().triples_with_p(p)
    }
    #[inline]
    /// Mimmic the [`triples_with_o`](../trait.Graph.html#method.triples_with_o) method.
    fn gw_triples_with_o<T>(&'a self, o: &'a Term<T>) -> GTripleSource<'a, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().triples_with_o(o)
    }
    #[inline]
    /// Mimmic the [`triples_with_sp`](../trait.Graph.html#method.triples_with_sp) method.
    fn gw_triples_with_sp<T, U>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
    ) -> GTripleSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().triples_with_sp(s, p)
    }
    #[inline]
    /// Mimmic the [`triples_with_so`](../trait.Graph.html#method.triples_with_so) method.
    fn gw_triples_with_so<T, U>(
        &'a self,
        s: &'a Term<T>,
        o: &'a Term<U>,
    ) -> GTripleSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().triples_with_so(s, o)
    }
    #[inline]
    /// Mimmic the [`triples_with_po`](../trait.Graph.html#method.triples_with_po) method.
    fn gw_triples_with_po<T, U>(
        &'a self,
        p: &'a Term<T>,
        o: &'a Term<U>,
    ) -> GTripleSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().triples_with_po(p, o)
    }
    #[inline]
    /// Mimmic the [`triples_with_spo`](../trait.Graph.html#method.triples_with_spo) method.
    fn gw_triples_with_spo<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
    ) -> GTripleSource<'a, Self::Wrapped>
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
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
    ) -> GResult<'a, Self::Wrapped, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().contains(s, p, o)
    }

    #[inline]
    /// Mimmic the [`subjects`](../trait.Graph.html#method.subjects) method.
    fn gw_subjects(&'a self) -> GResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().subjects()
    }

    #[inline]
    /// Mimmic the [`predicates`](../trait.Graph.html#method.predicates) method.
    fn gw_predicates(&'a self) -> GResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().predicates()
    }

    #[inline]
    /// Mimmic the [`objects`](../trait.Graph.html#method.objects) method.
    fn gw_objects(&'a self) -> GResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().objects()
    }

    #[inline]
    /// Mimmic the [`iris`](../trait.Graph.html#method.iris) method.
    fn gw_iris(&'a self) -> GResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().iris()
    }

    #[inline]
    /// Mimmic the [`bnodes`](../trait.Graph.html#method.bnodes) method.
    fn gw_bnodes(&'a self) -> GResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().bnodes()
    }

    #[inline]
    /// Mimmic the [`literals`](../trait.Graph.html#method.literals) method.
    fn gw_literals(&'a self) -> GResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().literals()
    }

    #[inline]
    /// Mimmic the [`variables`](../trait.Graph.html#method.variables) method.
    fn gw_variables(&'a self) -> GResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().variables()
    }
}

macro_rules! impl_graph_for_wrapper {
    ($wrapper: ty) => {
        impl<'a> $crate::graph::Graph<'a> for $wrapper {
            impl_graph_for_wrapper!();
        }
    };
    () => {
        type Triple = <<Self as $crate::graph::inmem::GraphWrapper<'a>>::Wrapped as $crate::graph::Graph<'a>>::Triple;
        type Error = <<Self as $crate::graph::inmem::GraphWrapper<'a>>::Wrapped as $crate::graph::Graph<'a>>::Error;

        #[inline]
        fn triples(&'a self) -> $crate::graph::GTripleSource<'a, Self> {
            $crate::graph::inmem::GraphWrapper::gw_triples(self)
        }
        #[inline]
        fn triples_with_s<T_> (&'a self, s: &'a $crate::term::Term<T_>) -> $crate::graph::GTripleSource<'a, Self> where
            T_: $crate::term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_s(self, s)
        }
        #[inline]
        fn triples_with_p<T_> (&'a self, p: &'a $crate::term::Term<T_>) -> $crate::graph::GTripleSource<'a, Self> where
            T_: $crate::term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_p(self, p)
        }
        #[inline]
        fn triples_with_o<T_> (&'a self, o: &'a $crate::term::Term<T_>) -> $crate::graph::GTripleSource<'a, Self> where
            T_: $crate::term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_o(self, o)
        }
        #[inline]
        fn triples_with_sp<T_, U_> (&'a self, s: &'a $crate::term::Term<T_>, p: &'a $crate::term::Term<U_>) -> $crate::graph::GTripleSource<'a, Self> where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_sp(self, s, p)
        }
        #[inline]
        fn triples_with_so<T_, U_> (&'a self, s: &'a $crate::term::Term<T_>, o: &'a $crate::term::Term<U_>) -> $crate::graph::GTripleSource<'a, Self> where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_so(self, s, o)
        }
        #[inline]
        fn triples_with_po<T_, U_> (&'a self, p: &'a $crate::term::Term<T_>, o: &'a $crate::term::Term<U_>) -> $crate::graph::GTripleSource<'a, Self> where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_po(self, p, o)
        }
        #[inline]
        fn triples_with_spo<T_, U_, V_> (&'a self, s: &'a $crate::term::Term<T_>, p: &'a $crate::term::Term<U_>, o: &'a $crate::term::Term<V_>) -> $crate::graph::GTripleSource<'a, Self> where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_triples_with_spo(self, s, p, o)
        }

        #[inline]
        fn contains<T_, U_, V_> (&'a self, s: &'a $crate::term::Term<T_>, p: &'a $crate::term::Term<U_>, o: &'a $crate::term::Term<V_>) -> $crate::graph::GResult<'a, Self, bool> where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
        {
            $crate::graph::inmem::GraphWrapper::gw_contains(self, s, p, o)
        }

        #[inline]
        fn subjects(&'a self) -> GResult<'a, Self, std::collections::HashSet<$crate::graph::GTerm<'a, Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_subjects(self)
        }

        #[inline]
        fn predicates(&'a self) -> GResult<'a, Self, std::collections::HashSet<$crate::graph::GTerm<'a, Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_predicates(self)
        }

        #[inline]
        fn objects(&'a self) -> GResult<'a, Self, std::collections::HashSet<$crate::graph::GTerm<'a, Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_objects(self)
        }

        #[inline]
        fn iris(&'a self) -> GResult<'a, Self, std::collections::HashSet<$crate::graph::GTerm<'a, Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_iris(self)
        }

        #[inline]
        fn bnodes(&'a self) -> GResult<'a, Self, std::collections::HashSet<$crate::graph::GTerm<'a, Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_bnodes(self)
        }

        #[inline]
        fn literals(&'a self) -> GResult<'a, Self, std::collections::HashSet<$crate::graph::GTerm<'a, Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_literals(self)
        }

        #[inline]
        fn variables(&'a self) -> GResult<'a, Self, std::collections::HashSet<$crate::graph::GTerm<'a, Self>>> {
            $crate::graph::inmem::GraphWrapper::gw_variables(self)
        }
    };
}

/// An indexed graph wrapper wraps an [`IndexedGraph`] and augments some of its methods,
/// through hooks of the forme `before_x` and `after_x`.
///
/// This trait is designed to add mutability to [`GraphWrapper`],
/// through yje `impl_indexed_graph_for_wrapper!` macro.
///
/// [`IndexedGraph`]: ../index/trait.IndexedGraph.html
/// [`GraphWrapper`]: ./trait.GraphWrapper.html
pub trait IndexedGraphWrapper<T>
where
    T: IndexedGraph,
{
    /// Hook to be executed at the end of
    /// [`IndexedGraph::insert_indexed`](../index/trait.IndexedGraph.html#tymethod.insert_indexed).
    fn igw_hook_insert_indexed(&mut self, modified: &Option<[T::Index; 3]>);

    /// Hook to be executed at the end of
    /// [`IndexedGraph::remove_indexed`](../index/trait.IndexedGraph.html#tymethod.remove_indexed).
    fn igw_hook_remove_indexed(&mut self, modified: &Option<[T::Index; 3]>);

    /// Hook to be executed at the end of
    /// [`IndexedGraph::shrink_to_fit`](../index/trait.IndexedGraph.html#tymethod.shrink_to_fit).
    fn igw_hook_shrink_to_fit(&mut self);
}

macro_rules! impl_indexed_graph_for_wrapper {
    ($wrapper: ty) => {
        impl $crate::graph::index::IndexedGraph for $wrapper
        where
            T: $crate::graph::index::IndexedGraph + for<'a> $crate::graph::Graph<'a, Triple = [&'a $crate::term::Term<<T as $crate::graph::index::IndexedGraph>::TermData>; 3]>,
        {
            impl_indexed_graph_for_wrapper!();
        }
    };
    () => {
        type Index = T::Index;
        type TermData = T::TermData;

        #[inline]
        fn get_index<U>(&self, t: &$crate::term::Term<U>) -> Option<Self::Index>
        where
            U: $crate::term::TermData,
        {
            self.get_wrapped().get_index(t)
        }

        #[inline]
        fn get_term(& self, i: Self::Index) -> Option<&Term<Self::TermData>> {
            self.get_wrapped().get_term(i)
        }

        fn insert_indexed<U, V, W>(
            &mut self,
            s: &$crate::term::Term<U>,
            p: &$crate::term::Term<V>,
            o: &$crate::term::Term<W>,
        ) -> Option<[Self::Index; 3]>
        where
            U: $crate::term::TermData,
            V: $crate::term::TermData,
            W: $crate::term::TermData,
        {
            let modified = self.get_wrapped_mut().insert_indexed(s, p, o);
            self.igw_hook_insert_indexed(&modified);
            modified
        }

        fn remove_indexed<U, V, W>(
            &mut self,
            s: &$crate::term::Term<U>,
            p: &$crate::term::Term<V>,
            o: &$crate::term::Term<W>,
        ) -> Option<([Self::Index; 3])>
        where
            U: $crate::term::TermData,
            V: $crate::term::TermData,
            W: $crate::term::TermData,
        {
            let modified = self.get_wrapped_mut().remove_indexed(s, p, o);
            self.igw_hook_remove_indexed(&modified);
            modified
        }

        fn shrink_to_fit(&mut self) {
            self.get_wrapped_mut().shrink_to_fit();
            self.igw_hook_shrink_to_fit();
        }
    };
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially in ./inmem.rs).
}
