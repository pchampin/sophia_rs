// this module is transparently re-exported by its parent `graph`

use std::collections::HashSet;

use crate::triple::Triple;

use super::*;

type TermDataT<'a, W> = <<W as Graph<'a>>::Triple as Triple<'a>>::TermData;
type GResultTermSet<'a, W> = GResult<'a, W, HashSet<Term<TermDataT<'a, W>>>>;

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
        T: AsRef<str> + Clone + Eq + Hash,
    {
        self.get_wrapped().triples_with_s(s)
    }
    #[inline]
    /// Mimmic the [`triples_with_p`](../trait.Graph.html#method.triples_with_p) method.
    fn gw_triples_with_p<T>(&'a self, p: &'a Term<T>) -> GTripleSource<'a, Self::Wrapped>
    where
        T: AsRef<str> + Clone + Eq + Hash,
    {
        self.get_wrapped().triples_with_p(p)
    }
    #[inline]
    /// Mimmic the [`triples_with_o`](../trait.Graph.html#method.triples_with_o) method.
    fn gw_triples_with_o<T>(&'a self, o: &'a Term<T>) -> GTripleSource<'a, Self::Wrapped>
    where
        T: AsRef<str> + Clone + Eq + Hash,
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
        T: AsRef<str> + Clone + Eq + Hash,
        U: AsRef<str> + Clone + Eq + Hash,
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
        T: AsRef<str> + Clone + Eq + Hash,
        U: AsRef<str> + Clone + Eq + Hash,
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
        T: AsRef<str> + Clone + Eq + Hash,
        U: AsRef<str> + Clone + Eq + Hash,
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
        T: AsRef<str> + Clone + Eq + Hash,
        U: AsRef<str> + Clone + Eq + Hash,
        V: AsRef<str> + Clone + Eq + Hash,
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
        T: AsRef<str> + Clone + Eq + Hash,
        U: AsRef<str> + Clone + Eq + Hash,
        V: AsRef<str> + Clone + Eq + Hash,
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
        impl<'a> Graph<'a> for $wrapper {
            impl_graph_for_wrapper!();
        }
    };
    () => {
        type Triple = <<Self as GraphWrapper<'a>>::Wrapped as Graph<'a>>::Triple;
        type Error = <<Self as GraphWrapper<'a>>::Wrapped as Graph<'a>>::Error;

        #[inline]
        fn triples(&'a self) -> GTripleSource<'a, Self> {
            GraphWrapper::gw_triples(self)
        }
        #[inline]
        fn triples_with_s<T_> (&'a self, s: &'a Term<T_>) -> GTripleSource<'a, Self> where
            T_: AsRef<str> + Clone + Eq + std::hash::Hash,
        {
            GraphWrapper::gw_triples_with_s(self, s)
        }
        #[inline]
        fn triples_with_p<T_> (&'a self, p: &'a Term<T_>) -> GTripleSource<'a, Self> where
            T_: AsRef<str> + Clone + Eq + std::hash::Hash,
        {
            GraphWrapper::gw_triples_with_p(self, p)
        }
        #[inline]
        fn triples_with_o<T_> (&'a self, o: &'a Term<T_>) -> GTripleSource<'a, Self> where
            T_: AsRef<str> + Clone + Eq + std::hash::Hash,
        {
            GraphWrapper::gw_triples_with_o(self, o)
        }
        #[inline]
        fn triples_with_sp<T_, U_> (&'a self, s: &'a Term<T_>, p: &'a Term<U_>) -> GTripleSource<'a, Self> where
            T_: AsRef<str> + Clone + Eq + std::hash::Hash,
            U_: AsRef<str> + Clone + Eq + std::hash::Hash,
        {
            GraphWrapper::gw_triples_with_sp(self, s, p)
        }
        #[inline]
        fn triples_with_so<T_, U_> (&'a self, s: &'a Term<T_>, o: &'a Term<U_>) -> GTripleSource<'a, Self> where
            T_: AsRef<str> + Clone + Eq + std::hash::Hash,
            U_: AsRef<str> + Clone + Eq + std::hash::Hash,
        {
            GraphWrapper::gw_triples_with_so(self, s, o)
        }
        #[inline]
        fn triples_with_po<T_, U_> (&'a self, p: &'a Term<T_>, o: &'a Term<U_>) -> GTripleSource<'a, Self> where
            T_: AsRef<str> + Clone + Eq + std::hash::Hash,
            U_: AsRef<str> + Clone + Eq + std::hash::Hash,
        {
            GraphWrapper::gw_triples_with_po(self, p, o)
        }
        #[inline]
        fn triples_with_spo<T_, U_, V_> (&'a self, s: &'a Term<T_>, p: &'a Term<U_>, o: &'a Term<V_>) -> GTripleSource<'a, Self> where
            T_: AsRef<str> + Clone + Eq + std::hash::Hash,
            U_: AsRef<str> + Clone + Eq + std::hash::Hash,
            V_: AsRef<str> + Clone + Eq + std::hash::Hash,
        {
            GraphWrapper::gw_triples_with_spo(self, s, p, o)
        }

        #[inline]
        fn contains<T_, U_, V_> (&'a self, s: &'a Term<T_>, p: &'a Term<U_>, o: &'a Term<V_>) -> GResult<'a, Self, bool> where
            T_: AsRef<str> + Clone + Eq + std::hash::Hash,
            U_: AsRef<str> + Clone + Eq + std::hash::Hash,
            V_: AsRef<str> + Clone + Eq + std::hash::Hash,
        {
            GraphWrapper::gw_contains(self, s, p, o)
        }

        #[inline]
        fn subjects(&'a self) -> GResult<'a, Self, std::collections::HashSet<Term<<Self::Triple as crate::triple::Triple<'a>>::TermData>>> {
            GraphWrapper::gw_subjects(self)
        }

        #[inline]
        fn predicates(&'a self) -> GResult<'a, Self, std::collections::HashSet<Term<<Self::Triple as crate::triple::Triple<'a>>::TermData>>> {
            GraphWrapper::gw_predicates(self)
        }

        #[inline]
        fn objects(&'a self) -> GResult<'a, Self, std::collections::HashSet<Term<<Self::Triple as crate::triple::Triple<'a>>::TermData>>> {
            GraphWrapper::gw_objects(self)
        }

        #[inline]
        fn iris(&'a self) -> GResult<'a, Self, std::collections::HashSet<Term<<Self::Triple as crate::triple::Triple<'a>>::TermData>>> {
            GraphWrapper::gw_iris(self)
        }

        #[inline]
        fn bnodes(&'a self) -> GResult<'a, Self, std::collections::HashSet<Term<<Self::Triple as crate::triple::Triple<'a>>::TermData>>> {
            GraphWrapper::gw_bnodes(self)
        }

        #[inline]
        fn literals(&'a self) -> GResult<'a, Self, std::collections::HashSet<Term<<Self::Triple as crate::triple::Triple<'a>>::TermData>>> {
            GraphWrapper::gw_literals(self)
        }

        #[inline]
        fn variables(&'a self) -> GResult<'a, Self, std::collections::HashSet<Term<<Self::Triple as crate::triple::Triple<'a>>::TermData>>> {
            GraphWrapper::gw_variables(self)
        }
    };
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially in graph::inmem -- the mod.rs file).
}
