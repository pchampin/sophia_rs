// this module is transparently re-exported by its parent `graph`

use super::*;

use ::error::*;

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
pub trait GraphWrapper<'a>
{
    /// The type of the wrapped graph.
    type Wrapped: Graph<'a>;

    /// Borrow the wrapped graph.
    fn get_wrapped(&'a self) -> &'a Self::Wrapped;

    /// Borrow the wrapped graph mutably.
    fn get_wrapped_mut(&'a mut self) -> &'a mut Self::Wrapped;

    #[inline]
    /// Mimmic the [`iter`](../trait.Graph.html#tymethod.iter) method.
    fn gw_iter(&'a self) -> GTripleSource<'a, Self::Wrapped> {
        self.get_wrapped().iter()
    }

    #[inline]
    /// Mimmic the [`iter_for_s`](../trait.Graph.html#method.iter_for_s) method.
    fn gw_iter_for_s<T> (&'a self, s: &'a Term<T>) -> GTripleSource<'a, Self::Wrapped> where
        T: Borrow<str>,
    {
        self.get_wrapped().iter_for_s(s)
    }
    #[inline]
    /// Mimmic the [`iter_for_p`](../trait.Graph.html#method.iter_for_p) method.
    fn gw_iter_for_p<T> (&'a self, p: &'a Term<T>) -> GTripleSource<'a, Self::Wrapped> where
        T: Borrow<str>,
    {
        self.get_wrapped().iter_for_p(p)
    }
    #[inline]
    /// Mimmic the [`iter_for_o`](../trait.Graph.html#method.iter_for_o) method.
    fn gw_iter_for_o<T> (&'a self, o: &'a Term<T>) -> GTripleSource<'a, Self::Wrapped> where
        T: Borrow<str>,
    {
        self.get_wrapped().iter_for_o(o)
    }
    #[inline]
    /// Mimmic the [`iter_for_sp`](../trait.Graph.html#method.iter_for_sp) method.
    fn gw_iter_for_sp<T, U> (&'a self, s: &'a Term<T>, p: &'a Term<U>) -> GTripleSource<'a, Self::Wrapped> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        self.get_wrapped().iter_for_sp(s, p)
    }
    #[inline]
    /// Mimmic the [`iter_for_so`](../trait.Graph.html#method.iter_for_so) method.
    fn gw_iter_for_so<T, U> (&'a self, s: &'a Term<T>, o: &'a Term<U>) -> GTripleSource<'a, Self::Wrapped> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        self.get_wrapped().iter_for_so(s, o)
    }
    #[inline]
    /// Mimmic the [`iter_for_po`](../trait.Graph.html#method.iter_for_po) method.
    fn gw_iter_for_po<T, U> (&'a self, p: &'a Term<T>, o: &'a Term<U>) -> GTripleSource<'a, Self::Wrapped> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        self.get_wrapped().iter_for_po(p, o)
    }
    #[inline]
    /// Mimmic the [`iter_for_spo`](../trait.Graph.html#method.iter_for_spo) method.
    fn gw_iter_for_spo<T, U, V> (&'a self, s: &'a Term<T>, p: &'a Term<U>, o: &'a Term<V>) -> GTripleSource<'a, Self::Wrapped> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        self.get_wrapped().iter_for_spo(s, p, o)
    }

    #[inline]
    /// Mimmic the [`contains`](../trait.Graph.html#method.contains) method.
    fn gw_contains(&'a self, s: &'a RefTerm, p: &'a RefTerm, o: &'a RefTerm) -> Result<bool> {
        self.get_wrapped().contains(s, p, o)
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

        #[inline]
        fn iter(&'a self) -> GTripleSource<'a, Self> {
            GraphWrapper::gw_iter(self)
        }
        #[inline]
        fn iter_for_s<T_> (&'a self, s: &'a Term<T_>) -> GTripleSource<'a, Self> where
            T_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_s(self, s)
        }
        #[inline]
        fn iter_for_p<T_> (&'a self, p: &'a Term<T_>) -> GTripleSource<'a, Self> where
            T_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_p(self, p)
        }
        #[inline]
        fn iter_for_o<T_> (&'a self, o: &'a Term<T_>) -> GTripleSource<'a, Self> where
            T_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_o(self, o)
        }
        #[inline]
        fn iter_for_sp<T_, U_> (&'a self, s: &'a Term<T_>, p: &'a Term<U_>) -> GTripleSource<'a, Self> where
            T_: Borrow<str>,
            U_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_sp(self, s, p)
        }
        #[inline]
        fn iter_for_so<T_, U_> (&'a self, s: &'a Term<T_>, o: &'a Term<U_>) -> GTripleSource<'a, Self> where
            T_: Borrow<str>,
            U_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_so(self, s, o)
        }
        #[inline]
        fn iter_for_po<T_, U_> (&'a self, p: &'a Term<T_>, o: &'a Term<U_>) -> GTripleSource<'a, Self> where
            T_: Borrow<str>,
            U_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_po(self, p, o)
        }
        #[inline]
        fn iter_for_spo<T_, U_, V_> (&'a self, s: &'a Term<T_>, p: &'a Term<U_>, o: &'a Term<V_>) -> GTripleSource<'a, Self> where
            T_: Borrow<str>,
            U_: Borrow<str>,
            V_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_spo(self, s, p, o)
        }

        #[inline]
        fn contains(&'a self, s: &'a RefTerm, p: &'a RefTerm, o: &'a RefTerm) -> Result<bool> {
            GraphWrapper::gw_contains(self, s, p, o)
        }
    };
}



#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially in graph::inmem -- the mod.rs file).
}