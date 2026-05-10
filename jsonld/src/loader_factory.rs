//! I define trait for loader factories.
//!

use std::marker::PhantomData;

use json_ld::Loader;

/// A trait for factory of document loaders.
pub trait LoaderFactory {
    /// Type of loaders this factory yields.
    type Loader<'l>: Loader + Send + Sync + 'l
    where
        Self: 'l;

    /// Yield a new loader.
    fn yield_loader(&self) -> Self::Loader<'_>;
}

/// A loader factory that yields default loaders.
#[derive(Debug, Clone, Default)]
pub struct DefaultLoaderFactory<L> {
    _phantom: PhantomData<L>,
}

impl<L> DefaultLoaderFactory<L>
where
    L: Loader + Default + Send + Sync,
{
    /// Create a new [`DefaultLoaderFactory`].
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

impl<L> LoaderFactory for DefaultLoaderFactory<L>
where
    L: Loader + Default + Send + Sync,
{
    type Loader<'l>
        = L
    where
        Self: 'l;

    #[inline]
    fn yield_loader(&self) -> Self::Loader<'_> {
        L::default()
    }
}

/// A loader factory that delegates to an underlying closure.
#[derive(Debug, Clone)]
pub struct ClosureLoaderFactory<L, F> {
    closure: F,
    _phantom: PhantomData<fn() -> L>,
}

impl<L, F> ClosureLoaderFactory<L, F>
where
    L: Loader + Send + Sync,
    F: Fn() -> L,
{
    /// Create a new [`ClosureLoaderFactory`] with given closure.
    #[inline]
    pub fn new(closure: F) -> Self {
        Self {
            closure,
            _phantom: PhantomData,
        }
    }
}

impl<L> ClosureLoaderFactory<L, fn() -> L>
where
    L: Loader + Send + Sync,
{
    /// Create a new [`ClosureLoaderFactory`] that yields loaders by cloning a template loader.
    #[inline]
    pub fn new_cloning(template_loader: L) -> ClosureLoaderFactory<L, impl Fn() -> L>
    where
        L: Clone,
    {
        ClosureLoaderFactory::new(move || template_loader.clone())
    }
}

impl<L, F> LoaderFactory for ClosureLoaderFactory<L, F>
where
    L: Loader + Send + Sync,
    F: Fn() -> L,
{
    type Loader<'l>
        = L
    where
        Self: 'l;

    #[inline]
    fn yield_loader(&self) -> Self::Loader<'_> {
        (self.closure)()
    }
}
