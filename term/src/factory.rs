//! A `TermFactory` can be used to create terms while preventing the proliferation of duplicate string.
//!
//! This is especially useful for  [`RcTerm`s](../index.html) and [`ArcTerm`s](../index.html),
//! for which two implementations of `TermFactory` are provided.

use std::rc;
use std::sync;

use weak_table::WeakHashSet;

use super::*;

/// Type alias for the terms produced by a term factory.
pub type FTerm<F> = Term<<F as TermFactory>::TermData>;

/// A factory for terms.
///
/// Implementors may cache terms or data to save memory or accelerate creation.
pub trait TermFactory {
    /// Data used by terms created by the factory.
    type TermData: TermData + for<'x> From<&'x str>;

    /// Get a `TermData` equal to `txt`.
    ///
    /// Mostly used internally to create new terms.
    fn get_term_data<T>(&mut self, txt: T) -> Self::TermData
    where
        T: TermData + Into<Self::TermData>;

    /// Get a new IRI without suffix.
    fn iri<T>(&mut self, iri: T) -> Result<FTerm<Self>>
    where
        T: TermData + Into<Self::TermData>,
    {
        Term::new_iri(self.get_term_data(iri))
    }

    /// Get a new suffixed IRI.
    fn iri2<T, U>(&mut self, ns: T, suffix: U) -> Result<FTerm<Self>>
    where
        T: TermData + Into<Self::TermData>,
        U: TermData + Into<Self::TermData>,
    {
        Term::new_iri_suffixed(self.get_term_data(ns), self.get_term_data(suffix))
    }

    /// Get a new blank node.
    fn bnode<T>(&mut self, id: T) -> Result<FTerm<Self>>
    where
        T: TermData + Into<Self::TermData>,
    {
        Term::new_bnode(self.get_term_data(id))
    }

    /// Get a new language-tagged literal.
    fn literal_lang<T, U>(&mut self, txt: T, lang: U) -> Result<FTerm<Self>>
    where
        T: TermData + Into<Self::TermData>,
        U: TermData + Into<Self::TermData>,
    {
        Term::new_literal_lang(self.get_term_data(txt), self.get_term_data(lang))
    }

    /// Get a new typed literal.
    fn literal_dt<T, U>(&mut self, txt: T, dt: Term<U>) -> Result<FTerm<Self>>
    where
        T: TermData + Into<Self::TermData>,
        U: TermData + Into<Self::TermData>,
    {
        Term::new_literal_dt(self.get_term_data(txt), self.convert_term(dt))
    }

    /// Get a new variable.
    fn variable<T>(&mut self, name: T) -> Result<FTerm<Self>>
    where
        T: TermData + Into<Self::TermData>,
    {
        Term::new_variable(self.get_term_data(name))
    }

    /// Convert a term, using `TermData` from this factory.
    fn convert_term<T>(&mut self, other: Term<T>) -> FTerm<Self>
    where
        T: TermData + Into<Self::TermData>,
    {
        other.map(|txt| self.get_term_data(txt))
    }

    /// Clone a term, using `TermData` from this factory.
    fn clone_term<T>(&mut self, other: &T) -> FTerm<Self>
    where
        T: TTerm + ?Sized,
    {
        RefTerm::from(other).clone_map(|txt| self.get_term_data(txt.as_ref()))
    }

    /// Release memory that the factory no longer uses.
    fn shrink_to_fit(&mut self);
}

/// A `TermFactory` ref-counting the data given out.
pub type RcTermFactory = WeakHashSet<rc::Weak<str>>;

impl TermFactory for RcTermFactory {
    type TermData = Rc<str>;

    fn get_term_data<T>(&mut self, txt: T) -> Rc<str>
    where
        T: TermData + Into<Rc<str>>,
    {
        if let Some(term_data) = self.get(txt.as_ref()) {
            term_data
        } else {
            let term_data = Into::<Rc<str>>::into(txt);
            self.insert(Rc::clone(&term_data));
            term_data
        }
    }

    fn shrink_to_fit(&mut self) {
        WeakHashSet::shrink_to_fit(self);
    }
}

/// A `TermFactory` ref-counting the data given out.
pub type ArcTermFactory = WeakHashSet<sync::Weak<str>>;

impl TermFactory for ArcTermFactory {
    type TermData = sync::Arc<str>;

    fn get_term_data<T>(&mut self, txt: T) -> sync::Arc<str>
    where
        T: TermData + Into<sync::Arc<str>>,
    {
        if let Some(term_data) = self.get(txt.as_ref()) {
            term_data
        } else {
            let term_data = Into::<sync::Arc<str>>::into(txt);
            self.insert(sync::Arc::clone(&term_data));
            term_data
        }
    }

    fn shrink_to_fit(&mut self) {
        WeakHashSet::shrink_to_fit(self);
    }
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
