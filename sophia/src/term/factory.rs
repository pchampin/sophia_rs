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
    type TermData: TermData;

    /// Get new `TermData`.
    ///
    /// Mostly used internal to create new terms.
    fn get_term_data(&mut self, txt: &str) -> Self::TermData;

    /// Get a new IRI without suffix.
    fn iri<T>(&mut self, iri: T) -> Result<FTerm<Self>>
    where
        T: TermData,
    {
        Term::new_iri(self.get_term_data(iri.as_ref()))
    }

    /// Get a new suffixed IRI.
    fn iri2<T, U>(&mut self, ns: T, suffix: U) -> Result<FTerm<Self>>
    where
        T: TermData,
        U: TermData,
    {
        Term::new_iri_suffixed(
            self.get_term_data(ns.as_ref()),
            self.get_term_data(suffix.as_ref()),
        )
    }

    /// Get a new blank node.
    fn bnode<T>(&mut self, id: T) -> Result<FTerm<Self>>
    where
        T: TermData,
    {
        Term::new_bnode(self.get_term_data(id.as_ref()))
    }

    /// Get a new language-tagged literal.
    fn literal_lang<T, U>(&mut self, txt: T, lang: U) -> Result<FTerm<Self>>
    where
        T: TermData,
        U: TermData,
    {
        Term::new_literal_lang(
            self.get_term_data(txt.as_ref()),
            self.get_term_data(lang.as_ref()),
        )
    }

    /// Get a new typed literal.
    fn literal_dt<T, U>(&mut self, txt: T, dt: Term<U>) -> Result<FTerm<Self>>
    where
        T: TermData,
        U: TermData,
        // Self::TermData: Debug,
    {
        Term::new_literal_dt(self.get_term_data(txt.as_ref()), self.copy(&dt))
    }

    /// Get a new variable.
    fn variable<T>(&mut self, name: T) -> Result<FTerm<Self>>
    where
        T: TermData,
    {
        Term::new_variable(self.get_term_data(name.as_ref()))
    }

    /// Copy a term.
    ///
    /// The `TermData` of the copy is derived from the factory.
    fn copy<T>(&mut self, other: &Term<T>) -> FTerm<Self>
    where
        T: TermData,
    {
        other.copy_with(|txt| self.get_term_data(txt))
    }

    // fn copy_normalized<T>(&mut self, other: &Term<T>, norm: Normalization) -> FTerm<Self>
    // where
    //     T: TermData,
    // {
    //     Term::normalized_with(other, |txt| self.get_term_data(txt), norm)
    // }

    /// Release memory that the factory no longer uses.
    fn shrink_to_fit(&mut self);
}

/// A `TermFactory` ref-counting the data given out.
pub type RcTermFactory = WeakHashSet<rc::Weak<str>>;

impl TermFactory for RcTermFactory {
    type TermData = Rc<str>;

    fn get_term_data(&mut self, txt: &str) -> Rc<str> {
        if let Some(term_data) = self.get(txt) {
            term_data
        } else {
            let term_data: Rc<str> = Rc::from(txt);
            self.insert(term_data.clone());
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

    fn get_term_data(&mut self, txt: &str) -> sync::Arc<str> {
        if let Some(term_data) = self.get(txt) {
            term_data
        } else {
            let term_data: sync::Arc<str> = sync::Arc::from(txt);
            self.insert(term_data.clone());
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
