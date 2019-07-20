//! A `TermFactory` can be used to create terms while preventing the proliferation of duplicate string.
//!
//! This is especially useful for  [`RcTerm`s](../index.html) and [`ArcTerm`s](../index.html),
//! for which two implementations of `TermFactory` are provided.

use std::rc;
use std::sync;

use weak_table::WeakHashSet;

use super::graph_id::GraphId;
use super::*;

/// Type alias for the terms produced by a term factory.
pub type FTerm<F> = Term<<F as TermFactory>::TermData>;

/// Type alias for the terms produced by a term factory.
pub type FGraphId<F> = GraphId<<F as TermFactory>::TermData>;

pub trait TermFactory {
    type TermData: TermData;

    fn get_term_data(&mut self, txt: &str) -> Self::TermData;

    fn iri<T>(&mut self, iri: T) -> Result<FTerm<Self>>
    where
        T: TermData,
    {
        Term::new_iri(self.get_term_data(iri.as_ref()))
    }

    fn iri2<T, U>(&mut self, ns: T, suffix: U) -> Result<FTerm<Self>>
    where
        T: TermData,
        U: TermData,
    {
        Term::new_iri2(
            self.get_term_data(ns.as_ref()),
            self.get_term_data(suffix.as_ref()),
        )
    }

    fn bnode<T>(&mut self, id: T) -> Result<FTerm<Self>>
    where
        T: TermData,
    {
        Term::new_bnode(self.get_term_data(id.as_ref()))
    }

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

    fn literal_dt<T, U>(&mut self, txt: T, dt: Term<U>) -> Result<FTerm<Self>>
    where
        T: TermData,
        U: TermData,
        // Self::TermData: Debug,
    {
        Term::new_literal_dt(self.get_term_data(txt.as_ref()), self.copy(&dt))
    }

    fn variable<T>(&mut self, name: T) -> Result<FTerm<Self>>
    where
        T: TermData,
    {
        Term::new_variable(self.get_term_data(name.as_ref()))
    }

    fn copy<T>(&mut self, other: &Term<T>) -> FTerm<Self>
    where
        T: TermData,
    {
        Term::from_with(other, |txt| self.get_term_data(txt))
    }

    fn copy_normalized<T>(&mut self, other: &Term<T>, norm: Normalization) -> FTerm<Self>
    where
        T: TermData,
    {
        Term::normalized_with(other, |txt| self.get_term_data(txt), norm)
    }

    fn shrink_to_fit(&mut self);
}

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
