use std::rc;
use std::sync;

use weak_table::{WeakHashSet};

use super::*;

trait TermFactory {
    type Holder: Borrow<str>;

    fn get_holder(&mut self, txt: &str) -> Self::Holder;

    fn iri<T> (&mut self, iri: T) -> Result<Term<Self::Holder>, Err> where
        T: Borrow<str>,
    {
        Term::new_iri(self.get_holder(iri.borrow()))
    }

    fn iri2<T, U> (&mut self, ns: T, suffix: U) -> Result<Term<Self::Holder>, Err> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Term::new_iri2(self.get_holder(ns.borrow()), self.get_holder(suffix.borrow()))
    }

    fn bnode<T> (&mut self, id: T) -> Result<Term<Self::Holder>, Err> where
        T: Borrow<str>,
    {
        Term::new_bnode(self.get_holder(id.borrow()))
    }

    fn literal_lang<T, U> (&mut self, txt: T, lang: U) -> Result<Term<Self::Holder>, Err> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        Term::new_literal_lang(self.get_holder(txt.borrow()), self.get_holder(lang.borrow()))
    }

    fn literal_dt<T, U> (&mut self, txt: T, dt: Term<U>) -> Result<Term<Self::Holder>, Err> where
        T: Borrow<str>,
        U: Borrow<str>,
        Self::Holder: Debug,
    {
        Term::new_literal_dt(self.get_holder(txt.borrow()), self.copy(&dt))
    }

    fn variable<T> (&mut self, name: T) -> Result<Term<Self::Holder>, Err> where
        T: Borrow<str>,
    {
        Term::new_variable(self.get_holder(name.borrow()))
    }

    fn copy<T> (&mut self, other: &Term<T>) -> Term<Self::Holder> where
        T: Borrow<str>,
    {
        
        Term::from_with(other, |txt| self.get_holder(txt))
    }
}



pub type RcTermFactory = WeakHashSet<rc::Weak<str>>;

impl TermFactory for RcTermFactory {
    type Holder = Rc<str>;

    fn get_holder(&mut self, txt: &str) -> Rc<str> {
        if let Some(holder) = self.get(txt) {
            holder
        } else {
            let holder: Rc<str> = Rc::from(txt);
            self.insert(holder.clone());
            holder
        }
    }
}

pub type ArcTermFactory = WeakHashSet<sync::Weak<str>>;

impl TermFactory for ArcTermFactory {
    type Holder = sync::Arc<str>;

    fn get_holder(&mut self, txt: &str) -> sync::Arc<str> {
        if let Some(holder) = self.get(txt) {
            holder
        } else {
            let holder: sync::Arc<str> = sync::Arc::from(txt);
            self.insert(holder.clone());
            holder
        }
    }
}