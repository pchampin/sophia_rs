// TODO properly document this

use std::borrow::Borrow;

use ::term::*;

pub trait Triple<T: Borrow<str>> {
    fn s(&self) -> &Term<T>;
    fn p(&self) -> &Term<T>;
    fn o(&self) -> &Term<T>;
}

impl<'a, T: Borrow<str>> Triple<T> for (&'a Term<T>, &'a Term<T>, &'a Term<T>) {
    #[inline] fn s(&self) -> &Term<T> { self.0 }
    #[inline] fn p(&self) -> &Term<T> { self.1 }
    #[inline] fn o(&self) -> &Term<T> { self.2 }
}

impl<T: Borrow<str>> Triple<T> for (Term<T>, Term<T>, Term<T>) {
    #[inline] fn s(&self) -> &Term<T> { &self.0 }
    #[inline] fn p(&self) -> &Term<T> { &self.1 }
    #[inline] fn o(&self) -> &Term<T> { &self.2 }
}
