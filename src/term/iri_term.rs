// this module is transparently re-exported by its parent `term`

use std::hash::{Hash, Hasher};

use super::*;

#[derive(Clone,Debug,Eq)]
pub struct IriTerm<T: Borrow<str>> {
    ns: T,
    suffix: Option<T>,
}

impl<T> IriTerm<T> where
    T: Borrow<str>,
{
    fn suffix_borrow(&self) -> &str {
        match self.suffix {
            Some(ref suffix) => suffix.borrow(),
            None         => "",
        }
    }

    pub fn value(&self) -> String {
        let ns = self.ns.borrow();
        let suffix = self.suffix_borrow();
        let mut ret = String::with_capacity(ns.len()+suffix.len());
        ret.push_str(ns);
        ret.push_str(suffix);
        ret
    }
}

impl<T> IriTerm<T> where
    T: Borrow<str>,
{

    pub fn new (ns: T, suffix: Option<T>) -> Result<IriTerm<T>, Err> {
        let ret = IriTerm{ns, suffix};
        if let Err(err) = Url::parse(&ret.value()) {
            return Err(Err::InvalidIri(err));
        }
        Ok(ret)
    }

    pub unsafe fn new_trusted (ns: T, suffix: Option<T>) -> IriTerm<T> {
        IriTerm{ns, suffix}
    }

    pub fn copy_with<'a, U, F> (other: &'a IriTerm<U>, factory: &mut F) -> IriTerm<T> where
        U: Borrow<str>,
        F: FnMut(&'a str) -> T,
    {
        let ns = factory(other.ns.borrow());
        let suffix = match other.suffix {
            Some(ref suffix) => Some(factory(suffix.borrow())),
            None => None,
        };
        IriTerm{ns, suffix}
    }
}

impl<T, U> PartialEq<IriTerm<U>> for IriTerm<T> where
    T: Borrow<str>,
    U: Borrow<str>,
{
    fn eq(&self, other: &IriTerm<U>) -> bool {
        let s_ns = self.ns.borrow();
        let s_sf = self.suffix_borrow();
        let o_ns = other.ns.borrow();
        let o_sf = other.suffix_borrow();
        (s_ns.len() + s_sf.len()) == (o_ns.len() + o_sf.len())
        &&
        {
            let mut eq = true;
            let it1 = s_ns.chars().chain(s_sf.chars());
            let it2 = o_ns.chars().chain(o_sf.chars());
            for (c1,c2) in it1.zip(it2) {
                if c1 != c2 { eq = false; break; }
            }
            eq
        }
    }
}

impl<'a, T> PartialEq<&'a str> for IriTerm<T> where
    T: Borrow<str>,
{
    fn eq(&self, other: &&'a str) -> bool {
        let s_ns = self.ns.borrow();
        let s_sf = self.suffix_borrow();
        (s_ns.len() + s_sf.len()) == (other.len())
        &&
        {
            let mut eq = true;
            let it1 = s_ns.chars().chain(s_sf.chars());
            let it2 = other.chars();
            for (c1,c2) in it1.zip(it2) {
                if c1 != c2 { eq = false; break; }
            }
            eq
        }
    }
}

impl<T> Hash for IriTerm<T> where
    T: Borrow<str>,
{
    fn hash<H:Hasher> (&self, state: &mut H) {
        state.write(self.ns.borrow().as_bytes());
        state.write(self.suffix_borrow().as_bytes());
        state.write_u8(0xff);
    }
}

