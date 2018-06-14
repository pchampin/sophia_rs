// this module is transparently re-exported by its parent `term`

use std::hash::{Hash, Hasher};

use super::*;
use super::iri::*;

#[derive(Clone,Debug,Eq)]
pub struct IriTerm<T: Borrow<str>> {
    ns: T,
    suffix: Option<T>,
    absolute: bool,
}

impl<T> IriTerm<T> where
    T: Borrow<str>,
{

    pub fn new (ns: T, suffix: Option<T>) -> Result<IriTerm<T>, Err> {
        let mut ret = IriTerm{ns, suffix, absolute: false};
        match ParsedIri::new(&ret.to_string()) {
            Err(err) => Err(Err::InvalidIri(format!("{:?}", err))),
            Ok(pi) => {
                ret.absolute = pi.is_absolute();
                Ok(ret)
            }
        }
    }

    pub unsafe fn new_unchecked (ns: T, suffix: Option<T>, absolute: Option<bool>) -> IriTerm<T> {
        match absolute {
            Some(absolute) => IriTerm{ns, suffix, absolute},
            None           => IriTerm::new(ns, suffix).unwrap(),
        }
    }

    pub fn from_with<'a, U, F> (other: &'a IriTerm<U>, mut factory: F) -> IriTerm<T> where
        U: Borrow<str>,
        F: FnMut(&'a str) -> T,
    {
        let ns = factory(other.ns.borrow());
        let suffix = match other.suffix {
            Some(ref suffix) => Some(factory(suffix.borrow())),
            None => None,
        };
        IriTerm{ns, suffix, absolute: other.absolute}
    }

    fn suffix_borrow(&self) -> &str {
        match self.suffix {
            Some(ref suffix) => suffix.borrow(),
            None         => "",
        }
    }

    pub fn to_string(&self) -> String {
        let ns = self.ns.borrow();
        let suffix = self.suffix_borrow();
        let mut ret = String::with_capacity(ns.len()+suffix.len());
        ret.push_str(ns);
        ret.push_str(suffix);
        ret
    }

    pub fn is_absolute(&self) -> bool {
        self.absolute
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



impl<'a> ParsedIri<'a> {
    pub fn join_iriterm<T> (&self, iri_term: &IriTerm<T>) -> IriTerm<T> where
        T: Borrow<str> + Clone + From<String>,
    {
        let parsed_ns = ParsedIri::new(iri_term.ns.borrow()).unwrap();
        let abs_ns = T::from(self.join(&parsed_ns).to_string());
        IriTerm {
            ns: abs_ns,
            suffix: iri_term.suffix.clone(),
            absolute: true,
        }
    }
}

