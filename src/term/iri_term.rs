// this module is transparently re-exported by its parent `term`

use std::hash::{Hash, Hasher};

use super::*;
use super::iri::*;

#[derive(Clone,Debug,Eq)]
pub struct IriTerm<T: Borrow<str>> {
    pub(crate) ns: T,
    pub(crate) suffix: Option<T>,
    pub(crate) absolute: bool,
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

    pub fn normalized_with<'a, U, F> (other: &'a IriTerm<U>, factory: F, norm: Normalization) -> IriTerm<T> where
        U: Borrow<str>,
        F: FnMut(&str) -> T,
    {
        match norm {
            Normalization::NoSuffix
                => Self::no_suffix_with(other, factory),
            Normalization::LastHashOrSlash
                => Self::last_hash_or_slash_with(other, factory),
        }
    }

    fn no_suffix_with<'a, U, F> (other: &'a IriTerm<U>, mut factory: F) -> IriTerm<T> where
        U: Borrow<str>,
        F: FnMut(&str) -> T,
    {
        let ns = match other.suffix {
            Some(_) => factory(&other.to_string()),
            None => factory(other.ns.borrow()),
        };
        IriTerm{ns, suffix: None, absolute: other.absolute}
    }

    fn last_hash_or_slash_with<'a, U, F> (other: &'a IriTerm<U>, mut factory: F) -> IriTerm<T> where
        U: Borrow<str>,
        F: FnMut(&str) -> T,
    {
        let sep = ['#', '/'];
        let ns = other.ns.borrow();
        let absolute = other.absolute;
        if let Some(ref suffix) = other.suffix {
            let suffix = suffix.borrow();
            if let Some(spos) = suffix.rfind(&sep[..]) {
                let mut new_ns = String::with_capacity(ns.len() + spos+1);
                new_ns.push_str(ns);
                new_ns.push_str(&suffix[..spos+1]);
                IriTerm {
                    ns: factory(&new_ns),
                    suffix: Some(factory(&suffix[spos+1..])),
                    absolute,
                }
            } else if let Some(npos) = ns.rfind(&sep[..]) {
                let mut new_suffix = String::with_capacity(ns.len()-npos-1 + suffix.len());
                new_suffix.push_str(&ns[npos+1..]);
                new_suffix.push_str(suffix);
                IriTerm {
                    ns: factory(&ns[..npos+1]),
                    suffix: Some(factory(&new_suffix)),
                    absolute,
                }
            } else {
                IriTerm {
                    ns: factory(&other.to_string()),
                    suffix: None,
                    absolute,
                }
            }
        } else {
            if let Some(npos) = ns.rfind(&sep[..]) {
                IriTerm {
                    ns: factory(&ns[..npos+1]),
                    suffix: Some(factory(&ns[npos+1..])),
                    absolute,
                }
            } else {
                IriTerm {
                    ns: factory(ns),
                    suffix: None,
                    absolute,
                }
            }
        }
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



// TODO document
#[derive(Clone,Copy)]
pub enum Normalization {
    NoSuffix,
    LastHashOrSlash,
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

