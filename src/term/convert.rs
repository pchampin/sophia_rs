// this module is transparently re-exported by its parent `term`
//
// Convenient implementation to convert usual Rust values to `Term`s.

use super::*;
use ::ns::xsd;

impl From<String> for Term<String> {
    fn from(val: String) -> Term<String> {
        let dt = Term::from(&xsd::string);
        unsafe { Term::new_literal_dt_unchecked(val, dt) }
    }
}

impl<'a> From<&'a str> for Term<&'a str> {
    fn from(val: &'a str) -> Term<&'a str> {
        let dt = xsd::string.clone();
        unsafe { Term::new_literal_dt_unchecked(val, dt) }
    }
}

impl From<bool> for Term<String> {
    fn from(val: bool) -> Term<String> {
        let txt = val.to_string();
        let dt = Term::from(&xsd::boolean);
        unsafe { Term::new_literal_dt_unchecked(txt, dt) }
    }
}

impl From<f64> for Term<String> {
    fn from(val: f64) -> Term<String> {
        let txt = val.to_string();
        let dt = Term::from(&xsd::double);
        unsafe { Term::new_literal_dt_unchecked(txt, dt) }
    }
}

impl From<i32> for Term<String> {
    fn from(val: i32) -> Term<String> {
        let txt = val.to_string();
        let dt = Term::from(&xsd::integer);
        unsafe { Term::new_literal_dt_unchecked(txt, dt) }
    }
}

impl From<u32> for Term<String> {
    fn from(val: u32) -> Term<String> {
        let txt = val.to_string();
        let dt = Term::from(&xsd::nonNegativeInteger);
        unsafe { Term::new_literal_dt_unchecked(txt, dt) }
    }
}

