// this module is transparently re-exported by its parent `term`
//
// Convenient implementation to convert usual Rust values to `Term`s.

use super::*;
use crate::ns::xsd;

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



#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string() {
        let t1 = Term::from("hello".to_string());
        let t2 = Term::new_literal_dt("hello", xsd::string.clone()).unwrap();
        assert_eq!(t1, t2);
    }

    #[test]
    fn test_str() {
        let t1 = Term::from("hello");
        let t2 = Term::new_literal_dt("hello", xsd::string.clone()).unwrap();
        assert_eq!(t1, t2);
    }

    #[test]
    fn test_bool() {
        let t1 = Term::from(true);
        let t2 = Term::new_literal_dt("true", xsd::boolean.clone()).unwrap();
        assert_eq!(t1, t2);
    }
    #[test]
    fn test_f64() {
        let t1 = Term::from(3.14_f64);
        let t2 = Term::new_literal_dt("3.14", xsd::double.clone()).unwrap();
        assert_eq!(t1, t2);
    }
    #[test]
    fn test_i32() {
        let t1 = Term::from(-42_i32);
        let t2 = Term::new_literal_dt("-42", xsd::integer.clone()).unwrap();
        assert_eq!(t1, t2);
    }
    #[test]
    fn test_u32() {
        let t1 = Term::from(42_u32);
        let t2 = Term::new_literal_dt("42", xsd::nonNegativeInteger.clone()).unwrap();
        assert_eq!(t1, t2);
    }
}