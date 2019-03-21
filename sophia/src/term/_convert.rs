// this module is transparently re-exported by its parent `term`
//
// Convenient implementation to convert usual Rust values to `Term`s.

use super::*;
use crate::ns::xsd;

impl From<String> for BoxTerm {
    fn from(val: String) -> BoxTerm {
        let dt = BoxTerm::from(&xsd::string);
        unsafe { BoxTerm::new_literal_dt_unchecked(val, dt) }
    }
}

impl<'a> From<&'a str> for RefTerm<'a> {
    fn from(val: &'a str) -> RefTerm<'a> {
        let dt = xsd::string;
        unsafe { RefTerm::new_literal_dt_unchecked(val, dt) }
    }
}

impl From<bool> for BoxTerm {
    fn from(val: bool) -> BoxTerm {
        let txt = val.to_string().into_boxed_str();
        let dt = BoxTerm::from(&xsd::boolean);
        unsafe { BoxTerm::new_literal_dt_unchecked(txt, dt) }
    }
}

impl From<f64> for BoxTerm {
    fn from(val: f64) -> BoxTerm {
        let txt = val.to_string();
        let dt = BoxTerm::from(&xsd::double);
        unsafe { BoxTerm::new_literal_dt_unchecked(txt, dt) }
    }
}

impl From<i32> for BoxTerm {
    fn from(val: i32) -> BoxTerm {
        let txt = val.to_string();
        let dt = BoxTerm::from(&xsd::integer);
        unsafe { BoxTerm::new_literal_dt_unchecked(txt, dt) }
    }
}

impl From<u32> for BoxTerm {
    fn from(val: u32) -> BoxTerm {
        let txt = val.to_string();
        let dt = BoxTerm::from(&xsd::nonNegativeInteger);
        unsafe { BoxTerm::new_literal_dt_unchecked(txt, dt) }
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
