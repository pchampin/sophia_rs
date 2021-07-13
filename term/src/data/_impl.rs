//! Implementation of conversions between `TermData`.
//!
//! A default `impl<T> FromData<T> for U where U: From<T>,` is not possible
//! as this may be corrupted by future changes in the std-crate.

use super::FromData;
use mownstr::MownStr;
use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;

// &str
// -ref
// impl<'a> FromData<&'a str> for &'a str {
//     fn from_data(t: &'a str) -> Self {
//         t.into()
//     }
// }
impl<'a> FromData<&'a Box<str>> for &'a str {
    fn from_data(t: &'a Box<str>) -> Self {
        t.as_ref()
    }
}
impl<'a> FromData<&'a Rc<str>> for &'a str {
    fn from_data(t: &'a Rc<str>) -> Self {
        t.as_ref()
    }
}
impl<'a> FromData<&'a Arc<str>> for &'a str {
    fn from_data(t: &'a Arc<str>) -> Self {
        t.as_ref()
    }
}
impl<'a> FromData<&'a Cow<'a, str>> for &'a str {
    fn from_data(t: &'a Cow<'a, str>) -> Self {
        t.as_ref()
    }
}
impl<'a> FromData<&'a MownStr<'a>> for &'a str {
    fn from_data(t: &'a MownStr<'a>) -> Self {
        t.as_ref()
    }
}

// String
// - owned
// impl FromData<String> for String {
//     fn from_data(t: String) -> Self {
//         t.into()
//     }
// }
impl FromData<Box<str>> for String {
    fn from_data(t: Box<str>) -> Self {
        t.into()
    }
}
impl FromData<Rc<str>> for String {
    fn from_data(t: Rc<str>) -> Self {
        t.as_ref().to_owned()
    }
}
impl FromData<Arc<str>> for String {
    fn from_data(t: Arc<str>) -> Self {
        t.as_ref().to_owned()
    }
}
impl<'a> FromData<Cow<'a, str>> for String {
    fn from_data(t: Cow<'a, str>) -> Self {
        t.into()
    }
}
impl<'a> FromData<MownStr<'a>> for String {
    fn from_data(t: MownStr<'a>) -> Self {
        t.to()
    }
}
// - ref
impl<'a> FromData<&'a str> for String {
    fn from_data(t: &'a str) -> Self {
        t.to_owned()
    }
}
impl<'a> FromData<&'a Box<str>> for String {
    fn from_data(t: &'a Box<str>) -> Self {
        t.as_ref().to_owned()
    }
}
impl<'a> FromData<&'a Rc<str>> for String {
    fn from_data(t: &'a Rc<str>) -> Self {
        t.as_ref().to_owned()
    }
}
impl<'a> FromData<&'a Arc<str>> for String {
    fn from_data(t: &'a Arc<str>) -> Self {
        t.as_ref().to_owned()
    }
}
impl<'a> FromData<&'a Cow<'a, str>> for String {
    fn from_data(t: &'a Cow<'a, str>) -> Self {
        t.as_ref().to_owned()
    }
}
impl<'a> FromData<&'a MownStr<'a>> for String {
    fn from_data(t: &'a MownStr<'a>) -> Self {
        t.as_ref().to_owned()
    }
}

// Box<str>
// - owned
impl FromData<String> for Box<str> {
    fn from_data(t: String) -> Self {
        t.into()
    }
}
// impl FromData<Box<str>> for Box<str> {
//     fn from_data(t: Box<str>) -> Self {
//         t.into()
//     }
// }
impl FromData<Rc<str>> for Box<str> {
    fn from_data(t: Rc<str>) -> Self {
        t.as_ref().into()
    }
}
impl FromData<Arc<str>> for Box<str> {
    fn from_data(t: Arc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<Cow<'a, str>> for Box<str> {
    fn from_data(t: Cow<'a, str>) -> Self {
        t.into()
    }
}
impl<'a> FromData<MownStr<'a>> for Box<str> {
    fn from_data(t: MownStr<'a>) -> Self {
        t.to()
    }
}
// - ref
impl<'a> FromData<&'a str> for Box<str> {
    fn from_data(t: &'a str) -> Self {
        t.into()
    }
}
impl<'a> FromData<&'a Box<str>> for Box<str> {
    fn from_data(t: &'a Box<str>) -> Self {
        t.clone()
    }
}
impl<'a> FromData<&'a Rc<str>> for Box<str> {
    fn from_data(t: &'a Rc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Arc<str>> for Box<str> {
    fn from_data(t: &'a Arc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Cow<'a, str>> for Box<str> {
    fn from_data(t: &'a Cow<'a, str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a MownStr<'a>> for Box<str> {
    fn from_data(t: &'a MownStr<'a>) -> Self {
        t.as_ref().into()
    }
}

// Rc<str>
// - owned
impl FromData<String> for Rc<str> {
    fn from_data(t: String) -> Self {
        t.into()
    }
}
impl FromData<Box<str>> for Rc<str> {
    fn from_data(t: Box<str>) -> Self {
        t.into()
    }
}
// impl FromData<Rc<str>> for Rc<str> {
//     fn from_data(t: Rc<str>) -> Self {
//         t.as_ref().into()
//     }
// }
impl FromData<Arc<str>> for Rc<str> {
    fn from_data(t: Arc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<Cow<'a, str>> for Rc<str> {
    fn from_data(t: Cow<'a, str>) -> Self {
        t.into()
    }
}
impl<'a> FromData<MownStr<'a>> for Rc<str> {
    fn from_data(t: MownStr<'a>) -> Self {
        t.to()
    }
}
// - ref
impl<'a> FromData<&'a str> for Rc<str> {
    fn from_data(t: &'a str) -> Self {
        t.into()
    }
}
impl<'a> FromData<&'a Box<str>> for Rc<str> {
    fn from_data(t: &'a Box<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Rc<str>> for Rc<str> {
    fn from_data(t: &'a Rc<str>) -> Self {
        t.clone()
    }
}
impl<'a> FromData<&'a Arc<str>> for Rc<str> {
    fn from_data(t: &'a Arc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Cow<'a, str>> for Rc<str> {
    fn from_data(t: &'a Cow<'a, str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a MownStr<'a>> for Rc<str> {
    fn from_data(t: &'a MownStr<'a>) -> Self {
        t.as_ref().into()
    }
}

// Arc<str>
// - owned
impl FromData<String> for Arc<str> {
    fn from_data(t: String) -> Self {
        t.into()
    }
}
impl FromData<Box<str>> for Arc<str> {
    fn from_data(t: Box<str>) -> Self {
        t.into()
    }
}
impl FromData<Rc<str>> for Arc<str> {
    fn from_data(t: Rc<str>) -> Self {
        t.as_ref().into()
    }
}
// impl FromData<Arc<str>> for Arc<str> {
//     fn from_data(t: Arc<str>) -> Self {
//         t.as_ref().into()
//     }
// }
impl<'a> FromData<Cow<'a, str>> for Arc<str> {
    fn from_data(t: Cow<'a, str>) -> Self {
        t.into()
    }
}
impl<'a> FromData<MownStr<'a>> for Arc<str> {
    fn from_data(t: MownStr<'a>) -> Self {
        t.to()
    }
}
// - ref
impl<'a> FromData<&'a str> for Arc<str> {
    fn from_data(t: &'a str) -> Self {
        t.into()
    }
}
impl<'a> FromData<&'a Box<str>> for Arc<str> {
    fn from_data(t: &'a Box<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Rc<str>> for Arc<str> {
    fn from_data(t: &'a Rc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Arc<str>> for Arc<str> {
    fn from_data(t: &'a Arc<str>) -> Self {
        t.clone()
    }
}
impl<'a> FromData<&'a Cow<'a, str>> for Arc<str> {
    fn from_data(t: &'a Cow<'a, str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a MownStr<'a>> for Arc<str> {
    fn from_data(t: &'a MownStr<'a>) -> Self {
        t.as_ref().into()
    }
}

// Cow<'a, str>
// - owned
impl<'a> FromData<String> for Cow<'a, str> {
    fn from_data(t: String) -> Self {
        t.into()
    }
}
impl<'a> FromData<Box<str>> for Cow<'a, str> {
    fn from_data(t: Box<str>) -> Self {
        let s: String = t.into();
        s.into()
    }
}
impl<'a> FromData<Rc<str>> for Cow<'a, str> {
    fn from_data(t: Rc<str>) -> Self {
        let s = t.as_ref().to_owned();
        s.into()
    }
}
impl<'a> FromData<Arc<str>> for Cow<'a, str> {
    fn from_data(t: Arc<str>) -> Self {
        let s = t.as_ref().to_owned();
        s.into()
    }
}
// impl<'a> FromData<Cow<'a,str>> for Cow<'a, str> {
//     fn from_data(t: Cow<'a, str>) -> Self {
//         t.into()
//     }
// }
impl<'a> FromData<MownStr<'a>> for Cow<'a, str> {
    fn from_data(t: MownStr<'a>) -> Self {
        t.to::<String>().into()
    }
}
// - ref
impl<'a> FromData<&'a str> for Cow<'a, str> {
    fn from_data(t: &'a str) -> Self {
        t.into()
    }
}
impl<'a> FromData<&'a Box<str>> for Cow<'a, str> {
    fn from_data(t: &'a Box<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Rc<str>> for Cow<'a, str> {
    fn from_data(t: &'a Rc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Arc<str>> for Cow<'a, str> {
    fn from_data(t: &'a Arc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Cow<'a, str>> for Cow<'a, str> {
    fn from_data(t: &'a Cow<'a, str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a MownStr<'a>> for Cow<'a, str> {
    fn from_data(t: &'a MownStr<'a>) -> Self {
        t.as_ref().into()
    }
}

// MownStr<'a>
// - owned
impl<'a> FromData<String> for MownStr<'a> {
    fn from_data(t: String) -> Self {
        t.into()
    }
}
impl<'a> FromData<Box<str>> for MownStr<'a> {
    fn from_data(t: Box<str>) -> Self {
        t.into()
    }
}
impl<'a> FromData<Rc<str>> for MownStr<'a> {
    fn from_data(t: Rc<str>) -> Self {
        Box::<str>::from(t.as_ref()).into()
    }
}
impl<'a> FromData<Arc<str>> for MownStr<'a> {
    fn from_data(t: Arc<str>) -> Self {
        Box::<str>::from(t.as_ref()).into()
    }
}
impl<'a> FromData<Cow<'a, str>> for MownStr<'a> {
    fn from_data(t: Cow<'a, str>) -> Self {
        t.into()
    }
}
// impl<'a> FromData<MownStr<'a>> for MownStr<'a> {
//     fn from_data(t: MownStr<'a>) -> Self {
//         t.to::<String>().into()
//     }
// }
// - ref
impl<'a> FromData<&'a str> for MownStr<'a> {
    fn from_data(t: &'a str) -> Self {
        t.into()
    }
}
impl<'a> FromData<&'a Box<str>> for MownStr<'a> {
    fn from_data(t: &'a Box<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Rc<str>> for MownStr<'a> {
    fn from_data(t: &'a Rc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Arc<str>> for MownStr<'a> {
    fn from_data(t: &'a Arc<str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a Cow<'a, str>> for MownStr<'a> {
    fn from_data(t: &'a Cow<'a, str>) -> Self {
        t.as_ref().into()
    }
}
impl<'a> FromData<&'a MownStr<'a>> for MownStr<'a> {
    fn from_data(t: &'a MownStr<'a>) -> Self {
        t.as_ref().into()
    }
}
