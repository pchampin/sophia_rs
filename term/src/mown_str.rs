//! [`MownStr`](./enum.MownStr.html)
//! is either a borrowed reference to a `str` or an own `Box<str>`.

use std::borrow::Cow;
use std::fmt;
use std::hash;
use std::ops::Deref;

/// "Maybe own str":
/// either a borrowed reference to a `str` or an own `Box<str>`.
///
/// It does not try to be mutable, nor generic,
/// which makes it lighter than, for example, `Cow<str>`.
#[derive(Clone)]
pub enum MownStr<'a> {
    /// Variant simply borrowing `str`
    Ref(&'a str),
    /// Variany owning `str` into a box
    Own(Box<str>),
}

use MownStr::*;

// Construct a MownStr

impl<'a> From<&'a str> for MownStr<'a> {
    fn from(other: &'a str) -> MownStr<'a> {
        Ref(other)
    }
}

impl<'a> From<Box<str>> for MownStr<'a> {
    fn from(other: Box<str>) -> MownStr<'a> {
        Own(other)
    }
}

impl<'a> From<String> for MownStr<'a> {
    fn from(other: String) -> MownStr<'a> {
        Own(other.into())
    }
}

impl<'a> From<Cow<'a, str>> for MownStr<'a> {
    fn from(other: Cow<'a, str>) -> MownStr<'a> {
        match other {
            Cow::Borrowed(r) => Ref(r),
            Cow::Owned(s) => Own(s.into()),
        }
    }
}

// Using a MownStr as a str

impl<'a> Deref for MownStr<'a> {
    type Target = str;
    fn deref(&self) -> &str {
        match self {
            Ref(r) => r,
            Own(b) => &b,
        }
    }
}

impl<'a> AsRef<str> for MownStr<'a> {
    fn as_ref(&self) -> &str {
        &*self
    }
}

impl<'a> std::borrow::Borrow<str> for MownStr<'a> {
    fn borrow(&self) -> &str {
        &*self
    }
}

// Comparing between MownStr

impl<'a> hash::Hash for MownStr<'a> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state)
    }
}

impl<'a> PartialEq for MownStr<'a> {
    fn eq(&self, other: &MownStr<'a>) -> bool {
        self.deref() == other.deref()
    }
}

impl<'a> Eq for MownStr<'a> {}

impl<'a> PartialOrd for MownStr<'a> {
    fn partial_cmp(&self, other: &MownStr<'a>) -> Option<std::cmp::Ordering> {
        self.deref().partial_cmp(other.deref())
    }
}

impl<'a> Ord for MownStr<'a> {
    fn cmp(&self, other: &MownStr<'a>) -> std::cmp::Ordering {
        self.deref().cmp(other.deref())
    }
}

// Comparing MownStr with str

impl<'a> PartialEq<&'a str> for MownStr<'a> {
    fn eq(&self, other: &&'a str) -> bool {
        self.deref() == *other
    }
}

impl<'a> PartialOrd<&'a str> for MownStr<'a> {
    fn partial_cmp(&self, other: &&'a str) -> Option<std::cmp::Ordering> {
        self.deref().partial_cmp(*other)
    }
}

impl<'a> PartialEq<MownStr<'a>> for &'a str {
    fn eq(&self, other: &MownStr<'a>) -> bool {
        self == &other.deref()
    }
}

impl<'a> PartialOrd<MownStr<'a>> for &'a str {
    fn partial_cmp(&self, other: &MownStr<'a>) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.deref())
    }
}

// Formatting

impl<'a> fmt::Debug for MownStr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.deref(), f)
    }
}

impl<'a> fmt::Display for MownStr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.deref(), f)
    }
}

// Converting

impl<'a> From<MownStr<'a>> for Box<str> {
    fn from(other: MownStr<'a>) -> Box<str> {
        match other {
            Ref(r) => Box::from(r),
            Own(b) => b,
        }
    }
}

impl<'a> From<MownStr<'a>> for String {
    fn from(other: MownStr<'a>) -> String {
        match other {
            Ref(r) => String::from(r),
            Own(b) => b.into(),
        }
    }
}

impl<'a> From<MownStr<'a>> for Cow<'a, str> {
    fn from(other: MownStr<'a>) -> Cow<'a, str> {
        match other {
            Ref(r) => Cow::Borrowed(r),
            Own(b) => Cow::Owned(b.into()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn test_build_ref() {
        let mown: MownStr = "hello".into();
        assert!(match mown {
            Ref(_) => true,
            Own(_) => false,
        });
    }

    #[test]
    fn test_build_own_from_box() {
        let bx: Box<str> = "hello".into();
        let mown: MownStr = bx.into();
        assert!(match mown {
            Ref(_) => false,
            Own(_) => true,
        });
    }

    #[test]
    fn test_build_own_from_string() {
        let mown: MownStr = "hello".to_string().into();
        assert!(match mown {
            Ref(_) => false,
            Own(_) => true,
        });
    }

    #[test]
    fn test_build_ref_from_cow() {
        let mown: MownStr = Cow::Borrowed("hello").into();
        assert!(match mown {
            Ref(_) => true,
            Own(_) => false,
        });
    }

    #[test]
    fn test_build_own_from_cow() {
        let mown: MownStr = Cow::<str>::Owned("hello".to_string()).into();
        assert!(match mown {
            Ref(_) => false,
            Own(_) => true,
        });
    }

    #[test]
    fn test_deref() {
        let txt = "hello";
        let mown1: MownStr = txt.into();
        assert_eq!(&*mown1, txt);
        assert_eq!(&mown1[..], txt);
        let mown2: MownStr = txt.to_string().into();
        assert_eq!(&*mown2, txt);
        assert_eq!(&mown2[..], txt);
    }

    #[test]
    fn test_hash() {
        let txt = "hello";
        let mown1: MownStr = txt.into();
        let mown2: MownStr = txt.to_string().into();

        let mut set = HashSet::new();
        set.insert(mown1.clone());
        assert!(set.contains(&mown1));
        assert!(set.contains(&mown2));
        assert!(set.contains(txt));

        let mut set = HashSet::new();
        set.insert(mown2.clone());
        assert!(set.contains(&mown1));
        assert!(set.contains(&mown2));
        assert!(set.contains(txt));
    }

    #[test]
    fn test_eq() {
        let txt = "hello";
        let mown1: MownStr = txt.into();
        let mown2: MownStr = txt.to_string().into();

        assert_eq!(mown1, txt);
        assert_eq!(mown1, mown1);
        assert_eq!(mown1, mown2);
        assert_eq!(mown2, txt);
        assert_eq!(mown2, mown1);
        assert_eq!(mown2, mown2);
        assert_eq!(txt, mown1);
        assert_eq!(txt, mown2);
    }

    #[test]
    fn test_order() {
        let txt = "hello";
        let mown1: MownStr = txt[..4].into();
        let mown2: MownStr = txt[..3].to_string().into();

        assert!(mown1 <= txt);
        assert!(mown1 <= mown1);
        assert!(mown1 >= mown2);
        assert!(mown2 <= txt);
        assert!(mown2 <= mown1);
        assert!(mown2 >= mown2);
        assert!(txt >= mown1);
        assert!(txt >= mown2);
    }

    #[test]
    fn test_display() {
        let mown1: MownStr = "hello".into();
        let mown2: MownStr = "hello".to_string().into();
        assert_eq!(format!("{:?}", mown1), "\"hello\"");
        assert_eq!(format!("{:?}", mown2), "\"hello\"");
        assert_eq!(format!("{}", mown1), "hello");
        assert_eq!(format!("{}", mown2), "hello");
    }
}
