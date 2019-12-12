//! Implementation of IRIs as per [\[RFC 3987\]](https://tools.ietf.org/html/rfc3987).

use regex::Regex;
use std::fmt;

/// Error caused by IRI parsing
#[derive(Clone, Copy, Debug)]
pub struct IriError<'a>(&'a str);

impl std::error::Error for IriError<'_> {}

impl fmt::Display for IriError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Error while parsing IRI <{}>", self.0)
    }
}

/// Check whether txt is a valid (absolute or relative) IRI reference.
#[inline]
pub fn is_valid_iri_ref(txt: &str) -> bool {
    IRI_REGEX.is_match(txt) || IRELATIVE_REF_REGEX.is_match(txt)
}

/// Check whether txt is an absolute IRI reference.
#[inline]
pub fn is_absolute_iri_ref(txt: &str) -> bool {
    IRI_REGEX.is_match(txt)
}

/// Check whether txt is a relative IRI reference.
#[inline]
pub fn is_relative_iri_ref(txt: &str) -> bool {
    IRELATIVE_REF_REGEX.is_match(txt)
}

/// Keeps track of the different components of an IRI reference.
///
/// NB: this type does not store the actual text of the IRI reference,
/// it borrows it from one (or possibly several) external `str`s.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct IriRefStructure<'a> {
    scheme: Option<&'a str>,
    authority: Option<&'a str>,
    path: Vec<&'a str>,
    query: Option<&'a str>,
    fragment: Option<&'a str>,
}

// NB: path complies with the following rules:
// - does not contain the seperators ('/')
// - its first element is "" if the path starts with '/'
// - its last element is "" if the path ends with a '/'

impl<'a> IriRefStructure<'a> {
    /// Parse the given `str` as an IRI reference,
    /// and return its inner structure (or fail with an `IriError`).
    pub fn new(txt: &'a str) -> Result<IriRefStructure<'a>, IriError<'a>> {
        let mut pi = IriRefStructure::default();
        let path: Option<&str>;
        if let Some(cap) = IRI_REGEX.captures(txt) {
            pi.scheme = cap.get(1).map(|m| m.as_str());
            pi.authority = cap.get(2).map(|m| m.as_str());
            pi.query = cap.get(6).map(|m| m.as_str());
            pi.fragment = cap.get(7).map(|m| m.as_str());
            path = cap
                .get(3)
                .or_else(|| cap.get(4))
                .or_else(|| cap.get(5))
                .map(|m| m.as_str())
                .filter(|s| !s.is_empty());
        } else if let Some(cap) = IRELATIVE_REF_REGEX.captures(txt) {
            pi.authority = cap.get(1).map(|m| m.as_str());
            pi.query = cap.get(5).map(|m| m.as_str());
            pi.fragment = cap.get(6).map(|m| m.as_str());
            path = cap
                .get(2)
                .or_else(|| cap.get(3))
                .or_else(|| cap.get(4))
                .map(|m| m.as_str())
                .filter(|s| !s.is_empty());
        } else {
            return Err(IriError(txt));
        }
        if let Some(path) = path {
            path.split('/').for_each(|i| pi.path.push(i))
        }
        Ok(pi)
    }

    /// Return `true` if this IRI reference is absolute.
    pub fn is_absolute(&self) -> bool {
        self.scheme.is_some()
    }

    /// Resolve `iri_ref` using this IRI reference as the base.
    ///
    /// NB: the resulting `IriRefStructure`
    /// may borrow parts from both parts.
    pub fn join(&self, iri_ref: &IriRefStructure<'a>) -> IriRefStructure<'a> {
        let (scheme, authority, query, fragment);
        let mut path;
        if iri_ref.scheme.is_some() {
            scheme = iri_ref.scheme;
            authority = iri_ref.authority;
            path = iri_ref.path.clone();
            remove_dot_segments(&mut path);
            query = iri_ref.query;
        } else {
            scheme = self.scheme;
            if iri_ref.authority.is_some() {
                authority = iri_ref.authority;
                path = iri_ref.path.clone();
                remove_dot_segments(&mut path);
                query = iri_ref.query;
            } else {
                authority = self.authority;
                if iri_ref.path.is_empty() {
                    path = self.path.clone();
                    query = iri_ref.query.or(self.query);
                } else {
                    if iri_ref.path[0] == "" {
                        path = iri_ref.path.clone();
                        remove_dot_segments(&mut path);
                    } else {
                        path = merge(&self, &iri_ref.path);
                        remove_dot_segments(&mut path);
                    }
                    query = iri_ref.query;
                }
            }
        }
        fragment = iri_ref.fragment;
        IriRefStructure {
            scheme,
            authority,
            path,
            query,
            fragment,
        }
    }
}

impl fmt::Display for IriRefStructure<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(scheme) = self.scheme {
            write!(f, "{}:", scheme)?;
        }
        if let Some(authority) = self.authority {
            write!(f, "//{}", authority)?;
        }
        if !self.path.is_empty() {
            write!(f, "{}", self.path[0])?;
            for p in &self.path[1..] {
                write!(f, "/{}", p)?;
            }
        }
        if let Some(query) = self.query {
            write!(f, "?{}", query)?;
        }
        if let Some(fragment) = self.fragment {
            write!(f, "#{}", fragment)?;
        }
        Ok(())
    }
}

fn merge<'a>(base: &IriRefStructure<'a>, path: &[&'a str]) -> Vec<&'a str> {
    let mut v = Vec::new();
    if base.authority.is_some() && base.path.is_empty() {
        v.push(""); // resulting path must have a leading '/'
    }
    v.extend(base.path.iter().take(base.path.len() - 1).cloned());
    v.extend(path.iter().cloned());
    v
}

fn remove_dot_segments(path: &mut Vec<&str>) {
    if path.is_empty() {
        return;
    }
    let mut i = 0;
    let last = path[path.len() - 1];
    if last == "." || last == ".." {
        path.push("");
    }
    while i < path.len() {
        if path[i] == "." {
            path.remove(i);
        } else if path[i] == ".." {
            if i != 0 && (i != 1 || path[0] != "") {
                path.remove(i - 1);
                i -= 1;
            }
            path.remove(i);
        } else {
            i += 1;
        }
    }
}

lazy_static! {
    static ref IRI_REGEX: Regex = Regex::new(r"(?x)^
        #scheme
       ( # CAPTURE scheme
        [A-Za-z] [-A-Za-z0-9+.]*
       )
        :
        #ihier_part
        (?: #iauthority + ipath_abempty
          //
         ( # CAPTURE iauthority
          (?: # iuserinfo
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:]
          |
            %[0-9a-fA-F]{2}
          )*
          @
          )?
          # ihost
          (?: # ip_literal
             \[
            (?: # ipv6address
              (?:
                (?:[0-9a-fA-F]{1,4}:){6}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                ::
                (?:[0-9a-fA-F]{1,4}:){5}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){4}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,1}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){3}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,2}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){2}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,3}:[0-9a-fA-F]{1,4})?
                ::
                [0-9a-fA-F]{1,4}:
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,4}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,5}:[0-9a-fA-F]{1,4})?
                ::
                [0-9a-fA-F]{1,4}
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,6}:[0-9a-fA-F]{1,4})?
                ::
              )
            | # ipvfuture
              v[0-9a-fA-F]+ \. [-A-Za-z0-9._~!$&'()*+,;=:]+
            )
             \]
          | # ipv4address
            (?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5])) (?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3}
          | # ireg_name
              (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=]
              | %[0-9a-fA-F]{2}
              )*
          )
          (?:
            :
            [0-9]* # port
          )?
         )
          #ipath_abempty
         ( # CAPTURE ipath_abempty
          (?:
            /
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
          )*
         )
        | #ipath_absolute
         ( # CAPTURE ipath_absolute
          /
          (?:
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
            (?:
              /
              (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
              | %[0-9a-fA-F]{2}
              )*
            )*
          )?
         )
        | #ipath_rootless
         ( # CAPTURE ipath_rootless
          (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
          | %[0-9a-fA-F]{2}
          )+
          (?:
            /
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
          )*
         )
        )? # optional because of ipath_empty
        (?: # ?iquery
          \?
         ( # CAPTURE iquery
          (?:
            [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@'\u{E000}-\u{F8FF}\u{F0000}-\u{FFFFD}\u{100000}-\u{10FFFD}/?]
            | %[0-9a-fA-F]{2}
          )*
         )
        )?
        (?: # #ifragment
          \#
         ( # CAPTURE ifragment
          (?:
            [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@/?]
            | %[0-9a-fA-F]{2}
          )*
         )
        )?
    $").unwrap();

    static ref IRELATIVE_REF_REGEX: Regex = Regex::new(r"(?x)^
        #irelative_part
        (?: #iauthority + ipath_abempty
          //
         ( # CAPTURE iauthority
          (?: # iuserinfo
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:]
          |
            %[0-9a-fA-F]{2}
          )*
          @
          )?
          # ihost
          (?: # ip_literal
             \[
            (?: # ipv6address
              (?:
                (?:[0-9a-fA-F]{1,4}:){6}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                ::
                (?:[0-9a-fA-F]{1,4}:){5}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){4}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,1}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){3}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,2}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){2}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,3}:[0-9a-fA-F]{1,4})?
                ::
                [0-9a-fA-F]{1,4}:
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,4}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,5}:[0-9a-fA-F]{1,4})?
                ::
                [0-9a-fA-F]{1,4}
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,6}:[0-9a-fA-F]{1,4})?
                ::
              )
            | # ipvfuture
              v[0-9a-fA-F]+ \. [-A-Za-z0-9._~!$&'()*+,;=:]+
            )
             \]
          | # ipv4address
            (?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5])) (?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3}
          | # ireg_name
              (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=]
              | %[0-9a-fA-F]{2}
              )*
          )
          (?:
            :
            [0-9]* # port
          )?
         )
          #ipath_abempty
         ( # CAPTURE ipath_abempty
          (?:
            /
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
          )*
         )
        | #ipath_absolute
         ( # CAPTURE ipath_absolute
          /
          (?:
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
            (?:
              /
              (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
              | %[0-9a-fA-F]{2}
              )*
            )*
          )?
         )
        | #ipath_noscheme
         ( # CAPTURE ipath_noscheme
          (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=@]
          | %[0-9a-fA-F]{2}
          )+
          (?:
            /
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
          )*
         )
        )? # optional because of ipath_empty
        (?: # ?iquery
          \?
         ( # CAPTURE iquery
          (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@'\u{E000}-\u{F8FF}\u{F0000}-\u{FFFFD}\u{100000}-\u{10FFFD}/?]
          | %[0-9a-fA-F]{2}
          )*
         )
        )?
        (?: # #ifragment
            \#
         ( # CAPTURE ifragment
          (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@/?]
          | %[0-9a-fA-F]{2}
          )*
         )
        )?
    $").unwrap();
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn positive() {
        for (txt, parsed) in POSITIVE_IRIS {
            let rpi = IriRefStructure::new(txt);
            assert!(rpi.is_ok(), format!("<{}> → {:?}", txt, rpi));
            let pi = rpi.unwrap();
            assert_eq!(pi.is_absolute(), parsed.0);
            assert_eq!(pi.scheme, parsed.1);
            assert_eq!(pi.authority, parsed.2);
            assert_eq!(&pi.path[..], parsed.3);
            assert_eq!(pi.query, parsed.4);
            assert_eq!(pi.fragment, parsed.5);
            assert_eq!(&pi.to_string(), txt);
        }
    }

    #[test]
    fn negative() {
        for txt in NEGATIVE_IRIS {
            let rpi = IriRefStructure::new(txt);
            assert!(rpi.is_err(), format!("<{}> → {:?}", txt, rpi));
        }
    }

    #[test]
    fn relative() {
        let base = IriRefStructure::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = IriRefStructure::new(rel).unwrap();
            let gpt = base.join(&rel);
            assert_eq!(&gpt.to_string(), abs);
        }
    }

    #[test]
    fn regex_abs() {
        for (txt, parsed) in POSITIVE_IRIS {
            assert_eq!(IRI_REGEX.is_match(txt), parsed.0);
        }
        for txt in NEGATIVE_IRIS {
            assert!(!IRI_REGEX.is_match(txt));
        }
    }

    #[test]
    fn regex_rel() {
        for (txt, parsed) in POSITIVE_IRIS {
            assert_eq!(IRELATIVE_REF_REGEX.is_match(txt), !parsed.0);
        }
        for txt in NEGATIVE_IRIS {
            assert!(!IRELATIVE_REF_REGEX.is_match(txt));
        }
    }

    const POSITIVE_IRIS: &[(
        &str,
        (
            bool,
            Option<&str>,
            Option<&str>,
            &[&str],
            Option<&str>,
            Option<&str>,
        ),
    )] = &[
        ("http:", (true, Some("http"), None, &[], None, None)),
        (
            "http://example.org",
            (true, Some("http"), Some("example.org"), &[], None, None),
        ),
        (
            "http://127.0.0.1",
            (true, Some("http"), Some("127.0.0.1"), &[], None, None),
        ),
        (
            "http://[::]",
            (true, Some("http"), Some("[::]"), &[], None, None),
        ),
        (
            "http://%0D",
            (true, Some("http"), Some("%0D"), &[], None, None),
        ),
        (
            "http://example.org/",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                None,
                None,
            ),
        ),
        (
            "http://éxample.org/",
            (
                true,
                Some("http"),
                Some("éxample.org"),
                &["", ""],
                None,
                None,
            ),
        ),
        (
            "http://user:pw@example.org:1234/",
            (
                true,
                Some("http"),
                Some("user:pw@example.org:1234"),
                &["", ""],
                None,
                None,
            ),
        ),
        (
            "http://example.org/foo/bar/baz",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "foo", "bar", "baz"],
                None,
                None,
            ),
        ),
        (
            "http://example.org/foo/bar/",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "foo", "bar", ""],
                None,
                None,
            ),
        ),
        (
            "http://example.org/foo/bar/bàz",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "foo", "bar", "bàz"],
                None,
                None,
            ),
        ),
        (
            "http://example.org/foo/.././/bar",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "foo", "..", ".", "", "bar"],
                None,
                None,
            ),
        ),
        (
            "http://example.org/!$&'()*+,=:@/foo%0D",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "!$&'()*+,=:@", "foo%0D"],
                None,
                None,
            ),
        ),
        (
            "http://example.org/?abc",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                Some("abc"),
                None,
            ),
        ),
        (
            "http://example.org/?!$&'()*+,=:@/?\u{E000}",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                Some("!$&'()*+,=:@/?\u{E000}"),
                None,
            ),
        ),
        (
            "http://example.org/#def",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                None,
                Some("def"),
            ),
        ),
        (
            "http://example.org/?abc#def",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                Some("abc"),
                Some("def"),
            ),
        ),
        (
            "tag:abc/def",
            (true, Some("tag"), None, &["abc", "def"], None, None),
        ),
        ("tag:", (true, Some("tag"), None, &[], None, None)),
        ("foo", (false, None, None, &["foo"], None, None)),
        ("..", (false, None, None, &[".."], None, None)),
        (
            "//example.org",
            (false, None, Some("example.org"), &[], None, None),
        ),
        ("?", (false, None, None, &[], Some(""), None)),
        ("#", (false, None, None, &[], None, Some(""))),
        ("?#", (false, None, None, &[], Some(""), Some(""))),
        (
            "http://example.org/#Andr%C3%A9",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                None,
                Some("Andr%C3%A9"),
            ),
        ),
        (
            "http://example.org/?Andr%C3%A9",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                Some("Andr%C3%A9"),
                None,
            ),
        ),
        (
            "?Andr%C3%A9#Andr%C3%A9",
            (
                false,
                None,
                None,
                &[],
                Some("Andr%C3%A9"),
                Some("Andr%C3%A9"),
            ),
        ),
    ];

    const NEGATIVE_IRIS: &[&str] = &[
        "http://[/",
        "http://a/[",
        "http://a/]",
        "http://a/|",
        "http://a/ ",
        "http://a/\u{E000}",
        "[",
        "]",
        "|",
        " ",
        "\u{E000}",
    ];

    const RELATIVE_IRIS: &[(&str, &str)] = &[
        // all relative iris are resolved agains http://a/b/c/d;p?q
        // normal examples from https://tools.ietf.org/html/rfc3986#section-5.4.1
        ("g:h", "g:h"),
        ("g", "http://a/b/c/g"),
        ("./g", "http://a/b/c/g"),
        ("g/", "http://a/b/c/g/"),
        ("/g", "http://a/g"),
        ("//g", "http://g"),
        ("?y", "http://a/b/c/d;p?y"),
        ("g?y", "http://a/b/c/g?y"),
        ("#s", "http://a/b/c/d;p?q#s"),
        ("g#s", "http://a/b/c/g#s"),
        ("g?y#s", "http://a/b/c/g?y#s"),
        (";x", "http://a/b/c/;x"),
        ("g;x", "http://a/b/c/g;x"),
        ("g;x?y#s", "http://a/b/c/g;x?y#s"),
        ("", "http://a/b/c/d;p?q"),
        (".", "http://a/b/c/"),
        ("./", "http://a/b/c/"),
        ("..", "http://a/b/"),
        ("../", "http://a/b/"),
        ("../g", "http://a/b/g"),
        ("../..", "http://a/"),
        ("../../", "http://a/"),
        ("../../g", "http://a/g"),
        // abnormal example from https://tools.ietf.org/html/rfc3986#section-5.4.2
        ("../../../g", "http://a/g"),
        ("../../../../g", "http://a/g"),
        ("/./g", "http://a/g"),
        ("/../g", "http://a/g"),
        ("g.", "http://a/b/c/g."),
        (".g", "http://a/b/c/.g"),
        ("g..", "http://a/b/c/g.."),
        ("..g", "http://a/b/c/..g"),
        ("./../g", "http://a/b/g"),
        ("./g/.", "http://a/b/c/g/"),
        ("g/./h", "http://a/b/c/g/h"),
        ("g/../h", "http://a/b/c/h"),
        ("g;x=1/./y", "http://a/b/c/g;x=1/y"),
        ("g;x=1/../y", "http://a/b/c/y"),
        ("g?y/./x", "http://a/b/c/g?y/./x"),
        ("g?y/../x", "http://a/b/c/g?y/../x"),
        ("g#s/./x", "http://a/b/c/g#s/./x"),
        ("g#s/../x", "http://a/b/c/g#s/../x"),
    ];
}
