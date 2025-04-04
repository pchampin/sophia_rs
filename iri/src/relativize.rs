//! Implement relativization of an IRI against a base IRI.

use std::{borrow::Cow, ops::Deref};

use crate::{Iri, IriRef, resolve::BaseIri};

/// A relativizer can be used to relativize multiple IRIs against the same base.
#[derive(Clone, Debug)]
pub struct Relativizer<T> {
    base: T,
    query_end: usize,
    path_end: usize,
    slashes: Vec<usize>,
    pseudoroot: usize, // not the actual root, but the point below which we don't relativize
}

impl<T: Deref<Target = str>> Relativizer<T> {
    /// Build a [`Relativizer`] from a [`BaseIri`].
    ///
    /// Relative IRI references produced by this [`Relativizer`] will contain at most `parents` ../
    pub fn new(base: BaseIri<T>, parents: u8) -> Self {
        let s = base.as_str();
        let path_begin =
            base.scheme().len() + 1 + base.authority().map(|a| a.len() + 2).unwrap_or(0);
        let path_end = path_begin + base.path().len();
        let query_end = s[path_end..]
            .find('#')
            .map(|i| i + path_end)
            .unwrap_or(s.len());
        let path_end = s[path_end..query_end]
            .find('?')
            .map(|i| i + path_end)
            .unwrap_or(query_end);
        let mut slashes = Vec::with_capacity(parents as usize + 1);
        let mut pos = path_end;
        for _ in 0..=parents {
            let i = s[path_begin..pos].rfind('/').unwrap_or(0);
            if i > 0 {
                pos = i + path_begin;
                slashes.push(pos);
            } else {
                // no slash was found, or it was found at the start of the path
                break;
            }
        }
        let has_root = s[path_begin..].starts_with('/');
        let pseudoroot = if slashes.len() > parents as usize {
            slashes.pop().unwrap() + 1
        } else if has_root {
            path_begin + 1
        } else {
            path_begin
        };
        let base = base.into_inner();

        Self {
            base,
            query_end,
            path_end,
            slashes,
            pseudoroot,
        }
    }

    /// The base of this [`Relativizer`]
    pub fn base(&self) -> Iri<&str> {
        Iri::new_unchecked(&self.base)
    }

    /// Relativize the given IRI against the base of this [`Relativizer`] if possible.
    pub fn relativize<'a>(&self, iri: Iri<&'a str>) -> Option<IriRef<Cow<'a, str>>> {
        let lcp = longest_common_prefix(&self.base, iri.as_str());
        if lcp >= self.query_end {
            // iri is identicical to base or differs in the fragment only.
            // regardless, we must include the fragment (if any) in the relative IRI.
            Some(IriRef::new_unchecked(iri[self.query_end..].into()))
        } else if lcp > self.path_end {
            // both iri and base have a query and-or fragment (because lcp is *strictly* > to path_end)
            // and they differ in the query or presence thereof
            // (because if if they differed only in fragment, we would have matched above)
            // → we include query and-or fragment in the relative IRI
            Some(IriRef::new_unchecked(iri[self.path_end..].into()))
        } else if lcp == self.path_end
            && (iri.len() == self.path_end || iri[self.path_end..].starts_with(['?', '#']))
        {
            // both iri and base have exactly the same path, but differ after
            // → same as above
            Some(IriRef::new_unchecked(iri[self.path_end..].into()))
        } else if lcp >= self.pseudoroot {
            // iri and base have similar paths
            for (nb, slash) in self.slashes.iter().copied().enumerate() {
                if lcp > slash {
                    return if nb == 0 {
                        if iri.len() == slash + 1 || iri[slash + 1..].starts_with(['?', '#']) {
                            // insert ./ if there is no path element after the last slash
                            Some(IriRef::new_unchecked(
                                format!("./{}", &iri[slash + 1..]).into(),
                            ))
                        } else {
                            Some(IriRef::new_unchecked(iri[slash + 1..].into()))
                        }
                    } else {
                        // insert the expected amount of '../'
                        let mut parts = vec![".."; nb + 1];
                        parts[nb] = &iri[slash + 1..];
                        Some(IriRef::new_unchecked(parts.join("/").into()))
                    };
                }
            }
            if self.slashes.is_empty() {
                if iri[self.pseudoroot - 1..].starts_with('/')
                    && (iri.len() == self.pseudoroot
                        || iri[self.pseudoroot..].starts_with(['?', '#']))
                {
                    Some(IriRef::new_unchecked(
                        format!("./{}", &iri[self.pseudoroot..]).into(),
                    ))
                } else {
                    Some(IriRef::new_unchecked(iri[self.pseudoroot..].into()))
                }
            } else {
                let nb = self.slashes.len();
                let mut parts = vec![".."; nb + 1];
                parts[nb] = &iri[self.pseudoroot..];
                Some(IriRef::new_unchecked(parts.join("/").into()))
            }
        } else {
            // iri and base are too different to relativize
            None
        }
    }
}

fn longest_common_prefix(s1: &str, s2: &str) -> usize {
    s1.bytes()
        .zip(s2.bytes())
        .take_while(|&(b1, b2)| b1 == b2)
        .count()
}

#[cfg(test)]
mod test {
    use test_case::{test_case, test_matrix};

    use super::*;

    #[test_case("http://a/b/c/d?e#f?g", 16, 14, vec![12, 10], 9)]
    #[test_case("http://a/b/c/d?e", 16, 14, vec![12, 10], 9)]
    #[test_case("http://a/b/c/d#f?g", 14, 14, vec![12, 10], 9)]
    #[test_case("http://a/b/c/d", 14, 14, vec![12, 10], 9)]
    #[test_case("http://a/b/c/", 13, 13, vec![12, 10], 9)]
    #[test_case("http://a/b/c", 12, 12, vec![10], 9)]
    #[test_case("http://a/b/", 11, 11, vec![10], 9)]
    #[test_case("http://a/b", 10, 10, vec![], 9)]
    #[test_case("http://a/", 9, 9, vec![], 9)]
    #[test_case("http://a/?e#f", 11, 9, vec![], 9)]
    #[test_case("x-ample:ab/c/d?e#f?g", 16, 14, vec![12, 10], 8)]
    #[test_case("x-ample:ab/c/d?e", 16, 14, vec![12, 10], 8)]
    #[test_case("x-ample:ab/c/d#f?g", 14, 14, vec![12, 10], 8)]
    #[test_case("x-ample:ab/c/d", 14, 14, vec![12, 10], 8)]
    #[test_case("x-ample:ab/c/", 13, 13, vec![12, 10], 8)]
    #[test_case("x-ample:ab/c", 12, 12, vec![10], 8)]
    #[test_case("x-ample:ab/", 11, 11, vec![10], 8)]
    #[test_case("x-ample:ab", 10, 10, vec![], 8)]
    #[test_case("x-ample:", 8, 8, vec![], 8)]
    #[test_case("x-ample:?e#f", 10, 8, vec![], 8)]
    fn new_relativizer(
        iri: &str,
        query_end: usize,
        path_end: usize,
        slashes: Vec<usize>,
        pseudoroot: usize,
    ) {
        let b = BaseIri::new(iri).unwrap();
        for parents in 0..=3 {
            let r = Relativizer::new(b.as_ref(), parents);
            assert_eq!(r.query_end, query_end);
            assert_eq!(r.path_end, path_end);
            let (exp_slashes, exp_pseudoroot) = get_exp(&slashes, pseudoroot, parents as usize);
            assert_eq!(r.slashes, exp_slashes);
            assert_eq!(r.pseudoroot, exp_pseudoroot);
        }
    }

    fn get_exp(slashes: &[usize], pseudoroot: usize, parents: usize) -> (&[usize], usize) {
        if slashes.len() <= parents {
            (slashes, pseudoroot)
        } else {
            (&slashes[..parents], slashes[parents] + 1)
        }
    }

    #[test_matrix(
        [
            0,
            1,
            2,
            3,
        ],
        [
            "http://a/b/c/d?q#f?f",
            "http://a/b/c/d?q",
            "http://a/b/c/d#f?f",
            "http://a/b/c/d",
            "x-ample:bb/c/d?q#f?f",
            "x-ample:bb/c/d?q",
            "x-ample:bb/c/d#f?f",
            "x-ample:bb/c/d",
        ],
        [
            "",
            "#F0",
            "?Q0",
            "?Q0#F0",
            "P0",
            "P0#F0",
            "P0?Q0",
            "P0?Q0#F0",
            "./    #DOT", // the comment will be stripped, it is only here to give the test a unique name
            "./#F1",
            "./?Q1",
            "./?Q1#F1",
            "../P1",
            "../P1#F2",
            "../P1?Q2",
            "../P1?Q2#F2",
            "../     #2DOTS",
            "../#F2",
            "../?Q2",
            "../?Q2#F2",
            "../../P2",
            "../../P2#F3",
            "../../P2?Q3",
            "../../P2?Q3#F3",
            "../../    #4DOTS",
            "../../#F3",
            "../../?Q3",
            "../../?Q3#F3",
        ]
    )]
    fn relativize(parents: u8, base: &str, exp: &str) {
        let base = BaseIri::new(base).unwrap();
        let exp: IriRef<Cow<str>> = IriRef::new(exp.split(" ").next().unwrap().into()).unwrap(); // strip comment
        let exp_depth: u8 = if exp.starts_with("../../") {
            2
        } else if exp.starts_with("../") {
            1
        } else {
            0
        };

        let rel = Relativizer::new(base.as_ref(), parents);
        let abs = base.resolve(exp.as_ref());
        let got = rel.relativize(abs.as_ref());
        if exp_depth <= parents {
            assert_eq!(got, Some(exp));
        } else {
            assert!(got.is_none());
        }
    }
}
