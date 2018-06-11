use pest::{Error, Parser, {iterators::Pair}};

/*
use std::io::{BufRead};

use pest::{Error, Parser, iterators::Pair};

use super::common::unescape_str;
use super::super::graph::MutableGraph;
use super::super::ns::xsd;
use super::super::strstash::DumbStash;
use super::super::term::{Term, stash::TermStash};
*/

#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("iri.pest");

#[derive(Parser)]
#[grammar = "term/iri.pest"]
pub struct IriParser;

#[derive(Debug, Default, Eq, PartialEq)]
pub struct ParsedIri<'a> {
    scheme: Option<&'a str>,
    authority: Option<&'a str>,
    path: Vec<&'a str>,
    query: Option<&'a str>,
    fragment: Option<&'a str>,
}

// NB: path complies with the following rules:
// - does not contain the seperators ('/'), but
// - its first element may be '/' if the path has a root,
// - its last element is "" if the path ends with a '/'

impl<'a> ParsedIri<'a> {
    pub fn new(txt: &'a str) -> Result<ParsedIri<'a>, Error<'a, Rule>> {
        let mut pi = ParsedIri::default();
        pi.fill_with(IriParser::parse(Rule::main, txt)?.next().unwrap());
        Ok(pi)
    }

    fn fill_with(&mut self, pair: Pair<'a, Rule>) {
        for subpair in pair.into_inner() {
            #[cfg(test)] println!("=== {:?} {:?}", subpair.as_rule(), subpair.as_str());
            match subpair.as_rule() {
                Rule::iri => {
                    self.fill_with(subpair);
                }
                Rule::irelative_ref => {
                    self.fill_with(subpair);
                }
                Rule::scheme => {
                    debug_assert!(self.scheme.is_none());
                    self.scheme = Some(subpair.as_str());
                }
                Rule::ihier_part |
                Rule::irelative_part => {
                    self.fill_with(subpair);
                }
                Rule::iquery => {
                    debug_assert!(self.query.is_none());
                    self.query = Some(subpair.as_str());
                }
                Rule::ifragment => {
                    debug_assert!(self.fragment.is_none());
                    self.fragment = Some(subpair.as_str());
                }
                Rule::iauthority => {
                    debug_assert!(self.authority.is_none());
                    self.authority = Some(subpair.as_str());
                }
                Rule::ipath_abempty => {
                    if subpair.as_str().len() > 0 {
                        self.path.push("/");
                        self.fill_with(subpair);
                    }
                }
                Rule::ipath_absolute => {
                    self.path.push("/");
                    self.fill_with(subpair);
                }
                Rule::ipath_noscheme |
                Rule::ipath_rootless => {
                    self.fill_with(subpair);
                }
                Rule::ipath_empty => {
                }
                Rule::isegment |
                Rule::isegment_nz |
                Rule::isegment_nz_nc => {
                    self.path.push(subpair.as_str());
                }
                _ => panic!(format!("Can't handle rule {:?}", subpair.as_rule()))
            }
        }
    }

    pub fn is_absolute(&self) -> bool {
        self.scheme.is_some()
    }

    pub fn serialize(&self) -> String {
        unimplemented!()
    }

    pub fn join(&self, iri_ref: &str) -> ParsedIri {
        unimplemented!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn positive() {
        for (txt, parsed) in POSITIVE_IRIS.iter() {
            let rpi = ParsedIri::new(txt);
            assert!(rpi.is_ok(), format!("<{}> → {:?}", txt, rpi));
            let pi = rpi.unwrap();
            assert_eq!(pi.is_absolute(), parsed.0);
            assert_eq!(pi.scheme, parsed.1);
            assert_eq!(pi.authority, parsed.2);
            assert_eq!(&pi.path[..], parsed.3);
            assert_eq!(pi.query, parsed.4);
            assert_eq!(pi.fragment, parsed.5);
        }
    }

    const POSITIVE_IRIS: &[(&str, (bool, Option<&str>, Option<&str>, &[&str], Option<&str>, Option<&str>))] = &[
        ("http:",
            (true, Some("http"), None, &[], None, None)),
        ("http://example.org",
            (true, Some("http"), Some("example.org"), &[], None, None)),
        ("http://127.0.0.1",
            (true, Some("http"), Some("127.0.0.1"), &[], None, None)),
        ("http://[::]",
            (true, Some("http"), Some("[::]"), &[], None, None)),
        ("http://%0D",
            (true, Some("http"), Some("%0D"), &[], None, None)),
        ("http://example.org/",
            (true, Some("http"), Some("example.org"), &["/", ""], None, None)),
        ("http://éxample.org/",
            (true, Some("http"), Some("éxample.org"), &["/", ""], None, None)),
        ("http://user:pw@example.org:1234/",
            (true, Some("http"), Some("user:pw@example.org:1234"), &["/", ""], None, None)),
        ("http://example.org/foo/bar/baz",
            (true, Some("http"), Some("example.org"), &["/", "foo", "bar", "baz"], None, None)),
        ("http://example.org/foo/bar/",
            (true, Some("http"), Some("example.org"), &["/", "foo", "bar", ""], None, None)),
        ("http://example.org/foo/bar/bàz",
            (true, Some("http"), Some("example.org"), &["/", "foo", "bar", "bàz"], None, None)),
        ("http://example.org/foo/.././/bar",
            (true, Some("http"), Some("example.org"), &["/", "foo", "..", ".", "", "bar"], None, None)),
        ("http://example.org/!$&'()*+,=:@/foo%0D",
            (true, Some("http"), Some("example.org"), &["/", "!$&'()*+,=:@", "foo%0D"], None, None)),
        ("http://example.org/?abc",
            (true, Some("http"), Some("example.org"), &["/", ""], Some("abc"), None)),
        ("http://example.org/?!$&'()*+,=:@/?\u{E000}",
            (true, Some("http"), Some("example.org"), &["/", ""], Some("!$&'()*+,=:@/?\u{E000}"), None)),
        ("http://example.org/#def",
            (true, Some("http"), Some("example.org"), &["/", ""], None, Some("def"))),
        ("http://example.org/?abc#def",
            (true, Some("http"), Some("example.org"), &["/", ""], Some("abc"), Some("def"))),
        ("tag:abc/def",
            (true, Some("tag"), None, &["abc", "def"], None, None)),
        ("tag:",
            (true, Some("tag"), None, &[], None, None)),

        ("foo",
            (false, None, None, &["foo"], None, None)),
        ("..",
            (false, None, None, &[".."], None, None)),
        ("//example.org",
            (false, None, Some("example.org"), &[], None, None)),
        ("?",
            (false, None, None, &[], Some(""), None)),
        ("#",
            (false, None, None, &[], None, Some(""))),
        ("?#",
            (false, None, None, &[], Some(""), Some(""))),
    ];

    #[test]
    fn negative() {
        for txt in NEGATIVE_IRIS.iter() {
            let rpi = ParsedIri::new(txt);
            assert!(rpi.is_err(), format!("<{}> → {:?}", txt, rpi));
        }
    }

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

}