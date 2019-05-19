//! Parser for RDF XML.

use std::borrow::Cow;
use std::io::{BufRead, BufReader, Read};
use std::result::Result as StdResult;

use pest::error::Error as PestError;
use pest::{iterators::Pairs, Parser};

use super::common::*;
use super::nt::{pair_to_term, PestNtqParser, Rule};
use crate::error::*;
use crate::quad::Quad;
use crate::term::{graph_key::GraphKey, Term};
use crate::triple::Triple;

#[derive(Clone, Debug, Default)]
pub struct Config;

impl Config {
    fn parse_xmlread<'a, B: BufRead + 'a>(
        &self,
        r: quick_xml::Reader<B>,
    ) -> impl Iterator<Item = Result<XmlQuad>> + 'a {
        None.into_iter()
    }

    #[inline]
    pub fn parse_bufread<'a, B: BufRead + 'a>(
        &self,
        bufread: B,
    ) -> impl Iterator<Item = Result<XmlQuad>> + 'a {
        self.parse_xmlread(quick_xml::Reader::from_reader(bufread))
    }

    #[inline]
    pub fn parse_read<'a, R: Read + 'a>(
        &self,
        read: R,
    ) -> impl Iterator<Item = Result<XmlQuad>> + 'a {
        self.parse_bufread(BufReader::new(read))
    }

    #[inline]
    pub fn parse_str<'a>(&self, txt: &'a str) -> impl Iterator<Item = Result<XmlQuad>> + 'a {
        self.parse_xmlread(quick_xml::Reader::from_str(txt))
    }
}

pub struct XmlQuad {
    s: Term<String>,
    p: Term<String>,
    o: Term<String>,
}

impl<'a> Triple<'a> for XmlQuad {
    type TermData = String;
    fn s(&self) -> &Term<Self::TermData> {
        &self.s
    }
    fn p(&self) -> &Term<Self::TermData> {
        &self.p
    }
    fn o(&self) -> &Term<Self::TermData> {
        &self.o
    }
}

#[cfg(test)]
mod test {

    use crate::graph::inmem::HashGraph;
    use crate::graph::inmem::TermIndexMapU;
    use crate::graph::Graph;
    use crate::term::factory::RcTermFactory;
    use crate::triple::stream::TripleSource;
    use crate::triple::Triple;
    use std::ffi::OsStr;
    use std::fmt::Debug;
    use std::fmt::Formatter;
    use std::fmt::Result as FmtResult;
    use std::fs::{read_dir, File};
    use std::io;
    use std::path::Path;

    type TestGraph = HashGraph<TermIndexMapU<u64, RcTermFactory>>;

    impl PartialEq for TestGraph {
        fn eq(&self, other: &Self) -> bool {
            // check self is contained in other
            for res in <Self as Graph>::triples(&self) {
                if let Ok(triple) = res {
                    if !other.contains(triple.s(), triple.p(), triple.o()).unwrap() {
                        return false;
                    }
                } else {
                    return false;
                }
            }

            // check other is contained in self
            for res in <Self as Graph>::triples(&other) {
                if let Ok(triple) = res {
                    if !self.contains(triple.s(), triple.p(), triple.o()).unwrap() {
                        return false;
                    }
                } else {
                    return false;
                }
            }

            // both graphs are included in each other so they are equal
            true
        }
    }

    impl Debug for TestGraph {
        fn fmt(&self, f: &mut Formatter) -> FmtResult {
            let v: Vec<_> = self.triples().collect();
            v.fmt(f)
        }
    }

    #[test]
    fn w3c_test_suite() {
        fn do_test_suite() -> io::Result<()> {
            let rdf_ext = OsStr::new("rdf");
            let nt_ext = OsStr::new("nt");

            let suite = Path::new("..").join("rdf-tests").join("rdf-xml");
            if !suite.exists() || !suite.is_dir() {
                panic!("rdf-tests/rdf-xml not found, can not check W3C test-suite. cf README.md");
            }

            let mut tested = 0;

            for e in read_dir(&suite)? {
                let entry = e?;
                if entry.file_type()?.is_dir() {
                    for c in read_dir(entry.path())? {
                        let case = c?;
                        if case.path().extension() == Some(rdf_ext) {
                            if case.path().with_extension(nt_ext).is_file() {
                                // the reference N-Triples file
                                let ntparser = crate::parser::nt::Config::default();
                                let ntfile = File::open(case.path().with_extension(nt_ext))?;
                                let mut expected = TestGraph::new();
                                ntparser.parse_read(ntfile).in_graph(&mut expected).unwrap();
                                // the test XML file
                                let xmlparser = super::Config::default();
                                let xmlfile = File::open(case.path())?;
                                let mut actual = TestGraph::new();
                                let res = xmlparser.parse_read(xmlfile).in_graph(&mut actual);

                                // check the XML parses without error
                                assert!(
                                    res.is_ok(),
                                    format!("{} should parse without error", case.path().display())
                                );
                                // check the XML produces the same graph - TODO
                                assert_eq!(actual, expected);

                                tested += 1;
                            } else if case.path().to_string_lossy().contains("error") {
                                let xmlparser = super::Config::default();
                                let xmlfile = File::open(case.path())?;
                                let mut actual = TestGraph::new();
                                assert!(
                                    xmlparser.parse_read(xmlfile).in_graph(&mut actual).is_err(),
                                    format!("{} should parse with error", case.path().display())
                                );

                                tested += 1;
                            }
                        }
                    }
                }
            }

            assert_ne!(
                tested, 0,
                "No test found in W3C test-suite, something must be wrong"
            );
            Ok(())
        }
        do_test_suite().unwrap()
    }
}
