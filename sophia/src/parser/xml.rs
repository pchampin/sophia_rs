//! Parser for RDF XML.

use std::collections::HashMap;
use std::collections::LinkedList;
use std::fmt::Debug;
use std::io::{BufRead, BufReader, Read};
use std::ops::RangeFrom;
use std::rc::Rc;

use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use quick_xml::events::Event;

use super::common::*;
use crate::error::*;
use crate::ns::rdf;
use crate::ns::xsd;
use crate::ns::Namespace;
use crate::term::factory::RcTermFactory;
use crate::term::factory::TermFactory;
use crate::term::matcher::TermMatcher;
use crate::term::Term;
use crate::term::TermData;
use crate::triple::Triple;

// ---

#[derive(Clone, Debug, Default)]
pub struct Config;

impl Config {
    #[inline]
    pub fn parse_bufread<'a, B: BufRead + 'a>(
        &self,
        bufread: B,
    ) -> impl Iterator<Item = Result<[Term<Rc<str>>; 3]>> + 'a {
        XmlParser::<_, RcTermFactory>::new(quick_xml::Reader::from_reader(bufread))
    }

    #[inline]
    pub fn parse_read<'a, R: Read + 'a>(
        &self,
        read: R,
    ) -> impl Iterator<Item = Result<[Term<Rc<str>>; 3]>> + 'a {
        self.parse_bufread(BufReader::new(read))
    }

    #[inline]
    pub fn parse_str<'a>(
        &self,
        txt: &'a str,
    ) -> impl Iterator<Item = Result<[Term<Rc<str>>; 3]>> + 'a {
        XmlParser::<_, RcTermFactory>::new(quick_xml::Reader::from_str(txt))
    }
}

// ---

#[derive(Clone)]
pub struct PrefixMapping<F: TermFactory> {
    default: Option<String>,
    mapping: HashMap<String, Namespace<F::TermData>>,
    factory: F,
}

impl<F: TermFactory + Default> Default for PrefixMapping<F> {
    fn default() -> Self {
        Self {
            default: None,
            mapping: HashMap::new(),
            factory: Default::default(),
        }
    }
}

impl<F: TermFactory> PrefixMapping<F> {
    pub fn add_prefix(&mut self, prefix: &str, value: &str) {
        if prefix == "_" {
            panic!("reserved prefix")
        } else {
            self.mapping.insert(
                String::from(prefix),
                Namespace::new(self.factory.get_term_data(value)).expect("FIXME"),
            );
        }
    }

    pub fn expand_curie_string(&mut self, curie_str: &str) -> Term<F::TermData> {
        if let Some(separator_idx) = curie_str.chars().position(|c| c == ':') {
            let prefix = &curie_str[..separator_idx];
            let reference = &curie_str[separator_idx + 1..];
            self.expand_curie(&prefix, &reference)
        } else {
            panic!("missing prefix")
        }
    }

    pub fn expand_curie(&mut self, prefix: &str, local: &str) -> Term<F::TermData> {
        if let Some(ns) = self.mapping.get(prefix) {
            ns.get(self.factory.get_term_data(local)).expect("FIXME")
        } else {
            panic!("no such namespace")
        }
    }
}

// ---

struct XmlParser<B: BufRead, F: TermFactory> {
    reader: quick_xml::Reader<B>,
    // The stack of namespaces: should be optimized.
    namespaces: Vec<PrefixMapping<F>>,
    // The stack of parents (for nested declarations)
    parents: Vec<Term<F::TermData>>,
    // The queue of produced triples
    triples: LinkedList<Result<[Term<F::TermData>; 3]>>,
    // `true` if we are currently in a node element.
    in_node: bool,
    //
    factory: F,
    //
    bnodes: RangeFrom<usize>,
}

impl<B, F> XmlParser<B, F>
where
    B: BufRead,
    F: TermFactory + Clone + Default,
    <F as TermFactory>::TermData: Debug,
{
    fn new(reader: quick_xml::Reader<B>) -> Self {
        Self {
            reader,
            parents: Vec::new(),
            namespaces: vec![PrefixMapping::default()],
            triples: LinkedList::new(),
            in_node: false,
            factory: Default::default(),
            bnodes: 0..,
        }
    }

    fn element_start(&mut self, e: BytesStart) {
        // Add a new namespace mapping (OPTIMISE ME)
        let mut ns = self.namespaces.last().unwrap().clone();
        for attr in e.attributes() {
            let a = attr.expect("FIXME");
            if a.key.starts_with(b"xmlns:") {
                ns.add_prefix(
                    std::str::from_utf8(&a.key[6..]).expect("FIXME"),
                    std::str::from_utf8(&a.value.as_ref()).expect("FIXME"),
                );
            }
        }
        self.namespaces.push(ns);

        // Ignore top-level rdf:RDF element
        if e.name() != b"rdf:RDF" {
            // Change the current element type
            self.in_node = !self.in_node;
            // Parse as a node of as a property
            if self.in_node {
                self.node_start(e)
            } else {
                self.predicate_start(e)
            }
        }
    }

    fn node_start(&mut self, e: BytesStart) {
        let ns = self.namespaces.last_mut().unwrap();

        // Separate node subject from other attributes
        let mut properties = HashMap::new();
        let mut subject = None;
        for attr in e.attributes().with_checks(true) {
            let a = attr.expect("FIXME");

            // ignore xmlns attributes (processed in element_start)
            if a.key.starts_with(b"xmlns:") {
                continue;
            }

            // try to extract the subject annotation
            let k = ns.expand_curie_string(std::str::from_utf8(a.key).expect("FIXME"));
            let v = a.unescape_and_decode_value(&self.reader).expect("FIXME");
            if k.matches(&rdf::about) {
                if subject.is_none() {
                    subject = Some(self.factory.iri(v).expect("FIXME"));
                } else {
                    panic!("cannot have rdf:ID, rdf:about and rdf:nodeId at the same time")
                }
            } else if k.matches(&rdf::ID) {

            } else if k.matches(&rdf::nodeID) {
                if subject.is_none() {
                    subject = Some(self.factory.bnode(format!("o{}", v)).expect("FIXME"));
                } else {
                    panic!("cannot have rdf:ID, rdf:about and rdf:nodeId at the same time")
                }
            } else {
                properties.insert(k, self.factory.literal_dt(v, xsd::string).expect("FIXME"));
            }
        }

        // Get subject and add it to the current nested stack
        let s: Term<_> = subject.unwrap_or(
            self.factory
                .bnode(format!("n{}", self.bnodes.next().unwrap()))
                .expect("FIXME"),
        );
        self.parents.push(s.clone());

        // Add the type as a triple if it is not `rdf:Description`
        let ty = ns.expand_curie_string(std::str::from_utf8(e.name()).expect("FIXME"));
        if ty != rdf::Description {
            self.triples
                .push_back(Ok([s.clone(), self.factory.copy(&rdf::type_), ty]));
        }

        // Add properties
        for (p, lit) in properties {
            self.triples.push_back(Ok([s.clone(), p, lit]))
        }

        // Add the entity as an object if it is not top-level
        if self.parents.len() > 1 {
            let o = s;
            let s = &self.parents[self.parents.len() - 3];
            let p = &self.parents[self.parents.len() - 2];
            self.triples.push_back(Ok([s.clone(), p.clone(), o]));
        }
    }

    fn predicate_start(&mut self, e: BytesStart) {
        let ns = self.namespaces.last_mut().unwrap();
        let p = ns.expand_curie_string(std::str::from_utf8(e.name()).expect("FIXME"));
        self.parents.push(p)
    }

    fn element_end(&mut self, e: BytesEnd) {
        // Change the current element type (if not in rdf:RDF)
        if e.name() != b"rdf:RDF" {
            self.in_node = !self.in_node;
            self.parents.pop();
        }
    }

    fn element_text(&mut self, e: BytesText) {
        if !self.in_node {
            self.predicate_text(e);
        }
    }

    // FIXME: datatype handler
    fn predicate_text(&mut self, e: BytesText) {
        if self.parents.len() > 1 {
            let s = &self.parents[self.parents.len() - 2];
            let p = &self.parents[self.parents.len() - 1];
            let o = self
                .factory
                .literal_dt(
                    e.unescape_and_decode(&self.reader).expect("FIXME"),
                    xsd::string,
                )
                .expect("FIXME");
            self.triples.push_back(Ok([s.clone(), p.clone(), o]));
        }
    }

    fn element_empty(&mut self, e: BytesStart) {
        // Add a new namespace mapping (OPTIMISE ME)
        let mut ns = self.namespaces.last().unwrap().clone();
        for attr in e.attributes() {
            let a = attr.expect("FIXME");
            if a.key.starts_with(b"xmlns:") {
                ns.add_prefix(
                    std::str::from_utf8(&a.key[6..]).expect("FIXME"),
                    std::str::from_utf8(&a.value.as_ref()).expect("FIXME"),
                );
            }
        }

        self.namespaces.push(ns);
        if self.in_node {
            self.predicate_empty(e)
        } else {
            self.node_empty(e)
        }
    }

    fn node_empty(&mut self, e: BytesStart) {}

    fn predicate_empty(&mut self, e: BytesStart) {
        let ns = self.namespaces.last_mut().unwrap();
        let p = ns.expand_curie_string(std::str::from_utf8(e.name()).expect("FIXME"));

        let mut object = None;
        for attr in e.attributes().with_checks(true) {
            let a = attr.expect("FIXME");

            // ignore xmlns attributes
            if a.key.starts_with(b"xmlns:") {
                continue;
            }

            // try to extract the annotation object
            let k = ns.expand_curie_string(std::str::from_utf8(a.key).expect("FIXME"));
            let v = a.unescape_and_decode_value(&self.reader).expect("FIXME");
            if k.matches(&rdf::resource) {
                if object.is_none() {
                    object = Some(self.factory.iri(v).expect("FIXME"));
                } else {
                    panic!("cannot have rdf:resource rdf:nodeId at the same time")
                }
            } else if k.matches(&rdf::nodeID) {

            }
        }

        let s = self.parents.last().unwrap();
        let o = object.unwrap(); // FIXME
        self.triples.push_back(Ok([s.clone(), p, o]));
    }
}

impl<B, F> Iterator for XmlParser<B, F>
where
    B: BufRead,
    F: TermFactory + Clone + Default,
    <F as TermFactory>::TermData: Debug,
{
    type Item = Result<[Term<F::TermData>; 3]>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(triple) = self.triples.pop_front() {
                return Some(triple);
            }
            match self.reader.read_event(&mut Vec::new()).unwrap() {
                Event::Eof => return None,
                Event::Start(s) => self.element_start(s),
                Event::Empty(e) => self.element_empty(e),
                Event::End(e) => self.element_end(e),
                // Event::Text(t) => self.element_text(t),
                _ => (),
            }
        }
    }
}

#[cfg(test)]
mod test {

    use crate::graph::inmem::HashGraph;
    use crate::graph::inmem::TermIndexMapU;
    use crate::graph::Graph;
    use crate::term::factory::RcTermFactory;
    use crate::term::matcher::TermMatcher;
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
            let mut triples_self = Vec::new();
            let mut triples_other = Vec::new();

            //
            for res in <Self as Graph>::triples(&self) {
                triples_self.push(res.unwrap());
            }
            for res in <Self as Graph>::triples(&other) {
                triples_other.push(res.unwrap());
            }

            //
            triples_self.sort_by_key(|t| (t.s().value(), t.p().value(), t.o().value()));
            triples_other.sort_by_key(|t| (t.s().value(), t.p().value(), t.o().value()));

            for (ts, to) in triples_self.into_iter().zip(triples_other.into_iter()) {
                if !ts.s().matches(to.s()) {
                    return false;
                }

                if !ts.p().matches(to.p()) {
                    return false;
                }

                if !ts.o().matches(to.o()) {
                    return false;
                }
            }

            // both graphs are included in each other so they are equal
            true
        }
    }

    impl Debug for TestGraph {
        fn fmt(&self, f: &mut Formatter) -> FmtResult {
            let mut v = Vec::new();
            for t in self.triples() {
                v.push(t.unwrap());
            }
            v.sort_by_key(|t| (t.s().value(), t.p().value(), t.o().value()));
            v.fmt(f)
        }
    }

    // #[test]
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
                                println!("{}", case.path().display());

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
                                // check the XML produces the same graph
                                pretty_assertions::assert_eq!(
                                    actual,
                                    expected,
                                    "{} does not give expected results",
                                    case.path().display()
                                );

                                tested += 1;
                            } else if case.path().to_string_lossy().contains("error") {
                                // let xmlparser = super::Config::default();
                                // let xmlfile = File::open(case.path())?;
                                // let mut actual = TestGraph::new();
                                // assert!(
                                //     xmlparser.parse_read(xmlfile).in_graph(&mut actual).is_err(),
                                //     format!("{} should parse with error", case.path().display())
                                // );
                                //
                                // tested += 1;
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

    #[test]
    fn w3c_example_07() {
        let mut actual = TestGraph::new();
        super::Config::default()
            .parse_str(
                r#"<?xml version="1.0"?>
                    <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                            xmlns:dc="http://purl.org/dc/elements/1.1/"
                            xmlns:ex="http://example.org/stuff/1.0/">

                    <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
                             dc:title="RDF1.1 XML Syntax">
                    <ex:editor>
                      <rdf:Description ex:fullName="Dave Beckett">
                        <ex:homePage rdf:resource="http://purl.org/net/dajobe/" />
                      </rdf:Description>
                    </ex:editor>
                    </rdf:Description>

                    </rdf:RDF>
                "#,
            )
            .in_graph(&mut actual)
            .expect("failed parsing XML file");

        let mut expected = TestGraph::new();
        crate::parser::nt::Config::default()
            .parse_str(
                r#"
                <http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)" .
                _:genid1 <http://example.org/stuff/1.0/fullName> "Dave Beckett" .
                _:genid1 <http://example.org/stuff/1.0/homePage> <http://purl.org/net/dajobe/> .
                <http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> _:genid1 .
                "#,
            )
            .in_graph(&mut expected)
            .expect("could not parse N-Triples file");

        // pretty_assertions::assert_eq!(actual, expected);
    }

}
