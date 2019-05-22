//! Parser for RDF XML.

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::LinkedList;
use std::fmt::Debug;
use std::io::{BufRead, BufReader, Read};
use std::rc::Rc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use quick_xml::events::Event;

use crate::error::*;
use crate::ns::rdf;
use crate::ns::xml;
use crate::ns::xsd;
use crate::ns::Namespace;
use crate::term::factory::RcTermFactory;
use crate::term::factory::TermFactory;
use crate::term::iri_rfc3987::is_absolute_iri;
use crate::term::iri_rfc3987::is_relative_iri;
use crate::term::matcher::TermMatcher;
use crate::term::Term;

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

#[derive(Debug, Clone, Copy)]
enum ParsingState {
    Node,
    Predicate,
    Resource,
    Literal, // NB: not supported by quick-xml right now
}

// ---

#[derive(Debug)]
pub struct Scope<F: TermFactory> {
    /// The XML namespaces declared in this scope.
    ns: HashMap<String, Namespace<F::TermData>>,

    /// The default XML namespace to expand tags without namespaces with.
    default: Option<Namespace<F::TermData>>,

    /// The base IRI namespace to expand `rdf:ID`, `rdf:resource` and `rdf:about`.
    base: Option<Namespace<F::TermData>>,

    /// The term factory used to create new terms.
    factory: Rc<RefCell<F>>,

    /// The datatype of the containing element.
    datatype: Option<Term<F::TermData>>,

    /// The language tag of the containing element.
    lang: Option<F::TermData>,

    /// The text gathered in the current scope.
    text: Option<String>,

    /// The current count of list elements
    li: AtomicU64,
}

// We implement it ourselves instead of deriving so that:
// * F does not need to be `Clone` (deriving requires it).
// * we can clone `li` although `AtomicU64` is not `Clone`.
impl<F: TermFactory> Clone for Scope<F> {
    fn clone(&self) -> Self {
        Self {
            ns: self.ns.clone(),
            default: self.default.clone(),
            base: self.base.clone(),
            factory: self.factory.clone(),
            datatype: self.datatype.clone(),
            lang: self.lang.clone(),
            text: self.text.clone(),
            li: AtomicU64::new(self.li.load(Ordering::Relaxed)),
        }
    }
}

impl<F: TermFactory> Scope<F> {
    /// Create a new `Scope` with the given term factory.
    fn with_factory(f: F) -> Self {
        Self::with_factory_rc(Rc::new(RefCell::new(f)))
    }

    /// Create a new `Scope` from a shared smartpointer to a term factory.
    fn with_factory_rc(f: Rc<RefCell<F>>) -> Self {
        let mut scope = Self {
            ns: HashMap::new(),
            default: None,
            base: None,
            factory: f,
            datatype: None,
            lang: None,
            text: None,
            li: AtomicU64::new(1),
        };
        // These namespaces are always in scope
        scope
            .add_prefix("xml", "http://www.w3.org/XML/1998/namespace#")
            .unwrap();
        scope
            .add_prefix("xmlms", "https://www.w3.org/2000/xmlns/")
            .unwrap();
        scope
    }

    /// Add a new XML prefix to the namespace mapping.
    fn add_prefix(&mut self, prefix: &str, value: &str) -> Result<()> {
        if prefix == "_" {
            panic!("reserved prefix")
        } else {
            let mut f = self.factory.borrow_mut();
            self.ns.insert(
                String::from(prefix),
                Namespace::new(f.get_term_data(value))?,
            );
        }

        Ok(())
    }

    /// Set the default XML prefix.
    fn set_default(&mut self, default: &str) -> Result<()> {
        let mut f = self.factory.borrow_mut();
        self.default = Some(Namespace::new(f.get_term_data(default))?);
        Ok(())
    }

    /// Set the base IRI prefix.
    fn set_base(&mut self, base: &str) -> Result<()> {
        let mut f = self.factory.borrow_mut();
        self.base = Some(Namespace::new(f.get_term_data(base))?);
        Ok(())
    }

    fn set_datatype(&mut self, datatype: &str) -> Result<()> {
        self.datatype = Some(self.expand_iri(datatype)?);
        Ok(())
    }

    fn set_text<T: Into<Option<String>>>(&mut self, text: T) {
        self.text = text.into();
    }

    fn expand_attribute(&self, attr: &str) -> Result<Term<F::TermData>> {
        if let Some(separator_idx) = attr.chars().position(|c| c == ':') {
            let prefix = &attr[..separator_idx];
            let reference = &attr[separator_idx + 1..];
            if let Some(ns) = self.ns.get(prefix) {
                ns.get(self.factory.borrow_mut().get_term_data(reference))
            } else {
                panic!("unknown namespace: {}", prefix)
            }
        } else if let Some(ns) = &self.default {
            ns.get(self.factory.borrow_mut().get_term_data(attr))
        } else {
            panic!("missing prefix: {}", attr)
        }
    }

    fn expand_iri(&self, iri: &str) -> Result<Term<F::TermData>> {
        if is_absolute_iri(iri) {
            self.factory.borrow_mut().iri(iri)
        } else if is_relative_iri(iri) {
            if let Some(ns) = &self.base {
                ns.get(self.factory.borrow_mut().get_term_data(iri))
            } else {
                panic!("NO BASE IRI")
            }
        } else {
            Err(Error::from_kind(ErrorKind::InvalidIri(iri.to_owned())))
        }
    }

    fn expand_id(&self, id: &str) -> Result<Term<F::TermData>> {
        if id.starts_with("#") {
            self.expand_iri(id)
        } else {
            self.expand_iri(&format!("#{}", id))
        }
    }

    /// Create a new literal with the `rdf:type` and `xml:lang` in scope.
    fn new_literal(&self, text: String) -> Result<Term<F::TermData>> {
        match (&self.datatype, &self.lang) {
            (Some(dt), _) => self.factory.borrow_mut().literal_dt(text, dt.clone()),
            (None, Some(l)) => self.factory.borrow_mut().literal_lang(text, l.clone()),
            _ => self.factory.borrow_mut().literal_dt(text, xsd::string),
        }
    }

    /// Create a new `rdf:li` property.
    fn new_li(&self) -> Result<Term<F::TermData>> {
        if let Some(ns) = self.ns.get("rdf") {
            let mut f = self.factory.borrow_mut();
            ns.get(f.get_term_data(&format!("_{}", self.li.fetch_add(1, Ordering::Relaxed))))
        } else {
            panic!("undeclared `rdf` prefix !")
        }
    }

    /// Get the current `rdf:li` property.
    fn current_li(&self) -> Result<Term<F::TermData>> {
        if let Some(ns) = self.ns.get("rdf") {
            let mut f = self.factory.borrow_mut();
            ns.get(f.get_term_data(&format!("_{}", self.li.load(Ordering::Relaxed))))
        } else {
            panic!("undeclared `rdf` prefix !")
        }
    }
}

impl<F: TermFactory + Default> Default for Scope<F> {
    fn default() -> Self {
        Self::with_factory(Default::default())
    }
}

// ---

struct XmlParser<B: BufRead, F: TermFactory> {
    /// The underlying XML reader.
    reader: quick_xml::Reader<B>,

    /// The stack of scoped data (for nested declaration).
    scopes: Vec<Scope<F>>,

    /// The stack of parent elements (for nested declarations).
    parents: Vec<Term<F::TermData>>,

    // The queue of produced triples.
    triples: LinkedList<Result<[Term<F::TermData>; 3]>>,

    //
    factory: Rc<RefCell<F>>,

    //
    bnodes: AtomicU64,

    /// The current state of the parser.
    state: Vec<ParsingState>,
}

impl<B, F> XmlParser<B, F>
where
    B: BufRead,
    F: TermFactory + Clone + Default + Debug,
    <F as TermFactory>::TermData: Debug,
{
    // ---
    fn scope(&self) -> &Scope<F> {
        self.scopes.last().unwrap()
    }

    fn scope_mut(&mut self) -> &mut Scope<F> {
        self.scopes.last_mut().unwrap()
    }

    // Add a local scope (`lang`, `namespaces`, but not `parents`)
    fn enter_scope(&mut self, e: &BytesStart) -> Result<()> {
        // We are entering a new elements: text is not relevant anymore.
        let mut prev = self.scope_mut();
        prev.text = None;

        // Create a local scope using values from the outer one.
        let mut scope = prev.clone();
        scope.text = Some(String::new());
        scope.li.store(1, Ordering::Relaxed);

        // Update XML namespaces with those defined in the document.
        for attr in e.attributes().with_checks(true) {
            let a = attr.expect("FIXME");
            if a.key.starts_with(b"xmlns:") {
                scope
                    .add_prefix(
                        std::str::from_utf8(&a.key[6..]).expect("FIXME"),
                        &a.unescape_and_decode_value(&self.reader).expect("FIXME"),
                    )
                    .expect("FIXME");
            } else if a.key == b"xmlns" {
                scope.set_default(&a.unescape_and_decode_value(&self.reader).expect("FIXME"))?;
            } else if a.key == b"xml:base" {
                scope.set_base(&a.unescape_and_decode_value(&self.reader).expect("FIXME"))?;
            }
        }

        // Add current lang to scope or copy last one (OPTIMISE ME)
        for attr in e.attributes().with_checks(true) {
            let a = attr.expect("FIXME");
            if a.key == b"xml:lang" {
                scope.lang = if a.value.is_empty() {
                    None
                } else {
                    self.factory
                        .borrow_mut()
                        .get_term_data(&a.unescape_and_decode_value(&self.reader).unwrap())
                        .into()
                };
            }
        }

        // Make the newly created scope the local one.
        self.scopes.push(scope);
        Ok(())
    }

    // Exit the local scope
    fn leave_scope(&mut self) {
        self.scopes.pop().expect("FIXME");
    }

    // ---

    // Create a new bnode term (using `n` prefix).
    fn new_bnode(&self) -> Term<F::TermData> {
        self.factory
            .borrow_mut()
            .bnode(&format!("n{}", self.bnodes.fetch_add(1, Ordering::Relaxed)))
            .unwrap()
    }

    // Create a new predicate IRI from an XML name (or a RDF metasyntactic element)
    fn predicate_iri_start(&self, name: &str) -> Result<Term<F::TermData>> {
        let mut p = self.scope().expand_attribute(name)?;
        if p.matches(&rdf::li) {
            let parent_scope = self.scopes.get(self.scopes.len() - 2).unwrap();
            parent_scope.new_li()
        } else {
            Ok(p)
        }
    }

    // Retrieve a predicate IRI from an XML name
    fn predicate_iri_end(&self, name: &str) -> Result<Term<F::TermData>> {
        let mut p = self.scope().expand_attribute(name)?;
        if p.matches(&rdf::li) {
            let parent_scope = self.scopes.get(self.scopes.len() - 2).unwrap();
            parent_scope.current_li()
        } else {
            Ok(p)
        }
    }

    // ---

    fn new(reader: quick_xml::Reader<B>) -> Self {
        let factory: Rc<RefCell<F>> = Default::default();
        Self {
            reader,
            parents: Vec::new(),
            scopes: vec![Scope::with_factory_rc(factory.clone())],
            triples: LinkedList::new(),
            factory: factory,
            bnodes: AtomicU64::new(0),
            state: vec![ParsingState::Node],
        }
    }

    // ---

    fn element_start(&mut self, e: &BytesStart) {
        self.enter_scope(e);
        let res = match self.state.last().unwrap() {
            ParsingState::Node => self.node_start(e),
            ParsingState::Predicate => self.predicate_start(e),
            ParsingState::Resource => self.predicate_start(e),
            _ => unimplemented!(),
        };
    }

    fn node_start(&mut self, e: &BytesStart) {
        // Bail out if this the top level rdf:RDF
        if e.name() == b"rdf:RDF" {
            self.state.push(ParsingState::Node);
            self.parents.push(self.factory.borrow_mut().copy(&rdf::RDF));
            return;
        }

        // Separate node subject from other attributes
        let mut properties = HashMap::new();
        let mut subject = Vec::new();
        for attr in e.attributes().with_checks(true) {
            let a = attr.expect("FIXME");

            // ignore xmlns attributes (processed in element_start)
            if a.key.starts_with(b"xmlns:") || a.key == b"xmlns" {
                continue;
            }

            // try to extract the subject annotation
            let k = self
                .scope()
                .expand_attribute(std::str::from_utf8(a.key).expect("FIXME"))
                .expect("FIXME");
            let v = a.unescape_and_decode_value(&self.reader).expect("FIXME");

            if k.matches(&rdf::about) {
                subject.push(self.scope().expand_iri(&v).expect("INVALID IRI"));

            //
            // if is_absolute_iri(&v) {
            //     subject.push(self.factory.borrow_mut().iri(v).expect("FIXME"));
            // } else if is_relative_iri(&v) {
            //     subject.push(ns.expand_resource(&v));
            // }
            } else if k.matches(&rdf::ID) {
                subject.push(self.scope().expand_id(&v).expect("INVALID NAME"));
            // if v.starts_with("#") {
            //     subject.push(ns.expand_id(&v))
            // } else {
            //     subject.push(ns.expand_id(&format!("#{}", v)));
            // }
            } else if k.matches(&rdf::nodeID) {
                subject.push(
                    self.factory
                        .borrow_mut()
                        .bnode(&format!("o{}", v))
                        .expect("INVALID BNODE"),
                );
            } else if !k.matches(&xml::lang) {
                // Ignore xml:lang attributes
                properties.insert(
                    k,
                    self.factory
                        .borrow_mut()
                        .literal_dt(v, xsd::string)
                        .expect("FIXME"),
                );
            }
        }

        // Get subject and add it to the current nested stack
        if subject.len() > 1 {
            panic!("cannot have rdf:ID, rdf:about and rdf:nodeId at the same time")
        }
        let s: Term<_> = subject.pop().unwrap_or_else(|| self.new_bnode());
        self.parents.push(s.clone());

        // Add the type as a triple if it is not `rdf:Description`
        let ty = self
            .scope()
            .expand_attribute(
                std::str::from_utf8(e.name()).expect("INVALID DATATYPE IRI REFERENCE"),
            )
            .expect("INVALID DATATYPE IRI REFERENCE");
        if !ty.matches(&rdf::Description) {
            self.triples.push_back(Ok([
                s.clone(),
                self.factory.borrow_mut().copy(&rdf::type_),
                ty,
            ]));
        }

        // Add triples described by properties in XML attributes
        for (p, lit) in properties {
            self.triples.push_back(Ok([s.clone(), p, lit]))
        }

        // Next start event is expected to be a predicate
        self.state.push(ParsingState::Predicate);
    }

    fn predicate_start(&mut self, e: &BytesStart) {
        // Get the predicate and add it to the current nested stack
        // or build a new `rdf:_n` IRI if the predicate is `rdf:li`.
        let p = self
            .predicate_iri_start(std::str::from_utf8(e.name()).expect("FIXME"))
            .expect("INVALID PREDICATE IRI");
        self.parents.push(p);

        // Extract attributes relevant to the RDF syntax
        let mut next_state = ParsingState::Node;
        for attr in e.attributes().with_checks(true) {
            let a = attr.expect("FIXME");
            let k = self
                .scope()
                .expand_attribute(std::str::from_utf8(a.key).expect("FIXME"))
                .expect("INVALID ATTRIBUTE");
            if k.matches(&rdf::datatype) {
                let v = a.unescape_and_decode_value(&self.reader).expect("FIXME");
                self.scope_mut().set_datatype(&v);
            } else if k.matches(&rdf::parseType) {
                match a.value.as_ref() {
                    b"Resource" => {
                        self.parents.push(self.new_bnode());
                        next_state = ParsingState::Resource;
                    }
                    b"Literal" => next_state = ParsingState::Literal,
                    other => panic!("invalid parseType: {:?}", other),
                }
            }
        }
        self.state.push(next_state);
    }

    // ---

    fn element_end(&mut self, e: &BytesEnd) {
        println!("{:?}", self.state);
        println!(
            "exiting {:?}: {:?}",
            std::str::from_utf8(e.name()),
            self.parents
        );

        match self.state.pop().unwrap() {
            ParsingState::Node => self.predicate_end(e),
            ParsingState::Predicate => self.node_end(e),
            ParsingState::Resource => self.resource_end(e),
            _ => unimplemented!(),
        }

        self.leave_scope();
    }

    fn node_end(&mut self, e: &BytesEnd) {
        // Add the entity as a triple object if it is not top-level
        let o = self.parents.pop().unwrap();
        if self.parents.len() > 2 {
            let s = &self.parents[self.parents.len() - 2];
            let p = &self.parents[self.parents.len() - 1];
            if !s.matches(&rdf::RDF) {
                self.triples.push_back(Ok([s.clone(), p.clone(), o]));
            }
        }
    }

    fn predicate_end(&mut self, e: &BytesEnd) {
        // Build the predicate IRI
        let p = self
            .predicate_iri_end(std::str::from_utf8(e.name()).expect("FIXME"))
            .expect("INVALID PREDICATE IRI");

        // Get the literal value
        if self.parents.len() > 2 {
            if let Some(text) = self.scope_mut().text.take() {
                let s = self.parents[self.parents.len() - 2].clone();
                let o = self.scope_mut().new_literal(text).expect("FIXME");
                self.triples.push_back(Ok([s, p, o]));
            }
        }

        self.parents.pop();
    }

    fn resource_end(&mut self, e: &BytesEnd) {
        self.node_end(e);
        self.predicate_end(e)
    }

    // --- Text elements ----------------------------------------------------

    fn element_text(&mut self, e: &BytesText) {
        if self.scope().text.is_some() {
            let text = e.unescape_and_decode(&self.reader).expect("FIXME");
            self.scope_mut().set_text(text);
        }
    }

    // --- Empty elements ----------------------------------------------------

    fn element_empty(&mut self, e: &BytesStart) {
        self.enter_scope(e);

        match self.state.last().unwrap() {
            ParsingState::Node => self.node_empty(e),
            ParsingState::Predicate => self.predicate_empty(e),
            ParsingState::Resource => self.resource_empty(e),
            _ => (),
        }

        self.leave_scope();
    }

    fn node_empty(&mut self, e: &BytesStart) {
        // FIXME
    }

    fn predicate_empty(&mut self, e: &BytesStart) {
        let p = self
            .predicate_iri_start(std::str::from_utf8(e.name()).expect("FIXME"))
            .expect("INVALID PREDICATE IRI");

        let mut object = Vec::with_capacity(1);
        let mut attributes = HashMap::new();
        for attr in e.attributes().with_checks(true) {
            let a = attr.expect("FIXME");

            // try to extract the annotation object
            let k = self
                .scope()
                .expand_attribute(std::str::from_utf8(a.key).expect("FIXME"))
                .expect("FIXME");
            let v = a.unescape_and_decode_value(&self.reader).expect("FIXME");
            if k.matches(&rdf::resource) {
                object.push(self.scope().expand_iri(&v).expect("INVALID IRI"));
            } else if k.matches(&rdf::nodeID) {
                object.push(
                    self.factory
                        .borrow_mut()
                        .bnode(format!("o{}", v))
                        .expect("FIXME"),
                );
            } else if !k.matches(&xml::lang) && !a.key.starts_with(b"xmlns") {
                attributes.insert(k, v);
            }
        }

        match object.len() {
            0 => {
                let s = self.parents.last().unwrap().clone();
                let o = self.new_bnode();
                self.triples.push_back(Ok([s, p, o.clone()]));
                for (prop, value) in attributes.into_iter() {
                    let literal = self.scope().new_literal(value).expect("FIXME");
                    self.triples.push_back(Ok([o.clone(), prop, literal]));
                }
            }
            1 => {
                // Ignoring property attributes
                let s = self.parents.last().unwrap().clone();
                let o = object.pop().unwrap();
                self.triples.push_back(Ok([s, p, o]));
            }
            _ => {
                panic!("cannot have rdf:resource and rdf:nodeID at the same time");
            }
        }

        // let o = match object.len() {
        //     0 => panic!("missing resource in empty predicate !"),
        //     1 => object.pop().unwrap(),
        //     _ => panic!(""),
        // };
        // self.triples.push_back(Ok([s.clone(), p, o]));
    }

    fn resource_empty(&mut self, e: &BytesStart) {
        self.predicate_empty(e)
    }
}

impl<B, F> Iterator for XmlParser<B, F>
where
    B: BufRead,
    F: TermFactory + Clone + Default + Debug,
    <F as TermFactory>::TermData: Debug,
{
    type Item = Result<[Term<F::TermData>; 3]>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // First make sure to consume the queue.
            if let Some(triple) = self.triples.pop_front() {
                return Some(triple);
            }
            // Then process the next event to maybe produce triples
            match &self.reader.read_event(&mut Vec::new()).unwrap() {
                Event::Eof => return None,
                Event::Start(s) => self.element_start(s),
                Event::Empty(e) => self.element_empty(e),
                Event::End(e) => self.element_end(e),
                Event::Text(t) => self.element_text(t),
                _ => (),
            }
        }
    }
}

// ---

#[cfg(test)]
mod test {

    use std::fmt::Debug;
    use std::fmt::Formatter;
    use std::fmt::Result as FmtResult;

    use crate::graph::inmem::HashGraph;
    use crate::graph::inmem::TermIndexMapU;
    use crate::graph::Graph;
    use crate::term::factory::RcTermFactory;
    use crate::term::IriData;
    use crate::term::StaticTerm;
    use crate::term::Term;
    use crate::triple::stream::TripleSource;
    use crate::triple::Triple;

    type TestGraph = HashGraph<TermIndexMapU<u16, RcTermFactory>>;

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
    // fn w3c_test_suite() {
    //     fn do_test_suite() -> io::Result<()> {
    //         let rdf_ext = OsStr::new("rdf");
    //         let nt_ext = OsStr::new("nt");
    //
    //         let suite = Path::new("..").join("rdf-tests").join("rdf-xml");
    //         if !suite.exists() || !suite.is_dir() {
    //             panic!("rdf-tests/rdf-xml not found, can not check W3C test-suite. cf README.md");
    //         }
    //
    //         let mut tested = 0;
    //
    //         for e in read_dir(&suite)? {
    //             let entry = e?;
    //             if entry.file_type()?.is_dir() {
    //                 for c in read_dir(entry.path())? {
    //                     let case = c?;
    //                     if case.path().extension() == Some(rdf_ext) {
    //                         if case.path().with_extension(nt_ext).is_file() {
    //                             println!("{}", case.path().display());
    //
    //                             // the reference N-Triples file
    //                             let ntparser = crate::parser::nt::Config::default();
    //                             let ntfile = File::open(case.path().with_extension(nt_ext))?;
    //                             let mut expected = TestGraph::new();
    //                             ntparser.parse_read(ntfile).in_graph(&mut expected).unwrap();
    //                             // the test XML file
    //                             let xmlparser = super::Config::default();
    //                             let xmlfile = File::open(case.path())?;
    //                             let mut actual = TestGraph::new();
    //                             let res = xmlparser.parse_read(xmlfile).in_graph(&mut actual);
    //
    //                             // check the XML parses without error
    //                             assert!(
    //                                 res.is_ok(),
    //                                 format!("{} should parse without error", case.path().display())
    //                             );
    //                             // check the XML produces the same graph
    //                             pretty_assertions::assert_eq!(
    //                                 actual,
    //                                 expected,
    //                                 "{} does not give expected results",
    //                                 case.path().display()
    //                             );
    //
    //                             tested += 1;
    //                         } else if case.path().to_string_lossy().contains("error") {
    //                             // let xmlparser = super::Config::default();
    //                             // let xmlfile = File::open(case.path())?;
    //                             // let mut actual = TestGraph::new();
    //                             // assert!(
    //                             //     xmlparser.parse_read(xmlfile).in_graph(&mut actual).is_err(),
    //                             //     format!("{} should parse with error", case.path().display())
    //                             // );
    //                             //
    //                             // tested += 1;
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //
    //         assert_ne!(
    //             tested, 0,
    //             "No test found in W3C test-suite, something must be wrong"
    //         );
    //         Ok(())
    //     }
    //     do_test_suite().unwrap()
    // }

    macro_rules! w3c_example {
        ($name:ident, $xml:literal, $nt:literal) => {
            #[test]
            fn $name() {
                let mut g = TestGraph::new();
                super::Config::default()
                    .parse_str($xml)
                    .in_graph(&mut g)
                    .expect("failed parsing XML file");

                let mut nt = Vec::new();
                for triple in crate::parser::nt::Config::default().parse_str($nt) {
                    nt.push(triple.expect("N-Triples iterator failed"));
                }

                assert_eq!(g.len(), nt.len(), "unexpected number of triples: {:#?}", g);
                for t in nt.into_iter() {
                    assert!(
                        g.contains(t.s(), t.p(), t.o()).expect(".contains failed"),
                        "missing triple: ({:?} {:?} {:?}) in {:#?}",
                        t.s(),
                        t.p(),
                        t.o(),
                        g
                    );
                }
            }
        };
    }

    // Example 07: 'Complete RDF/XML description of Figure 1 graph'
    w3c_example! {
        w3c_example_07,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:dc="http://purl.org/dc/elements/1.1/"
                     xmlns:ex="http://example.org/stuff/1.0/">
              <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
                       dc:title="RDF/XML Syntax Specification (Revised)">
                <ex:editor>
                  <rdf:Description ex:fullName="Dave Beckett">
                <ex:homePage rdf:resource="http://purl.org/net/dajobe/" />
                  </rdf:Description>
                </ex:editor>
              </rdf:Description>
            </rdf:RDF>
        "#,
        r#"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)" .
           _:n0 <http://example.org/stuff/1.0/fullName> "Dave Beckett" .
           _:n0 <http://example.org/stuff/1.0/homePage> <http://purl.org/net/dajobe/> .
           <http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> _:n0 .
        "#
    }

    // Example 08: 'Complete example of xml:lang'
    w3c_example! {
        w3c_example_08,
        r#"<?xml version="1.0" encoding="utf-8"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:dc="http://purl.org/dc/elements/1.1/">
              <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
                <dc:title>RDF/XML Syntax Specification (Revised)</dc:title>
                <dc:title xml:lang="en">RDF/XML Syntax Specification (Revised)</dc:title>
                <dc:title xml:lang="en-US">RDF/XML Syntax Specification (Revised)</dc:title>
              </rdf:Description>

              <rdf:Description rdf:about="http://example.org/buecher/baum" xml:lang="de">
                <dc:title>Der Baum</dc:title>
                <dc:description>Das Buch ist außergewöhnlich</dc:description>
                <dc:title xml:lang="en">The Tree</dc:title>
              </rdf:Description>
            </rdf:RDF>
        "#,
        r#"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)" .
           <http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)"@en .
           <http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)"@en-us .
           <http://example.org/buecher/baum> <http://purl.org/dc/elements/1.1/title> "Der Baum"@de .
           <http://example.org/buecher/baum> <http://purl.org/dc/elements/1.1/description> "Das Buch ist au\u00DFergew\u00F6hnlich"@de .
           <http://example.org/buecher/baum> <http://purl.org/dc/elements/1.1/title> "The Tree"@en .
        "#
    }

    // Example 09: 'Complete example of rdf:parseType="Literal"'
    w3c_example! {
        w3c_example_09,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:ex="http://example.org/stuff/1.0/">
              <rdf:Description rdf:about="http://example.org/item01">
                <ex:size rdf:datatype="http://www.w3.org/2001/XMLSchema#int">123</ex:size>
              </rdf:Description>
            </rdf:RDF>
        "#,
        r#"<http://example.org/item01> <http://example.org/stuff/1.0/size> "123"^^<http://www.w3.org/2001/XMLSchema#int> .
        "#
    }

    // Example 10: 'Complete example of rdf:datatype'
    w3c_example! {
        w3c_example_10,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:ex="http://example.org/stuff/1.0/">
              <rdf:Description rdf:about="http://example.org/item01">
                <ex:size rdf:datatype="http://www.w3.org/2001/XMLSchema#int">123</ex:size>
              </rdf:Description>
            </rdf:RDF>
        "#,
        r#"<http://example.org/item01> <http://example.org/stuff/1.0/size> "123"^^<http://www.w3.org/2001/XMLSchema#int> .
        "#
    }

    // Example 11: 'Complete RDF/XML description of graph using rdf:nodeID identifying the blank node'
    w3c_example! {
        w3c_example_11,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:dc="http://purl.org/dc/elements/1.1/"
                     xmlns:ex="http://example.org/stuff/1.0/">
              <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
            		   dc:title="RDF/XML Syntax Specification (Revised)">
                <ex:editor rdf:nodeID="abc"/>
              </rdf:Description>

              <rdf:Description rdf:nodeID="abc"
                               ex:fullName="Dave Beckett">
                <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
              </rdf:Description>
            </rdf:RDF>
        "#,
        // This is with renamed node IDs
        r#"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)" .
           <http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> _:oabc .
           _:oabc <http://example.org/stuff/1.0/fullName> "Dave Beckett" .
           _:oabc <http://example.org/stuff/1.0/homePage> <http://purl.org/net/dajobe/> .
        "#
    }

    // Example 12: 'Complete example using rdf:parseType="Resource"'
    w3c_example! {
        w3c_example_12,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:dc="http://purl.org/dc/elements/1.1/"
                     xmlns:ex="http://example.org/stuff/1.0/">
              <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
            		   dc:title="RDF/XML Syntax Specification (Revised)">
                <ex:editor rdf:parseType="Resource">
                  <ex:fullName>Dave Beckett</ex:fullName>
                  <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
                </ex:editor>
              </rdf:Description>
            </rdf:RDF>
        "#,
        // This is with renamed node IDs
        r#"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)" .
           _:n0 <http://example.org/stuff/1.0/fullName> "Dave Beckett" .
           _:n0 <http://example.org/stuff/1.0/homePage> <http://purl.org/net/dajobe/> .
           <http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> _:n0 .
        "#
    }

    // Example 13: 'Complete example of property attributes on an empty property element'
    w3c_example! {
        w3c_example_13,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:dc="http://purl.org/dc/elements/1.1/"
                     xmlns:ex="http://example.org/stuff/1.0/">
              <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar"
            		   dc:title="RDF/XML Syntax Specification (Revised)">
                <ex:editor ex:fullName="Dave Beckett" />
                <!-- Note the ex:homePage property has been ignored for this example -->
              </rdf:Description>
            </rdf:RDF>
        "#,
        r#"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF/XML Syntax Specification (Revised)" .
           _:n0 <http://example.org/stuff/1.0/fullName> "Dave Beckett" .
           <http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> _:n0 .
        "#
    }

    // Example 14: 'Complete example with rdf:type'
    w3c_example! {
        w3c_example_14,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:dc="http://purl.org/dc/elements/1.1/"
                     xmlns:ex="http://example.org/stuff/1.0/">
              <rdf:Description rdf:about="http://example.org/thing">
                <rdf:type rdf:resource="http://example.org/stuff/1.0/Document"/>
                <dc:title>A marvelous thing</dc:title>
              </rdf:Description>
            </rdf:RDF>
        "#,
        r#"<http://example.org/thing> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/stuff/1.0/Document> .
           <http://example.org/thing> <http://purl.org/dc/elements/1.1/title> "A marvelous thing" .
        "#
    }

    // Example 15: 'Complete example using a typed node element to replace an rdf:type'
    w3c_example! {
        w3c_example_15,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:dc="http://purl.org/dc/elements/1.1/"
                     xmlns:ex="http://example.org/stuff/1.0/">
              <ex:Document rdf:about="http://example.org/thing">
                <dc:title>A marvelous thing</dc:title>
              </ex:Document>
            </rdf:RDF>
        "#,
        r#"<http://example.org/thing> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/stuff/1.0/Document> .
           <http://example.org/thing> <http://purl.org/dc/elements/1.1/title> "A marvelous thing" .
        "#
    }

    // Example 16: 'Complete example using rdf:ID and xml:base for shortening URIs'
    w3c_example! {
        w3c_example_16,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:ex="http://example.org/stuff/1.0/"
                     xml:base="http://example.org/here/">
              <rdf:Description rdf:ID="snack">
                <ex:prop rdf:resource="fruit/apple"/>
              </rdf:Description>
            </rdf:RDF>
        "#,
        r#"<http://example.org/here/#snack> <http://example.org/stuff/1.0/prop> <http://example.org/here/fruit/apple> .
        "#
    }

    // Example 17: 'Complex example using RDF list properties'
    w3c_example! {
        w3c_example_17,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
              <rdf:Seq rdf:about="http://example.org/favourite-fruit">
                <rdf:_1 rdf:resource="http://example.org/banana"/>
                <rdf:_2 rdf:resource="http://example.org/apple"/>
                <rdf:_3 rdf:resource="http://example.org/pear"/>
              </rdf:Seq>
            </rdf:RDF>
        "#,
        r#"<http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq> .
           <http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_1> <http://example.org/banana> .
           <http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_2> <http://example.org/apple> .
           <http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_3> <http://example.org/pear> .
        "#
    }

    // Example 18: 'Complete example using rdf:li property element for list properties'
    w3c_example! {
        w3c_example_18,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
              <rdf:Seq rdf:about="http://example.org/favourite-fruit">
                <rdf:li rdf:resource="http://example.org/banana"/>
                <rdf:li rdf:resource="http://example.org/apple"/>
                <rdf:li rdf:resource="http://example.org/pear"/>
              </rdf:Seq>
            </rdf:RDF>
        "#,
        r#"<http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq> .
           <http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_1> <http://example.org/banana> .
           <http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_2> <http://example.org/apple> .
           <http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_3> <http://example.org/pear> .
        "#
    }

    // Example 19: 'Complete example of a RDF collection of nodes using rdf:parseType="Collection"'
    w3c_example! {
        w3c_example_19,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:ex="http://example.org/stuff/1.0/">
                <rdf:Description rdf:about="http://example.org/basket">
                    <ex:hasFruit rdf:parseType="Collection">
                      <rdf:Description rdf:about="http://example.org/banana"/>
                      <rdf:Description rdf:about="http://example.org/apple"/>
                      <rdf:Description rdf:about="http://example.org/pear"/>
                    </ex:hasFruit>
                </rdf:Description>
            </rdf:RDF>
        "#,
        r#"_:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/banana> .
           _:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/apple> .
           _:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid2 .
           _:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/pear> .
           _:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid3 .
           _:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
           <http://example.org/basket> <http://example.org/stuff/1.0/hasFruit> _:genid1 .
        "#
    }

    // Example 20: 'Complete example of rdf:ID reifying a property element'
    w3c_example! {
        w3c_example_20,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:ex="http://example.org/stuff/1.0/"
                     xml:base="http://example.org/triples/">
              <rdf:Description rdf:about="http://example.org/">
                <ex:prop rdf:ID="triple1">blah</ex:prop>
              </rdf:Description>
            </rdf:RDF>
        "#,
        r#"<http://example.org/> <http://example.org/stuff/1.0/prop> "blah" .
           <http://example.org/triples/#triple1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement> .
           <http://example.org/triples/#triple1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#subject> <http://example.org/> .
           <http://example.org/triples/#triple1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate> <http://example.org/stuff/1.0/prop> .
           <http://example.org/triples/#triple1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#object> "blah" .
        "#
    }

    w3c_example! {
        nested_li,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
              <rdf:Seq rdf:about="http://example.org/favourite-fruit">
                <rdf:li rdf:resource="http://example.org/banana"/>
                <rdf:li>
                    <rdf:Seq rdf:about="http://example.org/berry">
                        <rdf:li rdf:resource="http://example.org/blueberry"/>
                        <rdf:li rdf:resource="http://example.org/strawberry"/>
                    </rdf:Seq>
                </rdf:li>
                <rdf:li rdf:resource="http://example.org/orange"/>
              </rdf:Seq>
            </rdf:RDF>
        "#,
        r#"<http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq> .
           <http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_1> <http://example.org/banana> .
           <http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_2> <http://example.org/berry> .
           <http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_3> <http://example.org/orange> .
           <http://example.org/berry> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq> .
           <http://example.org/berry> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_1> <http://example.org/blueberry> .
           <http://example.org/berry> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_2> <http://example.org/strawberry> .
        "#
    }

}
