//! Parser for RDF XML.

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::LinkedList;
use std::io::BufRead;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use quick_xml::events::Event;
use quick_xml::Reader;
use url::Url;

// use crate::error::*;
use crate::ns::rdf;
use crate::ns::xsd;
use crate::ns::Namespace;
use crate::parser::{LocatableError, TripleParser};
use sophia_term::factory::RcTermFactory;
use sophia_term::factory::TermFactory;
use sophia_term::iri::is_absolute_iri_ref;
use sophia_term::iri::is_relative_iri_ref;
use sophia_term::matcher::TermMatcher;
use sophia_term::StaticTerm;
use sophia_term::Term;
use sophia_term::TermError;

mod _error;
pub use self::_error::*;
mod _scope;
pub use self::_scope::*;
mod _handler;
use self::_handler::*;

const DEFAULT_BUFFER_SIZE: usize = 8 * 1024;

static RESERVED_NODE_NAMES: &[&StaticTerm] = &[
    &rdf::RDF,
    &rdf::ID,
    &rdf::about,
    &rdf::bagID,
    &rdf::parseType,
    &rdf::resource,
    &rdf::nodeID,
    &rdf::li,
    &rdf::aboutEach,
    &rdf::aboutEachPrefix,
];

static RESERVED_PROPERTY_NAMES: &[&StaticTerm] = &[
    &rdf::Description,
    &rdf::RDF,
    &rdf::ID,
    &rdf::about,
    &rdf::bagID,
    &rdf::parseType,
    &rdf::resource,
    &rdf::nodeID,
    &rdf::aboutEach,
    &rdf::aboutEachPrefix,
];

static RESERVED_ATTRIBUTES_NAMES: &[&StaticTerm] = &[
    &rdf::li,
    &rdf::aboutEach,
    &rdf::aboutEachPrefix,
    &rdf::bagID,
];

mod xmlname {

    use regex::Regex;

    lazy_static::lazy_static! {
        static ref XMLNAME_REGEX: Regex = Regex::new(r"(?x)^
            # NameStartChar
            [_A-Za-z\u{C0}-\u{D6}\u{D8}-\u{F6}\u{F8}-\u{2FF}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}]
            # NameChar
            [-.0-9\u{B7}_A-Za-z\u{C0}-\u{D6}\u{D8}-\u{F6}\u{F8}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{203F}-\u{2040}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}]*
        $").unwrap();
    }

    pub fn is_valid_xmlname(n: &str) -> bool {
        XMLNAME_REGEX.is_match(n)
    }
}

/// RDF/XML parser.
#[derive(Clone, Debug, Default)]
pub struct RdfXmlParser {
    base: Option<Url>,
}

impl RdfXmlParser {
    pub fn with_base(base: &str) -> Result<Self> {
        match Url::parse(base) {
            Ok(url) => Ok(Self { base: Some(url) }),
            Err(_) => Err(RdfError::InvalidBaseIri(base.to_owned()).into()),
        }
    }
}

impl<B: BufRead> TripleParser<B> for RdfXmlParser {
    type Source = RdfXmlSource<B, RcTermFactory>;
    fn parse(&self, data: B) -> Self::Source {
        match &self.base {
            Some(base) => RdfXmlSource::with_base(Reader::from_reader(data), base.clone()),
            None => RdfXmlSource::new(Reader::from_reader(data)),
        }
    }
}

def_mod_functions_for_bufread_parser!(RdfXmlParser, TripleParser);

// ---

/// A wrapper for `quick_xml::Reader` ignoring or merging some events.
pub struct XmlReader<B: BufRead> {
    inner: Reader<B>,
    event: Option<Event<'static>>, // actually 'buffer
    buffer: Vec<u8>,
}

impl<B: BufRead> XmlReader<B> {
    /// Read an XML event.
    pub fn read_event<'a>(&mut self, buf: &'a mut Vec<u8>) -> Result<Event<'a>, quick_xml::Error> {
        use quick_xml::events::Event::*;

        // Clear the event peeking cache if it is not empty.
        if let Some(e) = self.event.take() {
            return Ok(e);
        }

        // Get a `Start` event, or return if it is something else.
        let start = match self.inner.read_event(buf)? {
            Start(ref s) => s.clone(),
            other => return Ok(other),
        };

        // Get a `Text` event, return `Start`, `End`, `Empty` or `Eof`,
        // or ignore other event (such as `Comment`).
        // The `transmute` make the compiler think the event now has a
        // static lifetime, where it only has the lifetime of the struct.
        // This is OK because we never return an event exceeding the lifetime
        // of the `XmlReader` itself.
        loop {
            self.buffer.clear();
            match self.inner.read_event(&mut self.buffer)? {
                Text(ref e) if e.is_empty() => break,
                Comment(_) | CData(_) | Decl(_) | PI(_) | DocType(_) => (),
                other => unsafe {
                    self.event = Some(std::mem::transmute(other));
                    return Ok(Start(start));
                },
            }
        }

        // Get an `End` event, or return if it is something else with
        // semantic value.
        loop {
            self.buffer.clear();
            match self.inner.read_event(&mut self.buffer)? {
                End(_) => return Ok(Empty(start)),
                Text(ref e) if e.is_empty() => (),
                Comment(_) | CData(_) | Decl(_) | PI(_) | DocType(_) => (),
                other => unsafe {
                    self.event = Some(std::mem::transmute(other));
                    return Ok(Start(start));
                },
            }
        }
    }
}

impl<B: BufRead> From<Reader<B>> for XmlReader<B> {
    fn from(r: Reader<B>) -> Self {
        Self {
            inner: r,
            event: None,
            buffer: Vec::with_capacity(DEFAULT_BUFFER_SIZE),
        }
    }
}

impl<B: BufRead> Deref for XmlReader<B> {
    type Target = Reader<B>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// The triple source returned by RdfXmlParser.
pub struct RdfXmlSource<B: BufRead, F: TermFactory> {
    handler: XmlHandler<B, F>,
    buffer: Vec<u8>,
}

impl<B, F> RdfXmlSource<B, F>
where
    B: BufRead,
    F: TermFactory + Clone + Default,
{
    /// Create a new `RdfXmlSource` from the given `quick_xml::Reader`.
    fn new(reader: Reader<B>) -> Self {
        Self {
            handler: XmlHandler::new(reader),
            buffer: Vec::with_capacity(DEFAULT_BUFFER_SIZE),
        }
    }

    /// Create a new `RdfXmlSource` using the given URL as the top-level `xml:base`.
    fn with_base(reader: Reader<B>, base: Url) -> Self {
        Self {
            handler: XmlHandler::with_base(reader, base),
            buffer: Vec::with_capacity(DEFAULT_BUFFER_SIZE),
        }
    }
}

impl<B, F> Iterator for RdfXmlSource<B, F>
where
    B: BufRead,
    F: TermFactory + Clone + Default,
{
    type Item = Result<[Term<F::TermData>; 3]>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // First make sure to consume the queue.
            if let Some(res) = self.handler.triples.pop_front() {
                return Some(res);
            }

            //
            self.buffer.clear();

            // Then process the next event to maybe produce triples
            match self.handler.reader.read_event(&mut self.buffer) {
                Ok(Event::Eof) => return None,
                Ok(Event::Start(s)) => self.handler.element_start(&s),
                Ok(Event::Empty(e)) => self.handler.element_empty(&e),
                Ok(Event::End(e)) => self.handler.element_end(&e),
                Ok(Event::Text(t)) => self.handler.element_text(&t),
                Ok(_) => (),
                Err(e) => {
                    self.handler
                        .triples
                        .push_back(Err(e.locate_with(&self.handler.reader)));
                }
            }
        }
    }
}

// ---

#[cfg(test)]
#[allow(non_snake_case)]
mod test {

    use std::fmt::Debug;
    use std::fmt::Formatter;
    use std::fmt::Result as FmtResult;

    use crate::graph::inmem::HashGraph;
    use crate::graph::inmem::TermIndexMapU;
    use crate::graph::Graph;
    use crate::parser::TripleParser;
    use crate::triple::stream::TripleSource;
    use crate::triple::Triple;
    use sophia_term::factory::RcTermFactory;
    use sophia_term::{BoxTerm, CopyTerm, TTerm, Term};

    type TestGraph = HashGraph<TermIndexMapU<u16, RcTermFactory>>;

    impl Debug for TestGraph {
        fn fmt(&self, f: &mut Formatter) -> FmtResult {
            let mut v = Vec::new();
            for t in self.triples() {
                let t = t.unwrap();
                v.push([
                    BoxTerm::copy(t.s()),
                    BoxTerm::copy(t.p()),
                    BoxTerm::copy(t.o()),
                ]);
            }
            v.sort_by_key(|t| {
                (
                    t.s().value().to_string(),
                    t.p().value().to_string(),
                    t.o().value().to_string(),
                )
            });
            v.fmt(f)
        }
    }

    macro_rules! assert_graph_eq {
        ($l:ident, $r:ident) => {
            assert_eq!(
                $l.len(),
                $r.len(),
                "unexpected number of triples: {:#?}",
                $l
            );
            for t in $r.triples().map(Result::unwrap) {
                assert!(
                    $l.contains(t.s(), t.p(), t.o()).expect(".contains failed"),
                    "missing triple: ({:?} {:?} {:?}) in {:#?}",
                    t.s(),
                    t.p(),
                    t.o(),
                    $l
                );
            }
        };
    }

    macro_rules! rdf_test {
        ($(#[$attr:meta])* $suite:ident / $case:ident where $($l:pat => $r:expr),*) => {
            $(#[$attr])*
            #[test]
            fn $case() {
                let path = std::path::PathBuf::from("..")
                    .join("rdf-tests")
                    .join("rdf-xml")
                    .join(stringify!($suite).replace('_', "-"))
                    .join(stringify!($case).replace('_', "-"));

                let ntfile = std::fs::File::open(path.with_extension("nt")).unwrap();
                let xmlfile = std::fs::File::open(path.with_extension("rdf")).unwrap();

                let mut xml = TestGraph::new();
                $crate::parser::xml::RdfXmlParser::with_base(&format!(
                        "http://www.w3.org/2013/RDFXMLTests/{}/{}.rdf",
                        stringify!($suite).replace('_', "-"),
                        stringify!($case).replace('_', "-"),
                    ))
                    .unwrap()
                    .parse(std::io::BufReader::new(xmlfile))
                    .add_to_graph(&mut xml)
                    .expect("failed parsing XML file");

                let mut nt = TestGraph::new();
                $crate::parser::nt::parse_bufread(std::io::BufReader::new(ntfile))
                    .add_to_graph(&mut nt)
                    .expect("failed parsing N-Triples file");

                use std::rc::Rc;
                use sophia_term::factory::TermFactory;
                use crate::graph::MutableGraph;

                fn relabel(factory: &mut RcTermFactory, t: Term<Rc<str>>) -> Term<Rc<str>> {
                    if let Term::BNode(bnode) = t {
                        match bnode.as_str() {
                            $($l => factory.bnode($r).unwrap(),)*
                            other => factory.bnode(other).unwrap(),
                        }
                    } else {
                        t
                    }
                }

                let mut iso = TestGraph::new();
                let mut factory = RcTermFactory::default();
                for t in nt.triples().map(Result::unwrap) {
                    iso.insert(
                        &relabel(&mut factory, t.s().clone()),
                        &relabel(&mut factory, t.p().clone()),
                        &relabel(&mut factory, t.o().clone()),
                    ).unwrap();
                }

                assert_graph_eq!(xml, iso);
            }
        };
        ($(#[$attr:meta])* $suite:ident / $case:ident) => {
            rdf_test!($(#[$attr])* $suite / $case where);
        };
    }

    macro_rules! rdf_failure {
        ($(#[$attr:meta])* $suite:ident / $case:ident) => {
            $(#[$attr])*
            #[test]
            fn $case() {
                let path = std::path::PathBuf::from("..")
                    .join("rdf-tests")
                    .join("rdf-xml")
                    .join(stringify!($suite).replace('_', "-"))
                    .join(stringify!($case).replace('_', "-"));

                let xmlfile = std::fs::File::open(path.with_extension("rdf")).unwrap();
                let mut xml = TestGraph::new();
                assert!(
                    $crate::parser::xml::RdfXmlParser::with_base(&format!(
                        "http://www.w3.org/2013/RDFXMLTests/{}/{}.rdf",
                        stringify!($suite).replace('_', "-"),
                        stringify!($case).replace('_', "-"),
                    ))
                    .unwrap()
                    .parse(std::io::BufReader::new(xmlfile))
                    .add_to_graph(&mut xml)
                    .is_err()
                );
            }
        };
    }

    macro_rules! nt_test {
        ($name:ident, $xml:expr, $nt:expr) => {
            #[test]
            fn $name() {
                let mut xml = TestGraph::new();
                $crate::parser::xml::parse_str($xml)
                    .add_to_graph(&mut xml)
                    .expect("failed parsing XML file");

                let mut nt = TestGraph::new();
                $crate::parser::nt::parse_str($nt)
                    .add_to_graph(&mut nt)
                    .expect("failed parsing N-Triples file");

                assert_graph_eq!(xml, nt);
            }
        };
    }

    mod w3c_example {
        use super::*;

        // W3C Example 07: 'Complete RDF/XML description of Figure 1 graph'
        nt_test! {
            ex07,
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

        // W3C Example 08: 'Complete example of xml:lang'
        nt_test! {
            ex08,
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

        // W3C Example 09: 'Complete example of rdf:parseType="Literal"'
        nt_test! {
            ex09,
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

        // W3C Example 10: 'Complete example of rdf:datatype'
        nt_test! {
            ex10,
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

        // W3C Example 11: 'Complete RDF/XML description of graph using rdf:nodeID identifying the blank node'
        nt_test! {
            ex11,
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

        // W3C Example 12: 'Complete example using rdf:parseType="Resource"'
        nt_test! {
            ex12,
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

        // W3C Example 13: 'Complete example of property attributes on an empty property element'
        nt_test! {
            ex13,
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

        // W3C Example 14: 'Complete example with rdf:type'
        nt_test! {
            ex14,
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

        // W3C Example 15: 'Complete example using a typed node element to replace an rdf:type'
        nt_test! {
            ex15,
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

        // W3C Example 16: 'Complete example using rdf:ID and xml:base for shortening URIs'
        nt_test! {
            ex16,
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

        // W3C Example 17: 'Complex example using RDF list properties'
        nt_test! {
            ex17,
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

        // W3C Example 18: 'Complete example using rdf:li property element for list properties'
        nt_test! {
            ex18,
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

        // W3C Example 19: 'Complete example of a RDF collection of nodes using rdf:parseType="Collection"'
        nt_test! {
            ex19,
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
            r#"_:n0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/banana> .
               _:n0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:n1 .
               _:n1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/apple> .
               _:n1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:n2 .
               _:n2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/pear> .
               _:n2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
               <http://example.org/basket> <http://example.org/stuff/1.0/hasFruit> _:n0 .
            "#
        }

        // W3C Example 20: 'Complete example of rdf:ID reifying a property element'
        nt_test! {
            ex20,
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
    }

    mod amp_in_url {
        use super::*;

        rdf_test!(amp_in_url / test001);
    }

    mod datatypes {
        use super::*;

        rdf_test!(datatypes / test001);
        rdf_test!(datatypes / test002);
    }

    mod rdf_charmod_literals {
        use super::*;

        rdf_test!(rdf_charmod_literals / test001 where "a" => "n0");
    }

    mod rdf_charmod_uris {
        use super::*;

        rdf_test!(rdf_charmod_uris / test001);
        rdf_test!(rdf_charmod_uris / test002);
    }

    mod rdf_containers_syntax_vs_schema {
        use super::*;

        rdf_failure!(rdf_containers_syntax_vs_schema / error001);
        rdf_failure!(rdf_containers_syntax_vs_schema / error002);

        rdf_test!(rdf_containers_syntax_vs_schema / test001 where "bag" => "n0");
        rdf_test!(rdf_containers_syntax_vs_schema / test002 where "bag" => "n0");
        rdf_test!(rdf_containers_syntax_vs_schema / test003 where "bar" => "n0");
        rdf_test!(rdf_containers_syntax_vs_schema / test004 where "res2" => "n2", "bar" => "n0", "res" => "n1");
        rdf_test!(rdf_containers_syntax_vs_schema / test006 where "bag" => "n0");
        rdf_test!(rdf_containers_syntax_vs_schema / test007 where "d1" => "n0", "d2" => "n1");
        rdf_test!(rdf_containers_syntax_vs_schema / test008);
    }

    mod rdf_element_not_mandatory {
        use super::*;

        rdf_test!(rdf_element_not_mandatory / test001 where "a" => "n0");
    }

    mod rdf_ns_prefix_confusion {
        use super::*;

        rdf_test!(rdf_ns_prefix_confusion / test0001);
        rdf_test!(rdf_ns_prefix_confusion / test0003);
        rdf_test!(rdf_ns_prefix_confusion / test0004);
        rdf_test!(rdf_ns_prefix_confusion / test0005 where "genid" => "n0");
        rdf_test!(rdf_ns_prefix_confusion / test0006);
        rdf_test!(rdf_ns_prefix_confusion / test0009);
        rdf_test!(rdf_ns_prefix_confusion / test0010);
        rdf_test!(rdf_ns_prefix_confusion / test0011);
        rdf_test!(rdf_ns_prefix_confusion / test0012);
        rdf_test!(rdf_ns_prefix_confusion / test0013);
        rdf_test!(rdf_ns_prefix_confusion / test0014);
    }

    mod rdfms_abouteach {
        use super::*;

        rdf_failure!(rdfms_abouteach / error001);
        rdf_failure!(rdfms_abouteach / error002);
    }

    mod rdfms_difference_between_ID_and_about {
        use super::*;

        rdf_failure!(rdfms_difference_between_ID_and_about / error1);
        rdf_test!(rdfms_difference_between_ID_and_about / test1);
        rdf_test!(rdfms_difference_between_ID_and_about / test2);
        rdf_test!(rdfms_difference_between_ID_and_about / test3);
    }

    mod rdfms_duplicate_member_props {
        use super::*;

        rdf_test!(rdfms_duplicate_member_props / test001);
    }

    mod rdfms_empty_property_elements {
        use super::*;

        rdf_failure!(rdfms_empty_property_elements / error001);
        rdf_failure!(rdfms_empty_property_elements / error002);
        rdf_failure!(rdfms_empty_property_elements / error003);

        rdf_test!(rdfms_empty_property_elements / test001);
        rdf_test!(rdfms_empty_property_elements / test002);
        rdf_test!(rdfms_empty_property_elements / test003);
        rdf_test!(rdfms_empty_property_elements / test004 where "a1" => "n0");
        rdf_test!(rdfms_empty_property_elements / test005);
        rdf_test!(rdfms_empty_property_elements / test006 where "a1" => "n0");
        rdf_test!(rdfms_empty_property_elements / test007);
        rdf_test!(rdfms_empty_property_elements / test008);
        rdf_test!(rdfms_empty_property_elements / test009);
        rdf_test!(rdfms_empty_property_elements / test010 where "a1" => "n0");
        rdf_test!(rdfms_empty_property_elements / test011);
        rdf_test!(rdfms_empty_property_elements / test012 where "a1" => "n0");
        rdf_test!(rdfms_empty_property_elements / test013);
        rdf_test!(rdfms_empty_property_elements / test014 where "a1" => "n0");
        rdf_test!(rdfms_empty_property_elements / test015 where "a1" => "n0");
        rdf_test!(rdfms_empty_property_elements / test016);
        rdf_test!(rdfms_empty_property_elements / test017);
    }

    mod rdfms_identity_anon_resources {
        use super::*;

        rdf_test!(rdfms_identity_anon_resources / test001 where "j0" => "n0");
        rdf_test!(rdfms_identity_anon_resources / test002 where "j0" => "n0");
        rdf_test!(rdfms_identity_anon_resources / test003 where "j0" => "n0");
        rdf_test!(rdfms_identity_anon_resources / test004 where "j0" => "n0");
        rdf_test!(rdfms_identity_anon_resources / test005 where "j0" => "n0");
    }

    mod rdfms_not_id_and_resource_attr {
        use super::*;

        rdf_test!(rdfms_not_id_and_resource_attr / test001 where "j88090" => "n0", "j88091" => "n1");
        rdf_test!(rdfms_not_id_and_resource_attr / test002 where "j88093" => "n0");
        rdf_test!(rdfms_not_id_and_resource_attr / test004 where "j88101" => "n0");
        rdf_test!(rdfms_not_id_and_resource_attr / test005 where "j88106" => "n0");
    }

    mod rdfms_para196 {
        use super::*;

        rdf_test!(rdfms_para196 / test001);
    }

    mod rdfms_rdf_id {
        use super::*;

        rdf_failure!(rdfms_rdf_id / error001);
        rdf_failure!(rdfms_rdf_id / error002);
        rdf_failure!(rdfms_rdf_id / error003);
        rdf_failure!(rdfms_rdf_id / error004);
        rdf_failure!(rdfms_rdf_id / error005);
        rdf_failure!(rdfms_rdf_id / error006);
        rdf_failure!(rdfms_rdf_id / error007);
    }

    mod rdfms_rdf_names_use {
        use super::*;

        rdf_failure!(rdfms_rdf_names_use / error_001);
        rdf_failure!(rdfms_rdf_names_use / error_002);
        rdf_failure!(rdfms_rdf_names_use / error_003);
        rdf_failure!(rdfms_rdf_names_use / error_004);
        rdf_failure!(rdfms_rdf_names_use / error_005);
        rdf_failure!(rdfms_rdf_names_use / error_006);
        rdf_failure!(rdfms_rdf_names_use / error_007);
        rdf_failure!(rdfms_rdf_names_use / error_008);
        rdf_failure!(rdfms_rdf_names_use / error_009);
        rdf_failure!(rdfms_rdf_names_use / error_010);
        rdf_failure!(rdfms_rdf_names_use / error_011);
        rdf_failure!(rdfms_rdf_names_use / error_012);
        rdf_failure!(rdfms_rdf_names_use / error_013);
        rdf_failure!(rdfms_rdf_names_use / error_014);
        rdf_failure!(rdfms_rdf_names_use / error_015);
        rdf_failure!(rdfms_rdf_names_use / error_016);
        rdf_failure!(rdfms_rdf_names_use / error_017);
        rdf_failure!(rdfms_rdf_names_use / error_018);
        rdf_failure!(rdfms_rdf_names_use / error_019);
        rdf_failure!(rdfms_rdf_names_use / error_020);

        rdf_test!(rdfms_rdf_names_use / test_001);
        rdf_test!(rdfms_rdf_names_use / test_002);
        rdf_test!(rdfms_rdf_names_use / test_003);
        rdf_test!(rdfms_rdf_names_use / test_004);
        rdf_test!(rdfms_rdf_names_use / test_005);
        rdf_test!(rdfms_rdf_names_use / test_006);
        rdf_test!(rdfms_rdf_names_use / test_007);
        rdf_test!(rdfms_rdf_names_use / test_008);
        rdf_test!(rdfms_rdf_names_use / test_009);
        rdf_test!(rdfms_rdf_names_use / test_010);
        rdf_test!(rdfms_rdf_names_use / test_011);
        rdf_test!(rdfms_rdf_names_use / test_012);
        rdf_test!(rdfms_rdf_names_use / test_013);
        rdf_test!(rdfms_rdf_names_use / test_014);
        rdf_test!(rdfms_rdf_names_use / test_015);
        rdf_test!(rdfms_rdf_names_use / test_016);
        rdf_test!(rdfms_rdf_names_use / test_017);
        rdf_test!(rdfms_rdf_names_use / test_018);
        rdf_test!(rdfms_rdf_names_use / test_019);
        rdf_test!(rdfms_rdf_names_use / test_020);
        rdf_test!(rdfms_rdf_names_use / test_021);
        rdf_test!(rdfms_rdf_names_use / test_022);
        rdf_test!(rdfms_rdf_names_use / test_023);
        rdf_test!(rdfms_rdf_names_use / test_024);
        rdf_test!(rdfms_rdf_names_use / test_025);
        rdf_test!(rdfms_rdf_names_use / test_026);
        rdf_test!(rdfms_rdf_names_use / test_027);
        rdf_test!(rdfms_rdf_names_use / test_028);
        rdf_test!(rdfms_rdf_names_use / test_029);
        rdf_test!(rdfms_rdf_names_use / test_030);
        rdf_test!(rdfms_rdf_names_use / test_031);
        rdf_test!(rdfms_rdf_names_use / test_032);
        rdf_test!(rdfms_rdf_names_use / test_033);
        rdf_test!(rdfms_rdf_names_use / test_034);
        rdf_test!(rdfms_rdf_names_use / test_035);
        rdf_test!(rdfms_rdf_names_use / test_036);
        rdf_test!(rdfms_rdf_names_use / test_037);
    }

    mod rdfms_reification_required {
        use super::*;

        rdf_test!(rdfms_reification_required / test001);
    }

    mod rdfms_seq_representation {
        use super::*;

        rdf_test!(rdfms_seq_representation / test001
            where "a0" => "n0", "a1" => "n1", "a2" => "n2");
    }

    mod rdfms_syntax_incomplete {
        use super::*;

        rdf_failure!(rdfms_syntax_incomplete / error001);
        rdf_failure!(rdfms_syntax_incomplete / error002);
        rdf_failure!(rdfms_syntax_incomplete / error003);
        rdf_failure!(rdfms_syntax_incomplete / error004);
        rdf_failure!(rdfms_syntax_incomplete / error005);
        rdf_failure!(rdfms_syntax_incomplete / error006);

        rdf_test!(rdfms_syntax_incomplete / test001 where "j0" => "oa");
        rdf_test!(rdfms_syntax_incomplete / test002 where "j0A" => "oa", "j2" => "n0", "j1B" => "ob");
        rdf_test!(rdfms_syntax_incomplete / test003 where "j0A" => "oa");
        rdf_test!(rdfms_syntax_incomplete / test004 where "j1A" => "oa", "j2" => "n1", "j0" => "n0");
    }

    mod rdfms_uri_substructure {
        use super::*;

        rdf_test!(rdfms_uri_substructure / test001 where "a" => "n0");
    }

    mod rdfms_xml_literal_namespaces {
        use super::*;

        rdf_test!(
            #[ignore]
            rdfms_xml_literal_namespaces
                / test001
        );
        rdf_test!(
            #[ignore]
            rdfms_xml_literal_namespaces
                / test002
        );
    }

    mod rdfms_xmllang {
        use super::*;

        rdf_test!(rdfms_xmllang / test001);
        rdf_test!(rdfms_xmllang / test002);
        rdf_test!(rdfms_xmllang / test003);
        rdf_test!(rdfms_xmllang / test004);
        rdf_test!(rdfms_xmllang / test005);
        rdf_test!(rdfms_xmllang / test006);
    }

    mod rdfs_domain_and_range {
        use super::*;

        rdf_test!(rdfs_domain_and_range / test001);
        rdf_test!(rdfs_domain_and_range / test002);
    }

    mod unrecognised_xml_attributes {
        use super::*;

        rdf_test!(unrecognised_xml_attributes / test001);
        rdf_test!(unrecognised_xml_attributes / test002);
    }

    // FIXME(@althonos): requires `parseType=Literal` to work.
    mod xml_canon {
        use super::*;

        rdf_test!(
            #[ignore]
            xml_canon
                / test001
        );
    }

    mod xmlbase {
        use super::*;

        rdf_test!(xmlbase / test001);
        rdf_test!(xmlbase / test002 where "j0" => "n0");
        rdf_test!(xmlbase / test003);
        rdf_test!(xmlbase / test004 where "j0" => "n0");
        rdf_test!(xmlbase / test006);
        rdf_test!(xmlbase / test007);
        rdf_test!(xmlbase / test008);
        rdf_test!(xmlbase / test009);
        rdf_test!(xmlbase / test010);
        rdf_test!(xmlbase / test011);
        rdf_test!(xmlbase / test013);
        rdf_test!(xmlbase / test014);
    }

    // Check that nested `rdf:li` keeps independent counters for nested elements.
    nt_test! {
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

    // Check that an empty node is used as a leaf.
    nt_test! {
        empty_node,
        r#"<?xml version="1.0"?>
            <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                     xmlns:ex="http://example.org/stuff/1.0/">
              <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
                <ex:editor>
                    <rdf:Description rdf:about="http://example.org/user/dave-beckett"/>
                </ex:editor>
              </rdf:Description>
            </rdf:RDF>
        "#,
        r#"<http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> <http://example.org/user/dave-beckett> .
        "#
    }
}
