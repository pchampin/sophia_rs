//! Parser for RDF XML.

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::LinkedList;
use std::fmt::Debug;
use std::io::{BufRead, BufReader, Read};
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use quick_xml::events::Event;
use quick_xml::Reader;
use quick_xml::Result as XmlResult;
use url::Url;

use crate::error::*;
use crate::ns::rdf;
use crate::ns::xsd;
use crate::ns::Namespace;
use crate::term::factory::RcTermFactory;
use crate::term::factory::TermFactory;
use crate::term::iri_rfc3987::is_absolute_iri;
use crate::term::iri_rfc3987::is_relative_iri;
use crate::term::iri_rfc3987::is_valid_iri;
use crate::term::matcher::TermMatcher;
use crate::term::StaticTerm;
use crate::term::Term;

static RESERVED_NODE_NAMES: &'static [StaticTerm] = &[
    rdf::RDF,
    rdf::ID,
    rdf::about,
    rdf::bagID,
    rdf::parseType,
    rdf::resource,
    rdf::nodeID,
    rdf::li,
    rdf::aboutEach,
    rdf::aboutEachPrefix,
];

static RESERVED_PROPERTY_NAMES: &'static [StaticTerm] = &[
    rdf::Description,
    rdf::RDF,
    rdf::ID,
    rdf::about,
    rdf::bagID,
    rdf::parseType,
    rdf::resource,
    rdf::nodeID,
    rdf::aboutEach,
    rdf::aboutEachPrefix,
];

static RESERVED_ATTRIBUTES_NAMES: &'static [StaticTerm] = &[
    rdf::li,
    rdf::aboutEach,
    rdf::aboutEachPrefix,
    rdf::bagID,
];

mod xmlname {

    use pest::Parser;

    #[cfg(debug_assertions)]
    const _GRAMMAR: &str = include_str!("xmlname.pest");

    #[derive(Parser)]
    #[grammar = "parser/xmlname.pest"]
    struct PestXmlNameParser;

    pub fn is_valid_xmlname(n: &str) -> bool {
        PestXmlNameParser::parse(Rule::Name, n).is_ok()
    }

    pub fn validate(n: &str) -> Result<&str, super::error::Error> {
        if is_valid_xmlname(n) {
            Ok(n)
        } else {
            Err(super::error::Error::from_kind(
                super::error::ErrorKind::InvalidXmlName(n.to_string())
            ))
        }
    }
}


// ---
pub mod error {
    error_chain! {
        types {
            Error, ErrorKind, ResultExt;
        }
        errors {
            XmlError(e: ::quick_xml::Error) {
                description("xml parser failed")
                display("xml parser failed: {:?}", e)
            }
            DuplicateId(id: String) {
                description("duplicate ID")
                display("duplicate ID: {:?}", id)
            }
            InvalidNodeName(n: String) {
                description("invalid property name")
                display("invalid property name: {:?}", n)
            }
            InvalidPropertyName(n: String) {
                description("invalid property name")
                display("invalid property name: {:?}", n)
            }
            InvalidAttribute(n: String) {
                description("invalid attribute")
                display("invalid attribute: {:?}", n)
            }
            InvalidXmlName(n: String) {
                description("invalid XML name")
                display("invalid XML name: {:?}", n)
            }
            InvalidParseType(ty: String) {
                description("invalid `parseType` value in this context")
                display("cannot use `parseType={}` in this context", ty)
            }
            AmbiguousSubject {
                description("cannot have `rdf:ID`, `rdf:nodeID` and `rdf:about` at the same time")
                display("cannot have `rdf:ID`, `rdf:nodeID` and `rdf:about` at the same time")
            }
            InvalidPrefix(p: String) {
                description("invalid prefix")
                display("{:?} cannot be used as a prefix", p)
            }
        }
    }

    impl From<quick_xml::Error> for Error {
        fn from(e: quick_xml::Error) -> Self {
            Self::from_kind(ErrorKind::XmlError(e))
        }
    }
}

/// RDF/XML parser configuration.
///
/// For more information,
/// see the [uniform interface] of parsers.
///
/// [uniform interface]: ../index.html#uniform-interface
///
#[derive(Clone, Debug, Default)]
pub struct Config {
    base: Option<Url>,
}

impl Config {
    pub fn with_base(base: &str) -> Result<Self> {
        match Url::parse(base) {
            Ok(url) => Ok(Self { base: Some(url) }),
            Err(_) => Err(Error::from_kind(ErrorKind::InvalidIri(base.to_owned()))),
        }
    }
}

impl Config {
    #[inline]
    pub fn parse_bufread<'a, B: BufRead + 'a>(
        &self,
        bufread: B,
    ) -> impl Iterator<Item = Result<[Term<Rc<str>>; 3]>> + 'a {
        type Parser<B> = XmlParser<B, RcTermFactory>;
        match &self.base {
            Some(base) => Parser::with_base(Reader::from_reader(bufread), base.clone()),
            None => Parser::new(Reader::from_reader(bufread)),
        }
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
        type Parser<B> = XmlParser<B, RcTermFactory>;
        match &self.base {
            Some(base) => Parser::with_base(Reader::from_str(txt), base.clone()),
            None => Parser::new(Reader::from_str(txt)),
        }
    }
}

// ---

/// The state of the parser.
#[derive(Debug, Clone, Copy)]
enum ParsingState {
    /// The parser is in a predicate, and the next expected element is a node.
    Node,
    /// The parser is in a node, and the next expected element is a predicate.
    Predicate,
    /// The parser is in a resource predicate, and the next expected element
    /// is a predicate.
    Resource,
    /// The parser is in a predicate and will process its content as a literal.
    Literal, // NB: not supported by quick-xml right now
    /// The parser is in a reified element, and the next expected element is
    /// a text to be used as a reified triple object.
    Res,
    /// The parser is in a collection, and the next expected element is a node.
    Collection,
    /// The parser is in a collection, and the next expected element is a node,
    /// but exiting this does not exit the collection.
    CollectionItem,
}

// ---

/// A wrapper for `quick_xml::Reader` ignoring or merging some events.
pub struct XmlReader<B: BufRead> {
    inner: Reader<B>,
    event: Option<Event<'static>>, // actually 'buffer
    buffer: Vec<u8>,
}

impl<B: BufRead> XmlReader<B> {
    /// Read an XML event.
    pub fn read_event<'a>(&mut self, buf: &'a mut Vec<u8>) -> XmlResult<Event<'a>> {
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
            buffer: Vec::new(),
        }
    }
}

impl<B: BufRead> Deref for XmlReader<B> {
    type Target = Reader<B>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

// ---

/// Data relevant to an XML scope.
#[derive(Debug)]
pub struct Scope<F: TermFactory> {
    /// The XML namespaces declared in this scope.
    ns: HashMap<String, Namespace<F::TermData>>,
    /// The default XML namespace to expand tags without namespaces with.
    default: Option<Namespace<F::TermData>>,
    /// The base IRI namespace to expand `rdf:ID`, `rdf:resource` and `rdf:about`.
    base: Option<Url>,
    /// The term factory used to create new terms.
    factory: Rc<RefCell<F>>,
    /// The datatype of the containing element.
    datatype: Option<Term<F::TermData>>,
    /// The language tag of the containing element.
    lang: Option<F::TermData>,
    /// The text gathered in the current scope.
    text: Option<String>,
    /// The current count of list elements
    li: AtomicUsize,
    /// The
    collection: Vec<Term<F::TermData>>,
}

// We implement it ourselves instead of deriving so that:
// * F does not need to be `Clone` (deriving requires it).
// * we can clone `li` although `AtomicUsize` is not `Clone`.
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
            li: AtomicUsize::new(self.li.load(Ordering::Relaxed)),
            collection: self.collection.clone(),
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
            li: AtomicUsize::new(1),
            collection: Vec::new(),
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
            Err(Error::from(self::error::Error::from(
                self::error::ErrorKind::InvalidPrefix(prefix.into()),
            )))
        } else {
            let mut f = self.factory.borrow_mut();
            self.ns.insert(
                String::from(prefix),
                Namespace::new(f.get_term_data(value))?,
            );
            Ok(())
        }
    }

    /// Set the default XML prefix.
    fn set_default(&mut self, default: &str) -> Result<()> {
        let mut f = self.factory.borrow_mut();
        self.default = Some(Namespace::new(f.get_term_data(default))?);
        Ok(())
    }

    /// Set the base IRI prefix.
    fn set_base(&mut self, base: &str) -> Result<()> {
        // Accept the URL only if it is a valid URL and a valid base.
        if let Ok(url) = Url::parse(base) {
            if !url.cannot_be_a_base() {
                self.base = Some(url);
                return Ok(());
            }
        }

        Err(Error::from_kind(ErrorKind::InvalidIri(String::from(base))))
    }

    /// Set the scope datatype.
    fn set_datatype(&mut self, datatype: &str) -> Result<()> {
        self.datatype = Some(self.expand_iri(datatype)?);
        Ok(())
    }

    /// Set the scope text.
    fn set_text<T: Into<Option<String>>>(&mut self, text: T) {
        self.text = text.into();
    }

    /// Expand an XML attribute in the form `namespace:id` into an IRI.
    ///
    /// This uses the `xmlns` default namespace to expand local attributes,
    /// or any declared namespace in the current scope.
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

    /// Expand an IRI reference (in a `rdf:resource` or `rdf:about`) into an IRI.
    ///
    /// This uses `xml:base` to expand local resources, and does nothing in
    /// case the IRI is already in expanded form.
    fn expand_iri(&self, iri: &str) -> Result<Term<F::TermData>> {
        fn dec(x: &str) -> std::borrow::Cow<str> {
            url::percent_encoding::percent_decode(x.as_bytes())
                .decode_utf8()
                .unwrap()
        }

        if is_relative_iri(iri) {
            if let Some(url) = &self.base {
                match url.join(iri) {
                    Ok(u) => self.factory.borrow_mut().iri(&dec(u.as_str())),
                    Err(e) => bail!(ErrorKind::InvalidIri(String::from(iri))),
                }
            } else {
                panic!("NO BASE IRI")
            }
        } else {
            self.factory.borrow_mut().iri(&dec(iri))
        }
    }

    /// Expand an ID (in a `rdf:ID`) into an IRI.
    ///
    /// This also uses `xml:base` to expand local resources, and prefixes
    /// identifiers in the document with a `#` if needed.
    fn expand_id(&self, id: &str) -> Result<Term<F::TermData>> {
        if !xmlname::is_valid_xmlname(id) {
            return Err(Error::from(self::error::Error::from_kind(
                self::error::ErrorKind::InvalidXmlName(id.to_string())
            )))
        } else if id.starts_with("#") {
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

    /// Create a new `rdf:li` property by incrementing the scope `li` counter.
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
            ns.get(f.get_term_data(&format!("_{}", self.li.load(Ordering::Relaxed) - 1)))
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

/// An XML parser supporting any term factory as a backend.
struct XmlParser<B: BufRead, F: TermFactory> {
    /// The underlying XML reader.
    reader: XmlReader<B>,

    /// The stack of scoped data (for nested declaration).
    scopes: Vec<Scope<F>>,

    /// The stack of parent elements (for nested declarations).
    parents: Vec<Term<F::TermData>>,

    // The queue of produced triples.
    triples: LinkedList<Result<[Term<F::TermData>; 3]>>,

    //
    factory: Rc<RefCell<F>>,

    //
    bnodes: AtomicUsize,

    //
    ids: HashSet<Term<F::TermData>>,

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

    /// Get a reference to the current scope.
    fn scope(&self) -> &Scope<F> {
        self.scopes.last().unwrap()
    }

    /// Get a mutable reference to the current scope.
    fn scope_mut(&mut self) -> &mut Scope<F> {
        self.scopes.last_mut().unwrap()
    }

    /// Get a reference to the parent scope.
    fn parent_scope(&self) -> &Scope<F> {
        &self.scopes[self.scopes.len() - 2]
    }

    /// Get a mutable reference to the current scope.
    fn parent_scope_mut(&mut self) -> &mut Scope<F> {
        let l = self.scopes.len();
        &mut self.scopes[l - 2]
    }

    // Enter a new scope, parsing `xml:lang`, `xmlns` and `rdf:datatype`.
    fn enter_scope(&mut self, e: &BytesStart) -> Result<()> {
        // We are entering a new elements: text is not relevant anymore.
        let mut prev = self.scope_mut();
        prev.text = None;

        // Create a local scope using values from the outer one.
        let mut scope = prev.clone();
        scope.text = Some(String::new());
        scope.collection = Vec::new();
        scope.li.store(1, Ordering::Relaxed);

        // * Update XML namespaces with those defined in the document.
        // * Change scope language if there is any `xml:lang` attribute
        // * Fail if there is an invalid `rdf:li` attribute
        for attr in e.attributes().with_checks(true) {
            let a = attr.map_err(self::error::Error::from)?;
            if a.key.starts_with(b"xmlns:") {
                scope.add_prefix(
                    &self.reader.decode(&a.key[6..]),
                    &a.unescape_and_decode_value(&self.reader)
                        .map_err(self::error::Error::from)?,
                )?;
            } else if a.key == b"xmlns" {
                scope.set_default(
                    &a.unescape_and_decode_value(&self.reader)
                        .map_err(self::error::Error::from)?,
                )?;
            } else if a.key == b"xml:base" {
                scope.set_base(
                    &a.unescape_and_decode_value(&self.reader)
                        .map_err(self::error::Error::from)?,
                )?;
            } else if a.key == b"xml:lang" {
                scope.lang = if a.value.is_empty() {
                    None
                } else {
                    let v = &a
                        .unescape_and_decode_value(&self.reader)
                        .map_err(|e| self::error::Error::from(e))?;
                    self.factory.borrow_mut().get_term_data(v).into()
                };
            }
        }

        // Make the newly created scope the local one.
        self.scopes.push(scope);
        Ok(())
    }

    // Exit the local scope.
    fn leave_scope(&mut self) {
        self.scopes.pop().expect("XML is not balanced");
    }

    // ---

    /// Create a new bnode term (using `n` prefix).
    fn new_bnode(&self) -> Term<F::TermData> {
        self.factory
            .borrow_mut()
            .bnode(&format!("n{}", self.bnodes.fetch_add(1, Ordering::Relaxed)))
            .unwrap()
    }

    /// Rename a bnode using the `nodeID` in the document (using `o` prefix)
    fn rename_bnode(&self, id: &str) -> Result<Term<F::TermData>> {
        if xmlname::is_valid_xmlname(id) {
            self.factory.borrow_mut().bnode(&format!("o{}", id))
        } else {
            Err(Error::from(self::error::Error::from_kind(
                self::error::ErrorKind::InvalidXmlName(id.to_string())
            )))
        }
    }

    /// Check the given `ID` is unique.
    fn check_unique_id(&mut self, id: Term<F::TermData>) -> Result<Term<F::TermData>> {
        if self.ids.contains(&id) {
            let kind = self::error::ErrorKind::DuplicateId(id.value());
            Err(Error::from(self::error::Error::from_kind(kind)))
        } else {
            self.ids.insert(id.clone());
            Ok(id)
        }
    }

    /// Create a new predicate IRI from an XML name (or a RDF metasyntactic element)
    fn predicate_iri_start(&self, name: &str) -> Result<Term<F::TermData>> {
        let p = self.scope().expand_attribute(name)?;
        if p.matches(&rdf::li) {
            self.parent_scope().new_li()
        } else {
            Ok(p)
        }
    }

    /// Retrieve a predicate IRI from an XML name
    fn predicate_iri_end(&self, name: &str) -> Result<Term<F::TermData>> {
        let p = self.scope().expand_attribute(name)?;
        if p.matches(&rdf::li) {
            self.parent_scope().current_li()
        } else {
            Ok(p)
        }
    }

    // ---

    /// Create a new `XmlParser` from the given `quick_xml::Reader`.
    fn new(reader: Reader<B>) -> Self {
        let factory: Rc<RefCell<F>> = Default::default();
        Self {
            reader: XmlReader::from(reader),
            parents: Vec::new(),
            scopes: vec![Scope::with_factory_rc(factory.clone())],
            triples: LinkedList::new(),
            factory: factory,
            bnodes: AtomicUsize::new(0),
            state: vec![ParsingState::Node],
            ids: HashSet::new(),
        }
    }

    /// Create a new `XmlParser` using the given URL as the top-level `xml:base`.
    fn with_base(reader: Reader<B>, base: Url) -> Self {
        let mut parser = Self::new(reader);
        let mut scope = parser.scope_mut();
        scope.base = Some(base);
        parser
    }

    // ---

    fn element_start(&mut self, e: &BytesStart) {
        if let Err(e) = self.enter_scope(e) {
            self.triples.push_back(Err(e));
        }

        if let Err(e) = match self.state.last().unwrap() {
            ParsingState::Node => self.node_start(e),
            ParsingState::Predicate => self.predicate_start(e),
            ParsingState::Resource => self.predicate_start(e),
            ParsingState::Collection => self.collection_start(e),
            ParsingState::CollectionItem => self.collection_item_start(e),
            ParsingState::Res => panic!("expecting text, not new element"),
            ParsingState::Literal => unimplemented!("entering element as literal"),
        } {
            self.triples.push_back(Err(e));
        }
    }

    fn node_start(&mut self, e: &BytesStart) -> Result<()> {
        // Get node type from the XML attribute.
        let ty = self
            .scope()
            .expand_attribute(&self.reader.decode(e.name()))?;

        // Bail out if in a top-level rdf:RDF element
        if rdf::RDF.matches(&ty) && self.parents.is_empty() {
            self.state.push(ParsingState::Node);
            self.parents.push(self.factory.borrow_mut().copy(&rdf::RDF));
            return Ok(());
        }

        //
        if RESERVED_NODE_NAMES.matches(&ty) {
            let kind = self::error::ErrorKind::InvalidNodeName(ty.value());
            return Err(Error::from(self::error::Error::from_kind(kind)));
        }

        // Separate node subject from other attributes
        let mut properties = HashMap::new();
        let mut subject = Vec::new();
        for attr in e.attributes().with_checks(true) {
            let a = attr.map_err(self::error::Error::from)?;

            // ignore xml attributes (processed in element_start)
            if a.key.starts_with(b"xml") {
                continue;
            }

            // try to extract the subject annotation
            let k = self.scope().expand_attribute(&self.reader.decode(a.key))?;
            let v = a
                .unescape_and_decode_value(&self.reader)
                .map_err(self::error::Error::from)?;

            if k.matches(&rdf::about) {
                subject.push(self.scope().expand_iri(&v)?);
            } else if k.matches(&rdf::ID) {
                let id = self.scope().expand_id(&v)?;
                subject.push(self.check_unique_id(id)?);
            } else if k.matches(&rdf::nodeID) {
                subject.push(self.rename_bnode(&v)?);
            } else if k.matches(&rdf::type_) {
                properties.insert(k, self.scope().expand_iri(&v)?);
            } else if RESERVED_ATTRIBUTES_NAMES.matches(&k) {
                let kind = self::error::ErrorKind::InvalidAttribute(k.value());
                return Err(Error::from(self::error::Error::from_kind(kind)));
            } else {
                properties.insert(k, self.scope().new_literal(v)?);
            }
        }

        // Get subject and add it to the current nested stack
        if subject.len() > 1 {
            return Err(
                self::error::Error::from_kind(self::error::ErrorKind::AmbiguousSubject).into(),
            );
        }
        let s: Term<_> = subject.pop().unwrap_or_else(|| self.new_bnode());
        self.parents.push(s.clone());

        // Add the type as a triple if it is not `rdf:Description`
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
        Ok(())
    }

    fn predicate_start(&mut self, e: &BytesStart) -> Result<()> {
        // Get the predicate and add it to the current nested stack
        // or build a new `rdf:_n` IRI if the predicate is `rdf:li`.
        let p = self.predicate_iri_start(&self.reader.decode(e.name()))?;

        // Fail if the property is among forbidden names.
        if RESERVED_PROPERTY_NAMES.matches(&p) {
            let kind = self::error::ErrorKind::InvalidNodeName(p.value());
            return Err(Error::from(self::error::Error::from_kind(kind)));
        } else {
            self.parents.push(p);
        }

        // Extract attributes relevant to the RDF syntax
        let mut attributes = HashMap::new();
        let mut next_state = ParsingState::Node;
        let mut object = Vec::with_capacity(1);
        for attr in e.attributes().with_checks(true) {
            let a = attr.map_err(self::error::Error::from)?;

            // Ignore `xml` attributes
            if a.key.starts_with(b"xml") {
                continue;
            }

            let k = self.scope().expand_attribute(&self.reader.decode(a.key))?;
            if k.matches(&rdf::datatype) {
                let v = a
                    .unescape_and_decode_value(&self.reader)
                    .map_err(self::error::Error::from)?;
                self.scope_mut().set_datatype(&v)?;
            } else if k.matches(&rdf::ID) {
                let v = a
                    .unescape_and_decode_value(&self.reader)
                    .map_err(self::error::Error::from)?;
                let id = self.scope().expand_id(&v)?;
                object.push(self.check_unique_id(id)?);
                next_state = ParsingState::Res;
            } else if k.matches(&rdf::resource) {
                let v = a
                    .unescape_and_decode_value(&self.reader)
                    .map_err(self::error::Error::from)?;
                object.push(self.scope().expand_iri(&v)?);
                next_state = ParsingState::Predicate;
            } else if k.matches(&rdf::parseType) {
                match a.value.as_ref() {
                    b"Resource" => {
                        if object.is_empty() {
                            object.push(self.new_bnode());
                        }
                        self.scope_mut().set_text(None);
                        next_state = ParsingState::Resource;
                    }
                    b"Collection" => {
                        next_state = ParsingState::Collection;
                    }
                    b"Literal" => {
                        self.scope_mut().set_datatype(&rdf::XMLLiteral.value())?;
                        next_state = ParsingState::Literal;
                    }
                    other => panic!("invalid parseType: {:?}", other),
                }
            } else if RESERVED_ATTRIBUTES_NAMES.matches(&k) {
                let kind = self::error::ErrorKind::InvalidAttribute(k.value());
                return Err(Error::from(self::error::Error::from_kind(kind)));
            } else {
                let v = a
                    .unescape_and_decode_value(&self.reader)
                    .map_err(self::error::Error::from)?;
                attributes.insert(k, self.scope().new_literal(v)?);
                next_state = ParsingState::Resource;
            }
        }

        // Extract subjet and object of the triple
        let s = self.parents.last().unwrap().clone();
        let o = match object.len() {
            0 if !attributes.is_empty() => Some(self.new_bnode()),
            0 if attributes.is_empty() => None,
            1 => Some(object.last().unwrap().clone()),
            _ => {
                return Err(
                    self::error::Error::from_kind(self::error::ErrorKind::AmbiguousSubject).into(),
                )
            }
        };

        // Make the predicate a resource element if an objec tis present.
        if let Some(o) = o {
            self.parents.push(o.clone());
            std::mem::replace(self.state.last_mut().unwrap(), ParsingState::Resource);
            for (k, v) in attributes.into_iter() {
                self.triples.push_back(Ok([o.clone(), k, v]));
            }
        }

        self.state.push(next_state);
        Ok(())
    }

    fn collection_start(&mut self, e: &BytesStart) -> Result<()> {
        self.state.push(ParsingState::CollectionItem);
        self.collection_item_start(e)
    }

    fn collection_item_start(&mut self, e: &BytesStart) -> Result<()> {
        // Start the inner node element and get its IRI.
        self.node_start(e)?;
        let new_iri = self.parents.last().unwrap().clone();
        // Add the iri of the node to the parent scope (not current!)
        self.parent_scope_mut().collection.push(new_iri);
        Ok(())
    }

    // ---

    fn element_end(&mut self, e: &BytesEnd) {
        if let Err(e) = match self.state.pop().unwrap() {
            ParsingState::Node => self.predicate_end(e),
            ParsingState::Predicate => self.node_end(),
            ParsingState::Literal => self.predicate_end(e),
            ParsingState::Resource => self.resource_end(e),
            ParsingState::CollectionItem => self.collection_item_end(),
            ParsingState::Collection => self.collection_end(e),
            ParsingState::Res => self.res_end(),
        } {
            self.triples.push_back(Err(e));
        }
        self.leave_scope();
    }

    fn node_end(&mut self) -> Result<()> {
        // Add the entity as a triple object if it is not top-level
        let o = self.parents.pop().unwrap();
        if self.parents.len() > 2 {
            let s = &self.parents[self.parents.len() - 2];
            let p = &self.parents[self.parents.len() - 1];
            if !s.matches(&rdf::RDF) {
                self.triples.push_back(Ok([s.clone(), p.clone(), o]));
            }
        }

        Ok(())
    }

    fn predicate_end(&mut self, e: &BytesEnd) -> Result<()> {
        // Build the predicate IRI
        let p = self.predicate_iri_end(&self.reader.decode(e.name()))?;

        // Get the literal value
        if self.parents.len() > 1 {
            if let Some(text) = self.scope_mut().text.take() {
                let s = self.parents[self.parents.len() - 2].clone();
                let o = self.scope_mut().new_literal(text)?;
                self.triples.push_back(Ok([s, p, o]));
            }
        }

        self.parents.pop();
        Ok(())
    }

    fn resource_end(&mut self, e: &BytesEnd) -> Result<()> {
        // End of the implicit node element
        self.node_end()?;
        // Drop text, since it is not relevant in a Resource predicate.
        self.scope_mut().text.take();
        // End of the resource predicate
        self.predicate_end(e)
    }

    fn collection_item_end(&mut self) -> Result<()> {
        // End of the node parent.
        self.parents.pop();
        // Remove `CollectionItem`
        self.state.pop();
        Ok(())
    }

    fn collection_end(&mut self, e: &BytesEnd) -> Result<()> {
        let collection = self.scope().collection.clone();
        if !collection.is_empty() {
            let mut node = self.new_bnode();
            let mut elements = collection.into_iter().peekable();

            self.triples.push_back(Ok([
                self.parents[self.parents.len() - 2].clone(),
                self.parents[self.parents.len() - 1].clone(),
                node.clone(),
            ]));

            while let Some(e) = elements.next() {
                self.triples.push_back(Ok([
                    node.clone(),
                    self.factory.borrow_mut().copy(&rdf::first),
                    e.clone(),
                ]));
                if elements.peek().is_some() {
                    let next_node = self.new_bnode();
                    self.triples.push_back(Ok([
                        node,
                        self.factory.borrow_mut().copy(&rdf::rest),
                        next_node.clone(),
                    ]));
                    node = next_node;
                } else {
                    let mut f = self.factory.borrow_mut();
                    self.triples.push_back(Ok([
                        node.clone(),
                        f.copy(&rdf::rest),
                        f.copy(&rdf::nil),
                    ]));
                }
            }
        }

        self.predicate_end(e)
    }

    fn res_end(&mut self) -> Result<()> {
        // Subject, predicate, object and ID of the reified triple
        let id = self.parents.pop().unwrap();
        let p = self.parents.pop().unwrap();
        let s = self.parents.last().unwrap().clone();
        let txt = self.scope_mut().text.take().unwrap_or_default();
        let o = self.scope().new_literal(txt)?;

        // Types for the reification
        let mut factory = self.factory.borrow_mut();
        let ty = factory.copy(&rdf::type_);
        let subject = factory.copy(&rdf::subject);
        let predicate = factory.copy(&rdf::predicate);
        let object = factory.copy(&rdf::object);
        let stmt = factory.copy(&rdf::Statement);

        // Add all triples
        self.triples
            .push_back(Ok([s.clone(), p.clone(), o.clone()]));
        self.triples.push_back(Ok([id.clone(), ty, stmt]));
        self.triples.push_back(Ok([id.clone(), subject, s]));
        self.triples.push_back(Ok([id.clone(), predicate, p]));
        self.triples.push_back(Ok([id.clone(), object, o]));

        Ok(())
    }

    // --- Text elements ----------------------------------------------------

    fn element_text(&mut self, e: &BytesText) {
        if self.scope().text.is_some() {
            match e.unescape_and_decode(&self.reader) {
                Ok(text) => self.scope_mut().set_text(text),
                Err(e) => self
                    .triples
                    .push_back(Err(self::error::Error::from(e).into())),
            }
        }
    }

    // --- Empty elements ----------------------------------------------------

    fn element_empty(&mut self, e: &BytesStart) {
        if let Err(e) = self.enter_scope(e) {
            self.triples.push_back(Err(e));
        }

        if let Err(e) = match self.state.last().unwrap() {
            ParsingState::Node => self.node_empty(e),
            ParsingState::Predicate => self.predicate_empty(e),
            ParsingState::Resource => self.resource_empty(e),
            ParsingState::Collection => self.collection_item_empty(e),
            ParsingState::CollectionItem => unreachable!(),
            ParsingState::Res => panic!("expected end element, not empty"),
            ParsingState::Literal => unimplemented!("empty element as literal"),
        } {
            self.triples.push_back(Err(e));
        }

        self.leave_scope();
    }

    fn node_empty(&mut self, e: &BytesStart) -> Result<()> {
        self.node_start(e)?;
        self.state.pop();
        self.node_end()
    }

    fn predicate_empty(&mut self, e: &BytesStart) -> Result<()> {
        let p = self.predicate_iri_start(&self.reader.decode(e.name()))?;

        // Fail if the property is among forbidden names.
        if RESERVED_PROPERTY_NAMES.matches(&p) {
            let kind = self::error::ErrorKind::InvalidNodeName(p.value());
            return Err(Error::from(self::error::Error::from_kind(kind)));
        }

        let mut object = Vec::with_capacity(1);
        let mut attributes = HashMap::new();
        let mut parse_type = None;
        let mut reification = None;

        // Extract attributes
        for attr in e.attributes().with_checks(true) {
            let a = attr.map_err(self::error::Error::from)?;

            // ignore XML attributes (processed when entering scope)
            if a.key.starts_with(b"xml") {
                continue;
            }

            // try to extract the annotation object
            let k = self.scope().expand_attribute(&self.reader.decode(a.key))?;
            let v = a
                .unescape_and_decode_value(&self.reader)
                .map_err(self::error::Error::from)?;
            if k.matches(&rdf::resource) {
                object.push(self.scope().expand_iri(&v)?);
            } else if k.matches(&rdf::nodeID) {
                object.push(self.rename_bnode(&v)?);
            } else if k.matches(&rdf::ID) {
                let id = self.scope().expand_id(&v)?;
                reification = Some(self.check_unique_id(id)?);
            } else if k.matches(&rdf::parseType) {
                match a.value.as_ref() {
                    b"Resource" => parse_type = Some(&b"Resource"[..]),
                    b"Literal" => parse_type = Some(&b"Literal"[..]),
                    other => panic!("invalid parseType: {:?}", other),
                };
            } else if RESERVED_ATTRIBUTES_NAMES.matches(&k) {
                let kind = self::error::ErrorKind::InvalidAttribute(k.value());
                return Err(Error::from(self::error::Error::from_kind(kind)));
            } else {
                attributes.insert(k, v);
            }
        }

        // Make sure to create the right kind of object if `parseType` was
        // explicitly given in the source document.
        if parse_type == Some(b"Resource") && object.is_empty() {
            object.push(self.new_bnode());
        } else if parse_type == Some(b"Literal") {
            if object.is_empty() && attributes.is_empty() {
                let xmlliteral = self.factory.borrow_mut().copy(&rdf::XMLLiteral);
                let mut scope = self.scope_mut();
                scope.datatype = Some(xmlliteral);
            } else {
                let kind = self::error::ErrorKind::InvalidParseType("Literal".to_string());
                return Err(Error::from(self::error::Error::from_kind(kind)));
            }
        }

        // Extract subjet and object of the triple
        let s = self.parents.last().unwrap().clone();
        let o = match object.len() {
            0 if !attributes.is_empty() => self.new_bnode(),
            1 => object.last().unwrap().clone(),
            0 if attributes.is_empty() => self.scope().new_literal(String::new())?,
            _ => {
                return Err(
                    self::error::Error::from_kind(self::error::ErrorKind::AmbiguousSubject).into(),
                )
            }
        };

        // Add the triple and all subsequent triples as attributes
        self.triples
            .push_back(Ok([s.clone(), p.clone(), o.clone()]));
        for (prop, value) in attributes.into_iter() {
            let literal = self.scope().new_literal(value)?;
            self.triples.push_back(Ok([o.clone(), prop, literal]));
        }

        // Reify the triple if needed.
        if let Some(id) = reification {
            // Types for the reification
            let mut factory = self.factory.borrow_mut();
            let ty = factory.copy(&rdf::type_);
            let subject = factory.copy(&rdf::subject);
            let predicate = factory.copy(&rdf::predicate);
            let obj = factory.copy(&rdf::object);
            let stmt = factory.copy(&rdf::Statement);

            // Add all triples
            self.triples.push_back(Ok([id.clone(), ty, stmt]));
            self.triples.push_back(Ok([id.clone(), subject, s]));
            self.triples.push_back(Ok([id.clone(), predicate, p]));
            self.triples.push_back(Ok([id.clone(), obj, o]));
        }

        Ok(())
    }

    fn resource_empty(&mut self, e: &BytesStart) -> Result<()> {
        self.predicate_empty(e)
    }

    fn collection_item_empty(&mut self, e: &BytesStart) -> Result<()> {
        self.collection_start(e)?;
        self.state.pop();
        self.collection_item_end()
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
        let mut buffer = Vec::new();
        loop {
            // First make sure to consume the queue.
            if let Some(res) = self.triples.pop_front() {
                return Some(res);
            }
            // Then process the next event to maybe produce triples
            match self.reader.read_event(&mut buffer) {
                Ok(Event::Eof) => return None,
                Ok(Event::Start(s)) => self.element_start(&s),
                Ok(Event::Empty(e)) => self.element_empty(&e),
                Ok(Event::End(e)) => self.element_end(&e),
                Ok(Event::Text(t)) => self.element_text(&t),
                Ok(_) => (),
                Err(e) => {
                    let kind = self::error::ErrorKind::XmlError(e);
                    let err = self::error::Error::from_kind(kind);
                    self.triples.push_back(Err(Error::from(err)));
                }
            }
            // Finally clear the buffer if we are going to use it again.
            buffer.clear();
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
                $crate::parser::xml::Config::with_base(&format!(
                        "http://www.w3.org/2013/RDFXMLTests/{}/{}.rdf",
                        stringify!($suite).replace('_', "-"),
                        stringify!($case).replace('_', "-"),
                    ))
                    .unwrap()
                    .parse_read(xmlfile)
                    .in_graph(&mut xml)
                    .expect("failed parsing XML file");

                let mut nt = TestGraph::new();
                $crate::parser::nt::Config::default()
                    .parse_read(ntfile)
                    .in_graph(&mut nt)
                    .expect("failed parsing N-Triples file");

                use std::rc::Rc;
                use crate::term::factory::TermFactory;
                use crate::graph::MutableGraph;

                fn relabel(factory: &mut RcTermFactory, t: Term<Rc<str>>) -> Term<Rc<str>> {
                    if let Term::BNode(bnode) = t {
                        match bnode.as_ref() {
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
                    $crate::parser::xml::Config::with_base(&format!(
                        "http://www.w3.org/2013/RDFXMLTests/{}/{}.rdf",
                        stringify!($suite).replace('_', "-"),
                        stringify!($case).replace('_', "-"),
                    ))
                    .unwrap()
                    .parse_read(xmlfile)
                    .in_graph(&mut xml)
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
                $crate::parser::xml::Config::default()
                    .parse_str($xml)
                    .in_graph(&mut xml)
                    .expect("failed parsing XML file");

                let mut nt = TestGraph::new();
                $crate::parser::nt::Config::default()
                    .parse_str($nt)
                    .in_graph(&mut nt)
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
        rdf_test!(
            #[ignore]
            rdf_charmod_uris
                / test002
        );
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
