use super::*;
use crate::parser::LocatableResult;
use sophia_api::term::TTerm;

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

/// An XML parser supporting any term factory as a backend.
pub(super) struct XmlHandler<B: BufRead, F: TermFactory> {
    /// The underlying XML reader.
    pub(super) reader: XmlReader<B>,
    /// The stack of scoped data (for nested declaration).
    pub(super) scopes: Vec<Scope<F>>,
    /// The stack of parent elements (for nested declarations).
    pub(super) parents: Vec<Term<F::TermData>>,
    /// The queue of produced triples.
    pub(super) triples: LinkedList<Result<[Term<F::TermData>; 3]>>,
    //
    pub(super) factory: Rc<RefCell<F>>,
    //
    pub(super) bnodes: AtomicUsize,
    //
    pub(super) ids: HashSet<Term<F::TermData>>,
    /// The current state of the parser.
    state: Vec<ParsingState>,
}

impl<B, F> XmlHandler<B, F>
where
    B: BufRead,
    F: TermFactory + Clone + Default,
{
    // ---

    /// Get a reference to the current scope.
    fn scope(&self) -> &Scope<F> {
        &self.scopes[self.scopes.len() - 1]
    }

    /// Get a mutable reference to the current scope.
    fn scope_mut(&mut self) -> &mut Scope<F> {
        let l = self.scopes.len();
        &mut self.scopes[l - 1]
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
            let a = attr.locate_err_with(&self.reader)?;
            if a.key.starts_with(b"xmlns:") {
                scope
                    .add_prefix(
                        &self
                            .reader
                            .decode(&a.key[6..])
                            .locate_err_with(&self.reader)?,
                        &a.unescape_and_decode_value(&self.reader)
                            .locate_err_with(&self.reader)?,
                    )
                    .locate_err_with(&self.reader)?;
            } else if a.key == b"xmlns" {
                scope
                    .set_default(
                        &a.unescape_and_decode_value(&self.reader)
                            .locate_err_with(&self.reader)?,
                    )
                    .locate_err_with(&self.reader)?;
            } else if a.key == b"xml:base" {
                scope
                    .set_base(
                        &a.unescape_and_decode_value(&self.reader)
                            .locate_err_with(&self.reader)?,
                    )
                    .locate_err_with(&self.reader)?;
            } else if a.key == b"xml:lang" {
                scope.lang = if a.value.is_empty() {
                    None
                } else {
                    let v = &a
                        .unescape_and_decode_value(&self.reader)
                        .locate_err_with(&self.reader)?;
                    self.factory.borrow_mut().get_term_data(v.as_str()).into()
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
            .bnode(format!("n{}", self.bnodes.fetch_add(1, Ordering::Relaxed)).as_str())
            .expect("always produces a correct BNode")
    }

    /// Rename a bnode using the `nodeID` in the document (using `o` prefix)
    fn rename_bnode(&self, id: &str) -> Result<Term<F::TermData>> {
        if xmlname::is_valid_xmlname(id) {
            self.factory
                .borrow_mut()
                .bnode(format!("o{}", id).as_str())
                .map_err(|e| RdfError::from(e).locate_with(&self.reader))
        } else {
            Err(RdfError::InvalidXmlName(id.to_owned())).locate_err_with(&self.reader)
        }
    }

    /// Check the given `ID` is unique.
    fn check_unique_id(&mut self, id: Term<F::TermData>) -> Result<Term<F::TermData>> {
        if self.ids.contains(&id) {
            Err(RdfError::DuplicateId(id.value().to_string())).locate_err_with(&self.reader)
        } else {
            self.ids.insert(id.clone());
            Ok(id)
        }
    }

    /// Create a new predicate IRI from an XML name (or a RDF metasyntactic element)
    fn predicate_iri_start(&self, name: &str) -> Result<Term<F::TermData>> {
        let p = self
            .scope()
            .expand_attribute(name)
            .locate_err_with(&self.reader)?;
        if p.matches(&rdf::li) {
            self.parent_scope().new_li().locate_err_with(&self.reader)
        } else {
            Ok(p)
        }
    }

    /// Retrieve a predicate IRI from an XML name
    fn predicate_iri_end(&self, name: &str) -> Result<Term<F::TermData>> {
        let p = self
            .scope()
            .expand_attribute(name)
            .locate_err_with(&self.reader)?;
        if p.matches(&rdf::li) {
            self.parent_scope()
                .current_li()
                .locate_err_with(&self.reader)
        } else {
            Ok(p)
        }
    }

    // ---

    /// Create a new `XmlHandler` from the given `quick_xml::Reader`.
    pub(super) fn new(reader: Reader<B>) -> Self {
        let factory: Rc<RefCell<F>> = Default::default();
        Self {
            reader: XmlReader::from(reader),
            parents: Vec::new(),
            scopes: vec![Scope::with_factory_rc(factory.clone())],
            triples: LinkedList::new(),
            factory,
            bnodes: AtomicUsize::new(0),
            state: vec![ParsingState::Node],
            ids: HashSet::new(),
        }
    }

    /// Create a new `XmlHandler` using the given URL as the top-level `xml:base`.
    pub(super) fn with_base(reader: Reader<B>, base: Url) -> Self {
        let mut parser = Self::new(reader);
        let mut scope = parser.scope_mut();
        scope.base = Some(base);
        parser
    }

    // ---

    pub(super) fn element_start(&mut self, e: &BytesStart) {
        if let Err(e) = self.enter_scope(e) {
            self.triples.push_back(Err(e));
        }

        let step = match self.state.last().unwrap() {
            ParsingState::Node => self.node_start(e),
            ParsingState::Predicate => self.predicate_start(e),
            ParsingState::Resource => self.predicate_start(e),
            ParsingState::Collection => self.collection_start(e),
            ParsingState::CollectionItem => self.collection_item_start(e),
            ParsingState::Literal => unimplemented!("entering element as literal"),
            ParsingState::Res => match self.reader.decode(e.name()) {
                Ok(name) => Err(RdfError::UnexpectedEvent {
                    expected: format!("<{}>", name),
                    found: "text".to_owned(),
                })
                .locate_err_with(&self.reader),
                Err(e) => Err(e).locate_err_with(&self.reader),
            },
        };

        if let Err(e) = step {
            self.triples.push_back(Err(e));
        }
    }

    fn node_start(&mut self, e: &BytesStart) -> Result<()> {
        // Get node type from the XML attribute.
        let ty = self
            .scope()
            .expand_attribute(&self.reader.decode(e.name()).locate_err_with(&self.reader)?)
            .locate_err_with(&self.reader)?;

        // Return early if in a top-level rdf:RDF element
        if rdf::RDF.matches(&ty) && self.parents.is_empty() {
            self.state.push(ParsingState::Node);
            self.parents
                .push(self.factory.borrow_mut().clone_term(&rdf::RDF));
            return Ok(());
        }

        // Bail out if the node has an invalid name.
        if RESERVED_NODE_NAMES.matches(&ty) {
            Err(RdfError::InvalidNodeName(ty.value().to_string())).locate_err_with(&self.reader)?;
        }

        // Separate node subject from other attributes
        let mut properties = HashMap::new();
        let mut subject = Vec::new();
        for attr in e.attributes().with_checks(true) {
            let a = attr.locate_err_with(&self.reader)?;

            // ignore xml attributes (processed in element_start)
            if a.key.starts_with(b"xml") {
                continue;
            }

            // try to extract the subject annotation
            let k = self
                .scope()
                .expand_attribute(&self.reader.decode(a.key).locate_err_with(&self.reader)?)
                .locate_err_with(&self.reader)?;
            let v = a
                .unescape_and_decode_value(&self.reader)
                .locate_err_with(&self.reader)?;

            if k.matches(&rdf::about) {
                subject.push(self.scope().expand_iri(&v).locate_err_with(&self.reader)?);
            } else if k.matches(&rdf::ID) {
                let id = self.scope().expand_id(&v).locate_err_with(&self.reader)?;
                subject.push(self.check_unique_id(id)?);
            } else if k.matches(&rdf::nodeID) {
                subject.push(self.rename_bnode(&v)?);
            } else if k.matches(&rdf::type_) {
                properties.insert(
                    k,
                    self.scope().expand_iri(&v).locate_err_with(&self.reader)?,
                );
            } else if RESERVED_ATTRIBUTES_NAMES.matches(&k) {
                Err(RdfError::InvalidAttribute(k.value().to_string()))
                    .locate_err_with(&self.reader)?;
            } else {
                properties.insert(
                    k,
                    self.scope().new_literal(v).locate_err_with(&self.reader)?,
                );
            }
        }

        // Get subject and add it to the current nested stack
        if subject.len() > 1 {
            return Err(RdfError::AmbiguousSubject).locate_err_with(&self.reader);
        }
        let s: Term<_> = subject.pop().unwrap_or_else(|| self.new_bnode());
        self.parents.push(s.clone());

        // Add the type as a triple if it is not `rdf:Description`
        if !ty.matches(&rdf::Description) {
            let type_ = self.factory.borrow_mut().clone_term(&rdf::type_);
            self.triples.push_back(Ok([s.clone(), type_, ty]));
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
        let pred =
            self.predicate_iri_start(&self.reader.decode(e.name()).locate_err_with(&self.reader)?)?;

        // Fail if the property is among forbidden names.
        if RESERVED_PROPERTY_NAMES.matches(&pred) {
            Err(RdfError::InvalidPropertyName(pred.value().to_string()))
                .locate_err_with(&self.reader)?;
        } else {
            self.parents.push(pred);
        }

        // Extract attributes relevant to the RDF syntax
        let mut attributes = HashMap::new();
        let mut next_state = ParsingState::Node;
        let mut object = Vec::with_capacity(1);
        for attr in e.attributes().with_checks(true) {
            let a = attr.locate_err_with(&self.reader)?;

            // Ignore `xml` attributes
            if a.key.starts_with(b"xml") {
                continue;
            }

            let k = self
                .scope()
                .expand_attribute(&self.reader.decode(a.key).locate_err_with(&self.reader)?)
                .locate_err_with(&self.reader)?;
            if k.matches(&rdf::datatype) {
                let v = a
                    .unescape_and_decode_value(&self.reader)
                    .locate_err_with(&self.reader)?;
                self.scope_mut()
                    .set_datatype(&v)
                    .locate_err_with(&self.reader)?;
            } else if k.matches(&rdf::ID) {
                let v = a
                    .unescape_and_decode_value(&self.reader)
                    .locate_err_with(&self.reader)?;
                let id = self.scope().expand_id(&v).locate_err_with(&self.reader)?;
                object.push(self.check_unique_id(id)?);
                next_state = ParsingState::Res;
            } else if k.matches(&rdf::resource) {
                let v = a
                    .unescape_and_decode_value(&self.reader)
                    .locate_err_with(&self.reader)?;
                object.push(self.scope().expand_iri(&v).locate_err_with(&self.reader)?);
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
                        self.scope_mut()
                            .set_datatype(&rdf::XMLLiteral.value())
                            .locate_err_with(&self.reader)?;
                        next_state = ParsingState::Literal;
                    }
                    other => {
                        let ty = String::from_utf8_lossy(other).to_string();
                        Err(RdfError::InvalidParseType(ty)).locate_err_with(&self.reader)?;
                    }
                }
            } else if RESERVED_ATTRIBUTES_NAMES.matches(&k) {
                Err(RdfError::InvalidAttribute(k.value().to_string()))
                    .locate_err_with(&self.reader)?;
            } else {
                let v = a
                    .unescape_and_decode_value(&self.reader)
                    .locate_err_with(&self.reader)?;
                attributes.insert(
                    k,
                    self.scope().new_literal(v).locate_err_with(&self.reader)?,
                );
                next_state = ParsingState::Resource;
            }
        }

        // Extract object of the triple
        let o = match object.len() {
            0 if !attributes.is_empty() => Some(self.new_bnode()),
            0 if attributes.is_empty() => None,
            1 => Some(object.last().unwrap().clone()),
            _ => return Err(RdfError::AmbiguousSubject).locate_err_with(&self.reader),
        };

        // Make the predicate a resource element if an object is present.
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

    pub(super) fn element_end(&mut self, e: &BytesEnd) {
        let step = match self.state.pop().unwrap() {
            ParsingState::Node => self.predicate_end(e),
            ParsingState::Predicate => self.node_end(),
            ParsingState::Literal => self.predicate_end(e),
            ParsingState::Resource => self.resource_end(e),
            ParsingState::CollectionItem => self.collection_item_end(),
            ParsingState::Collection => self.collection_end(e),
            ParsingState::Res => self.res_end(),
        };

        if let Err(e) = step {
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
        let p =
            self.predicate_iri_end(&self.reader.decode(e.name()).locate_err_with(&self.reader)?)?;

        // Get the literal value
        if self.parents.len() > 1 {
            if let Some(text) = self.scope_mut().text.take() {
                let s = self.parents[self.parents.len() - 2].clone();
                let o = self
                    .scope_mut()
                    .new_literal(text)
                    .locate_err_with(&self.reader)?;
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
                    self.factory.borrow_mut().clone_term(&rdf::first),
                    e.clone(),
                ]));
                if elements.peek().is_some() {
                    let next_node = self.new_bnode();
                    self.triples.push_back(Ok([
                        node,
                        self.factory.borrow_mut().clone_term(&rdf::rest),
                        next_node.clone(),
                    ]));
                    node = next_node;
                } else {
                    let mut f = self.factory.borrow_mut();
                    self.triples.push_back(Ok([
                        node.clone(),
                        f.clone_term(&rdf::rest),
                        f.clone_term(&rdf::nil),
                    ]));
                }
            }
        }

        self.predicate_end(e)
    }

    fn res_end(&mut self) -> Result<()> {
        // Subject, predicate, object and ID of the reified triple
        let id = self.parents.pop().unwrap();
        let pred = self.parents.pop().unwrap();
        let sbj = self.parents.last().unwrap().clone();
        let txt = self.scope_mut().text.take().unwrap_or_default();
        let obj = self
            .scope()
            .new_literal(txt)
            .locate_err_with(&self.reader)?;

        // Add all triples
        self.triples
            .push_back(Ok([sbj.clone(), pred.clone(), obj.clone()]));
        self.reify(id, sbj, pred, obj);

        Ok(())
    }

    // --- Text elements ----------------------------------------------------

    pub(super) fn element_text(&mut self, e: &BytesText) {
        if self.scope().text.is_some() {
            match e.unescape_and_decode(&self.reader) {
                Ok(text) => self.scope_mut().set_text(text),
                Err(e) => self.triples.push_back(Err(e.locate_with(&self.reader))),
            }
        }
    }

    // --- Empty elements ----------------------------------------------------

    pub(super) fn element_empty(&mut self, e: &BytesStart) {
        if let Err(e) = self.enter_scope(e) {
            self.triples.push_back(Err(e));
        }

        let step = match self.state.last().unwrap() {
            ParsingState::Node => self.node_empty(e),
            ParsingState::Predicate => self.predicate_empty(e),
            ParsingState::Resource => self.resource_empty(e),
            ParsingState::Collection => self.collection_item_empty(e),
            ParsingState::CollectionItem => unreachable!(),
            ParsingState::Literal => unimplemented!("empty element as literal"),
            ParsingState::Res => match self.reader.decode(e.name()) {
                Ok(name) => Err(RdfError::UnexpectedEvent {
                    expected: format!("<{}/>", name),
                    found: "end".to_owned(),
                })
                .locate_err_with(&self.reader),
                Err(e) => Err(e).locate_err_with(&self.reader),
            },
        };

        if let Err(e) = step {
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
        let pred =
            self.predicate_iri_start(&self.reader.decode(e.name()).locate_err_with(&self.reader)?)?;

        // Fail if the property is among forbidden names.
        if RESERVED_PROPERTY_NAMES.matches(&pred) {
            return Err(RdfError::InvalidPropertyName(pred.value().to_string()))
                .locate_err_with(&self.reader);
        }

        let mut object = Vec::with_capacity(1);
        let mut attributes = HashMap::new();
        let mut parse_type = None;
        let mut reification = None;

        // Extract attributes
        for attr in e.attributes().with_checks(true) {
            let a = attr.locate_err_with(&self.reader)?;

            // ignore XML attributes (processed when entering scope)
            if a.key.starts_with(b"xml") {
                continue;
            }

            // try to extract the annotation object
            let k = self
                .scope()
                .expand_attribute(&self.reader.decode(a.key).locate_err_with(&self.reader)?)
                .locate_err_with(&self.reader)?;
            let v = a
                .unescape_and_decode_value(&self.reader)
                .locate_err_with(&self.reader)?;
            if k.matches(&rdf::resource) {
                object.push(self.scope().expand_iri(&v).locate_err_with(&self.reader)?);
            } else if k.matches(&rdf::nodeID) {
                object.push(self.rename_bnode(&v)?);
            } else if k.matches(&rdf::ID) {
                let id = self.scope().expand_id(&v).locate_err_with(&self.reader)?;
                reification = Some(self.check_unique_id(id)?);
            } else if k.matches(&rdf::parseType) {
                match a.value.as_ref() {
                    b"Resource" => parse_type = Some(&b"Resource"[..]),
                    b"Literal" => parse_type = Some(&b"Literal"[..]),
                    other => {
                        let ty = String::from_utf8_lossy(other).to_string();
                        return Err(RdfError::InvalidParseType(ty)).locate_err_with(&self.reader);
                    }
                };
            } else if RESERVED_ATTRIBUTES_NAMES.matches(&k) {
                return Err(RdfError::InvalidAttribute(k.value().to_string()))
                    .locate_err_with(&self.reader);
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
                let xmlliteral = self.factory.borrow_mut().clone_term(&rdf::XMLLiteral);
                let mut scope = self.scope_mut();
                scope.datatype = Some(xmlliteral);
            } else {
                return Err(RdfError::InvalidParseType("Literal".to_owned()))
                    .locate_err_with(&self.reader);
            }
        }

        // Extract subject and object of the triple
        let sbj = self.parents.last().unwrap().clone();
        let obj = match object.len() {
            0 if !attributes.is_empty() => self.new_bnode(),
            1 => object.last().unwrap().clone(),
            0 if attributes.is_empty() => self
                .scope()
                .new_literal(String::new())
                .locate_err_with(&self.reader)?,
            _ => return Err(RdfError::AmbiguousSubject).locate_err_with(&self.reader),
        };

        // Add the triple and all subsequent triples as attributes
        self.triples
            .push_back(Ok([sbj.clone(), pred.clone(), obj.clone()]));
        for (prop, value) in attributes.into_iter() {
            let literal = self
                .scope()
                .new_literal(value)
                .locate_err_with(&self.reader)?;
            self.triples.push_back(Ok([obj.clone(), prop, literal]));
        }

        // Reify the triple if needed.
        if let Some(id) = reification {
            self.reify(id, sbj, pred, obj);
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

    // ---

    fn reify(
        &mut self,
        id: Term<F::TermData>,
        sbj: Term<F::TermData>,
        pred: Term<F::TermData>,
        obj: Term<F::TermData>,
    ) {
        let mut factory = self.factory.borrow_mut();
        let ty = factory.clone_term(&rdf::type_);
        let subject = factory.clone_term(&rdf::subject);
        let predicate = factory.clone_term(&rdf::predicate);
        let object = factory.clone_term(&rdf::object);
        let stmt = factory.clone_term(&rdf::Statement);

        self.triples.push_back(Ok([id.clone(), ty, stmt]));
        self.triples.push_back(Ok([id.clone(), subject, sbj]));
        self.triples.push_back(Ok([id.clone(), predicate, pred]));
        self.triples.push_back(Ok([id, object, obj]));
    }
}
