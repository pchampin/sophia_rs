use super::*;

type ScopeResult<T, E = RdfError> = std::result::Result<T, E>;

/// Data relevant to an XML scope.
#[derive(Debug)]
pub struct Scope<F: TermFactory> {
    /// The XML namespaces declared in this scope.
    pub(super) ns: HashMap<String, Namespace<F::TermData>>,
    /// The default XML namespace to expand tags without namespaces with.
    pub(super) default: Option<Namespace<F::TermData>>,
    /// The base IRI namespace to expand `rdf:ID`, `rdf:resource` and `rdf:about`.
    pub(super) base: Option<Url>,
    /// The term factory used to create new terms.
    pub(super) factory: Rc<RefCell<F>>,
    /// The datatype of the containing element.
    pub(super) datatype: Option<Term<F::TermData>>,
    /// The language tag of the containing element.
    pub(super) lang: Option<F::TermData>,
    /// The text gathered in the current scope.
    pub(super) text: Option<String>,
    /// The current count of list elements
    pub(super) li: AtomicUsize,
    /// The
    pub(super) collection: Vec<Term<F::TermData>>,
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
    pub fn with_factory(f: F) -> Self {
        Self::with_factory_rc(Rc::new(RefCell::new(f)))
    }

    /// Create a new `Scope` from a shared smartpointer to a term factory.
    pub fn with_factory_rc(f: Rc<RefCell<F>>) -> Self {
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
            .add_prefix("xmlns", "https://www.w3.org/2000/xmlns/")
            .unwrap();
        scope
    }

    /// Add a new XML prefix to the namespace mapping.
    pub fn add_prefix(&mut self, prefix: &str, value: &str) -> ScopeResult<()> {
        if prefix == "_" {
            Err(RdfError::InvalidPrefixBlank)
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
    pub fn set_default(&mut self, default: &str) -> ScopeResult<()> {
        let mut f = self.factory.borrow_mut();
        self.default = Some(Namespace::new(f.get_term_data(default))?);
        Ok(())
    }

    /// Set the base IRI prefix.
    pub fn set_base(&mut self, base: &str) -> ScopeResult<()> {
        let url = Url::parse(base)?;

        if !url.cannot_be_a_base() {
            self.base = Some(url);
            Ok(())
        } else {
            Err(RdfError::InvalidBaseIri(base.to_owned()))
        }
    }

    /// Set the scope datatype.
    pub fn set_datatype(&mut self, datatype: &str) -> ScopeResult<()> {
        self.datatype = Some(self.expand_iri(datatype)?);
        Ok(())
    }

    /// Set the scope text.
    pub fn set_text<T: Into<Option<String>>>(&mut self, text: T) {
        self.text = text.into();
    }

    /// Expand an XML attribute in the form `namespace:id` into an IRI.
    ///
    /// This uses the `xmlns` default namespace to expand local attributes,
    /// or any declared namespace in the current scope.
    pub fn expand_attribute(&self, attr: &str) -> ScopeResult<Term<F::TermData>> {
        if let Some(separator_idx) = attr.chars().position(|c| c == ':') {
            let prefix = &attr[..separator_idx];
            let reference = &attr[separator_idx + 1..];
            if let Some(ns) = self.ns.get(prefix) {
                Ok(self.factory.borrow_mut().clone_term(&ns.get(reference)?))
            } else {
                Err(RdfError::UnknownNamespace(prefix.to_owned()))
            }
        } else if let Some(ns) = &self.default {
            Ok(self.factory.borrow_mut().clone_term(&ns.get(attr)?))
        } else {
            Err(RdfError::UnknownNamespace("_".to_owned()))
        }
    }

    /// Expand an IRI reference (in a `rdf:resource` or `rdf:about`) into an IRI.
    ///
    /// This uses `xml:base` to expand local resources, and does nothing in
    /// case the IRI is already in expanded form.
    pub fn expand_iri(&self, iri: &str) -> ScopeResult<Term<F::TermData>> {
        let mut factory = self.factory.borrow_mut();
        if is_relative_iri_ref(iri) {
            // NB: We should not be percent-encoding, but `url::Url::parse`
            // does it anyway: as a fudge, we percent-decode any input that
            // contained non-ASCII characters back. This may cause strange
            // behaviour with URLs that contain a mix of percent-encoded and
            // raw Unicode characters, but this is the best we can do without
            // reimplementing the `url` crate from scratch.
            let ascii = iri.chars().all(|c| c.is_ascii());

            fn decode(s: &str) -> std::borrow::Cow<str> {
                percent_encoding::percent_decode(s.as_bytes())
                    .decode_utf8()
                    .expect("always OK since validated with `is_relative_iri_ref`")
            }

            if let Some(url) = &self.base {
                match url.join(iri) {
                    Ok(u) if ascii => Ok(factory.iri(u.as_ref())?),
                    Ok(u) => Ok(factory.iri(decode(u.as_ref()).as_ref())?),
                    Err(e) => Err(e.into()),
                }
            } else {
                Err(RdfError::NoBaseIri(iri.to_owned()))
            }
        } else if is_absolute_iri_ref(iri) {
            Ok(factory.iri(iri)?)
        } else {
            Err(TermError::InvalidIri(iri.to_owned()).into())
        }
    }

    /// Expand an ID (in a `rdf:ID`) into an IRI.
    ///
    /// This also uses `xml:base` to expand local resources, and prefixes
    /// identifiers in the document with a `#` if needed.
    pub fn expand_id(&self, id: &str) -> ScopeResult<Term<F::TermData>> {
        if !xmlname::is_valid_xmlname(id) {
            Err(RdfError::InvalidXmlName(id.to_owned()))
        } else if id.starts_with('#') {
            self.expand_iri(id)
        } else {
            self.expand_iri(&format!("#{}", id))
        }
    }

    /// Create a new literal with the `rdf:type` and `xml:lang` in scope.
    pub fn new_literal(&self, text: String) -> ScopeResult<Term<F::TermData>> {
        let term = match (&self.datatype, &self.lang) {
            (Some(dt), _) => self
                .factory
                .borrow_mut()
                .literal_dt(text.as_str(), dt.clone())?,
            (None, Some(l)) => self
                .factory
                .borrow_mut()
                .literal_lang(text.as_str(), l.clone())?,
            _ => self
                .factory
                .borrow_mut()
                .literal_dt(text.as_str(), StaticTerm::from(xsd::string))?,
        };
        Ok(term)
    }

    /// Create a new `rdf:li` property by incrementing the scope `li` counter.
    pub fn new_li(&self) -> ScopeResult<Term<F::TermData>> {
        if let Some(ns) = self.ns.get("rdf") {
            let suffix = format!("_{}", self.li.fetch_add(1, Ordering::Relaxed));
            let itemprop = ns.get(&suffix)?;
            let mut f = self.factory.borrow_mut();
            Ok(f.clone_term(&itemprop))
        } else {
            Err(RdfError::UnknownNamespace("rdf".to_owned()))
        }
    }

    /// Get the current `rdf:li` property.
    pub fn current_li(&self) -> ScopeResult<Term<F::TermData>> {
        if let Some(ns) = self.ns.get("rdf") {
            let suffix = format!("_{}", self.li.load(Ordering::Relaxed) - 1);
            let itemprop = ns.get(&suffix)?;
            let mut f = self.factory.borrow_mut();
            Ok(f.clone_term(&itemprop))
        } else {
            Err(RdfError::UnknownNamespace("rdf".to_owned()))
        }
    }
}

impl<F: TermFactory + Default> Default for Scope<F> {
    fn default() -> Self {
        Self::with_factory(Default::default())
    }
}
