use super::*;
use crate::ns::rdf;

lazy_static::lazy_static! {
    static ref RDF_LANG_STRING: Box<str> = rdf::langString.iri().unwrap().unwrap().into();
    static ref RDF_DIR_LANG_STRING: Box<str> = rdf::dirLangString.iri().unwrap().unwrap().into();
}

/// A straightforward implementation of [`Term`] as an enum.
#[derive(Clone, Debug)]
pub enum SimpleTerm<'a> {
    /// An [RDF IRI](https://www.w3.org/TR/rdf11-concepts/#section-IRIs)
    Iri(IriRef<MownStr<'a>>),
    /// An RDF [blank node](https://www.w3.org/TR/rdf11-concepts/#section-blank-nodes)
    BlankNode(BnodeId<MownStr<'a>>),
    /// An RDF [literal](https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal)
    LiteralDatatype(MownStr<'a>, IriRef<MownStr<'a>>),
    /// An RDF [language-tagged string](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tagged-string), potentially with base direction
    LiteralLanguage(MownStr<'a>, LanguageTag<MownStr<'a>>, Option<BaseDirection>),
    /// An RDF-star [quoted triple](https://www.w3.org/2021/12/rdf-star.html#dfn-quoted)
    Triple(Box<[Self; 3]>),
    /// A SPARQL or Notation3 variable
    Variable(VarName<MownStr<'a>>),
}

use SimpleTerm::*;

impl<'a> Term for SimpleTerm<'a> {
    type BorrowTerm<'x>
        = &'x Self
    where
        'a: 'x;

    fn kind(&self) -> TermKind {
        match self {
            Iri(_) => TermKind::Iri,
            BlankNode(_) => TermKind::BlankNode,
            LiteralDatatype(..) | LiteralLanguage(..) => TermKind::Literal,
            Triple(_) => TermKind::Triple,
            Variable(_) => TermKind::Variable,
        }
    }
    fn iri(&self) -> Option<IriRef<MownStr>> {
        if let Iri(iri) = self {
            Some(IriRef::new_unchecked(iri.borrowed()))
        } else {
            None
        }
    }
    fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
        if let BlankNode(bnid) = self {
            Some(BnodeId::new_unchecked(bnid.borrowed()))
        } else {
            None
        }
    }
    fn lexical_form(&self) -> Option<MownStr> {
        match self {
            LiteralDatatype(val, _) | LiteralLanguage(val, _, _) => Some(MownStr::from(&val[..])),
            _ => None,
        }
    }
    fn datatype(&self) -> Option<IriRef<MownStr>> {
        match self {
            LiteralDatatype(_, iri) => Some(IriRef::new_unchecked(iri.borrowed())),
            LiteralLanguage(_, _, None) => {
                Some(IriRef::new_unchecked(MownStr::from_ref(&RDF_LANG_STRING)))
            }
            LiteralLanguage(_, _, Some(_)) => Some(IriRef::new_unchecked(MownStr::from_ref(
                &RDF_DIR_LANG_STRING,
            ))),
            _ => None,
        }
    }
    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        if let LiteralLanguage(_, tag, _) = self {
            Some(LanguageTag::new_unchecked(MownStr::from_ref(tag)))
        } else {
            None
        }
    }
    fn base_direction(&self) -> Option<BaseDirection> {
        if let LiteralLanguage(_, _, dir) = self {
            *dir
        } else {
            None
        }
    }
    fn variable(&self) -> Option<VarName<MownStr>> {
        if let Variable(name) = self {
            Some(VarName::new_unchecked(MownStr::from_ref(name)))
        } else {
            None
        }
    }
    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        if let Triple(triple) = self {
            let [s, p, o] = triple.as_ref();
            Some([s, p, o])
        } else {
            None
        }
    }
    fn to_triple(self) -> Option<[Self; 3]> {
        if let Triple(triple) = self {
            Some(*triple)
        } else {
            None
        }
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }
}

fn ensure_owned(m: MownStr) -> MownStr<'static> {
    if m.is_owned() {
        let m = m.clone();
        // Safety: the transmute bellow is safe, because if m.is_owned() is true,
        // then it's data is not restricted to lifetime 'a.
        unsafe { std::mem::transmute::<mownstr::MownStr<'_>, mownstr::MownStr<'_>>(m) }
    } else {
        m.to_string().into()
    }
}

impl FromTerm for SimpleTerm<'static> {
    fn from_term<T: Term>(term: T) -> Self {
        match term.kind() {
            TermKind::Iri => SimpleTerm::Iri(term.iri().unwrap().map_unchecked(ensure_owned)),
            TermKind::BlankNode => {
                SimpleTerm::BlankNode(term.bnode_id().unwrap().map_unchecked(ensure_owned))
            }
            TermKind::Literal => {
                let lex = ensure_owned(term.lexical_form().unwrap());
                if let Some(tag) = term.language_tag() {
                    let tag = tag.map_unchecked(ensure_owned);
                    let dir = term.base_direction();
                    SimpleTerm::LiteralLanguage(lex, tag, dir)
                } else {
                    let dt = term.datatype().unwrap().map_unchecked(ensure_owned);
                    SimpleTerm::LiteralDatatype(lex, dt)
                }
            }
            TermKind::Triple => {
                let t = term.triple().unwrap();
                SimpleTerm::Triple(Box::new([
                    Self::from_term(t.s()),
                    Self::from_term(t.p()),
                    Self::from_term(t.o()),
                ]))
            }
            TermKind::Variable => {
                SimpleTerm::Variable(term.variable().unwrap().map_unchecked(ensure_owned))
            }
        }
    }
}

impl TryFromTerm for SimpleTerm<'static> {
    type Error = std::convert::Infallible;

    fn try_from_term<T: Term>(term: T) -> Result<Self, Self::Error> {
        Ok(Self::from_term(term))
    }
}

impl<'a> SimpleTerm<'a> {
    /// Build a [`SimpleTerm`] that borrows as much as possible from the original `term`.
    ///
    /// NB: depending on the implementation of `term`,
    /// some data might still be allocated.
    pub fn from_term_ref<T>(term: &'a T) -> Self
    where
        T: Term + ?Sized,
    {
        match term.kind() {
            TermKind::Iri => SimpleTerm::Iri(term.iri().unwrap()),
            TermKind::BlankNode => SimpleTerm::BlankNode(term.bnode_id().unwrap()),
            TermKind::Literal => {
                let lex = term.lexical_form().unwrap();
                if let Some(tag) = term.language_tag() {
                    let dir = term.base_direction();
                    SimpleTerm::LiteralLanguage(lex, tag, dir)
                } else {
                    let dt = term.datatype().unwrap();
                    SimpleTerm::LiteralDatatype(lex, dt)
                }
            }
            TermKind::Triple => {
                let t = term.triple().unwrap();
                SimpleTerm::Triple(Box::new([
                    SimpleTerm::<'static>::from_term(t.s()),
                    SimpleTerm::<'static>::from_term(t.p()),
                    SimpleTerm::<'static>::from_term(t.o()),
                ]))
            }
            TermKind::Variable => SimpleTerm::Variable(term.variable().unwrap()),
        }
    }

    /// Build a [`SimpleTerm`] of kind [`Triple`](TermKind::Triple) from any triple.
    pub fn from_triple<T: crate::triple::Triple>(triple: T) -> Self {
        Self::Triple(Box::new([
            triple.s().into_term(),
            triple.p().into_term(),
            triple.o().into_term(),
        ]))
    }
}

impl<T: Term> PartialEq<T> for SimpleTerm<'_> {
    fn eq(&self, other: &T) -> bool {
        Term::eq(self, other.borrow_term())
    }
}

impl Eq for SimpleTerm<'_> {}

impl std::hash::Hash for SimpleTerm<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(self, state)
    }
}

impl<T: Term> PartialOrd<T> for SimpleTerm<'_> {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        Some(Term::cmp(self, other.borrow_term()))
    }
}

impl Ord for SimpleTerm<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        Term::cmp(self, other)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ns::xsd;

    #[test]
    fn iri_from_scratch() {
        let value = IriRef::new_unchecked(MownStr::from_ref("http://example.org/"));
        let t = SimpleTerm::Iri(value.clone());
        assert_consistent_term_impl(&t);
        assert_eq!(t.borrow_term(), &t);
        assert_eq!(t.kind(), TermKind::Iri);
        assert_eq!(t.iri(), Some(value));
    }

    #[test]
    fn bnode_from_scratch() {
        let value = BnodeId::new_unchecked(MownStr::from_ref("b1"));
        let t = SimpleTerm::BlankNode(value.clone());
        assert_consistent_term_impl(&t);
        assert_eq!(t.borrow_term(), &t);
        assert_eq!(t.kind(), TermKind::BlankNode);
        assert_eq!(t.bnode_id(), Some(value));
    }

    #[test]
    fn literal_dt_from_scratch() {
        let value = MownStr::from_ref("hello world");
        let datatype = IriRef::new_unchecked(MownStr::from_ref("http://example.org/"));
        let t = SimpleTerm::LiteralDatatype(value.clone(), datatype.clone());
        assert_consistent_term_impl(&t);
        assert_eq!(t.borrow_term(), &t);
        assert_eq!(t.kind(), TermKind::Literal);
        assert_eq!(t.lexical_form(), Some(value));
        assert_eq!(t.datatype(), Some(datatype));
    }

    #[test]
    fn literal_lang_from_scratch() {
        let value = MownStr::from_ref("hello world");
        let tag = LanguageTag::new_unchecked(MownStr::from_ref("en-US"));
        let t = SimpleTerm::LiteralLanguage(value.clone(), tag.clone(), None);
        assert_consistent_term_impl(&t);
        assert_eq!(t.borrow_term(), &t);
        assert_eq!(t.kind(), TermKind::Literal);
        assert_eq!(t.lexical_form(), Some(value));
        assert_eq!(t.language_tag(), Some(tag));
        assert_eq!(t.base_direction(), None);
    }

    #[test]
    fn literal_dir_lang_from_scratch() {
        let value = MownStr::from_ref("hello world");
        let tag = LanguageTag::new_unchecked(MownStr::from_ref("en-US"));
        let dir = BaseDirection::Ltr;
        let t = SimpleTerm::LiteralLanguage(value.clone(), tag.clone(), Some(dir));
        assert_consistent_term_impl(&t);
        assert_eq!(t.borrow_term(), &t);
        assert_eq!(t.kind(), TermKind::Literal);
        assert_eq!(t.lexical_form(), Some(value));
        assert_eq!(t.language_tag(), Some(tag));
        assert_eq!(t.base_direction(), Some(dir));
    }

    #[test]
    fn variable_from_scratch() {
        let value = VarName::new_unchecked(MownStr::from_ref("x"));
        let t = SimpleTerm::Variable(value.clone());
        assert_consistent_term_impl(&t);
        assert_eq!(t.borrow_term(), &t);
        assert_eq!(t.kind(), TermKind::Variable);
        assert_eq!(t.variable(), Some(value));
    }

    #[test]
    fn triple_from_scratch() {
        let s: SimpleTerm<'_> = BnodeId::new_unchecked(MownStr::from_ref("s")).into_term();
        let p: SimpleTerm<'_> = IriRef::new_unchecked(MownStr::from_ref("p")).into_term();
        let o: SimpleTerm<'_> = "o".into_term();
        let spo = [s.clone(), p.clone(), o.clone()];
        let t = SimpleTerm::Triple(Box::new(spo.clone()));
        assert_consistent_term_impl(&t);
        assert_eq!(t.borrow_term(), &t);
        assert_eq!(t.kind(), TermKind::Triple);
        assert_eq!(t.triple(), Some([&s, &p, &o]));
        assert_eq!(t.clone().to_triple(), Some(spo.clone()));
        assert_eq!(t.constituents().collect::<Vec<_>>(), vec![&t, &s, &p, &o]);
        assert_eq!(
            t.clone().to_constituents().collect::<Vec<_>>(),
            t.constituents().cloned().collect::<Vec<_>>()
        );
        assert_eq!(t.atoms().collect::<Vec<_>>(), vec![&s, &p, &o]);
        assert_eq!(
            t.clone().to_atoms().collect::<Vec<_>>(),
            Vec::from(spo.clone())
        );
    }

    #[test]
    fn nested_triple_from_scratch() {
        let s1: SimpleTerm<'_> = BnodeId::new_unchecked(MownStr::from_ref("s")).into_term();
        let p1: SimpleTerm<'_> = IriRef::new_unchecked(MownStr::from_ref("p")).into_term();
        let o1: SimpleTerm<'_> = "o".into_term();
        let spo1 = [s1.clone(), p1.clone(), o1.clone()];
        let t1 = SimpleTerm::Triple(Box::new(spo1));
        let s: SimpleTerm<'_> = s1.clone();
        let p: SimpleTerm<'_> = p1.clone();
        let o = t1.clone();
        let spo2 = [s.clone(), p.clone(), o.clone()];
        let t = SimpleTerm::Triple(Box::new(spo2.clone()));
        assert_consistent_term_impl(&t);
        assert_eq!(t.borrow_term(), &t);
        assert_eq!(t.kind(), TermKind::Triple);
        assert_eq!(t.triple(), Some([&s, &p, &o]));
        assert_eq!(t.clone().to_triple(), Some(spo2.clone()));
        assert_eq!(
            t.constituents().collect::<Vec<_>>(),
            vec![&t, &s, &p, &t1, &s1, &p1, &o1]
        );
        assert_eq!(
            t.clone().to_constituents().collect::<Vec<_>>(),
            t.constituents().cloned().collect::<Vec<_>>()
        );
        assert_eq!(t.atoms().collect::<Vec<_>>(), vec![&s, &p, &s1, &p1, &o1]);
        assert_eq!(
            t.clone().to_atoms().collect::<Vec<_>>(),
            t.atoms().cloned().collect::<Vec<_>>()
        );
    }

    #[test]
    fn iri_from_term() {
        let t: SimpleTerm<'_> = rdf::type_.into_term();
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::Iri);
        assert_eq!(t.iri(), rdf::type_.iri());
    }

    #[test]
    fn literal_from_term() {
        let t: SimpleTerm<'_> = "hello world".into_term();
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::Literal);
        assert_eq!(t.lexical_form().unwrap(), "hello world");
        assert_eq!(t.datatype(), xsd::string.iri());

        let t: SimpleTerm<'_> = 42.into_term();
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::Literal);
        assert_eq!(t.lexical_form().unwrap(), "42");
        assert_eq!(t.datatype(), xsd::integer.iri());
    }

    #[test]
    fn bnode_from_term() {
        let b1 = BnodeId::new("b1").unwrap();
        let t: SimpleTerm<'_> = b1.into_term();
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::BlankNode);
        assert_eq!(t.bnode_id().unwrap(), b1);
    }

    #[test]
    fn triple_from_term() {
        let s: SimpleTerm<'_> = BnodeId::new_unchecked(MownStr::from_ref("s")).into_term();
        let p: SimpleTerm<'_> = IriRef::new_unchecked(MownStr::from_ref("p")).into_term();
        let o: SimpleTerm<'_> = "o".into_term();
        let spo = [s.clone(), p.clone(), o.clone()];
        let tr = SimpleTerm::from_triple(spo.spo());
        let t: SimpleTerm<'_> = tr.into_term();
        assert_consistent_term_impl(&t);
        assert_eq!(t.borrow_term(), &t);
        assert_eq!(t.kind(), TermKind::Triple);
        assert_eq!(t.triple(), Some([&s, &p, &o]));
        assert_eq!(t.clone().to_triple(), Some(spo.clone()));
        assert_eq!(t.constituents().collect::<Vec<_>>(), vec![&t, &s, &p, &o]);
        assert_eq!(
            t.clone().to_constituents().collect::<Vec<_>>(),
            t.constituents().cloned().collect::<Vec<_>>()
        );
        assert_eq!(t.atoms().collect::<Vec<_>>(), vec![&s, &p, &o]);
        assert_eq!(
            t.clone().to_atoms().collect::<Vec<_>>(),
            Vec::from(spo.clone())
        );
    }

    #[test]
    fn variable_from_term() {
        let v1 = VarName::new("v1").unwrap();
        let t: SimpleTerm<'_> = v1.into_term();
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::Variable);
        assert_eq!(t.variable().unwrap(), v1);
    }

    #[test]
    fn try_from_term() {
        let t: SimpleTerm<'_> = 42.try_into_term().unwrap();
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::Literal);
        assert_eq!(t.lexical_form().unwrap(), "42");
        assert_eq!(t.datatype(), xsd::integer.iri());
    }

    #[test]
    fn iri_from_term_ref() {
        let i = sophia_iri::Iri::new("http://example.com/").unwrap();
        let t = SimpleTerm::from_term_ref(&i);
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::Iri);
        assert_eq!(t.iri(), i.iri());
        assert!(t.iri().unwrap().unwrap().is_borrowed());
    }

    #[test]
    fn literal_from_term_ref() {
        let l = "hello world";
        let t = SimpleTerm::from_term_ref(&l);
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::Literal);
        assert_eq!(t.lexical_form().unwrap(), l);
        assert!(t.lexical_form().unwrap().is_borrowed());
    }

    #[test]
    fn bnode_from_term_ref() {
        let b = BnodeId::new("b1").unwrap();
        let t = SimpleTerm::from_term_ref(&b);
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::BlankNode);
        assert_eq!(t.bnode_id().unwrap(), b);
        assert!(t.bnode_id().unwrap().unwrap().is_borrowed());
    }

    #[test]
    fn triple_from_term_ref() {
        let s: SimpleTerm<'_> = BnodeId::new_unchecked(MownStr::from_ref("s")).into_term();
        let p: SimpleTerm<'_> = IriRef::new_unchecked(MownStr::from_ref("p")).into_term();
        let o: SimpleTerm<'_> = "o".into_term();
        let spo = [s.clone(), p.clone(), o.clone()];
        let tr = SimpleTerm::from_triple(spo.spo());
        let t = SimpleTerm::from_term_ref(&tr);
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::Triple);
        let inner_s = t.to_atoms().next().unwrap();
        assert!(inner_s.bnode_id().unwrap().unwrap().is_borrowed());
    }

    #[test]
    fn variable_from_term_ref() {
        let v = VarName::new("v1").unwrap();
        let t = SimpleTerm::from_term_ref(&v);
        assert_consistent_term_impl(&t);
        assert_eq!(t.kind(), TermKind::Variable);
        assert_eq!(t.variable().unwrap(), v);
        assert!(t.variable().unwrap().unwrap().is_borrowed());
    }
}
