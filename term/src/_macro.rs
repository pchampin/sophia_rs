//! A [stash]($type_name) is a collection of strings that can be reused
//! to avoid allocating identical string multiple times.

macro_rules! gen_term {
    ($type_name: ident, $wrapper: path, $mod_name: ident) => {
        mod $mod_name {
            use super::*;
            use sophia_api::MownStr;
            use sophia_api::term::{
                BaseDirection, BnodeId, FromTerm, IriRef, LanguageTag, Term, TermKind, TryFromTerm,
                VarName,
            };
            use $wrapper as W;

            #[doc = "An implementation of [`Term`] using "]
            #[doc = stringify!($wrapper)]
            #[doc = " under the hood."]
            #[derive(Clone, Debug)]
            pub enum $type_name {
                /// A straightforward implementation of [`Term`] as an enum.
                /// An [RDF IRI](https://www.w3.org/TR/rdf11-concepts/#section-IRIs)
                Iri(IriRef<W<str>>),
                /// An RDF [blank node](https://www.w3.org/TR/rdf11-concepts/#section-blank-nodes)
                BlankNode(BnodeId<W<str>>),
                /// An RDF [literal](https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal)
                Literal($crate::GenericLiteral<W<str>>),
                /// An RDF-star [quoted triple](https://www.w3.org/2021/12/rdf-star.html#dfn-quoted)
                Triple(W<[Self; 3]>),
                /// A SPARQL or Notation3 variable
                Variable(VarName<W<str>>),
            }

            impl Term for $type_name {
                type BorrowTerm<'x>
                    = &'x Self
                where
                    Self: 'x;

                fn kind(&self) -> sophia_api::term::TermKind {
                    match self {
                        $type_name::Iri(_) => TermKind::Iri,
                        $type_name::BlankNode(_) => TermKind::BlankNode,
                        $type_name::Literal(_) => TermKind::Literal,
                        $type_name::Triple(_) => TermKind::Triple,
                        $type_name::Variable(_) => TermKind::Variable,
                    }
                }

                fn borrow_term(&self) -> Self::BorrowTerm<'_> {
                    self
                }

                fn is_iri(&self) -> bool {
                    matches!(self, $type_name::Iri(..))
                }

                fn is_blank_node(&self) -> bool {
                    matches!(self, $type_name::BlankNode(..))
                }

                fn is_literal(&self) -> bool {
                    matches!(self, $type_name::Literal(..))
                }

                fn is_variable(&self) -> bool {
                    matches!(self, $type_name::Variable(..))
                }

                fn is_atom(&self) -> bool {
                    !matches!(self, $type_name::Triple(..))
                }

                fn is_triple(&self) -> bool {
                    matches!(self, $type_name::Triple(..))
                }

                fn iri(&self) -> Option<IriRef<MownStr>> {
                    if let $type_name::Iri(iri) = self {
                        Some(iri.as_ref().map_unchecked(MownStr::from_ref))
                    } else {
                        None
                    }
                }

                fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
                    if let $type_name::BlankNode(id) = self {
                        Some(id.as_ref().map_unchecked(MownStr::from_ref))
                    } else {
                        None
                    }
                }

                fn lexical_form(&self) -> Option<MownStr> {
                    if let $type_name::Literal(lit) = self {
                        lit.lexical_form()
                    } else {
                        None
                    }
                }

                fn datatype(&self) -> Option<IriRef<MownStr>> {
                    if let $type_name::Literal(lit) = self {
                        lit.datatype()
                    } else {
                        None
                    }
                }

                fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
                    if let $type_name::Literal(lit) = self {
                        lit.language_tag()
                    } else {
                        None
                    }
                }

                fn base_direction(&self) -> Option<BaseDirection> {
                    if let $type_name::Literal(lit) = self {
                        lit.base_direction()
                    } else {
                        None
                    }
                }

                fn variable(&self) -> Option<VarName<MownStr>> {
                    if let $type_name::Variable(name) = self {
                        Some(name.as_ref().map_unchecked(MownStr::from_ref))
                    } else {
                        None
                    }
                }

                fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
                    if let $type_name::Triple(spo) = self {
                        Some([
                            spo[0].borrow_term(),
                            spo[1].borrow_term(),
                            spo[2].borrow_term(),
                        ])
                    } else {
                        None
                    }
                }

                fn to_triple(self) -> Option<[Self; 3]>
                where
                    Self: Sized,
                {
                    if let $type_name::Triple(spo) = self {
                        Some(spo.as_ref().clone())
                    } else {
                        None
                    }
                }
            }

            impl<T: Term> PartialEq<T> for $type_name {
                fn eq(&self, other: &T) -> bool {
                    Term::eq(self, other.borrow_term())
                }
            }

            impl Eq for $type_name {}

            impl std::hash::Hash for $type_name {
                fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                    Term::hash(self, state)
                }
            }

            impl<T: Term> PartialOrd<T> for $type_name {
                fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
                    Some(Term::cmp(self, other.borrow_term()))
                }
            }

            impl Ord for $type_name {
                fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                    Term::cmp(self, other.borrow_term())
                }
            }

            impl FromTerm for $type_name {
                fn from_term<U: Term>(term: U) -> Self {
                    match term.kind() {
                        TermKind::Iri => {
                            // the following is safe because we checked term.kind()
                            let iri = unsafe { term.iri().unwrap_unchecked() };
                            $type_name::Iri(iri.as_ref().map_unchecked(Into::into))
                        }
                        TermKind::Literal => {
                            // the following is safe because we checked term.kind()
                            let lit =
                                unsafe { GenericLiteral::try_from_term(term).unwrap_unchecked() };
                            $type_name::Literal(lit)
                        }
                        TermKind::BlankNode => {
                            // the following is safe because we checked term.kind()
                            let id = unsafe { term.bnode_id().unwrap_unchecked() };
                            $type_name::BlankNode(id.as_ref().map_unchecked(Into::into))
                        }
                        TermKind::Triple => {
                            // the following is safe because we checked term.kind()
                            let spo = unsafe { term.triple().unwrap_unchecked() };
                            $type_name::Triple(W::new(spo.map(Self::from_term)))
                        }
                        TermKind::Variable => {
                            // the following is safe because we checked term.kind()
                            let name = unsafe { term.variable().unwrap_unchecked() };
                            $type_name::Variable(name.as_ref().map_unchecked(Into::into))
                        }
                    }
                }
            }

            impl From<IriRef<W<str>>> for $type_name {
                fn from(value: IriRef<W<str>>) -> Self {
                    $type_name::Iri(value.map_unchecked(Into::into))
                }
            }

            impl From<BnodeId<W<str>>> for $type_name {
                fn from(value: BnodeId<W<str>>) -> Self {
                    $type_name::BlankNode(value)
                }
            }

            impl From<(W<str>, IriRef<W<str>>)> for $type_name {
                fn from(value: (W<str>, IriRef<W<str>>)) -> Self {
                    $type_name::Literal(GenericLiteral::Typed(value.0, value.1))
                }
            }

            impl From<(W<str>, LanguageTag<W<str>>)> for $type_name {
                fn from(value: (W<str>, LanguageTag<W<str>>)) -> Self {
                    $type_name::Literal(GenericLiteral::LanguageString(value.0, value.1, None))
                }
            }

            impl From<(W<str>, LanguageTag<W<str>>, BaseDirection)> for $type_name {
                fn from(value: (W<str>, LanguageTag<W<str>>, BaseDirection)) -> Self {
                    $type_name::Literal(GenericLiteral::LanguageString(
                        value.0,
                        value.1,
                        Some(value.2),
                    ))
                }
            }

            impl From<VarName<W<str>>> for $type_name {
                fn from(value: VarName<W<str>>) -> Self {
                    $type_name::Variable(value)
                }
            }

            impl From<W<[$type_name; 3]>> for $type_name {
                fn from(value: W<[$type_name; 3]>) -> Self {
                    $type_name::Triple(value)
                }
            }

            #[cfg(test)]
            mod test {
                use super::*;
                use sophia_api::{
                    ns::{rdf, xsd},
                    term::{SimpleTerm, assert_consistent_term_impl},
                };

                #[test]
                fn gen_term_iri() {
                    let gt: $type_name = rdf::type_.into_term();
                    assert_consistent_term_impl(&gt);
                    assert_eq!(gt.kind(), TermKind::Iri);
                    assert!(Term::eq(&gt.iri().unwrap(), rdf::type_));
                }

                #[test]
                fn gen_term_bnode() {
                    let bn = BnodeId::new_unchecked("x");
                    let gt: $type_name = bn.into_term();
                    assert_consistent_term_impl(&gt);
                    assert_eq!(gt.kind(), TermKind::BlankNode);
                    assert_eq!(&gt.bnode_id().unwrap(), "x");
                }

                #[test]
                fn gen_term_typed_literal() {
                    let gt: $type_name = 42.into_term();
                    assert_consistent_term_impl(&gt);
                    assert_eq!(gt.kind(), TermKind::Literal);
                    assert_eq!(gt.lexical_form().unwrap(), "42");
                    assert!(Term::eq(&gt.datatype().unwrap(), xsd::integer));
                }

                #[test]
                fn gen_term_language_string() {
                    let en = LanguageTag::new_unchecked("en");
                    let gt: $type_name = ("hello" * en).into_term();
                    assert_consistent_term_impl(&gt);
                    assert_eq!(gt.kind(), TermKind::Literal);
                    assert_eq!(gt.lexical_form().unwrap(), "hello");
                    assert!(Term::eq(&gt.datatype().unwrap(), rdf::langString));
                    assert_eq!(gt.language_tag().unwrap(), en);
                }

                #[test]
                fn gen_term_triple() {
                    let spo = [rdf::type_, rdf::type_, rdf::Property].map(Term::into_term);
                    let qt = SimpleTerm::Triple(Box::new(spo));
                    let gt: $type_name = qt.into_term();
                    assert_consistent_term_impl(&gt);
                    assert_eq!(gt.kind(), TermKind::Triple);
                    let spo = gt.triple().unwrap();
                    assert!(Term::eq(spo[0], rdf::type_));
                    assert!(Term::eq(spo[1], rdf::type_));
                    assert!(Term::eq(spo[2], rdf::Property));
                }

                #[test]
                fn gen_term_variable() {
                    let v = VarName::new_unchecked("x");
                    let gt: $type_name = v.into_term();
                    assert_consistent_term_impl(&gt);
                    assert_eq!(gt.kind(), TermKind::Variable);
                    assert_eq!(&gt.variable().unwrap(), "x");
                }
            }
        }
        pub use $mod_name::$type_name;
    };
}

macro_rules! gen_stash {
    ($type_name: ident, $term_type: ident, $wrapper: path, $mod_name: ident) => {
        mod $mod_name {
            use std::{borrow::Borrow, collections::BTreeSet};
            use $wrapper as W;

            use sophia_api::term::{BnodeId, IriRef, LanguageTag, SimpleTerm, Term, VarName};

            use crate::{GenericLiteral, $term_type};

            #[doc = "A stash is a collection of strings that can be reused for generating [`"]
            #[doc = stringify!($term_type)]
            #[doc = "`] without reallocating identical strings multiple times, "]
            #[doc = "using the `copy_term` method."]
            #[derive(Clone, Debug)]
            pub struct $type_name {
                store: BTreeSet<W<str>>,
            }

            impl $type_name {
                /// Create a new empty stash
                #[must_use]
                pub fn new() -> Self {
                    Default::default()
                }

                /// Retrieve a value from the stash, if present
                #[must_use]
                pub fn get(&self, probe: &str) -> Option<&W<str>> {
                    self.store.get(probe)
                }

                /// Retrieve a value from the stash, inserting it if not present
                pub fn get_or_insert(&mut self, probe: &str) -> &W<str> {
                    if !self.store.contains(probe) {
                        let ins = W::from(probe);
                        self.store.insert(ins);
                    }
                    let ret = self.store.get(probe);
                    debug_assert!(ret.is_some());
                    // this is safe because we just inserted this element
                    unsafe { ret.unwrap_unchecked() }
                }

                /// How many values are stored in this stash
                #[must_use]
                pub fn len(&self) -> usize {
                    self.store.len()
                }

                /// Is this stash empty?
                #[must_use]
                pub fn is_empty(&self) -> bool {
                    self.store.is_empty()
                }
            }

            impl Default for $type_name {
                fn default() -> Self {
                    Self {
                        store: BTreeSet::default(),
                    }
                }
            }

            impl $type_name {
                /// Copy any [`Borrow<str>`] into an `T` backed on this stash.
                pub fn copy_str<U: Borrow<str>>(&mut self, iri: U) -> W<str> {
                    self.get_or_insert(iri.borrow()).clone()
                }

                /// Copy any [`IriRef`] into an [`IriRef<W<str>>`] backed on this stash.
                pub fn copy_iri<U: Borrow<str>>(&mut self, iri: IriRef<U>) -> IriRef<W<str>> {
                    IriRef::new_unchecked(self.copy_str(iri))
                }

                /// Copy any [`BnodeId`] into an [`BnodeId<W<str>>`] backed on this stash.
                pub fn copy_bnode_id<U: Borrow<str>>(
                    &mut self,
                    bnid: BnodeId<U>,
                ) -> BnodeId<W<str>> {
                    BnodeId::new_unchecked(self.copy_str(bnid))
                }

                /// Copy any [`LanguageTag`] into an [`LanguageTag<W<str>>`] backed on this stash.
                pub fn copy_language_tag<U: Borrow<str>>(
                    &mut self,
                    tag: LanguageTag<U>,
                ) -> LanguageTag<W<str>> {
                    LanguageTag::new_unchecked(self.copy_str(tag))
                }

                /// Copy any [`VarName`] into an [`VarName<W<str>>`] backed on this stash.
                pub fn copy_var_name<U: Borrow<str>>(&mut self, vn: VarName<U>) -> VarName<W<str>> {
                    VarName::new_unchecked(self.copy_str(vn))
                }

                #[doc = "Copy any [`Term`] into an [`"]
                #[doc = stringify!($term_type)]
                #[doc = "`] backed on this stash."]
                pub fn copy_term<U: Term>(&mut self, t: U) -> $term_type {
                    use SimpleTerm::*;
                    match t.as_simple() {
                        Iri(iri) => $term_type::Iri(self.copy_iri(iri)),
                        BlankNode(bnid) => $term_type::BlankNode(self.copy_bnode_id(bnid)),
                        LiteralDatatype(lex, dt) => $term_type::Literal(GenericLiteral::Typed(
                            self.copy_str(lex),
                            self.copy_iri(dt),
                        )),
                        LiteralLanguage(lex, tag, dir) => {
                            $term_type::Literal(GenericLiteral::LanguageString(
                                self.copy_str(lex),
                                self.copy_language_tag(tag),
                                dir,
                            ))
                        }
                        Triple(tr) => $term_type::Triple(W::new(tr.map(|t| self.copy_term(t)))),
                        Variable(vn) => $term_type::Variable(self.copy_var_name(vn)),
                    }
                }
            }
        }
        pub use $mod_name::$type_name;
    };
}
