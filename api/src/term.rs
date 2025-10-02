//! I define how RDF terms
//! (such as [IRIs](https://www.w3.org/TR/rdf11-concepts/#section-IRIs),
//! [blank nodes](https://www.w3.org/TR/rdf11-concepts/#section-blank-nodes)
//! and [literals](https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal))
//! are represented in Sophia.
//!
//! I provide the main trait [`Term`],
//! and a number of auxiliary types and traits, such as [`TermKind`], [`FromTerm`]...
use crate::ns::rdf;
use crate::triple::Triple;
use mownstr::MownStr;
use std::cmp::{Ord, Ordering};
use std::hash::Hash;

mod _cmp;
pub use _cmp::*;
mod _graph_name;
pub use _graph_name::*;
mod _macro;
mod _native_iri;
mod _native_literal;
mod _simple;
pub use _simple::*;

pub mod base_direction;
pub mod bnode_id;
pub mod language_tag;
pub mod matcher;
pub mod var_name;

/// This type is aliased from `sophia_iri` for convenience,
/// as it is required to implement [`Term`].
pub type IriRef<T> = sophia_iri::IriRef<T>;
// The following two types are also re-exported for the same reason.
pub use base_direction::BaseDirection;
pub use bnode_id::BnodeId;
pub use language_tag::LanguageTag;
pub use var_name::VarName;

lazy_static::lazy_static! {
    static ref RDF_LANG_STRING: Box<str> = crate::ns::rdf::langString.iri().unwrap().unwrap().into();
}

/// The different kinds of terms that a [`Term`] can represent.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum TermKind {
    /// An [RDF IRI](https://www.w3.org/TR/rdf11-concepts/#section-IRIs)
    Iri = 1,
    /// An RDF [literal](https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal)
    Literal = 2,
    /// An RDF [blank node](https://www.w3.org/TR/rdf11-concepts/#section-blank-nodes)
    BlankNode = 0,
    /// An RDF-star [quoted triple](https://www.w3.org/2021/12/rdf-star.html#dfn-quoted)
    Triple = 3,
    /// A SPARQL or Notation3 variable
    Variable = 4,
}
// NB: the associated integer values are used to define the order,
// in a way that is compatible with SPARQL.

/// A [generalized] RDF term.
///
/// # Implementation
///
/// The only method without a default implementation is [`kind`](Term::kind),
/// which indicates what kind of RDF term a given [`Term`] represents.
///
/// However, while all other methods have a default implementtation (returning `None`),
/// those corresponding to the supported kinds MUST be overridden accordingly,
/// otherwise they will panic.
/// See below for an explanation of this design choice.
///
/// In order to test that all the methods are implemented consistently,
/// consider using the macro [`assert_consistent_term_impl`].
/// The macro should be invoked on various instances of the type,
/// at least one for each [kind](Term::kind) that the type supports.
///
/// # Design rationale
///
/// The methods defined by this trait are not independent:
/// depending on the value returned by [`kind`](Term::kind),
/// other methods are expected to return `Some(...)` or `None` accordingly.
///
/// An alternative solution would have been for the variants of [`TermKind`]
/// to *contain* the corresponding values.
/// This would arguably have been more idiomatic for users,
/// and less error-prone for implementors of this trait.
///
/// However, this would have caused performance issues in some cases,
/// because the [`MownStr`] returned by, e.g.,
/// [`iri`](Term::iri) or [`lexical_form`](Term::lexical_form),
/// can be allocated *on demand* by some implementations.
///
/// [generalized]: crate#generalized-vs-strict-rdf-model
pub trait Term: std::fmt::Debug {
    /// A type of [`Term`] that can be borrowed from this type
    /// (i.e. that can be obtained from a simple reference to this type).
    /// It is used in particular for accessing constituents of quoted tripes ([`Term::triple`])
    /// or for sharing this term with a function that expects `T: Term` (rather than `&T`)
    /// using [`Term::borrow_term`].
    ///
    /// In "standard" cases, this type is either `&Self` or `Self`
    /// (for types implementing [`Copy`]).
    ///
    /// # Note to implementors
    /// * When in doubt, set `BorrowTerm<'x>` to `&'x Self`.
    /// * If your type implements [`Copy`],
    ///   consider setting it to `Self`.
    /// * If your type is a wrapper `W(T)` where `T: Term`,
    ///   consider setting it to `W(T::BorrowTerm<'x>)`.
    /// * If none of the options above are possible, your type is probably not a good fit for implementing [`Term`].
    type BorrowTerm<'x>: Term + Copy
    where
        Self: 'x;

    /// Return the kind of RDF term that this [`Term`] represents.
    fn kind(&self) -> TermKind;

    /// Return true if this [`Term`] is an IRI,
    /// i.e. if [`kind`](Term::kind) returns [`TermKind::Iri`].
    #[inline]
    fn is_iri(&self) -> bool {
        self.kind() == TermKind::Iri
    }

    /// Return true if this [`Term`] is a blank node,
    /// i.e. if [`kind`](Term::kind) returns [`TermKind::BlankNode`].
    #[inline]
    fn is_blank_node(&self) -> bool {
        self.kind() == TermKind::BlankNode
    }

    /// Return true if this [`Term`] is a literal,
    /// i.e. if [`kind`](Term::kind) returns [`TermKind::Literal`].
    #[inline]
    fn is_literal(&self) -> bool {
        self.kind() == TermKind::Literal
    }

    /// Return true if this [`Term`] is a variable,
    /// i.e. if [`kind`](Term::kind) returns [`TermKind::Variable`].
    #[inline]
    fn is_variable(&self) -> bool {
        self.kind() == TermKind::Variable
    }

    /// Return true if this [`Term`] is an atomic term,
    /// i.e. an [IRI](Term::is_iri),
    /// a [blank node](Term::is_blank_node),
    /// a [literal](Term::is_literal)
    /// or a [variable](Term::is_variable).
    #[inline]
    fn is_atom(&self) -> bool {
        use TermKind::*;
        match self.kind() {
            Iri | BlankNode | Literal | Variable => true,
            Triple => false,
        }
    }

    /// Return true if this [`Term`] is an RDF-star quoted triple,
    /// i.e. if [`kind`](Term::kind) returns [`TermKind::Triple`].
    #[inline]
    fn is_triple(&self) -> bool {
        self.kind() == TermKind::Triple
    }

    /// If [`kind`](Term::kind) returns [`TermKind::Iri`],
    /// return this IRI.
    /// Otherwise return `None`.
    ///
    /// # Note to implementors
    /// The default implementation assumes that [`Term::is_iri`] always return false.
    /// If that is not the case, this method must be explicit implemented.
    #[inline]
    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        self.is_iri()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    /// If [`kind`](Term::kind) returns [`TermKind::BlankNode`],
    /// return the locally unique label of this blank node.
    /// Otherwise return `None`.
    ///
    /// # Note to implementors
    /// The default implementation assumes that [`Term::is_blank_node`] always return false.
    /// If that is not the case, this method must be explicit implemented.
    #[inline]
    fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
        self.is_blank_node()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    /// If [`kind`](Term::kind) returns [`TermKind::Literal`],
    /// return the lexical form of this literal.
    /// Otherwise return `None`.
    ///
    /// # Note to implementors
    /// The default implementation assumes that [`Term::is_literal`] always return false.
    /// If that is not the case, this method must be explicit implemented.
    #[inline]
    fn lexical_form(&self) -> Option<MownStr<'_>> {
        self.is_literal()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    /// If [`kind`](Term::kind) returns [`TermKind::Literal`],
    /// return the datatype IRI of this literal.
    /// Otherwise return `None`.
    ///
    /// NB: if this literal is a language-tagged string,
    /// then this method MUST return `http://www.w3.org/1999/02/22-rdf-syntax-ns#langString`.
    ///
    /// # Note to implementors
    /// The default implementation assumes that [`Term::is_literal`] always return false.
    /// If that is not the case, this method must be explicit implemented.
    #[inline]
    fn datatype(&self) -> Option<IriRef<MownStr<'_>>> {
        self.is_literal()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    /// If [`kind`](Term::kind) returns [`TermKind::Literal`],
    /// and if this literal is a [directional-]language-tagged string,
    /// return its language tag.
    /// Otherwise return `None`.
    ///
    /// # Note to implementors
    /// The default implementation assumes that [`Term::is_literal`] always return false.
    /// If that is not the case, this method must be explicit implemented.
    #[inline]
    fn language_tag(&self) -> Option<LanguageTag<MownStr<'_>>> {
        self.is_literal()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    /// If [`kind`](Term::kind) returns [`TermKind::Literal`],
    /// and if this literal is a directional-language-tagged string,
    /// return its base direction.
    /// Otherwise return `None`.
    ///
    /// # Note to implementors
    /// The default implementation assumes that [`Term::is_literal`] always return false.
    /// If that is not the case, this method must be explicit implemented.
    #[inline]
    fn base_direction(&self) -> Option<BaseDirection> {
        self.is_literal()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    /// If [`kind`](Term::kind) returns [`TermKind::Variable`],
    /// return the name of this variable.
    /// Otherwise return `None`.
    ///
    /// # Note to implementors
    /// The default implementation assumes that [`Term::is_variable`] always return false.
    /// If that is not the case, this method must be explicit implemented.
    #[inline]
    fn variable(&self) -> Option<VarName<MownStr<'_>>> {
        self.is_variable()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    /// If [`kind`](Term::kind) returns [`TermKind::Triple`],
    /// return this triple.
    /// Otherwise return `None`.
    ///
    /// # Note to implementors
    /// The default implementation assumes that [`Term::is_triple`] always return false.
    /// If that is not the case, this method must be explicit implemented.
    #[inline]
    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        self.is_triple()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    /// If [`kind`](Term::kind) returns [`TermKind::Triple`],
    /// return this triple, consuming this term.
    /// Otherwise return `None`.
    ///
    /// # Note to implementors
    /// The default implementation assumes that [`Term::is_triple`] always return false.
    /// If that is not the case, this method must be explicit implemented.
    #[inline]
    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        self.is_triple()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    /// Get something implementing [`Term`] from a simple reference to `self`,
    /// representing the same RDF term as `self`.
    ///
    /// # Wny do functions in Sophia expect `T: Term` and never `&T: Term`?
    /// To understand the rationale of this design choice,
    /// consider an imaginary type `Foo`.
    /// A function `f(x: Foo)` requires users to waive the ownership of the `Foo` value they want to pass to the function.
    /// This is not always suited to the users needs.
    /// On the other hand, a function `g(x: &Foo)` not only allows, but *forces* its users to maintain ownership of the `Foo` value they want to pass to the function.
    /// Again, there are situations where this is not suitable.
    /// The standard solution to this problem is to use the [`Borrow`](std::borrow) trait:
    /// a function `h<T>(x: T) where T: Borrow<Foo>` allows `x` to be passed either by *value* (transferring ownership)
    /// or by reference (simply borrowing the caller's `Foo`).
    ///
    /// While this design pattern is usable with a single type (`Foo` in our example above),
    /// it is not usable with a trait, such as `Term`:
    /// the following trait bound is not valid in Rust: `T: Borrow<Term>`.
    /// Yet, we would like any function expecting a terms to be able to either take its ownership or simply borrow it,
    /// depending on the caller's needs and preferences.
    ///
    /// The `borrow_term` methods offer a solution to this problem,
    /// and therefore the trait bound `T: Term` must be thought of as equivalent to `T: Borrow<Term>`:
    /// the caller can chose to either waive ownership of its term (by passing it directly)
    /// or keep it (by passing the result of `borrow_term()` instead).
    fn borrow_term(&self) -> Self::BorrowTerm<'_>;

    /// Iter over all the constituents of this term.
    ///
    /// If this term is [atomic](Term::is_atom), the iterator yields only the term itself.
    /// If it is a quoted triple, the iterator yields the quoted triple itself,
    /// and the constituents of its subject, predicate and object.
    fn constituents<'s>(&'s self) -> Box<dyn Iterator<Item = Self::BorrowTerm<'s>> + 's> {
        let this_term = std::iter::once(self.borrow_term());
        match self.triple() {
            None => Box::new(this_term),
            Some(triple) => {
                Box::new(this_term.chain(triple.into_iter().flat_map(Term::to_constituents)))
            }
        }
    }

    /// Iter over all the constituents of this term, consuming it.
    ///
    /// See [Term::constituents].
    fn to_constituents<'a>(self) -> Box<dyn Iterator<Item = Self> + 'a>
    where
        Self: Clone + 'a,
    {
        if !self.is_triple() {
            Box::new(std::iter::once(self))
        } else {
            Box::new(
                std::iter::once(self.clone()).chain(
                    self.to_triple()
                        .unwrap()
                        .into_iter()
                        .flat_map(Term::to_constituents),
                ),
            )
        }
    }

    /// Iter over all the [atomic] constituents of this term.
    ///
    /// If this term is [atomic], the iterator yields only the term itself.
    /// If it is a quoted triple, the iterator yields the atoms of its subject, predicate and object.
    ///
    /// [atomic]: Term::is_atom
    fn atoms<'s>(&'s self) -> Box<dyn Iterator<Item = Self::BorrowTerm<'s>> + 's> {
        match self.triple() {
            None => Box::new(std::iter::once(self.borrow_term())),
            Some(triple) => Box::new(triple.into_iter().flat_map(Term::to_atoms)),
        }
    }

    /// Iter over all the [atomic](Term::is_atom) constituents of this term, consuming it.
    ///
    /// See [Term::atoms].
    fn to_atoms<'a>(self) -> Box<dyn Iterator<Item = Self> + 'a>
    where
        Self: Sized + 'a,
    {
        if !self.is_triple() {
            Box::new(std::iter::once(self))
        } else {
            Box::new(
                self.to_triple()
                    .unwrap()
                    .into_iter()
                    .flat_map(Term::to_atoms),
            )
        }
    }

    /// Check whether `self` and `other` represent the same RDF term.
    fn eq<T: Term>(&self, other: T) -> bool {
        let k1 = self.kind();
        let k2 = other.kind();
        if k1 != k2 {
            return false;
        }
        match k1 {
            TermKind::Iri => self.iri() == other.iri(),
            TermKind::BlankNode => self.bnode_id() == other.bnode_id(),
            TermKind::Literal => {
                self.lexical_form() == other.lexical_form()
                    && match (self.language_tag(), other.language_tag()) {
                        (None, None) => self.datatype() == other.datatype(),
                        (Some(tag1), Some(tag2)) if tag1 == tag2 => {
                            self.base_direction() == other.base_direction()
                        }
                        _ => false,
                    }
            }
            TermKind::Triple => self.triple().unwrap().eq(other.triple().unwrap()),
            TermKind::Variable => self.variable() == other.variable(),
        }
    }

    /// Compare two terms:
    /// * blank nodes < IRIs < literals < quoted triples < variables
    /// * IRIs, blank nodes and variables are ordered by their value
    /// * Literals are ordered by their datatype, then their language (if any),
    ///   then their base direction (if any), then their lexical form
    /// * Quoted triples are ordered in lexicographical order
    ///
    /// NB: literals are ordered by their *lexical* value,
    /// so for example, `"10"^^xsd:integer` comes *before* `"2"^^xsd:integer`.
    fn cmp<T>(&self, other: T) -> Ordering
    where
        T: Term,
    {
        let k1 = self.kind();
        let k2 = other.kind();
        k1.cmp(&k2).then_with(|| match k1 {
            TermKind::Iri => Ord::cmp(&self.iri().unwrap(), &other.iri().unwrap()),
            TermKind::BlankNode => Ord::cmp(&self.bnode_id().unwrap(), &other.bnode_id().unwrap()),
            TermKind::Variable => Ord::cmp(&self.variable().unwrap(), &other.variable().unwrap()),
            TermKind::Literal => {
                let tag1 = self.language_tag();
                let tag2 = other.language_tag();
                if let (Some(tag1), Some(tag2)) = (tag1, tag2) {
                    tag1.cmp(&tag2)
                        .then_with(|| {
                            let dir1 = self.base_direction();
                            let dir2 = other.base_direction();
                            match (dir1, dir2) {
                                (None, None) => Ordering::Equal,
                                (Some(_), None) => Ordering::Less,
                                (None, Some(_)) => Ordering::Greater,
                                (Some(dir1), Some(dir2)) => dir1.cmp(&dir2),
                            }
                        })
                        .then_with(|| {
                            self.lexical_form()
                                .unwrap()
                                .cmp(&other.lexical_form().unwrap())
                        })
                } else {
                    let dt1 = self.datatype().unwrap();
                    let dt2 = other.datatype().unwrap();
                    Ord::cmp(&dt1, &dt2).then_with(|| {
                        self.lexical_form()
                            .unwrap()
                            .cmp(&other.lexical_form().unwrap())
                    })
                }
            }
            TermKind::Triple => {
                let spo1 = self.triple().unwrap();
                let spo2 = other.triple().unwrap();
                Term::cmp(&spo1[0], spo2[0])
                    .then_with(|| Term::cmp(&spo1[1], spo2[1]))
                    .then_with(|| Term::cmp(&spo1[2], spo2[2]))
            }
        })
    }

    /// Compute an implementation-independent hash of this RDF term.
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let k = self.kind();
        k.hash(state);
        match k {
            TermKind::Iri => Hash::hash(self.iri().unwrap().as_str(), state),
            TermKind::BlankNode => Hash::hash(self.bnode_id().unwrap().as_str(), state),
            TermKind::Literal => {
                self.lexical_form().unwrap().hash(state);
                match self.language_tag() {
                    None => {
                        Hash::hash(self.datatype().unwrap().as_str(), state);
                    }
                    Some(tag) => {
                        '@'.hash(state);
                        tag.hash(state);
                        self.base_direction().hash(state);
                    }
                }
            }
            TermKind::Triple => {
                let t = self.triple().unwrap();
                t.s().hash(state);
                t.p().hash(state);
                t.o().hash(state);
            }
            TermKind::Variable => Hash::hash(self.variable().unwrap().as_str(), state),
        }
    }

    /// Convert this term in another type.
    ///
    /// This method is to [`FromTerm`] what [`Into::into`] is to [`From`].
    ///
    /// NB: if you want to make a *copy* of this term without consuming it,
    /// you can use `this_term.`[`borrow_term`](Term::borrow_term)`().into_term::<T>()`.
    #[inline]
    fn into_term<T: FromTerm>(self) -> T
    where
        Self: Sized,
    {
        T::from_term(self)
    }

    /// Try to convert this term into another type.
    ///
    /// This method is to [`TryFromTerm`] what [`TryInto::try_into`] is to [`TryFrom`].
    ///
    /// NB: if you want to make a *copy* of this term without consuming it,
    /// you can use `this_term.`[`borrow_term`](Term::borrow_term)`().try_into_term::<T>()`.
    #[inline]
    fn try_into_term<T: TryFromTerm>(self) -> Result<T, T::Error>
    where
        Self: Sized,
    {
        T::try_from_term(self)
    }

    /// Copies this term into a [`SimpleTerm`],
    /// borrowing as much as possible from `self`
    /// (calling [`SimpleTerm::from_term_ref`]).
    #[inline]
    fn as_simple(&self) -> SimpleTerm<'_> {
        SimpleTerm::from_term_ref(self)
    }
}

impl<'a, T> Term for &'a T
where
    T: Term<BorrowTerm<'a> = &'a T> + ?Sized,
{
    type BorrowTerm<'x>
        = Self
    where
        'a: 'x;

    fn kind(&self) -> TermKind {
        (*self).kind()
    }
    fn is_iri(&self) -> bool {
        (*self).is_iri()
    }
    fn is_blank_node(&self) -> bool {
        (*self).is_blank_node()
    }
    fn is_literal(&self) -> bool {
        (*self).is_literal()
    }
    fn is_variable(&self) -> bool {
        (*self).is_variable()
    }
    fn is_triple(&self) -> bool {
        (*self).is_triple()
    }
    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        (*self).iri()
    }
    fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
        (*self).bnode_id()
    }
    fn lexical_form(&self) -> Option<MownStr<'_>> {
        (*self).lexical_form()
    }
    fn datatype(&self) -> Option<IriRef<MownStr<'_>>> {
        (*self).datatype()
    }
    fn language_tag(&self) -> Option<LanguageTag<MownStr<'_>>> {
        (*self).language_tag()
    }
    fn base_direction(&self) -> std::option::Option<BaseDirection> {
        (*self).base_direction()
    }
    fn variable(&self) -> Option<VarName<MownStr<'_>>> {
        (*self).variable()
    }
    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        (*self).triple()
    }
    fn to_triple(self) -> Option<[Self; 3]> {
        (*self).triple()
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
    fn eq<U: Term>(&self, other: U) -> bool {
        (*self).eq(other)
    }
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (*self).hash(state)
    }
}

//

/// A type that can be built from any term.
///
/// See also [`TryFromTerm`]
pub trait FromTerm: Sized {
    /// Copy `term` into an instance of this type.
    fn from_term<T: Term>(term: T) -> Self;
}

/// A type that can be built from some terms.
///
/// See also [`FromTerm`]
pub trait TryFromTerm: Sized {
    /// The error type produced when failing to copy a given term
    type Error: std::error::Error + Send + Sync + 'static;
    /// Try to copy `term` into an instance of this type.
    fn try_from_term<T: Term>(term: T) -> Result<Self, Self::Error>;
}

/// Test that the given term is consistent in its implementation of the [`Term`] trait.
///
/// NB: it may be necessary to explicitly specify the parameter `T`,
/// even when the type of `t` is known. E.g.: ``assert_consistent_term_impl::<MyTerm>(&t)``.
pub fn assert_consistent_term_impl<T>(t: &T)
where
    T: Term + Clone,
{
    let k = t.kind();
    if k == TermKind::Iri {
        assert!(t.is_iri());
        assert!(t.iri().is_some());
    } else {
        assert!(!t.is_iri());
        assert!(t.iri().is_none());
    }
    if k == TermKind::BlankNode {
        assert!(t.is_blank_node());
        assert!(t.bnode_id().is_some());
    } else {
        assert!(!t.is_blank_node());
        assert!(t.bnode_id().is_none());
    }
    if k == TermKind::Literal {
        assert!(t.is_literal());
        assert!(t.lexical_form().is_some());
        assert!(t.datatype().is_some());
        let datatype = t.datatype().unwrap();
        if rdf::langString == datatype {
            assert!(t.language_tag().is_some());
            assert!(t.base_direction().is_none());
        } else if rdf::dirLangString == datatype {
            assert!(t.language_tag().is_some());
            assert!(t.base_direction().is_some());
        } else {
            assert!(t.language_tag().is_none());
        }
    } else {
        assert!(!t.is_literal());
        assert!(t.lexical_form().is_none());
        assert!(t.datatype().is_none());
        assert!(t.language_tag().is_none());
        assert!(t.base_direction().is_none());
    }
    if k == TermKind::Variable {
        assert!(t.is_variable());
        assert!(t.variable().is_some());
    } else {
        assert!(!t.is_variable());
        assert!(t.variable().is_none());
    }
    if k == TermKind::Triple {
        assert!(t.is_triple());
        assert!(t.triple().is_some());
        assert!(t.clone().to_triple().is_some());
    } else {
        assert!(!t.is_triple());
        assert!(t.triple().is_none());
        assert!(t.clone().to_triple().is_none());
    }
    if k != TermKind::Triple {
        assert!(t.is_atom());
        assert!(t.constituents().count() == 1);
        assert!(t.constituents().next().unwrap().eq(t.borrow_term()));
        assert!(t.clone().to_constituents().count() == 1);
        assert!(t.clone().to_constituents().next().unwrap().eq(t.clone()));
        assert!(t.atoms().count() == 1);
        assert!(t.atoms().next().unwrap().eq(t.borrow_term()));
        assert!(t.clone().to_atoms().count() == 1);
        assert!(t.clone().to_atoms().next().unwrap().eq(t.clone()));
    } else {
        assert!(!t.is_atom());
        assert!(t.constituents().count() >= 4);
        assert!(t.clone().to_constituents().count() >= 4);
        assert!(t.atoms().count() >= 3);
        assert!(t.clone().to_atoms().count() >= 3);
    }
    t.eq(t.borrow_term());
}

#[cfg(test)]
#[allow(dead_code)]
mod check_implementability {
    use super::*;

    // three different implementations of Term using different strategies for Self::Triple

    #[derive(Clone, Copy, Debug)]
    struct Term1 {
        nested: bool,
    }

    const BN1: Term1 = Term1 { nested: false };

    impl Term for Term1 {
        type BorrowTerm<'x> = Self;

        fn kind(&self) -> TermKind {
            match self.nested {
                false => TermKind::BlankNode,
                true => TermKind::Triple,
            }
        }
        fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
            (!self.nested).then(|| BnodeId::new_unchecked("t1".into()))
        }
        fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
            self.nested.then_some([BN1, BN1, BN1])
        }
        fn borrow_term(&self) -> Self::BorrowTerm<'_> {
            *self
        }
    }

    #[derive(Clone, Copy, Debug)]
    struct Term2 {
        nested: bool,
    }

    const BN2: Term2 = Term2 { nested: false };

    impl Term for Term2 {
        type BorrowTerm<'x> = &'x Self;

        fn kind(&self) -> TermKind {
            match self.nested {
                false => TermKind::BlankNode,
                true => TermKind::Triple,
            }
        }
        fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
            (!self.nested).then(|| BnodeId::new_unchecked("t2".into()))
        }
        fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
            self.nested.then_some([&BN2, &BN2, &BN2])
        }
        fn borrow_term(&self) -> Self::BorrowTerm<'_> {
            self
        }
    }

    #[derive(Clone, Debug)]
    struct Term3(Option<Box<[Term3; 3]>>);

    impl Term for Term3 {
        type BorrowTerm<'x> = &'x Self;

        fn kind(&self) -> TermKind {
            match self.0 {
                None => TermKind::BlankNode,
                Some(_) => TermKind::Triple,
            }
        }
        fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
            match self.0 {
                None => Some(BnodeId::new_unchecked("t3".into())),
                Some(_) => None,
            }
        }
        fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
            if let Some(b) = &self.0 {
                let [s, p, o] = b.as_ref();
                Some([s, p, o])
            } else {
                None
            }
        }
        fn borrow_term(&self) -> Self::BorrowTerm<'_> {
            self
        }
    }
}

#[cfg(test)]
/// Simplistic Term parser, useful for writing test cases.
/// The syntax is a subset of Turtle 1.2
/// (with the caveat that triple terms still use the Turtle-star syntax,
///  i.e. << ... >> instead of <<( ... )>> )
pub(crate) fn ez_term(txt: &str) -> SimpleTerm<'_> {
    use sophia_iri::IriRef;
    match txt.as_bytes() {
        [b'<', b'<', .., b'>', b'>'] => {
            let subterms: Vec<&str> = txt[2..txt.len() - 2].split(' ').collect();
            assert_eq!(subterms.len(), 3);
            SimpleTerm::Triple(Box::new([
                ez_term(subterms[0]),
                ez_term(subterms[1]),
                ez_term(subterms[2]),
            ]))
        }
        [b'<', .., b'>'] => IriRef::new_unchecked(&txt[1..txt.len() - 1]).into_term(),
        [b':', ..] => {
            let iri = format!("tag:{}", &txt[1..]);
            SimpleTerm::Iri(IriRef::new_unchecked(iri.into()))
        }
        [b'_', b':', ..] => BnodeId::new_unchecked(&txt[2..]).into_term(),
        [b'\'', .., b'\''] => (&txt[1..txt.len() - 1]).into_term(),
        [b'\'', .., b'\'', b'@', _, _] => SimpleTerm::LiteralLanguage(
            (&txt[1..txt.len() - 4]).into(),
            LanguageTag::new_unchecked(txt[txt.len() - 2..].into()),
            None,
        ),
        [b'\'', .., b'\'', b'@', _, _, b'-', b'-', _, _, _] => SimpleTerm::LiteralLanguage(
            (&txt[1..txt.len() - 5]).into(),
            LanguageTag::new_unchecked(txt[txt.len() - 7..txt.len() - 5].into()),
            Some(txt[txt.len() - 3..].parse().unwrap()),
        ),
        [c, ..] if c.is_ascii_digit() => txt.parse::<i32>().unwrap().into_term(),
        [b'?', ..] => VarName::new_unchecked(&txt[1..]).into_term(),
        _ => panic!("Unable to parse term"),
    }
}

#[cfg(test)]
mod test_term_impl {
    use super::*;
    use test_case::test_case;

    // order with terms of the same kind
    #[test_case("<tag:a>", "<tag:b>")]
    #[test_case("_:u", "_:v")]
    #[test_case("'a'", "'b'")]
    #[test_case("10", "2")]
    #[test_case("'a'@en", "'a'@fr")]
    #[test_case("'a'@en--ltr", "'a'@en--rtl")]
    #[test_case("?x", "?y")]
    #[test_case("<<_:s <tag:p> 'o1'>>", "<<_:s <tag:p> 'o2'>>")]
    #[test_case("<<_:s <tag:p1> 'o2'>>", "<<_:s <tag:p2> 'o1'>>")]
    #[test_case("<<_:s1 <tag:p2> 'o'>>", "<<_:s2 <tag:p1> 'o'>>")]
    // order across different literals
    #[test_case("2", "'10'")]
    #[test_case("'b'@en", "'a'")]
    #[test_case("'b'@en--ltr", "'a'@en")]
    // order across term kinds
    #[test_case("_:b", "<tag:a>")]
    #[test_case("_:b", "'s'")]
    #[test_case("_:b", "<<_:q <tag:q> 'q'>>")]
    #[test_case("_:b", "?p")]
    #[test_case("<tag:a>", "'s'")]
    #[test_case("<tag:a>", "<<_:q <tag:q> 'q'>>")]
    #[test_case("<tag:a>", "?p")]
    #[test_case("'s'", "<<_:q <tag:q> 'q'>>")]
    #[test_case("'s'", "?p")]
    #[test_case("<<_:q <tag:q> 'q'>>", "?p")]
    fn cmp_terms(t1: &str, t2: &str) {
        let t1 = ez_term(t1);
        let t2 = ez_term(t2);
        assert_eq!(Term::cmp(&t1, &t1), std::cmp::Ordering::Equal);
        assert_eq!(Term::cmp(&t2, &t2), std::cmp::Ordering::Equal);
        assert_eq!(Term::cmp(&t1, &t2), std::cmp::Ordering::Less);
        assert_eq!(Term::cmp(&t2, &t1), std::cmp::Ordering::Greater);
    }
}
