//! I define how [RDF triples](https://www.w3.org/TR/rdf11-concepts/#section-triples)
//! are represented in Sophia.
//!
//! I provide the main trait [`Triple`].
//!
//! An RDF triple expresses a single fact.
//! Its formed of three terms called *subject*, *predicate* and *object*.
//!
//! You can think of a triple as a sentence of the form
//! "subject verb complement"
//! (although the *predicate* is often better expressed as a relationship than a verb).
//! Examples :
//!
//! * John is a person.
//! * John was born in Paris.
//! * John knows Jane.
//! * John's family name is "Doe".
//!
use crate::quad::Spog;
use crate::term::{matcher::TermMatcher, GraphName, Term};

/// Type alias for terms borrowed from a triple.
pub type TBorrowTerm<'a, T> = <<T as Triple>::Term as Term>::BorrowTerm<'a>;

/// This trait represents an abstract RDF triple,
/// and provide convenient methods for working with triples.
pub trait Triple {
    /// The type of [`Term`] contained by this triple
    type Term: Term;

    /// The subject of this triple.
    fn s(&self) -> TBorrowTerm<Self>;

    /// The predicate of this triple.
    fn p(&self) -> TBorrowTerm<Self>;

    /// The object of this triple.
    fn o(&self) -> TBorrowTerm<Self>;

    /// The three components of this triple, as a triple of borrowed terms.
    ///
    /// See also [`Triple::to_spo`].
    #[inline]
    fn spo(&self) -> [TBorrowTerm<Self>; 3] {
        [self.s(), self.p(), self.o()]
    }

    /// Consume this triple, returning its subject.
    fn to_s(self) -> Self::Term where Self: Sized {
        let [s, _, _] = self.to_spo();
        s
    }

    /// Consume this triple, returning its predicate.
    fn to_p(self) -> Self::Term where Self: Sized {
        let [_, p, _] = self.to_spo();
        p
    }

    /// Consume this triple, returning its object.
    fn to_o(self) -> Self::Term where Self: Sized {
        let [_, _, o] = self.to_spo();
        o
    }

    /// Consume this triple, returning all its components.
    ///
    /// See also [`Triple::spo`].
    fn to_spo(self) -> [Self::Term; 3] where Self: Sized ;

    /// Checks that the constituents terms of this triple match the respective matchers.
    fn matched_by<S, P, O>(&self, sm: S, pm: P, om: O) -> bool
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
    {
        sm.matches(&self.s()) && pm.matches(&self.p()) && om.matches(&self.o())
    }

    /// Check whether `other` is term-wise equal (using [`Term::eq`]) to `self`.
    ///
    /// See also [`eq_spo`](Triple::eq_spo), [`matched_by`](Triple::matched_by).
    #[inline]
    fn eq<T: Triple>(&self, other: T) -> bool {
        self.eq_spo(other.s(), other.p(), other.o())
    }

    /// Check whether the triple (`s`, `p`, `o`) is term-wise equal (using [`Term::eq`]) to `self`.
    ///
    /// See also [`eq`](Triple::eq), [`matched_by`](Triple::matched_by).
    fn eq_spo<S: Term, P: Term, O: Term>(&self, s: S, p: P, o: O) -> bool {
        self.s().eq(s) && self.p().eq(p) && self.o().eq(o)
    }

    /// Convert this triple to a [`Quad`](crate::quad::Quad) in the default graph.
    ///
    /// NB: if you do not wish to consume this triple,
    /// you can combine this method with [`spo`](Triple::spo) as below:
    /// ```
    /// # use sophia_api::quad::Quad;
    /// # use sophia_api::triple::Triple;
    /// # fn test<T: Triple>(t: &T) -> impl Quad + '_ {
    ///     t.spo().into_quad()   
    /// # }
    /// ```
    ///
    /// See also [`Triple::into_quad_from`].
    fn into_quad(self) -> Spog<Self::Term> where Self: Sized {
        (self.to_spo(), None)
    }

    /// Convert this triple to a [`Quad`](crate::quad::Quad) in the given named graph.
    ///
    /// See also [`Triple::into_quad`].
    fn into_quad_from(self, graph_name: GraphName<Self::Term>) -> Spog<Self::Term> where Self: Sized {
        (self.to_spo(), graph_name)
    }
}

impl<T: Term> Triple for [T; 3] {
    type Term = T;

    fn s(&self) -> TBorrowTerm<Self> {
        self[0].borrow_term()
    }
    fn p(&self) -> TBorrowTerm<Self> {
        self[1].borrow_term()
    }
    fn o(&self) -> TBorrowTerm<Self> {
        self[2].borrow_term()
    }
    fn to_spo(self) -> [Self::Term; 3] {
        self
    }
}

#[cfg(test)]
mod check_implementability {
    use super::*;
    use crate::term::*;
    use mownstr::MownStr;

    #[derive(Clone, Copy, Debug)]
    struct MyBnode(usize);

    impl Term for MyBnode {
        type BorrowTerm<'x> = Self;

        fn kind(&self) -> TermKind {
            TermKind::BlankNode
        }
        fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
            Some(BnodeId::new_unchecked(MownStr::from(format!(
                "b{}",
                self.0
            ))))
        }
        fn borrow_term(&self) -> Self::BorrowTerm<'_> {
            *self
        }
    }

    #[derive(Clone, Copy, Debug)]
    struct MyTriple([usize; 3]);

    impl Triple for MyTriple {
        type Term = MyBnode;

        fn s(&self) -> TBorrowTerm<Self> {
            MyBnode(self.0[0])
        }
        fn p(&self) -> TBorrowTerm<Self> {
            MyBnode(self.0[1])
        }
        fn o(&self) -> TBorrowTerm<Self> {
            MyBnode(self.0[2])
        }
        fn to_s(self) -> Self::Term {
            self.s()
        }
        fn to_p(self) -> Self::Term {
            self.p()
        }
        fn to_o(self) -> Self::Term {
            self.o()
        }
        fn to_spo(self) -> [Self::Term; 3] {
            [self.s(), self.p(), self.o()]
        }
    }

    #[allow(dead_code)] // only checks that this compiles
    fn check_triple_impl(t: [SimpleTerm; 3]) {
        fn foo<T: Triple>(t: T) {
            println!("{:?}", t.s().kind());
        }
        let rt = t.spo();
        foo(rt);
        {
            let rt2 = t.spo();
            foo(rt2);
        }
        foo(rt);
        foo(rt.spo());
        foo(t);

        let mt = MyTriple([1, 2, 3]);
        let rmt = mt.spo();
        foo(rmt);
        {
            let rmt2 = mt.spo();
            foo(rmt2);
        }
        foo(rmt);
        foo(rmt.spo());
        foo(mt);
    }
}

#[cfg(test)]
mod test_triple {
    use super::*;
    use crate::term::SimpleTerm;
    use sophia_iri::IriRef;

    const S: IriRef<&str> = IriRef::new_unchecked_const("tag:s");
    const P: IriRef<&str> = IriRef::new_unchecked_const("tag:o");
    const O: IriRef<&str> = IriRef::new_unchecked_const("tag:p");

    #[test]
    fn triple_matched_by() {
        use crate::term::matcher::Any;
        let t = [S, P, O];

        assert!(t.matched_by(Any, Any, Any));
        assert!(t.matched_by([S], [P], [O]));
        assert!(t.matched_by([O, S], [S, P], [P, O]));
        let istag = |t: SimpleTerm| t.iri().map(|iri| iri.starts_with("tag:")).unwrap_or(false);
        assert!(t.matched_by(istag, istag, istag));

        let none: Option<IriRef<&str>> = None;
        assert!(!t.matched_by(none, Any, Any));
        assert!(!t.matched_by(Any, none, Any));
        assert!(!t.matched_by(Any, Any, none));
        assert!(!t.matched_by([P, O], Any, Any));
        assert!(!t.matched_by(Any, [S, O], Any));
        assert!(!t.matched_by(Any, Any, [S, P]));
        let notag = |t: SimpleTerm| t.iri().map(|iri| !iri.starts_with("tag:")).unwrap_or(true);
        assert!(!t.matched_by(notag, Any, Any));
        assert!(!t.matched_by(Any, notag, Any));
        assert!(!t.matched_by(Any, Any, notag));

        let ts = vec![
            [S, P, S],
            [S, P, P],
            [S, P, O],
            [P, P, S],
            [P, P, P],
            [P, P, O],
            [O, P, S],
            [O, P, P],
            [O, P, O],
        ];
        let c = ts
            .iter()
            .filter(|t| t.matched_by([S, O], Any, [S, O]))
            .count();
        assert_eq!(c, 4);
    }
}
