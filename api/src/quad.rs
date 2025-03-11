//! A quad expresses a single fact within a context.
//! Quads are like RDF [`triples`](crate::triple)
//! augmented with an optional graph name.
//!
//! They are the individual statements of an RDF [datasets](crate::dataset).
use crate::term::matcher::{GraphNameMatcher, TermMatcher};
use crate::term::{GraphName, Term, graph_name_eq};

/// Type alias for terms borrowed from a quad.
pub type QBorrowTerm<'a, T> = <<T as Quad>::Term as Term>::BorrowTerm<'a>;
/// The typical structure representing a Quad of terms T
pub type Spog<T> = ([T; 3], GraphName<T>);
/// An alternative structure representing a Quad of terms T
/// (useful when sorting first by graph is required)
pub type Gspo<T> = (GraphName<T>, [T; 3]);

/// This trait represents an abstract RDF quad,
/// and provide convenient methods for working with quads.
pub trait Quad {
    /// The type of [`Term`] used by this quad when borrowing it
    type Term: Term;

    /// The subject of this quad.
    fn s(&self) -> QBorrowTerm<Self>;

    /// The predicate of this quad.
    fn p(&self) -> QBorrowTerm<Self>;

    /// The object of this quad.
    fn o(&self) -> QBorrowTerm<Self>;

    /// The graph name of this quad.
    ///
    /// `None` means that this quad belongs to the default graph of the dataset.
    fn g(&self) -> GraphName<QBorrowTerm<Self>>;

    /// The four components of this quad, as a quad of borrowed terms.
    ///
    /// See also [`Quad::to_spog`].
    #[inline]
    fn spog(&self) -> Spog<QBorrowTerm<Self>> {
        ([self.s(), self.p(), self.o()], self.g())
    }

    /// Consume this quad, returning its subject.
    fn to_s(self) -> Self::Term
    where
        Self: Sized,
    {
        let [s, _, _] = self.to_spog().0;
        s
    }

    /// Consume this quad, returning its predicate.
    fn to_p(self) -> Self::Term
    where
        Self: Sized,
    {
        let [_, p, _] = self.to_spog().0;
        p
    }

    /// Consume this quad, returning its object.
    fn to_o(self) -> Self::Term
    where
        Self: Sized,
    {
        let [_, _, o] = self.to_spog().0;
        o
    }

    /// Consume this quad, returning its graph name.
    fn to_g(self) -> GraphName<Self::Term>
    where
        Self: Sized,
    {
        self.to_spog().1
    }

    /// Consume this quad, returning all its components.
    ///
    /// See also [`Quad::spog`].
    fn to_spog(self) -> Spog<Self::Term>
    where
        Self: Sized;

    /// Checks that the constituents terms of this quad match the respective matchers.
    fn matched_by<S, P, O, G>(&self, sm: S, pm: P, om: O, gm: G) -> bool
    where
        S: TermMatcher,
        P: TermMatcher,
        O: TermMatcher,
        G: GraphNameMatcher,
    {
        sm.matches(&self.s())
            && pm.matches(&self.p())
            && om.matches(&self.o())
            && gm.matches(self.g().as_ref())
    }

    /// Check whether `other` is term-wise equal (using [`Term::eq`]) to `self`.
    ///
    /// See also [`eq_spog`](Quad::eq_spog), [`matched_by`](Quad::matched_by).
    #[inline]
    fn eq<T: Quad>(&self, other: T) -> bool {
        self.eq_spog(other.s(), other.p(), other.o(), other.g())
    }

    /// Check whether the quad (`s`, `p`, `o`) is term-wise equal (using [`Term::eq`]) to `self`.
    ///
    /// See also [`eq`](Quad::eq), [`matched_by`](Quad::matched_by).
    fn eq_spog<S: Term, P: Term, O: Term, G: Term>(
        &self,
        s: S,
        p: P,
        o: O,
        g: GraphName<G>,
    ) -> bool {
        self.s().eq(s) && self.p().eq(p) && self.o().eq(o) && graph_name_eq(self.g(), g)
    }

    /// Convert this quad to a [`Triple`](crate::triple::Triple) (dropping the graph name)
    ///
    /// NB: if you do not wish to consume this quad,
    /// you can combine this method with [`spog`](Quad::spog) as below:
    /// ```
    /// # use sophia_api::quad::Quad;
    /// # use sophia_api::triple::Triple;
    /// # fn test<T: Quad>(q: &T) -> impl Triple + '_ {
    ///     q.spog().into_triple()   
    /// # }
    /// ```
    fn into_triple(self) -> [Self::Term; 3]
    where
        Self: Sized,
    {
        self.to_spog().0
    }
}

impl<T: Term> Quad for [T; 4] {
    type Term = T;

    fn s(&self) -> QBorrowTerm<Self> {
        self[0].borrow_term()
    }
    fn p(&self) -> QBorrowTerm<Self> {
        self[1].borrow_term()
    }
    fn o(&self) -> QBorrowTerm<Self> {
        self[2].borrow_term()
    }
    fn g(&self) -> GraphName<QBorrowTerm<Self>> {
        Some(self[3].borrow_term())
    }
    fn to_s(self) -> Self::Term {
        let [s, _, _, _] = self;
        s
    }
    fn to_p(self) -> Self::Term {
        let [_, p, _, _] = self;
        p
    }
    fn to_o(self) -> Self::Term {
        let [_, _, o, _] = self;
        o
    }
    fn to_g(self) -> GraphName<Self::Term> {
        let [_, _, _, g] = self;
        Some(g)
    }
    fn to_spog(self) -> Spog<Self::Term> {
        let [s, p, o, g] = self;
        ([s, p, o], Some(g))
    }
}

// Spog<T>
impl<T: Term> Quad for ([T; 3], GraphName<T>) {
    type Term = T;

    fn s(&self) -> QBorrowTerm<Self> {
        self.0[0].borrow_term()
    }
    fn p(&self) -> QBorrowTerm<Self> {
        self.0[1].borrow_term()
    }
    fn o(&self) -> QBorrowTerm<Self> {
        self.0[2].borrow_term()
    }
    fn g(&self) -> GraphName<QBorrowTerm<Self>> {
        self.1.as_ref().map(|gn| gn.borrow_term())
    }
    fn to_s(self) -> Self::Term {
        let [s, _, _] = self.0;
        s
    }
    fn to_p(self) -> Self::Term {
        let [_, p, _] = self.0;
        p
    }
    fn to_o(self) -> Self::Term {
        let [_, _, o] = self.0;
        o
    }
    fn to_g(self) -> GraphName<Self::Term> {
        self.1
    }
    fn to_spog(self) -> Spog<Self::Term> {
        self
    }
}

// Gspo<T>
impl<T: Term> Quad for (GraphName<T>, [T; 3]) {
    type Term = T;

    fn s(&self) -> QBorrowTerm<Self> {
        self.1[0].borrow_term()
    }
    fn p(&self) -> QBorrowTerm<Self> {
        self.1[1].borrow_term()
    }
    fn o(&self) -> QBorrowTerm<Self> {
        self.1[2].borrow_term()
    }
    fn g(&self) -> GraphName<QBorrowTerm<'_, Self>> {
        self.0.as_ref().map(|gn| gn.borrow_term())
    }
    fn to_s(self) -> Self::Term {
        let [s, _, _] = self.1;
        s
    }
    fn to_p(self) -> Self::Term {
        let [_, p, _] = self.1;
        p
    }
    fn to_o(self) -> Self::Term {
        let [_, _, o] = self.1;
        o
    }
    fn to_g(self) -> GraphName<Self::Term> {
        self.0
    }
    fn to_spog(self) -> Spog<Self::Term> {
        let (g, spo) = self;
        (spo, g)
    }
}

/// Iter over all the components of a [`Quad`]
pub fn iter_spog<T: Quad>(q: T) -> impl Iterator<Item = T::Term> {
    let (spo, g) = q.to_spog();
    spo.into_iter().chain(g)
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
    struct MyQuad([usize; 4]);

    impl Quad for MyQuad {
        type Term = MyBnode;

        fn s(&self) -> QBorrowTerm<Self> {
            MyBnode(self.0[0])
        }
        fn p(&self) -> QBorrowTerm<Self> {
            MyBnode(self.0[1])
        }
        fn o(&self) -> QBorrowTerm<Self> {
            MyBnode(self.0[2])
        }
        fn g(&self) -> GraphName<QBorrowTerm<Self>> {
            match self.0[3] {
                0 => None,
                n => Some(MyBnode(n)),
            }
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
        fn to_g(self) -> GraphName<Self::Term> {
            self.g()
        }
        fn to_spog(self) -> Spog<Self::Term> {
            ([self.s(), self.p(), self.o()], self.g())
        }
    }

    #[allow(dead_code)] // only checks that this compiles
    fn check_quad_impl(t: [SimpleTerm; 4]) {
        fn foo<T: Quad>(t: T) {
            println!("{:?}", t.s().kind());
        }
        let rt = t.spog();
        foo(rt);
        {
            let rt2 = t.spog();
            foo(rt2);
        }
        foo(rt);
        foo(rt.spog());
        foo(t);

        let mt = MyQuad([1, 2, 3, 0]);
        let rmt = mt.spog();
        foo(rmt);
        {
            let rmt2 = mt.spog();
            foo(rmt2);
        }
        foo(rmt);
        foo(rmt.spog());
        foo(mt);
    }
}

#[cfg(test)]
mod test_quad {
    use super::*;
    use crate::term::SimpleTerm;
    use sophia_iri::IriRef;

    const S: IriRef<&str> = IriRef::new_unchecked_const("tag:s");
    const P: IriRef<&str> = IriRef::new_unchecked_const("tag:o");
    const O: IriRef<&str> = IriRef::new_unchecked_const("tag:p");
    const G: GraphName<IriRef<&str>> = Some(IriRef::new_unchecked_const("tag:g"));

    #[test]
    fn quad_matched_by() {
        use crate::term::matcher::Any;
        let q = ([S, P, O], G);

        assert!(q.matched_by(Any, Any, Any, Any));
        assert!(q.matched_by([S], [P], [O], [G]));
        assert!(q.matched_by([O, S], [S, P], [P, O], [None, G]));
        let istag = |t: SimpleTerm| t.iri().map(|iri| iri.starts_with("tag:")).unwrap_or(false);
        assert!(q.matched_by(istag, istag, istag, istag.gn()));

        let none: Option<IriRef<&str>> = None;
        assert!(!q.matched_by(none, Any, Any, Any));
        assert!(!q.matched_by(Any, none, Any, Any));
        assert!(!q.matched_by(Any, Any, none, Any));
        assert!(!q.matched_by(Any, Any, Any, none.gn()));
        assert!(!q.matched_by([P, O], Any, Any, Any));
        assert!(!q.matched_by(Any, [S, O], Any, Any));
        assert!(!q.matched_by(Any, Any, [S, P], Any));
        assert!(!q.matched_by(Any, Any, Any, [S, P, O].gn()));
        let notag = |t: SimpleTerm| t.iri().map(|iri| !iri.starts_with("tag:")).unwrap_or(true);
        assert!(!q.matched_by(notag, Any, Any, Any));
        assert!(!q.matched_by(Any, notag, Any, Any));
        assert!(!q.matched_by(Any, Any, notag, Any));
        assert!(!q.matched_by(Any, Any, Any, notag.gn()));

        let ts = vec![
            ([S, P, S], G),
            ([S, P, P], G),
            ([S, P, O], G),
            ([P, P, S], G),
            ([P, P, P], G),
            ([P, P, O], G),
            ([O, P, S], G),
            ([O, P, P], G),
            ([O, P, O], G),
            ([S, P, S], None),
            ([P, P, P], None),
            ([O, P, O], None),
        ];
        let c = ts
            .iter()
            .filter(|t| t.matched_by([S, O], Any, [S, O], Any))
            .count();
        assert_eq!(c, 6);

        let default = G.filter(|_| false);
        let c = ts
            .iter()
            .filter(|t| t.matched_by([S], Any, Any, [default]))
            .count();
        assert_eq!(c, 1);
    }
}
