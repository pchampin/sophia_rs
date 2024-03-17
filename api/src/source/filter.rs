//! I define [`FilterTripleSource`] and [`FilterQuadSource`],
//! the result type of [`TripleSource::filter_triples`] and [`QuadSource::filter_quads`] respectively.

use super::*;

/// The result type of [`Source::filter_items`].
pub struct FilterSource<S, P> {
    pub(in super) source: S,
    pub(in super) predicate: P,
}

impl<S, P> Source for FilterSource<S, P>
where
    S: Source,
    P: FnMut(&S::Item<'_>) -> bool,
{
    type Item<'x> = S::Item<'x>;
    type Error = S::Error;

    fn try_for_some_item<E, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E>
    where
        E: Error,
        F: FnMut(Self::Item<'_>) -> Result<(), E>,
    {
        let p = &mut self.predicate;
        self.source.try_for_some_item(|i| {
            if p(&i) {
                f(i)?;
            }
            Ok(())
        })
    }

    fn size_hint_items(&self) -> (usize, Option<usize>) {
        (0, self.source.size_hint_items().1)
    }
}

mod _triple {
    use super::*;

    /// The result type of [`TripleSource::filter_triples`].
    pub struct FilterTripleSource<S, P> {
        pub(in super::super) source: S,
        pub(in super::super) predicate: P,
    }

    impl<S, P> Source for FilterTripleSource<S, P>
    where
        S: TripleSource,
        P: FnMut(&TSTriple<S>) -> bool,
    {
        type Item<'x> = TSTriple<'x, S>;
        type Error = S::Error;

        fn try_for_some_item<E, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E>
        where
            E: Error,
            F: FnMut(Self::Item<'_>) -> Result<(), E>,
        {
            let p = &mut self.predicate;
            self.source.try_for_some_item(|t| {
                if p(S::ri2t(&t)) {
                    f(S::i2t(t))?;
                }
                Ok(())
            })
        }

        fn size_hint_items(&self) -> (usize, Option<usize>) {
            (0, self.source.size_hint_items().1)
        }
    }
}
pub use _triple::*;

/// Maintenance: any change in the _triple module above
/// should be reflected in the _quad module below.
///
/// An easy way to do this is to replace _quad with the code of _triple,
/// replacing all occurrences of `Triple` with `Quad`,
/// and all occurrences of `triple` with `quad`.
mod _quad {
    use super::*;
    /// The result type of [`QuadSource::filter_quads`].
    pub struct FilterQuadSource<S, P> {
        pub(in super::super) source: S,
        pub(in super::super) predicate: P,
    }

    impl<S, P> QuadSource for FilterQuadSource<S, P>
    where
        S: QuadSource,
        P: FnMut(&S::Quad<'_>) -> bool,
    {
        type Quad<'x> = S::Quad<'x>;
        type Error = S::Error;

        fn try_for_some_quad<E, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E>
        where
            E: Error,
            F: FnMut(Self::Quad<'_>) -> Result<(), E>,
        {
            let p = &mut self.predicate;
            self.source.try_for_some_quad(|t| {
                if p(&t) {
                    f(t)?;
                }
                Ok(())
            })
        }

        fn size_hint_quads(&self) -> (usize, Option<usize>) {
            (0, self.source.size_hint_quads().1)
        }
    }
}
pub use _quad::*;

#[cfg(test)]
mod test {
    use super::*;
    use crate::dataset::{Dataset, MutableDataset};
    use crate::graph::{Graph, MutableGraph};
    use crate::quad::{Quad, Spog};
    use crate::term::ez_term;
    use crate::term::{SimpleTerm, Term};
    use crate::triple::Triple;

    #[test]
    fn s_filter_items() {
        let v = vec![
            "foo",
            "bar",
            "baz",
        ];
        let mut w = vec![];
        v.into_iter()
            .into_source()
            .filter_items(|t| t.starts_with("b"))
            .for_each_item(|t| {
                w.push(t);
            })
            .unwrap();
        assert_eq!(
            w,
            vec![ "bar", "baz", ],
        )
    }

    #[test]
    fn ts_filter_triples() {
        let g = vec![
            [ez_term(":a"), ez_term(":b"), ez_term(":c")],
            [ez_term(":d"), ez_term(":e"), ez_term(":f")],
            [ez_term(":g"), ez_term(":h"), ez_term(":i")],
        ];
        let mut h: Vec<[SimpleTerm; 3]> = vec![];
        g.triples()
            .filter_triples(|t| !Term::eq(t.p(), &ez_term(":e")))
            .for_each_triple(|t| {
                h.insert_triple(t).unwrap();
            })
            .unwrap();
        assert_eq!(
            h,
            vec![
                [ez_term(":a"), ez_term(":b"), ez_term(":c")],
                [ez_term(":g"), ez_term(":h"), ez_term(":i")],
            ]
        )
    }

    #[test]
    fn qs_filter_triples() {
        let d = vec![
            ([ez_term(":a"), ez_term(":b"), ez_term(":c")], None),
            ([ez_term(":d"), ez_term(":e"), ez_term(":f")], None),
            ([ez_term(":g"), ez_term(":h"), ez_term(":i")], None),
        ];
        let mut h: Vec<Spog<SimpleTerm>> = vec![];
        d.quads()
            .filter_quads(|q| !Term::eq(q.p(), &ez_term(":e")))
            .for_each_quad(|q| {
                h.insert_quad(q).unwrap();
            })
            .unwrap();
        assert_eq!(
            h,
            vec![
                ([ez_term(":a"), ez_term(":b"), ez_term(":c")], None),
                ([ez_term(":g"), ez_term(":h"), ez_term(":i")], None),
            ]
        )
    }
}
