//! I define [`FilterSource`], the result type of [`Source::filter_items`].
//! I also define [`FilterTripleSource`] and [`FilterQuadSource`],
//! which are required to ensure that the output of
//! [`TripleSource::filter_triples`] and [`QuadSource::filter_quads`]
//! are recognized as a [`TripleSource`] and a [`QuadSource`], respectively.

use super::*;

/// The result type of [`Source::filter_items`].
pub struct FilterSource<S, P> {
    pub(super) source: S,
    pub(super) predicate: P,
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
    pub struct FilterTripleSource<S, P>(pub(crate) FilterSource<S, P>);

    impl<S, P> Source for FilterTripleSource<S, P>
    where
        S: TripleSource,
        P: FnMut(&S::Item<'_>) -> bool,
    {
        type Item<'x> = TSTriple<'x, S>;
        type Error = S::Error;

        fn try_for_some_item<E, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E>
        where
            E: Error,
            F: FnMut(Self::Item<'_>) -> Result<(), E>,
        {
            self.0.try_for_some_item(|i| f(S::i2t(i)))
        }

        fn size_hint_items(&self) -> (usize, Option<usize>) {
            (0, self.0.size_hint_items().1)
        }
    }
}
pub use _triple::*;

mod _quad {
    use super::*;

    /// The result type of [`QuadSource::filter_quads`].
    pub struct FilterQuadSource<S, P>(pub(crate) FilterSource<S, P>);

    impl<S, P> Source for FilterQuadSource<S, P>
    where
        S: QuadSource,
        P: FnMut(&S::Item<'_>) -> bool,
    {
        type Item<'x> = QSQuad<'x, S>;
        type Error = S::Error;

        fn try_for_some_item<E, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E>
        where
            E: Error,
            F: FnMut(Self::Item<'_>) -> Result<(), E>,
        {
            self.0.try_for_some_item(|i| f(S::i2q(i)))
        }

        fn size_hint_items(&self) -> (usize, Option<usize>) {
            (0, self.0.size_hint_items().1)
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
        let v = vec!["foo", "bar", "baz"];
        let mut w = vec![];
        v.into_iter()
            .into_source()
            .filter_items(|t| t.starts_with('b'))
            .for_each_item(|t| {
                w.push(t);
            })
            .unwrap();
        assert_eq!(w, vec!["bar", "baz",],)
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
            .filter_triples(|t| !Term::eq(t.p(), ez_term(":e")))
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
            .filter_quads(|q| !Term::eq(q.p(), ez_term(":e")))
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
