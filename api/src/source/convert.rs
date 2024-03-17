//! I define [`ToQuads`] and [`ToQuads`],
//! the result type of [`TripleSource::to_quads`] and [`QuadSource::to_triples`] respectively.

use crate::quad::{Quad, Spog};
use crate::triple::Triple;

use super::{Source, QuadSource, TripleSource};

/// The result type of [`Source::to_quads`].
pub struct ToQuads<TS>(pub(super) TS);

impl<TS: TripleSource> QuadSource for ToQuads<TS> {
    type Quad<'x> = Spog<<TS::Triple<'x> as Triple>::Term>;

    type Error = TS::Error;

    fn try_for_some_quad<E, F>(&mut self, mut f: F) -> super::StreamResult<bool, Self::Error, E>
    where
        E: std::error::Error,
        F: FnMut(Self::Quad<'_>) -> Result<(), E>,
    {
        self.0.try_for_some_triple(|t| {
            let quad = (t.to_spo(), None);
            f(quad)
        })
    }
}

/// The result type of [`QuadSource::to_triples`].
pub struct ToTriples<QS>(pub(super) QS);

impl<QS: QuadSource> Source for ToTriples<QS> {
    type Item<'x> = [<QS::Quad<'x> as Quad>::Term; 3];

    type Error = QS::Error;

    fn try_for_some_item<E, F>(&mut self, mut f: F) -> super::StreamResult<bool, Self::Error, E>
    where
        E: std::error::Error,
        F: FnMut(Self::Item<'_>) -> Result<(), E>,
    {
        self.0.try_for_some_quad(|q| {
            let triple = q.to_spog().0;
            f(triple)
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dataset::{Dataset, MutableDataset};
    use crate::graph::{Graph, MutableGraph};
    use crate::term::ez_term;
    use crate::term::SimpleTerm;

    #[test]
    fn ts_to_quads() {
        let g = vec![
            [ez_term(":a"), ez_term(":b"), ez_term(":c")],
            [ez_term(":d"), ez_term(":e"), ez_term(":f")],
            [ez_term(":g"), ez_term(":h"), ez_term(":i")],
        ];
        let mut h: Vec<Spog<SimpleTerm>> = vec![];
        g.triples()
            .to_quads()
            .for_each_quad(|q| {
                h.insert_quad(q).unwrap();
            })
            .unwrap();
        assert_eq!(
            h,
            vec![
                ([ez_term(":a"), ez_term(":b"), ez_term(":c")], None),
                ([ez_term(":d"), ez_term(":e"), ez_term(":f")], None),
                ([ez_term(":g"), ez_term(":h"), ez_term(":i")], None),
            ]
        )
    }

    #[test]
    fn qs_to_triples() {
        let d = vec![
            ([ez_term(":a"), ez_term(":b"), ez_term(":c")], None),
            ([ez_term(":d"), ez_term(":e"), ez_term(":f")], None),
            ([ez_term(":g"), ez_term(":h"), ez_term(":i")], None),
        ];
        let mut h: Vec<[SimpleTerm; 3]> = vec![];
        d.quads()
            .to_triples()
            .for_each_triple(|t| {
                h.insert_triple(t).unwrap();
            })
            .unwrap();
        assert_eq!(
            h,
            vec![
                [ez_term(":a"), ez_term(":b"), ez_term(":c")],
                [ez_term(":d"), ez_term(":e"), ez_term(":f")],
                [ez_term(":g"), ez_term(":h"), ez_term(":i")],
            ]
        )
    }
}
