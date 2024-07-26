//! I define [`FilterMapSource`] the result type of [`Source::filter_map_items`].
use super::*;
use std::{collections::VecDeque, error::Error};

/// The result of [`Source::filter_map_items`].
pub struct FilterMapSource<S, F> {
    pub(in super::super) source: S,
    pub(in super::super) filter_map: F,
}

impl<S, F, T> Source for FilterMapSource<S, F>
where
    S: Source,
    F: FnMut(S::Item<'_>) -> Option<T>,
{
    type Item<'x> = T;
    type Error = S::Error;

    fn try_for_some_item<E, F2>(&mut self, mut f: F2) -> StreamResult<bool, Self::Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
        F2: FnMut(Self::Item<'_>) -> Result<(), E>,
    {
        let filter_map = &mut self.filter_map;
        self.source.try_for_some_item(|t| match (filter_map)(t) {
            None => Ok(()),
            Some(out) => f(out),
        })
    }

    fn size_hint_items(&self) -> (usize, Option<usize>) {
        (0, self.source.size_hint_items().1)
    }
}

impl<S, F, T> IntoIterator for FilterMapSource<S, F>
where
    S: Source,
    F: FnMut(S::Item<'_>) -> Option<T>,
{
    type Item = Result<T, S::Error>;
    type IntoIter = FilterMapSourceIterator<S, F, T, S::Error>;

    fn into_iter(self) -> Self::IntoIter {
        FilterMapSourceIterator {
            source: self.source,
            filter_map: self.filter_map,
            buffer: VecDeque::new(),
        }
    }
}

/// [`Iterator`] implementation for the returned value of [`Source::filter_map_items`].
pub struct FilterMapSourceIterator<S, F, T, E> {
    source: S,
    filter_map: F,
    buffer: VecDeque<Result<T, E>>,
}

impl<S, F, T> Iterator for FilterMapSourceIterator<S, F, T, S::Error>
where
    S: Source,
    F: FnMut(S::Item<'_>) -> Option<T>,
{
    type Item = Result<T, S::Error>;
    fn next(&mut self) -> Option<Result<T, S::Error>> {
        let mut remaining = true;
        let mut buffer = VecDeque::new();
        std::mem::swap(&mut self.buffer, &mut buffer);
        while buffer.is_empty() && remaining {
            let resb = self.source.for_some_item(|i| {
                if let Some(t) = (self.filter_map)(i) {
                    buffer.push_back(Ok(t));
                }
            });
            match resb {
                Ok(b) => {
                    remaining = b;
                }
                Err(err) => {
                    buffer.push_back(Err(err));
                    remaining = false;
                }
            }
        }
        std::mem::swap(&mut self.buffer, &mut buffer);
        self.buffer.pop_front()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.source.size_hint_items()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dataset::{Dataset, MutableDataset};
    use crate::graph::{Graph, MutableGraph};
    use crate::quad::{Quad, Spog};
    use crate::term::ez_term;
    use crate::term::{SimpleTerm, Term};
    use crate::triple::Triple;

    // check that the result of TripleSource::filter_map_triples implements the expected traits,
    // and that they work as expected

    #[test]
    fn ts_filter_map_to_triples() {
        let g = vec![
            [ez_term(":a"), ez_term(":b"), ez_term(":c")],
            [ez_term(":d"), ez_term(":e"), ez_term(":f")],
            [ez_term(":g"), ez_term(":h"), ez_term(":i")],
        ];
        let mut h: Vec<[SimpleTerm; 3]> = vec![];
        g.triples()
            .filter_map_triples(|t| {
                (!Term::eq(t.p(), ez_term(":e"))).then_some([t.o(), t.p(), t.s()])
            })
            .for_each_triple(|t| {
                h.insert_triple(t).unwrap();
            })
            .unwrap();
        assert_eq!(
            h,
            vec![
                [ez_term(":c"), ez_term(":b"), ez_term(":a")],
                [ez_term(":i"), ez_term(":h"), ez_term(":g")],
            ]
        )
    }

    #[test]
    fn ts_filter_map_to_quads() {
        let g = vec![
            [ez_term(":a"), ez_term(":b"), ez_term(":c")],
            [ez_term(":g"), ez_term(":h"), ez_term(":i")],
        ];
        let mut h: Vec<Spog<SimpleTerm>> = vec![];
        g.triples()
            .filter_map_triples(|t| {
                (!Term::eq(t.p(), ez_term(":e"))).then_some(([t.o(), t.p(), t.s()], None))
            })
            .for_each_quad(|q| {
                h.insert_quad(q).unwrap();
            })
            .unwrap();
        assert_eq!(
            h,
            vec![
                ([ez_term(":c"), ez_term(":b"), ez_term(":a")], None),
                ([ez_term(":i"), ez_term(":h"), ez_term(":g")], None),
            ]
        )
    }

    #[test]
    fn ts_filter_map_iter() {
        let g = vec![
            [ez_term(":a"), ez_term(":b"), ez_term(":c")],
            [ez_term(":d"), ez_term(":e"), ez_term(":f")],
            [ez_term(":g"), ez_term(":h"), ez_term(":i")],
        ];
        let h: Result<Vec<String>, _> = g
            .triples()
            .filter_map_triples(|t| {
                (!Term::eq(t.p(), ez_term(":e"))).then_some(t.s().iri().unwrap().to_string())
            })
            .into_iter()
            .collect();
        assert_eq!(h.unwrap(), vec!["tag:a".to_string(), "tag:g".to_string(),])
    }

    // check that the result of QuadSource::filter_map_quads implements the expected traits
    // and that they work as expected

    #[test]
    fn qs_filter_map_to_triples() {
        let d = vec![
            ([ez_term(":a"), ez_term(":b"), ez_term(":c")], None),
            ([ez_term(":d"), ez_term(":e"), ez_term(":f")], None),
            ([ez_term(":g"), ez_term(":h"), ez_term(":i")], None),
        ];
        let mut h: Vec<[SimpleTerm; 3]> = vec![];
        d.quads()
            .filter_map_quads(|q| {
                (!Term::eq(q.p(), ez_term(":e"))).then_some([q.o(), q.p(), q.s()])
            })
            .for_each_triple(|t| {
                h.insert_triple(t).unwrap();
            })
            .unwrap();
        assert_eq!(
            h,
            vec![
                [ez_term(":c"), ez_term(":b"), ez_term(":a")],
                [ez_term(":i"), ez_term(":h"), ez_term(":g")],
            ]
        )
    }

    #[test]
    fn qs_filter_map_to_quads() {
        let d = vec![
            ([ez_term(":a"), ez_term(":b"), ez_term(":c")], None),
            ([ez_term(":d"), ez_term(":e"), ez_term(":f")], None),
            ([ez_term(":g"), ez_term(":h"), ez_term(":i")], None),
        ];
        let mut h: Vec<Spog<SimpleTerm>> = vec![];
        d.quads()
            .filter_map_quads(|q| {
                (!Term::eq(q.p(), ez_term(":e"))).then_some(([q.o(), q.p(), q.s()], q.g()))
            })
            .for_each_quad(|q| {
                h.insert_quad(q).unwrap();
            })
            .unwrap();
        assert_eq!(
            h,
            vec![
                ([ez_term(":c"), ez_term(":b"), ez_term(":a")], None),
                ([ez_term(":i"), ez_term(":h"), ez_term(":g")], None),
            ]
        )
    }

    #[test]
    fn qs_filter_map_iter() {
        let d = vec![
            ([ez_term(":a"), ez_term(":b"), ez_term(":c")], None),
            ([ez_term(":d"), ez_term(":e"), ez_term(":f")], None),
            ([ez_term(":g"), ez_term(":h"), ez_term(":i")], None),
        ];
        let h: Result<Vec<String>, _> = d
            .quads()
            .filter_map_quads(|q| {
                (!Term::eq(q.p(), ez_term(":e"))).then_some(q.s().iri().unwrap().to_string())
            })
            .into_iter()
            .collect();
        assert_eq!(h.unwrap(), vec!["tag:a".to_string(), "tag:g".to_string(),])
    }
}
