pub trait UsizeIteratorDedup: Iterator<Item = usize> + Sized {
    fn dedup(self) -> Dedup<Self> {
        Dedup {
            inner: self,
            last: None,
        }
    }
}
impl<I: Iterator<Item = usize>> UsizeIteratorDedup for I {}

pub struct Dedup<I> {
    inner: I,
    last: Option<usize>,
}

impl<I: Iterator<Item = usize>> Iterator for Dedup<I> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let cand = self.inner.next();
        if cand.is_none() || cand != self.last {
            self.last = cand;
            cand
        } else {
            self.next()
        }
    }
}
