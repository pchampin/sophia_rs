// this module is transparently re-exported by its parent `dataset`
use crate::dataset::_traits::*;
use crate::quad::*;
use crate::quad::stream::*;

/// The [`QuadSink`] returned by [`MutableDataset::inserter`].
/// 
/// [`QuadSink`]: ../quad/stream/trait.QuadSink.html
/// [`MutableDataset::inserter`]: trait.MutableDataset.html#method.inserter
///
pub struct Inserter<'a, D: ?Sized + 'a> {
    dataset: &'a mut D,
    count: usize,
}

impl<'a, D: MutableDataset + ?Sized + 'a> Inserter<'a, D> {
    pub fn new(dataset: &'a mut D) -> Self {
        Inserter { dataset, count: 0 }
    }
}

impl<'a, D: MutableDataset + ?Sized + 'a> QuadSink for Inserter<'a, D> {
    type Outcome = usize;
    type Error = <D as MutableDataset>::MutationError;

    fn feed<'b, Q: Quad<'b>>(&mut self, q: &Q) -> Result<(), Self::Error> {
        self.dataset.insert(q.s(), q.p(), q.o(), q.g()).map(|inserted| {
            if inserted {
                self.count += 1;
            }
        })
    }
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error> {
        Ok(self.count)
    }

}


/// The [`QuadSink`] returned by [`MutableDataset::remover`].
/// 
/// [`QuadSink`]: ../quad/stream/trait.QuadSink.html
/// [`MutableDataset::remover`]: trait.MutableDataset.html#method.remover
///
pub struct Remover<'a, D: ?Sized + 'a> {
    dataset: &'a mut D,
    count: usize,
}

impl<'a, D: MutableDataset + ?Sized + 'a> Remover<'a, D> {
    pub fn new(dataset: &'a mut D) -> Self {
        Remover { dataset, count: 0 }
    }
}

impl<'a, D: MutableDataset + ?Sized + 'a> QuadSink for Remover<'a, D> {
    type Outcome = usize;
    type Error = <D as MutableDataset>::MutationError;

    fn feed<'b, Q: Quad<'b>>(&mut self, q: &Q) -> Result<(), Self::Error> {
        self.dataset.remove(q.s(), q.p(), q.o(), q.g()).map(|removed| {
            if removed {
                self.count += 1;
            }
        })
    }
    fn finish(&mut self) -> Result<Self::Outcome, Self::Error> {
        Ok(self.count)
    }
}



#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the macro test_dataset_impl!).
}