use std::collections::BTreeSet;

pub trait RangeN {
    fn range1(&self, k1: usize) -> std::collections::btree_set::Range<'_, [usize; 3]>;
    fn range2(&self, k1: usize, k2: usize) -> std::collections::btree_set::Range<'_, [usize; 3]>;
    fn range3(
        &self,
        k1: usize,
        k2: usize,
        k3: usize,
    ) -> std::collections::btree_set::Range<'_, [usize; 3]>;
}

impl RangeN for BTreeSet<[usize; 3]> {
    fn range1(&self, k1: usize) -> std::collections::btree_set::Range<'_, [usize; 3]> {
        self.range([k1, 0, 0]..=[k1, usize::MAX, usize::MAX])
    }

    fn range2(&self, k1: usize, k2: usize) -> std::collections::btree_set::Range<'_, [usize; 3]> {
        self.range([k1, k2, 0]..=[k1, k2, usize::MAX])
    }

    fn range3(
        &self,
        k1: usize,
        k2: usize,
        k3: usize,
    ) -> std::collections::btree_set::Range<'_, [usize; 3]> {
        self.range([k1, k2, k3]..=[k1, k2, k3])
    }
}
