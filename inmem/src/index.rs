//! A [`TermIndex`] is a bidirectional assocuation of [terms](Term) with short numeric [indices](Index).
use sophia_api::term::{FromTerm, GraphName, SimpleTerm, Term};
use sophia_api::Error;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Abstraction of the short numeric indices representing [terms](Term) in a [`TermIndex`].
pub trait Index: Copy + std::fmt::Debug + Ord {
    /// The minimal value of this index type
    const ZERO: Self;
    /// The maximal value of this index type
    const MAX: Self;
    /// Convert from usize
    fn from_usize(other: usize) -> Self;
    /// Convert to usize
    fn into_usize(self) -> usize;
}

impl Index for usize {
    const ZERO: Self = 0;
    const MAX: Self = usize::MAX;
    fn from_usize(other: usize) -> Self {
        other
    }
    fn into_usize(self) -> usize {
        self
    }
}

impl Index for u32 {
    const ZERO: Self = 0;
    const MAX: Self = u32::MAX;
    fn from_usize(other: usize) -> Self {
        other
            .try_into()
            .map_err(|_| ())
            .expect("usize too big to be converted to u32")
    }
    fn into_usize(self) -> usize {
        self as usize
    }
}

impl Index for u16 {
    const ZERO: Self = 0;
    const MAX: Self = u16::MAX;
    fn from_usize(other: usize) -> Self {
        other
            .try_into()
            .map_err(|_| ())
            .expect("usize too big to be converted to u16")
    }
    fn into_usize(self) -> usize {
        self as usize
    }
}

//

/// A [`TermIndex`] is a bidirectional association of [terms](Term) with short numeric [indices](Index).
pub trait TermIndex {
    /// The type of [`Term`]s contained in this [`TermIndex`]
    type Term: Term;
    /// The type of [indices](Index) used by this [`TermIndex`]
    type Index: Index;
    /// The type of error that this [`TermIndex`] may raise
    type Error: Error + 'static;

    /// Get the index corresponding to term `t`, if it exists.
    ///
    /// Return `None` if this term-index does not contain a term equivalent to `t`.
    fn get_index<T: Term>(&self, t: T) -> Option<Self::Index>;
    /// Get the index corresponding to term `t`, adding it in the term-index if necessary.
    ///
    /// Returns an error if `t` can not be added in this term-index
    /// (some term-index may support only a subset of RDF terms).
    fn ensure_index<T: Term>(&mut self, t: T) -> Result<Self::Index, Self::Error>;
    /// Get the term corresponding to index `i`.
    ///
    /// # Precondition
    /// `i` must have been returned previously by [`get_index`](TermIndex::get_index) or [`ensure_index`](TermIndex::ensure_index),
    /// otherwise this method may panic.
    fn get_term(&self, i: Self::Index) -> <Self::Term as Term>::BorrowTerm<'_>;
}

/// [`GraphNameIndex`] extends [`TermIndex`] to support graph names.
///
/// This implies that one index value is reserved for the default graph.
pub trait GraphNameIndex: TermIndex {
    /// Get the index corresponding to the default Graph.
    ///
    /// This value is never returned by `get_index` or `ensure_index`.
    fn get_default_graph_index(&self) -> Self::Index;
    /// Get the graph name corresponding to index `i`.
    ///
    /// # Precondition
    /// `i` must have been returned previously by [`get_index`](TermIndex::get_index) or [`ensure_index`](TermIndex::ensure_index),
    /// otherwise this method may panic.
    fn get_graph_name(&self, i: Self::Index) -> GraphName<<Self::Term as Term>::BorrowTerm<'_>> {
        if i == self.get_default_graph_index() {
            None
        } else {
            Some(self.get_term(i))
        }
    }
    /// Get the index corresponding to graph name `n`, if it exists.
    fn get_graph_name_index<T: Term>(&self, g: GraphName<T>) -> Option<Self::Index> {
        match g {
            None => Some(self.get_default_graph_index()),
            Some(t) => self.get_index(t),
        }
    }
}

//

/// A generic implementation of [`TermIndex`].
#[derive(Clone, Debug, Default)]
pub struct SimpleTermIndex<I: Index> {
    t2i: HashMap<SimpleTerm<'static>, I>,
    i2t: Vec<SimpleTerm<'static>>,
}

impl<I: Index> SimpleTermIndex<I> {
    /// Construct an empty index
    pub fn new() -> Self {
        SimpleTermIndex {
            t2i: HashMap::new(),
            i2t: vec![],
        }
    }

    /// The number of terms in this index
    pub fn len(&self) -> usize {
        self.i2t.len()
    }

    /// Whether this index is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<I: Index> TermIndex for SimpleTermIndex<I> {
    type Term = SimpleTerm<'static>;
    type Index = I;
    type Error = TermIndexFullError;

    fn get_index<T: Term>(&self, t: T) -> Option<Self::Index> {
        self.t2i.get(&t.as_simple()).copied()
    }

    fn ensure_index<T: Term>(&mut self, t: T) -> Result<Self::Index, Self::Error> {
        let t = SimpleTerm::from_term(t);
        match self.t2i.entry(t) {
            Entry::Vacant(e) => {
                let i = I::from_usize(self.i2t.len());
                if i >= I::MAX {
                    return Err(TermIndexFullError());
                }
                let t2 = e.key().as_simple();
                // the following is safe,
                // because t2 borrows data from the key in self.t2i,
                // which will live as long as self, and will not be moved (Box<str>).
                let t2: SimpleTerm<'static> = unsafe { std::mem::transmute(t2) };
                self.i2t.push(t2);
                e.insert(i);
                Ok(i)
            }
            Entry::Occupied(e) => Ok(*e.get()),
        }
    }

    fn get_term(&self, i: Self::Index) -> <Self::Term as Term>::BorrowTerm<'_> {
        let i = i.into_usize();
        self.i2t[i].borrow_term()
    }
}

impl<I: Index> GraphNameIndex for SimpleTermIndex<I> {
    fn get_default_graph_index(&self) -> Self::Index {
        Self::Index::MAX
    }
}

/// An error type to indicate that a [`SimpleTermIndex`] is full
#[derive(thiserror::Error, Copy, Clone, Debug)]
#[error("This TermIndex can not contain more terms")]
pub struct TermIndexFullError();

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::ns::Namespace;
    use sophia_api::term::BnodeId;

    #[test]
    fn simple_term_index() -> Result<(), Box<dyn sophia_api::Error>> {
        let ex = Namespace::new_unchecked("https://example.com/ns/");
        let exa = ex.get("a")?;
        let exb = ex.get("b")?;
        let bn1 = BnodeId::new_unchecked("bn1");

        let mut sti = SimpleTermIndex::<u32>::new();
        assert!(sti.is_empty());
        assert_eq!(sti.len(), 0);
        assert_eq!(sti.get_default_graph_index(), u32::MAX);

        assert_eq!(sti.get_index(exa), None);
        assert_eq!(sti.get_index(exb), None);
        assert_eq!(sti.get_index(bn1), None);
        assert_eq!(sti.get_index("hello world"), None);
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index(exa)?, 0);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 1);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), None);
        assert_eq!(sti.get_index(bn1), None);
        assert_eq!(sti.get_index("hello world"), None);
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index(exb)?, 1);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 2);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), Some(1));
        assert_eq!(sti.get_index(bn1), None);
        assert_eq!(sti.get_index("hello world"), None);
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index(bn1)?, 2);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 3);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), Some(1));
        assert_eq!(sti.get_index(bn1), Some(2));
        assert_eq!(sti.get_index("hello world"), None);
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index("hello world")?, 3);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 4);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), Some(1));
        assert_eq!(sti.get_index(bn1), Some(2));
        assert_eq!(sti.get_index("hello world"), Some(3));
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index(42)?, 4);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 5);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), Some(1));
        assert_eq!(sti.get_index(bn1), Some(2));
        assert_eq!(sti.get_index("hello world"), Some(3));
        assert_eq!(sti.get_index(42), Some(4));

        assert_eq!(sti.ensure_index(exa)?, 0);
        assert_eq!(sti.ensure_index(exb)?, 1);
        assert_eq!(sti.ensure_index(bn1)?, 2);
        assert_eq!(sti.ensure_index("hello world")?, 3);
        assert_eq!(sti.ensure_index(42)?, 4);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 5);

        assert!(Term::eq(sti.get_term(0), exa));
        assert!(Term::eq(sti.get_term(1), exb));
        assert!(Term::eq(sti.get_term(2), bn1));
        assert!(Term::eq(sti.get_term(3), "hello world"));
        assert!(Term::eq(sti.get_term(4), 42));

        Ok(())
    }

    #[cfg(feature = "all_tests")]
    #[test]
    fn big_simple_term_index() {
        const MAX: i32 = 20_000;
        let mut sti = SimpleTermIndex::<u32>::new();
        for i in 0..MAX {
            assert_eq!(sti.ensure_index(i).unwrap(), i as u32);
        }
        assert_eq!(sti.len(), MAX as usize);
        for i in 0..MAX {
            assert_eq!(sti.ensure_index(i).unwrap(), i as u32);
        }
        assert_eq!(sti.len(), MAX as usize);
        for i in 0..MAX {
            assert!(Term::eq(sti.get_term(i as u32), i));
        }
    }

    impl Index for i8 {
        const ZERO: Self = 0;
        const MAX: Self = i8::MAX;
        fn from_usize(other: usize) -> Self {
            other
                .try_into()
                .map_err(|_| ())
                .expect("usize too big to be converted to i8")
        }
        fn into_usize(self) -> usize {
            self as usize
        }
    }

    #[test]
    fn full_simple_term_index() {
        let mut sti = SimpleTermIndex::<i8>::new();
        for i in 0..127 {
            assert_eq!(sti.ensure_index(i).unwrap(), i as i8);
        }
        assert!(sti.ensure_index(127).is_err());
    }
}
