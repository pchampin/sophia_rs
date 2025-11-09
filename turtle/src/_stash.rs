//! A stash is a vec of strings that does not drop its elements when they are popped,
//! in order to save allocation time when the element is reused.

use std::ops::Deref;
#[derive(Clone, Debug, Default)]
pub struct StringStash {
    vec: Vec<String>,
    len: usize,
}

impl StringStash {
    #[must_use]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            vec: Vec::with_capacity(cap),
            len: 0,
        }
    }

    pub fn push(&mut self) -> &mut String {
        self.len += 1;
        if self.len > self.vec.len() {
            debug_assert_eq!(self.len, self.vec.len() + 1);
            self.vec.push(String::with_capacity(256));
        } else {
            self.vec[self.len - 1].truncate(0);
        }
        &mut self.vec[self.len - 1]
    }

    pub fn top(&self) -> &str {
        debug_assert!(!self.is_empty());
        &self.vec[self.len - 1]
    }

    pub fn top_mut(&mut self) -> &mut String {
        debug_assert!(!self.is_empty());
        &mut self.vec[self.len - 1]
    }

    pub fn top2_mut(&mut self) -> [&mut String; 2] {
        debug_assert!(self.len() >= 2);
        let n = self.len() - 1;
        let (s1, s2) = self.vec.split_at_mut(n);
        [&mut s1[n - 1], &mut s2[0]]
    }

    pub fn pop(&mut self) {
        assert!(!self.is_empty());
        self.len -= 1;
    }

    pub fn empty(&mut self) {
        self.len = 0;
    }

    pub fn swap_top2(&mut self) {
        assert!(self.len() >= 2);
        self.vec.swap(self.len - 2, self.len - 1);
    }

    pub fn recycle(&mut self, old_idx: usize) -> usize {
        assert!(old_idx <= self.vec.len());
        debug_assert!(self.len() <= old_idx);
        let ret = self.len();
        self.vec.swap(ret, old_idx);
        self.len += 1;
        ret
    }
}

impl Deref for StringStash {
    type Target = [String];

    fn deref(&self) -> &Self::Target {
        &self.vec[..self.len]
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn stash() {
        let mut s = StringStash::default();
        assert_eq!(s.len(), 0);
        assert_eq!(s.vec.len(), 0);

        s.push().push_str("foo");
        assert_eq!(s.len(), 1);
        assert_eq!(s.top(), "foo");
        assert_eq!(s.vec.len(), 1);
        assert_eq!(&s.vec[0], "foo");

        s.push().push_str("bar");
        assert_eq!(s.len(), 2);
        assert_eq!(s.top(), "bar");
        assert_eq!(s.vec.len(), 2);
        assert_eq!(&s.vec[0], "foo");
        assert_eq!(&s.vec[1], "bar");

        s.pop();
        assert_eq!(s.len(), 1);
        assert_eq!(s.top(), "foo");
        assert_eq!(s.vec.len(), 2);
        assert_eq!(&s.vec[0], "foo");
        assert_eq!(&s.vec[1], "bar");

        s.push().push_str("toto");
        assert_eq!(s.len(), 2);
        assert_eq!(s.top(), "toto");
        assert_eq!(s.top2_mut()[0], "foo");
        assert_eq!(s.top2_mut()[1], "toto");
        assert_eq!(s.vec.len(), 2);
        assert_eq!(&s.vec[0], "foo");
        assert_eq!(&s.vec[1], "toto");

        s.swap_top2();
        assert_eq!(s.vec.len(), 2);
        assert_eq!(s.top(), "foo");
        assert_eq!(&s.vec[0], "toto");
        assert_eq!(&s.vec[1], "foo");

        s.empty();
        assert_eq!(s.len(), 0);
        assert_eq!(s.vec.len(), 2);
        assert_eq!(&s.vec[0], "toto");
        assert_eq!(&s.vec[1], "foo");

        s.recycle(1);
        assert_eq!(s.len(), 1);
        assert_eq!(s.top(), "foo");
        assert_eq!(s.vec.len(), 2);
        assert_eq!(&s.vec[0], "foo");
        assert_eq!(&s.vec[1], "toto");
    }
}
