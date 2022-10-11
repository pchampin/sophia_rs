use std::mem::transmute;
use std::{
    cell::RefCell,
    collections::{btree_map::Entry, BTreeMap},
    marker::PhantomData,
};

pub struct BNodeGen<'a> {
    map: BTreeMap<&'a str, Box<str>>,
    fresh: Vec<Box<str>>,
    _phantom: PhantomData<RefCell<()>>, // make this type !Sync
}
impl<'a> BNodeGen<'a> {
    pub fn new() -> Self {
        BNodeGen {
            map: BTreeMap::new(),
            fresh: vec![],
            _phantom: PhantomData,
        }
    }

    pub fn fresh(&self) -> &str {
        // safety: the following is OK because
        // BNodeGen is !Sync,
        // so no other task is using self.map nor self.fresh at the same time
        // (and we do not "leak mutability" outside of this method)
        let (map, fresh) = unsafe { self.inner_mut() };
        let n = map.len() + fresh.len() + 1;
        fresh.push(format!("_:b{}", n).into());
        let ret: *const str = fresh[fresh.len() - 1].as_ref();
        // safety: the following is OK because
        // that str is boxed in a fixed location in memory,
        // for as long as self lives.
        unsafe { &*ret }
    }

    pub fn get(&self, key: &str) -> &str {
        // safety: the following is OK because
        // BNodeGen is !Sync,
        // so no other task is using self.map nor self.fresh at the same time
        // (and we do not "leak mutability" outside of this method)
        let (map, fresh) = unsafe { self.inner_mut() };
        let n = map.len() + fresh.len() + 1;
        match map.entry(key) {
            Entry::Vacant(e) => {
                let val = format!("_:b{}", n).into();
                let val: *const str = e.insert(val).as_ref();
                // safety: see below
                unsafe { &*val }
            }
            Entry::Occupied(e) => {
                let val: *const str = e.get().as_ref();
                // safety: see below
                unsafe { &*val }
            }
        }
        // safety: the two pointer deference above are OK because
        // that str is boxed in a fixed location in memory,
        // for as long as self lives.
    }

    unsafe fn inner_mut(&self) -> (&mut BTreeMap<&'a str, Box<str>>, &mut Vec<Box<str>>) {
        let map: *mut BTreeMap<&'a str, Box<str>> = transmute(&self.map);
        let fresh: *mut Vec<Box<str>> = transmute(&self.fresh);
        (&mut *map, &mut *fresh)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn bnode_gen_get() {
        let b = BNodeGen::new();
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:b"), "_:b2");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:b"), "_:b2");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:d"), "_:b4");
        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:b"), "_:b2");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:b1"), "_:b5");
        assert_eq!(b.get("_:d"), "_:b4");
        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:b"), "_:b2");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:b5"), "_:b6");
        assert_eq!(b.get("_:b1"), "_:b5");
        assert_eq!(b.get("_:d"), "_:b4");
        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:b"), "_:b2");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:aa"), "_:b7");
        assert_eq!(b.get("_:b5"), "_:b6");
        assert_eq!(b.get("_:b1"), "_:b5");
        assert_eq!(b.get("_:d"), "_:b4");
        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:b"), "_:b2");
        assert_eq!(b.get("_:a"), "_:b1");
    }

    #[test]
    fn bnode_gen_fresh() {
        let b = BNodeGen::new();
        assert_eq!(b.fresh(), "_:b1");
        assert_eq!(b.fresh(), "_:b2");
        assert_eq!(b.fresh(), "_:b3");
    }

    #[test]
    fn bnode_gen_mix() {
        let b = BNodeGen::new();
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.fresh(), "_:b2");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.fresh(), "_:b4");
        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:b1"), "_:b5");
        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:b5"), "_:b6");
        assert_eq!(b.get("_:b1"), "_:b5");
        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:a"), "_:b1");

        assert_eq!(b.get("_:aa"), "_:b7");
        assert_eq!(b.get("_:b5"), "_:b6");
        assert_eq!(b.get("_:b1"), "_:b5");
        assert_eq!(b.get("_:c"), "_:b3");
        assert_eq!(b.get("_:a"), "_:b1");
    }

    #[test]
    fn bnode_gen_many() {
        let b = BNodeGen::new();
        let bnodes: Vec<_> = (0..10_000).map(|i| format!("_:x{}", i)).collect();
        let exp: Vec<_> = bnodes.iter().map(|x| (x, b.get(x))).collect();
        for (x, y) in &exp {
            assert_eq!(b.get(x), *y);
        }
    }
}
