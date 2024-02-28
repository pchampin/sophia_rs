//! I define implementations of [`sophia_api::term::Term`]:
//! * [`ArcTerm`] using [`Arc<str>`](std::sync::Arc) as the underlying text,
//!   making it cheap to clone and thread-safe;
//!   see also [`ArcStrStash`].
//! * [`RcTerm`] using [`Rc<str>`](std::rc::Rc) as the underlying text,
//!   making it cheap to clone;
//!   see also [`RcStrStash`].
#![deny(missing_docs)]

mod _generic;
pub use _generic::*;
#[macro_use]
mod _macro;

gen_term!(ArcTerm, std::sync::Arc, arc_term);
gen_term!(RcTerm, std::rc::Rc, rc_term);

gen_stash!(ArcStrStash, ArcTerm, std::sync::Arc, arc_stash);
gen_stash!(RcStrStash, RcTerm, std::rc::Rc, rc_stash);

#[cfg(test)]
mod test {
    use sophia_api::term::{BnodeId, FromTerm, SimpleTerm, Term, VarName};

    use super::*;

    #[test]
    #[allow(clippy::needless_borrow, unused_assignments)]
    fn arc_str_stash_iri() {
        let mut stash = ArcStrStash::new();
        assert_eq!(0, stash.len());
        let mut old_len = stash.len();

        let t1a = sophia_api::ns::xsd::integer;
        let t1b = stash.copy_term(t1a);
        assert!(Term::eq(&t1a, &t1b));
        assert_eq!(old_len + 1, stash.len());
        old_len = stash.len();

        let t2a = 42;
        let t2b = stash.copy_term(t2a);
        assert!(Term::eq(&t2a, &t2b));
        assert_eq!(old_len + 1, stash.len()); // datatype was already there
        old_len = stash.len();

        let t3a = "foo";
        let t3b = stash.copy_term(t3a);
        assert!(Term::eq(&t3a, &t3b));
        assert_eq!(old_len + 2, stash.len()); // lex + datatype where added
        old_len = stash.len();

        let t4a = "42";
        let t4b = stash.copy_term(t4a);
        assert!(Term::eq(&t4a, &t4b));
        assert_eq!(old_len, stash.len()); // all values where alreadt there
        old_len = stash.len();

        let t5a = BnodeId::new_unchecked("foo");
        let t5b = stash.copy_term(t5a);
        assert!(Term::eq(&t5a, &t5b));
        assert_eq!(old_len, stash.len()); // all values where alreadt there
        old_len = stash.len();

        let t6a = VarName::new_unchecked("foobar");
        let t6b = stash.copy_term(t6a);
        assert!(Term::eq(&t6a, &t6b));
        assert_eq!(old_len + 1, stash.len()); // all values where alreadt there
        old_len = stash.len();

        let t1c = stash.copy_term(&t1b);
        assert!(Term::eq(&t1a, &t1c));
        assert_eq!(old_len, stash.len()); // all values where alreadt there
        old_len = stash.len();

        let t7a: SimpleTerm<'static> =
            SimpleTerm::Triple(Box::new(["s", "p", "o"].map(SimpleTerm::from_term)));
        let t7b = stash.copy_term(&t7a);
        assert!(Term::eq(&t7a, &t7b));
        assert_eq!(old_len + 3, stash.len()); // all values where alreadt there
        old_len = stash.len();
    }
}
