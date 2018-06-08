extern crate myrdf2;

use myrdf2::term::*;

fn main() {
    println!("Hello, world!");

    let me = RefTerm::new_iri2("http://champin.net/#", "pa").unwrap();
    let me2 = BoxTerm::new_iri2("http://champin.net/#", "pa").unwrap();
    let me3 = RcTerm::copy(&me2);
    let me4 = me3.clone();
    let me5: RefTerm<'static> = me.clone();
    let me6: RefTerm = StaticTerm::copy(&me5);
    // warning, this does *not* produce a StaticTerm, but a RefTerm with a shorter lifetime
    assert_eq!(me, me2);
    assert_eq!(me2, me3);
    assert_eq!(me3, me4);
    assert_eq!(me4, me5);
    assert_eq!(me5, me6);
}
