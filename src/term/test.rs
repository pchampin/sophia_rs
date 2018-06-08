use super::*;

#[test]
fn iri() {
    let i = RefTerm::new_iri("http://champin.net/");
    if let Ok(Iri(iri)) = i {
        assert_eq!(iri, "http://champin.net/");
    } else {
        assert!(false, "Should have returned Ok(Iri(_))");
    }
    let i = RefTerm::new_iri("1://champin.net/");
    assert!(i.is_err());
}

#[test]
fn iri2() {
    let i = RefTerm::new_iri2("http://champin.net/#", "pa");
    if let Ok(Iri(iri)) = i {
        assert_eq!(iri, "http://champin.net/#pa");
    } else {
        assert!(false, "Should have returned Ok(Iri(_))");
    }
    let i = RefTerm::new_iri2("1://champin.net/", "pa");
    assert!(i.is_err());

}

#[test]
fn iri_eq_different_holders() {
    let i1 = BoxTerm::new_iri("http://champin.net/").unwrap();
    let i2 = RcTerm::new_iri("http://champin.net/").unwrap();
    let i3 = ArcTerm::new_iri("http://champin.net/").unwrap();
    let i4 = RefTerm::new_iri("http://champin.net/").unwrap();
    assert_eq!(i1, i2);
    assert_eq!(i1, i3);
    assert_eq!(i1, i4);
    assert_eq!(i2, i3);
    assert_eq!(i2, i4);
}
