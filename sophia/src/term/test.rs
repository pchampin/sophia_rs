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

#[test]
fn iriterm_normalized_no_suffix() {
    let norm = Normalization::NoSuffix;
    let i1 = IriTerm::new("http://champin.net/#", Some("pa")).unwrap();
    let i2 = IriTerm::normalized_with(&i1, |txt| String::from(txt), norm);
    assert_eq!(i1, i2);
    assert!(i2.suffix.is_none());
}

#[test]
fn iriterm_normalized_last_hash_or_slash() {
    let norm = Normalization::LastHashOrSlash;
    for (ns1, sf1, ns2, sf2) in &[
        ("http://champin.net/#pa", "",       "http://champin.net/#", "pa"),
        ("http://champin.net/#", "pa",       "http://champin.net/#", "pa"),
        ("http://champin.net/", "#pa",       "http://champin.net/#", "pa"),
        ("http://champin.net/", "foo/bar",   "http://champin.net/foo/", "bar"),
        ("tag:foo", "",                      "tag:foo", ""),
        ("tag:", "foo",                      "tag:foo", ""),
    ] {
        println!("{} {} -> {} {}", ns1, sf1, ns2, sf2);
        let sf1 = if sf1.len() == 0 { None } else { Some(*sf1) };
        let sf2 = if sf2.len() == 0 { None } else { Some(String::from(*sf2)) };

        let i1 = IriTerm::new(*ns1, sf1).unwrap();
        let i2 = IriTerm::normalized_with(&i1, |txt| String::from(txt), norm);
        assert_eq!(i1, i2);
        assert_eq!(&i2.ns[..], *ns2);
        assert_eq!(i2.suffix, sf2);
    }
}

#[test]
fn bnode_id_deref() {
    let b1 = BoxTerm::new_bnode("foo").unwrap();
    if let BNode(id1) = b1 {
        assert!(id1.starts_with("fo"));
    } else {
        panic!("b1 should be a BNode");
    }
}

#[test]
fn bnode_id_eq_str() {
    let b1 = BoxTerm::new_bnode("foo").unwrap();
    if let BNode(id1) = b1 {
        assert_eq!(id1, "foo");
        assert!(id1 == "foo");
    } else {
        panic!("b1 should be a BNode");
    }
}
