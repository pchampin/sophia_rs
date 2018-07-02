use super::*;

#[test]
fn iri() {
    let exp = "http://champin.net/";
    let i = RefTerm::new_iri("http://champin.net/");
    if let Ok(Iri(iri)) = i {
        assert_eq!(iri, exp);
        assert_eq!(iri.len(), exp.len());
        let s1 = iri.to_string();
        let s2: String = iri.chars().collect();
        assert_eq!(s1, exp);
        assert_eq!(s2, exp);
    } else {
        assert!(false, "Should have returned Ok(Iri(_))");
    }
    let i = RefTerm::new_iri("1://champin.net/");
    assert!(i.is_err());
}

#[test]
fn iri2() {
    let exp = "http://champin.net/#pa";
    let i = RefTerm::new_iri2("http://champin.net/#", "pa");
    if let Ok(Iri(iri)) = i {
        assert_eq!(iri, exp);
        assert_eq!(iri.len(), exp.len());
        let s1 = iri.to_string();
        let s2: String = iri.chars().collect();
        assert_eq!(s1, exp);
        assert_eq!(s2, exp);
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
fn iri_normalized_no_suffix() {
    let norm = Normalization::NoSuffix;
    let i1 = IriData::new("http://champin.net/#", Some("pa")).unwrap();
    let i2 = IriData::normalized_with(&i1, |txt| String::from(txt), norm);
    assert_eq!(i1, i2);
    assert!(i2.suffix.is_none());
}

#[test]
fn iri_normalized_last_hash_or_slash() {
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

        let i1 = IriData::new(*ns1, sf1).unwrap();
        let i2 = IriData::normalized_with(&i1, |txt| String::from(txt), norm);
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

#[test]
fn bnode_is_n3() {
    let pos = POSITIVE_1CHAR_IDS.iter().chain(POSITIVE_N3_BNODE_IDS.iter());
    for id in pos {
        let b = BoxTerm::new_bnode(*id).unwrap();
        if let BNode(bid) = b {
            assert!(bid.is_n3())
        } else {
            unreachable!()
        }
    }

    let neg = NEGATIVE_1CHAR_IDS.iter()
        .chain(NEGATIVE_N3_BNODE_IDS.iter())
        .chain(NT_ONLY_BNODE_IDS.iter());
    for id in neg {
        let b = BoxTerm::new_bnode(*id).unwrap();
        if let BNode(bid) = b {
            assert!(!bid.is_n3())
        } else {
            unreachable!()
        }
    }

    for id in [
        "a ", // spurious space
        "a.", // incomplete
    ].iter() {
        let b = BoxTerm::new_bnode(*id).unwrap();
        if let BNode(bid) = b {
            assert!(!bid.is_n3())
        } else {
            unreachable!()
        }
    }
}

#[test]
fn variable() {
    let pos = POSITIVE_1CHAR_IDS.iter().chain(POSITIVE_VARIABLES.iter());
    for id in pos {
        let b = BoxTerm::new_variable(*id);
        assert!(b.is_ok(), format!("{:?} should be accepted as a variable name", *id));
    }

    let neg = NEGATIVE_1CHAR_IDS.iter()
        .chain(NEGATIVE_VARIABLES.iter());
    for id in neg {
        let b = BoxTerm::new_variable(*id);
        assert!(b.is_err(), format!("{:?} should be refused as a variable name", *id));
    }
}

pub(crate) const POSITIVE_1CHAR_IDS:&[&str] = &[
    // PN_CHARS_BASE
    "a", "z", "A", "Z",
    "\u{00C0}", "\u{00D6}", "\u{00D8}", "\u{00F6}", "\u{00F8}", "\u{02FF}", "\u{0370}",
    "\u{037D}", "\u{037F}", "\u{1FFF}", "\u{200C}", "\u{200D}", "\u{2070}", "\u{218F}",
    "\u{2C00}", "\u{2FEF}", "\u{3001}", "\u{D7FF}", "\u{F900}", "\u{FDCF}", "\u{FDF0}",
    "\u{FFFD}", "\u{10000}", "\u{EFFFF}",
    // PN_CHARS_U
    "_",
    // other authorized leading character
    "0", "9",
];

pub(crate) const NEGATIVE_1CHAR_IDS:&[&str] = &[
    " ", "\t", "\n", "\r",
    ".", "&", "!", "@", ":",
    "\u{00BF}", "\u{00D7}", "\u{00F7}", "\u{0300}",
    "\u{036F}", "\u{037E}", "\u{2000}", "\u{200B}", "\u{200E}", "\u{206F}", "\u{2190}",
    "\u{2BFF}", "\u{2FF0}", "\u{3000}", "\u{E000}", "\u{F8FF}", "\u{FDD0}", "\u{FDEF}",
    "\u{FFFE}", "\u{FFFF}", "\u{F0000}",
    // PN_CHARS - PN_CHARS_BASE
    "-",  "\u{00B7}", "\u{0300}", "\u{036F}", "\u{203F}", "\u{2040}",
];

pub(crate) const POSITIVE_N3_BNODE_IDS:&[&str] = &[
    "abcdef", "_abcde", "0abcde",
    // trailing PN_CHARS - PN_CHARS_BASE
    "a-", "a\u{00B7}", "a\u{0300}", "a\u{036F}", "a\u{203F}", "a\u{2040}",
];

pub(crate) const NEGATIVE_N3_BNODE_IDS:&[&str] = &[
    "a b", "a\tb", "a\nb", "a\rb", "a&b", "a!c", "a@b",
];

pub(crate) const NT_ONLY_BNODE_IDS:&[&str] = &[
    ":", "a:b", ":_::__",
];

pub(crate) const POSITIVE_VARIABLES:&[&str] = &[
    "a\u{00B7}",
    "a\u{0300}", "a\u{036F}", "a\u{203F}", "a\u{2040}",
];

pub(crate) const NEGATIVE_VARIABLES:&[&str] = &[
    "a ",
    "a\u{00B6}", "a\u{00B8}",
    //"a\u{02FF}", "a\u{0370}", // accepted, as they are part of pn_chars_u
    "a\u{203E}", "a\u{2041}",
];