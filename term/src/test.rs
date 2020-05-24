use super::Term::*;
use super::*;
use sophia_api::ns::xsd;
use sophia_api::term::CopiableTerm;

fn h<H: std::hash::Hash>(x: &H) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    x.hash(&mut hasher);
    hasher.finish()
}

#[test]
fn iri() {
    let exp = "http://champin.net/";
    let i = RefTerm::new_iri("http://champin.net/").unwrap();
    assert_eq!(i.value(), exp);
    assert_eq!(format!("{}", i), format!("<{}>", exp));

    if let Iri(iri) = i {
        assert_eq!(iri.len(), exp.len());
        let s1 = iri.value();
        let s2: String = iri.chars().collect();
        assert_eq!(s1, exp);
        assert_eq!(s2, exp);
    } else {
        assert!(false, "Should have returned Iri(_)");
    }
    let res = RefTerm::new_iri("1://champin.net/");
    assert!(res.is_err());
}

#[test]
fn iri2() {
    let exp = "http://champin.net/#pa";
    let i = RefTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap();
    assert_eq!(i.value(), exp);
    assert_eq!(format!("{}", i), format!("<{}>", exp));

    if let Iri(iri) = i {
        assert_eq!(iri.len(), exp.len());
        let s1 = iri.value();
        let s2: String = iri.chars().collect();
        assert_eq!(s1, exp);
        assert_eq!(s2, exp);
    } else {
        assert!(false, "Should have returned Iri(_)");
    }
    let res = RefTerm::new_iri_suffixed("1://champin.net/", "pa");
    assert!(res.is_err());
}

#[test]
fn iri_eq_different_term_data() {
    let i1 = BoxTerm::new_iri("http://champin.net/").unwrap();
    let i2 = RcTerm::new_iri("http://champin.net/").unwrap();
    let i3 = ArcTerm::new_iri("http://champin.net/").unwrap();
    let i4 = RefTerm::new_iri("http://champin.net/").unwrap();
    assert_eq!(i1, i2);
    assert_eq!(h(&i1), h(&i2));
    assert_eq!(i1, i3);
    assert_eq!(h(&i1), h(&i3));
    assert_eq!(i1, i4);
    assert_eq!(h(&i1), h(&i4));
    assert_eq!(i2, i3);
    assert_eq!(h(&i2), h(&i3));
    assert_eq!(i2, i4);
    assert_eq!(h(&i2), h(&i4));
    assert_eq!(i3, i4);
    assert_eq!(h(&i3), h(&i4));
}

#[test]
fn iri_eq_different_cut() {
    let i1 = BoxTerm::new_iri("http://champin.net/#pa").unwrap();
    let i2 = BoxTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap();
    let i3 = BoxTerm::new_iri_suffixed("http://champin.net/", "#pa").unwrap();
    let i4 = BoxTerm::new_iri_suffixed("http://champin.", "net/#pa").unwrap();
    assert_eq!(i1, i2);
    assert_eq!(h(&i1), h(&i2));
    assert_eq!(i1, i3);
    assert_eq!(h(&i1), h(&i3));
    assert_eq!(i1, i4);
    assert_eq!(h(&i1), h(&i4));
    assert_eq!(i2, i3);
    assert_eq!(h(&i2), h(&i3));
    assert_eq!(i2, i4);
    assert_eq!(h(&i2), h(&i4));
    assert_eq!(i3, i4);
    assert_eq!(h(&i3), h(&i4));
}

#[test]
fn iri_similar_but_not_eq() {
    let i1 = BoxTerm::new_iri("http://champin.net/#pa").unwrap();
    let i2 = BoxTerm::new_iri("http://champin.net/#p").unwrap();
    assert_ne!(i1, i2);
    assert_ne!(h(&i1), h(&i2));
}

#[test]
fn iri_normalized_no_suffix() {
    let norm = Normalization::NoSuffix;
    let i1 = BoxTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap();
    let i2 = i1.normalized(norm);
    assert_eq!(i1, i2);
    if let Iri(i2) = i2 {
        assert!(i2.suffix.is_none());
    }
}

#[test]
fn iri_normalized_last_gen_delim() {
    let norm = Normalization::LastGenDelim;
    for (ns1, sf1, ns2, sf2) in &[
        ("http://champin.net/#pa", "", "http://champin.net/#", "pa"),
        ("http://champin.net/#", "pa", "http://champin.net/#", "pa"),
        ("http://champin.net/", "#pa", "http://champin.net/#", "pa"),
        (
            "http://champin.net/",
            "foo/bar",
            "http://champin.net/foo/",
            "bar",
        ),
        ("tag:foo", "", "tag:", "foo"),
        ("tag:", "foo", "tag:", "foo"),
        ("tag?foo", "", "tag?", "foo"),
        ("tag?", "foo", "tag?", "foo"),
        ("http://champin.net/foo/", "", "http://champin.net/foo/", ""),
        ("http://champin.net/", "foo/", "http://champin.net/foo/", ""),
        (
            "http://example.org/i18ñ#ê",
            "",
            "http://example.org/i18ñ#",
            "ê",
        ),
        (
            "http://example.org/i18ñ#",
            "ê",
            "http://example.org/i18ñ#",
            "ê",
        ),
    ] {
        let i1 = if sf1.len() == 0 {
            BoxTerm::new_iri(*ns1).unwrap()
        } else {
            BoxTerm::new_iri_suffixed(*ns1, *sf1).unwrap()
        };
        let i2 = i1.normalized(norm);
        assert_eq!(i1, i2);
        if let Iri(i2) = i2 {
            assert_eq!(&i2.ns[..], *ns2);
            let sf2 = if sf2.len() == 0 { None } else { Some(*sf2) };
            assert_eq!(i2.suffix.as_ref().map(AsRef::as_ref), sf2);
        }
    }
}

#[test]
fn bnode() {
    let b1 = BoxTerm::new_bnode("foo").unwrap();
    assert_eq!(&format!("{}", b1), "_:foo");

    if let BNode(id1) = b1 {
        assert_eq!(id1.as_str(), "foo");
    } else {
        panic!("b1 should be a BNode");
    }
}

#[test]
fn bnode_eq_different_term_data() {
    let b1 = BoxTerm::new_bnode("xyz").unwrap();
    let b2 = RcTerm::new_bnode("xyz").unwrap();
    let b3 = ArcTerm::new_bnode("xyz").unwrap();
    let b4 = RefTerm::new_bnode("xyz").unwrap();
    assert_eq!(b1, b2);
    assert_eq!(h(&b1), h(&b2));
    assert_eq!(b1, b3);
    assert_eq!(h(&b1), h(&b3));
    assert_eq!(b1, b4);
    assert_eq!(h(&b1), h(&b4));
    assert_eq!(b2, b3);
    assert_eq!(h(&b2), h(&b3));
    assert_eq!(b2, b4);
    assert_eq!(h(&b2), h(&b4));
    assert_eq!(b3, b4);
    assert_eq!(h(&b3), h(&b4));
}

#[test]
fn bnode_similar_but_not_eq() {
    let b1 = BoxTerm::new_bnode("xyz").unwrap();
    let b2 = BoxTerm::new_bnode("xyZ").unwrap();
    assert_ne!(b1, b2);
    assert_ne!(h(&b1), h(&b2));
}

#[test]
fn bnode_id_deref() {
    let b1 = BoxTerm::new_bnode("foo").unwrap();
    if let BNode(id1) = b1 {
        assert!(id1.starts_with("fo"));
    }
}

#[test]
fn bnode_id_eq_str() {
    let b1 = BoxTerm::new_bnode("foo").unwrap();
    if let BNode(id1) = b1 {
        assert_eq!(id1.value(), "foo");
    } else {
        panic!("b1 should be a BNode");
    }
}

#[test]
fn bnode_is_n3() {
    let pos = POSITIVE_1CHAR_IDS
        .iter()
        .chain(POSITIVE_N3_BNODE_IDS.iter());
    for id in pos {
        assert!(BoxTerm::new_bnode(*id).is_ok());
    }
}

#[test]
fn bnode_is_not_n3() {
    let neg = NEGATIVE_1CHAR_IDS
        .iter()
        .chain(NEGATIVE_N3_BNODE_IDS.iter())
        .chain(NT_ONLY_BNODE_IDS.iter());
    for id in neg {
        assert!(BoxTerm::new_bnode(*id).is_err());
    }

    for id in [
        "a ", // spurious space
        "a.", // incomplete
    ]
    .iter()
    {
        assert!(BoxTerm::new_bnode(*id).is_err());
    }
}

#[test]
fn literal_lang() {
    let lit = RefTerm::new_literal_lang("hello", "en").unwrap();
    assert_eq!(lit.value(), "hello");
    assert_eq!(&format!("{}", lit), "\"hello\"@en");

    let lit: literal::Literal<&str> = lit.try_into().expect("Should be a literal");

    assert_eq!(*lit.txt(), "hello");
    assert_eq!(
        *lit.lang().expect("Should have returned Literal::Lang"),
        "en"
    );

    let res = RefTerm::new_literal_lang("hello", "");
    assert!(res.is_err());
}

#[test]
fn literal_dt() {
    // Constructing from str
    let lit: BoxTerm = "hello".as_literal().copied();
    assert_eq!(lit.value(), "hello");
    assert_eq!(&format!("{}", lit), "\"hello\"");
    let lit: literal::Literal<Box<str>> = lit.try_into().expect("Should be a literal");
    assert_eq!(lit.dt(), xsd::string);
    assert_eq!(lit.txt().as_ref(), "hello");

    // Constructing from int
    let lit: BoxTerm = 42_i32.as_literal().copied();
    assert_eq!(lit.value(), "42");
    assert_eq!(
        &format!("{}", lit),
        "\"42\"^^<http://www.w3.org/2001/XMLSchema#int>",
    );
    let lit: literal::Literal<_> = lit.try_into().expect("Should be a literal");
    assert_eq!(lit.dt(), xsd::int);
    assert_eq!(lit.txt().as_ref(), "42");

    // Constructing with IRI datatype
    let lit = RefTerm::new_literal_dt("42", xsd::integer).unwrap();
    assert_eq!(lit.value(), "42");
    assert_eq!(
        &format!("{}", lit),
        "\"42\"^^<http://www.w3.org/2001/XMLSchema#integer>",
    );
    let lit: literal::Literal<_> = lit.try_into().unwrap();
    assert_eq!(lit.dt(), xsd::integer);
    assert_eq!(*lit.txt(), "42");
}

#[test]
fn literal_eq_different_term_data() {
    let l1 = BoxTerm::new_literal_lang("hello", "en").unwrap();
    let l2 = RcTerm::new_literal_lang("hello", "en").unwrap();
    let l3 = ArcTerm::new_literal_lang("hello", "en").unwrap();
    let l4 = RefTerm::new_literal_lang("hello", "en").unwrap();
    assert_eq!(l1, l2);
    assert_eq!(h(&l1), h(&l2));
    assert_eq!(l1, l3);
    assert_eq!(h(&l1), h(&l3));
    assert_eq!(l1, l4);
    assert_eq!(h(&l1), h(&l4));
    assert_eq!(l2, l3);
    assert_eq!(h(&l2), h(&l3));
    assert_eq!(l2, l4);
    assert_eq!(h(&l2), h(&l4));
    assert_eq!(l3, l4);
    assert_eq!(h(&l3), h(&l4));
}

#[test]
fn literal_eq_different_case() {
    let l1 = BoxTerm::new_literal_lang("hello", "en").unwrap();
    let l2 = RcTerm::new_literal_lang("hello", "EN").unwrap();
    assert_eq!(l1, l2);
    assert_eq!(h(&l1), h(&l2));
}

#[test]
fn literal_similar_but_not_eq() {
    let l1 = RefTerm::new_literal_lang("42", "en").unwrap();
    let l2 = RefTerm::new_literal_lang("42", "en-us").unwrap();
    let l3 = RefTerm::new_literal_dt("42", xsd::string.clone()).unwrap();
    let l4 = RefTerm::new_literal_dt("42", xsd::integer.clone()).unwrap();
    assert_ne!(l1, l2);
    assert_ne!(h(&l1), h(&l2));
    assert_ne!(l1, l3);
    assert_ne!(h(&l1), h(&l3));
    assert_ne!(l1, l4);
    assert_ne!(h(&l1), h(&l4));
    assert_ne!(l2, l3);
    assert_ne!(h(&l2), h(&l3));
    assert_ne!(l2, l4);
    assert_ne!(h(&l2), h(&l4));
    assert_ne!(l3, l4);
    assert_ne!(h(&l3), h(&l4));
}

#[test]
fn literal_normalized_no_suffix() {
    let norm = Normalization::NoSuffix;
    let dt = BoxTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap();
    let l1 = BoxTerm::new_literal_dt("hello", dt).unwrap();
    let l2 = l1.normalized(norm);
    assert_eq!(l1, l2);

    if let Literal(l2) = l2 {
        assert!(!l2.dt().has_suffix());
    }
}

#[test]
fn literal_normalized_last_gen_delim() {
    let norm = Normalization::LastGenDelim;
    for (ns1, sf1, ns2, sf2) in &[
        ("http://champin.net/#pa", "", "http://champin.net/#", "pa"),
        ("http://champin.net/#", "pa", "http://champin.net/#", "pa"),
        ("http://champin.net/", "#pa", "http://champin.net/#", "pa"),
        (
            "http://champin.net/",
            "foo/bar",
            "http://champin.net/foo/",
            "bar",
        ),
        ("tag:foo", "", "tag:", "foo"),
        ("tag:", "foo", "tag:", "foo"),
    ] {
        let dt = if sf1.len() == 0 {
            BoxTerm::new_iri(*ns1).unwrap()
        } else {
            BoxTerm::new_iri_suffixed(*ns1, *sf1).unwrap()
        };
        let l1 = BoxTerm::new_literal_dt("hello", dt).unwrap();
        let l2 = l1.normalized(norm);
        assert_eq!(l1, l2);
        if let Literal(l2) = l2 {
            let i2 = l2.dt();
            assert_eq!(&i2.ns[..], *ns2);
            let sf2 = if sf2.len() == 0 { None } else { Some(*sf2) };
            assert_eq!(i2.suffix, sf2);
        }
    }
}

#[test]
fn variable() {
    let pos = POSITIVE_1CHAR_IDS.iter().chain(POSITIVE_VARIABLES.iter());
    for id in pos {
        let res = BoxTerm::new_variable(*id);
        assert!(
            res.is_ok(),
            format!("{:?} should be accepted as a variable name", *id)
        );

        let var = res.unwrap();
        assert_eq!(&var.value(), id);
        assert_eq!(format!("{}", var), format!("?{}", id));
    }

    let neg = NEGATIVE_1CHAR_IDS.iter().chain(NEGATIVE_VARIABLES.iter());
    for id in neg {
        let res = BoxTerm::new_variable(*id);
        assert!(
            res.is_err(),
            format!("{:?} should be refused as a variable name", *id)
        );
    }
}

#[test]
fn variable_eq_different_term_data() {
    let v1 = BoxTerm::new_variable("xyz").unwrap();
    let v2 = RcTerm::new_variable("xyz").unwrap();
    let v3 = ArcTerm::new_variable("xyz").unwrap();
    let v4 = RefTerm::new_variable("xyz").unwrap();
    assert_eq!(v1, v2);
    assert_eq!(h(&v1), h(&v2));
    assert_eq!(v1, v3);
    assert_eq!(h(&v1), h(&v3));
    assert_eq!(v1, v4);
    assert_eq!(h(&v1), h(&v4));
    assert_eq!(v2, v3);
    assert_eq!(h(&v2), h(&v3));
    assert_eq!(v2, v4);
    assert_eq!(h(&v2), h(&v4));
    assert_eq!(v3, v4);
    assert_eq!(h(&v3), h(&v4));
}

#[test]
fn variable_similar_but_not_eq() {
    let v1 = BoxTerm::new_variable("xyz").unwrap();
    let v2 = BoxTerm::new_variable("xyZ").unwrap();
    assert_ne!(v1, v2);
    assert_ne!(h(&v1), h(&v2));
}

#[test]
fn term_similar_but_not_eq() {
    let txt = "foo";
    let t1 = StaticTerm::new_iri(txt).unwrap();
    let t2 = StaticTerm::new_literal_dt(txt, xsd::anyURI).unwrap();
    let t3 = StaticTerm::new_bnode(txt).unwrap();
    let t4 = StaticTerm::new_variable(txt).unwrap();
    assert_ne!(t1, t2);
    assert_ne!(h(&t1), h(&t2));
    assert_ne!(t1, t3);
    assert_ne!(h(&t1), h(&t3));
    assert_ne!(t1, t4);
    assert_ne!(h(&t1), h(&t4));
    assert_ne!(t2, t3);
    assert_ne!(h(&t2), h(&t3));
    assert_ne!(t2, t4);
    assert_ne!(h(&t2), h(&t4));
    assert_ne!(t3, t4);
    assert_ne!(h(&t3), h(&t4));
}

#[test]
fn map() {
    let mut cnt = 0;
    let t = StaticTerm::new_iri_suffixed("http://champin.net/", "#pa").unwrap();
    let t2 = t.map(|s| {
        cnt += s.len();
        String::from(s)
    });
    assert_eq!(cnt, "http://champin.net/".len() + "#pa".len());
    assert_eq!(t2, Term::<&str>::new_iri("http://champin.net/#pa").unwrap());
}

#[test]
fn map_into() {
    let t1 = StaticTerm::new_iri("http://champin.net/#pa").unwrap();
    let t2 = t1.map_into::<Box<str>>();
    let _3 = t2.clone().map_into::<Rc<str>>();
    let _4 = t2.clone().map_into::<Arc<str>>();
    let _5 = t2.map_into::<String>();
}

#[test]
fn convert() {
    let t1 = StaticTerm::new_iri("http://champin.net/#pa").unwrap();
    let t2 = t1.clone_into::<Box<str>>();
    let t3 = t2.clone_into::<Rc<str>>();
    let t4 = t3.clone_into::<Arc<str>>();
    let _5 = t4.clone_into::<&str>();
}

pub(crate) const POSITIVE_1CHAR_IDS: &[&str] = &[
    // PN_CHARS_BASE
    "a",
    "z",
    "A",
    "Z",
    "\u{00C0}",
    "\u{00D6}",
    "\u{00D8}",
    "\u{00F6}",
    "\u{00F8}",
    "\u{02FF}",
    "\u{0370}",
    "\u{037D}",
    "\u{037F}",
    "\u{1FFF}",
    "\u{200C}",
    "\u{200D}",
    "\u{2070}",
    "\u{218F}",
    "\u{2C00}",
    "\u{2FEF}",
    "\u{3001}",
    "\u{D7FF}",
    "\u{F900}",
    "\u{FDCF}",
    "\u{FDF0}",
    "\u{FFFD}",
    "\u{10000}",
    "\u{EFFFF}",
    // PN_CHARS_U
    "_",
    // other authorized leading character
    "0",
    "9",
];

pub(crate) const NEGATIVE_1CHAR_IDS: &[&str] = &[
    " ",
    "\t",
    "\n",
    "\r",
    ".",
    "&",
    "!",
    "@",
    ":",
    "\u{00BF}",
    "\u{00D7}",
    "\u{00F7}",
    "\u{0300}",
    "\u{036F}",
    "\u{037E}",
    "\u{2000}",
    "\u{200B}",
    "\u{200E}",
    "\u{206F}",
    "\u{2190}",
    "\u{2BFF}",
    "\u{2FF0}",
    "\u{3000}",
    "\u{E000}",
    "\u{F8FF}",
    "\u{FDD0}",
    "\u{FDEF}",
    "\u{FFFE}",
    "\u{FFFF}",
    "\u{F0000}",
    // PN_CHARS - PN_CHARS_BASE
    "-",
    "\u{00B7}",
    "\u{0300}",
    "\u{036F}",
    "\u{203F}",
    "\u{2040}",
];

pub(crate) const POSITIVE_N3_BNODE_IDS: &[&str] = &[
    "abcdef",
    "_abcde",
    "0abcde",
    // trailing PN_CHARS - PN_CHARS_BASE
    "a-",
    "a\u{00B7}",
    "a\u{0300}",
    "a\u{036F}",
    "a\u{203F}",
    "a\u{2040}",
];

pub(crate) const NEGATIVE_N3_BNODE_IDS: &[&str] =
    &["a b", "a\tb", "a\nb", "a\rb", "a&b", "a!c", "a@b"];

pub(crate) const NT_ONLY_BNODE_IDS: &[&str] = &[":", "a:b", ":_::__"];

pub(crate) const POSITIVE_VARIABLES: &[&str] = &[
    "a\u{00B7}",
    "a\u{0300}",
    "a\u{036F}",
    "a\u{203F}",
    "a\u{2040}",
];

pub(crate) const NEGATIVE_VARIABLES: &[&str] = &[
    "a ",
    "a\u{00B6}",
    "a\u{00B8}",
    //"a\u{02FF}", "a\u{0370}", // accepted, as they are part of pn_chars_u
    "a\u{203E}",
    "a\u{2041}",
];
