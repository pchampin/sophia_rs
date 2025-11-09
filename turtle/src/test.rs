use std::{collections::HashMap, sync::LazyLock};

#[rustfmt::skip]
pub(crate) static NT_SAMPLES: &[(&str, &str, usize)] = &[
    ("empty", "", 0),
    ("comment", "# a comment", 0),
    ("version", r#"VERSION "1.2""#, 0),
    ("triple i i i",  r#"<x:s> <x:p> <x:o>.              "#, 1),
    ("triple b i i",  r#"_:s   <x:p> <x:o>.              "#, 1),
    ("triple i i b",  r#"<x:s> <x:p> _:o.                "#, 1),
    ("triple b i b",  r#"_:s   <x:p> _:o.                "#, 1),
    ("triple i i l",  r#"<x:s> <x:p> "o".                "#, 1),
    ("triple b i l",  r#"_:s   <x:p> "o".                "#, 1),
    ("triple i i ld", r#"<x:s> <x:p> "o"^^<x:d>.         "#, 1),
    ("triple b i ld", r#"_:s   <x:p> "o"^^<x:d>.         "#, 1),
    ("triple i i ll", r#"<x:s> <x:p> "o"@en-UK.          "#, 1),
    ("triple b i ll", r#"_:s   <x:p> "o"@en-UK.          "#, 1),
    ("triple i i lb", r#"<x:s> <x:p> "o"@en-UK--ltr.     "#, 1),
    ("triple b i lb", r#"_:s   <x:p> "o"@en-UK--rtl.     "#, 1),
    ("triple i i t",  r#"<x:s> <x:p> <<(_:a <x:b> "c")>>."#, 1),
    ("triple b i t",  r#"_:s   <x:p> <<(_:a <x:b> "c")>>."#, 1),
    ("escape",  r#"<x:s> <x:p> "\\u0001\\u0002\\u0003\\u0004\\u0005\\u0006\\u0006\\u0007\\u0008\\u0009\\u000A\\u000B\\u000C\\u000D\\u000E\\u000F\\u0011\\u0012\\u0013\\u0014\\u0015\\u0016\\u0016\\u0017\\u0018\\u0019\\u001A\\u001B\\u001C\\u001D\\u001E\\u001F\\\"\\\\\uFFFE\uFFFF"."#, 1),
    ("escape useless",  r#"<x:s> <x:p> "\\u006f"."#, 1),
];

#[rustfmt::skip]
pub(crate) static NQ_SAMPLES: &[(&str, &str, usize)] = &[
    ("quad i i i i",  r#"<x:s> <x:p> <x:o> <x:g>.              "#, 1),
    ("quad b i i i",  r#"_:s   <x:p> <x:o> <x:g>.              "#, 1),
    ("quad i i b i",  r#"<x:s> <x:p> _:o <x:g>.                "#, 1),
    ("quad b i b i",  r#"_:s   <x:p> _:o <x:g>.                "#, 1),
    ("quad i i l i",  r#"<x:s> <x:p> "o" <x:g>.                "#, 1),
    ("quad b i l i",  r#"_:s   <x:p> "o" <x:g>.                "#, 1),
    ("quad i i ld i", r#"<x:s> <x:p> "o"^^<x:d> <x:g>.         "#, 1),
    ("quad b i ld i", r#"_:s   <x:p> "o"^^<x:d> <x:g>.         "#, 1),
    ("quad i i ll i", r#"<x:s> <x:p> "o"@en-UK <x:g>.          "#, 1),
    ("quad b i ll i", r#"_:s   <x:p> "o"@en-UK <x:g>.          "#, 1),
    ("quad i i lb i", r#"<x:s> <x:p> "o"@en-UK--ltr <x:g>.     "#, 1),
    ("quad b i lb i", r#"_:s   <x:p> "o"@en-UK--rtl <x:g>.     "#, 1),
    ("quad i i t i",  r#"<x:s> <x:p> <<(_:a <x:b> "c")>> <x:g>."#, 1),
    ("quad b i t i",  r#"_:s   <x:p> <<(_:a <x:b> "c")>> <x:g>."#, 1),
    ("quad i i i b",  r#"<x:s> <x:p> <x:o> _:g.                "#, 1),
    ("quad b i i b",  r#"_:s   <x:p> <x:o> _:g.                "#, 1),
    ("quad i i b b",  r#"<x:s> <x:p> _:o _:g.                  "#, 1),
    ("quad b i b b",  r#"_:s   <x:p> _:o _:g.                  "#, 1),
    ("quad i i l b",  r#"<x:s> <x:p> "o" _:g.                  "#, 1),
    ("quad b i l b",  r#"_:s   <x:p> "o" _:g.                  "#, 1),
    ("quad i i ld b", r#"<x:s> <x:p> "o"^^<x:d> _:g.           "#, 1),
    ("quad b i ld b", r#"_:s   <x:p> "o"^^<x:d> _:g.           "#, 1),
    ("quad i i ll b", r#"<x:s> <x:p> "o"@en-UK _:g.            "#, 1),
    ("quad b i ll b", r#"_:s   <x:p> "o"@en-UK _:g.            "#, 1),
    ("quad i i lb b", r#"<x:s> <x:p> "o"@en-UK--ltr _:g.       "#, 1),
    ("quad b i lb b", r#"_:s   <x:p> "o"@en-UK--rtl _:g.       "#, 1),
    ("quad i i t b",  r#"<x:s> <x:p> <<(_:a <x:b> "c")>> _:g.  "#, 1),
    ("quad b i t b",  r#"_:s   <x:p> <<(_:a <x:b> "c")>> _:g.  "#, 1),
];

pub(crate) static GNQ_SAMPLES: &[(&str, &str, usize)] = &[
    ("triple of bnodes", r#"_:s _:p _:o."#, 1),
    ("triple of literals", r#""s" "p" "o"."#, 1),
    (
        "triple of triple terms",
        r#"<<(<x:s> <x:p> <x:o>)>> <<(_:s _:p _:o)>> <<("s" "p" "o")>>."#,
        1,
    ),
    ("triple of variables", r#"?s ?p ?o."#, 1),
    ("quad of bnodes", r#"_:s _:p _:o _:g."#, 1),
    ("quad of literals", r#""s" "p" "o" "g"."#, 1),
    (
        "quad of triple terms",
        r#"<<(<x:s> <x:p> <x:o>)>> <<(_:s _:p _:o)>> <<("s" "p" "o")>> <<( ?s ?p ?o)>>."#,
        1,
    ),
    ("quad of variables", r#"?s ?p ?o ?g."#, 1),
    (
        "nested generalized triple term",
        r#"<<( <<(<x:s> <x:p> <x:o>)>> <<(_:s _:p _:o)>> <<("s" "p" "o")>> )>> ?p ?o."#,
        1,
    ),
];

pub(crate) static PREFIXES: &str = r#"
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX : <http://example.org/ns/>
"#;

pub(crate) static TURTLE_SAMPLES: &[(&str, &str, usize)] = &[
    (
        "factorized triples",
        r#"
        :alice a :Person; :name "Alice"; :age 42.
        :bob a :Person, :Man; :nick "bob"@fr, "bobby"@en; :admin true.
        "#,
        8,
    ),
    (
        "pretty literals",
        r#"
        [] <x:p> 42, 3.14, 0.314e1, true, "foo".
        "#,
        5,
    ),
    (
        "unpretty literals",
        r#"
        [] <x:p> "a"^^xsd:integer, "12"^^xsd:decimal, "1.2"^^xsd:double, "1"^^xsd:boolean.
        "#,
        4,
    ),
    (
        "lists",
        r#"
        <x:alice> <x:likes> ( 1 2 ( 3 4 ) 5 6 ), ("a" "b").
        "#,
        20,
    ),
    (
        "subject list",
        r"
        (1 2 3) a <tag:List>.
        ",
        7,
    ),
    (
        "malformed list",
        r"
        _:a rdf:first 42, 43; rdf:rest (44 45).
        _:b rdf:first 42; rdf:rest (43), (44).
        ",
        14,
    ),
    (
        "bnode cycles",
        r#"
        _:a :n "a"; :p [ :q [ :r _:a ]].
        _:b :n "b"; :s [ :s _:b ].
        _:c :b "c"; :t _:c.
        "#,
        9,
    ),
    (
        "reified subject",
        r"
        << :s :p :o >> :q :r.
        ",
        2,
    ),
    (
        "reified object",
        r"
        :s :p << :t :q :r >>.
        ",
        2,
    ),
    (
        "reified nested",
        r"
        << << :a :b :c >> :d << :e :f :g >> >> :h :i.
        ",
        4,
    ),
    (
        "annotation",
        r"
        :s :p :o {| :a :b, :c; :d :e |}.
        ",
        5,
    ),
    (
        "annotation nested",
        r"
        :s :p :o {|
            :a :b {| :x :y1 |},
                :c {| :x :y2 |};
            :d :e {| :x :y3 |}
        |}.
        ",
        11,
    ),
    ("anon in list", ":s :p ( [:p :o] [:p :o;] [] ).", 9),
    ("rdf:nil in reified triple", r#"<< :s :p rdf:nil >> ."#, 1),
];

pub(crate) static TRIG_SAMPLES: &[(&str, &str, usize)] = &[
    (
        "implicitly named graph iri",
        r"
            <x:g> { :s :p :o }
        ",
        1,
    ),
    (
        "implicitly named graph pname",
        r"
            :g { :s :p :o }
        ",
        1,
    ),
    (
        "implicitly named graph bnode",
        r"
            _:g { :s :p :o }
        ",
        1,
    ),
    (
        "implicitly named graph anon",
        r"
            [] { :s :p :o }
        ",
        1,
    ),
    (
        "explicitly named graph iri",
        r"
            GRAPH <x:g> { :s :p :o }
        ",
        1,
    ),
    (
        "explicitly named graph pname",
        r"
            GRAPH :g { :s :p :o }
        ",
        1,
    ),
    (
        "explicitly named graph bnode",
        r"
            GRAPH _:g { :s :p :o }
        ",
        1,
    ),
    (
        "explicitly named graph anon",
        r"
            GRAPH [] { :s :p :o }
        ",
        1,
    ),
    (
        "alternating graphs",
        r"
            :s :p :o1.
            GRAPH :g1 { :s :p :o2 }
            GRAPH :g2 { :s :p :o3 }
            GRAPH :g1 { :s :p :o4 }
            << :s :p :o ~ :r >>.
            GRAPH :g1 { :r :p :o5 }
        ",
        6,
    ),
];

pub(crate) static GTRIG_SAMPLES: &[(&str, &str, usize)] = &[
    (
        "all bnodes",
        r"
      _:b01 _:b02 _:b03 ~ _:b04.
      _:b05 { _:b06 _:b07 _:b08 }
      GRAPH _:b09 { _:b10 _:b11 _:b12 }
      << _:b13 _:b14 _:b15 ~ _:b16 >> _:b17 <<( _:b18 _:b19 _:b20 )>>.
      _:b21 _:b22 ( _:b23 _:b24 ).
    ",
        11,
    ),
    (
        "all anon",
        r"
      [] [] [] ~ [].
      [] { [] [] [] }
      GRAPH [] { [] [] [] }
      << [] [] [] ~ [] >> [] <<( [] [] [] )>>.
      [] [] ( [] [] ).
    ",
        11,
    ),
    (
        "all strings",
        r#"
      "s01" "s02" "s03" ~ "s04".
      "s05" { "s06" "s07" "s08" }
      GRAPH "s09" { "s10" "s11" "s12" }
      << "s13" "s14" "s15" ~ "s16" >> "s17" <<( "s18" "s19" "s20" )>>.
      "s21" "s22" ( "s23" "s24" ).
    "#,
        11,
    ),
    (
        "all numbers",
        r"
      .01 .02 .03 ~ .04.
      .05 { .06 .07 .08 }
      GRAPH .09 { .10 .11 .12 }
      << .13 .14 .15 ~ .16 >> .17 <<( .18 .19 .20 )>>.
      .21 .22 ( .23 .24 ).
    ",
        11,
    ),
    (
        "all variables",
        r"
      ?v01 ?v02 ?v03 ~ ?v04.
      ?v05 { ?v06 ?v07 ?v08 }
      GRAPH ?v09 { ?v10 ?v11 ?v12 }
      << ?v13 ?v14 ?v15 ~ ?v16 >> ?v17 <<( ?v18 ?v19 ?v20 )>>.
      ?v21 ?v22 ( ?v23 ?v24 ).
    ",
        11,
    ),
    (
        "all triple terms",
        r"
      <<( ?s ?p ?o01 )>> <<( ?s ?p ?o02 )>> <<( ?s ?p ?o03 )>> ~ <<( ?s ?p ?o04 )>>.
      <<( ?s ?p ?o05 )>> { <<( ?s ?p ?o06 )>> <<( ?s ?p ?o07 )>> <<( ?s ?p ?o08 )>> }
      GRAPH <<( ?s ?p ?o09 )>> { <<( ?s ?p ?o10 )>> <<( ?s ?p ?o11 )>> <<( ?s ?p ?o12 )>> }
      << <<( ?s ?p ?o13 )>> <<( ?s ?p ?o14 )>> <<( ?s ?p ?o15 )>> ~ <<( ?s ?p ?o16 )>> >> <<( ?s ?p ?o17 )>> <<( <<( ?s ?p ?o18 )>> <<( ?s ?p ?o19 )>> <<( ?s ?p ?o20 )>> )>>.
      <<( ?s ?p ?o21 )>> <<( ?s ?p ?o22 )>> ( <<( ?s ?p ?o23 )>> <<( ?s ?p ?o24 )>> ).
    ",
        11,
    ),
    (
        "almost all triple terms",
        r"
      <<( ?s ?p ?o01 )>> <<( ?s ?p ?o02 )>> <<( ?s ?p ?o03 )>> ~ <<( ?s ?p ?o04 )>>.
      ?g05 { <<( ?s ?p ?o06 )>> <<( ?s ?p ?o07 )>> <<( ?s ?p ?o08 )>> }
      GRAPH ?g09 { <<( ?s ?p ?o10 )>> <<( ?s ?p ?o11 )>> <<( ?s ?p ?o12 )>> }
     << <<( ?s ?p ?o13 )>> <<( ?s ?p ?o14 )>> <<( ?s ?p ?o15 )>> ~ <<( ?s ?p ?o16 )>> >> <<( ?s ?p ?o17 )>><<( <<( ?s ?p ?o18 )>> <<( ?s ?p ?o19 )>> <<( ?s ?p ?o20 )>> )>>.
      <<( ?s ?p ?o21 )>> <<( ?s ?p ?o22 )>> ( <<( ?s ?p ?o23 )>> <<( ?s ?p ?o24 )>> ).
    ",
        11,
    ),
    (
        "almost all reified triples",
        r"
      << ?s ?p _:b01 >> << ?s ?p _:b02 >> << ?s ?p _:b03 >> ~ _:b04.
      _:b05 { << ?s ?p _:b06 >> << ?s ?p _:b07 >> << ?s ?p _:b08 >> }
      GRAPH _:b09 { << ?s ?p _:b10 >> << ?s ?p _:b11 >> << ?s ?p _:b12 >> }
      << << ?s ?p _:b13 >> << ?s ?p _:b14 >> << ?s ?p _:b15 >> ~ _:b16 >> << ?s ?p _:b17 >> <<( _:b18 _:b19 _:b20 )>>.
      << ?s ?p _:b21 >> << ?s ?p _:b22 >> ( << ?s ?p _:b23 >> << ?s ?p _:b24 >> ).
    ",
        28,
    ),
    (
        "almost all bnode property lists",
        r"
      [?p _:b01] [?p _:b02] [?p _:b03] ~ _:b04.
      _:b05 { [?p _:b06] [?p _:b07] [?p _:b08] }
      GRAPH _:b09 { [?p _:b10] [?p _:b11] [?p _:b12] }
      << _:b13 :b14 _:b15 ~ _:b16 >> [?p _:b17] <<( _:b18 _:b19 _:b20 )>>.
      [?p _:b21] [?p _:b22] ( [?p _:b23] [?p _:b24] ).
    ",
        25,
    ),
    (
        "almost all collections",
        r"
      (?p _:b01) (?p _:b02) (?p _:b03) ~ _:b04.
      _:b05 { (?p _:b06) (?p _:b07) (?p _:b08) }
      GRAPH _:b09 { (?p _:b10) (?p _:b11) (?p _:b12) }
      << _:b13 _:b14 _:b15 ~ _:b16 >> (?p _:b17) <<( _:b18 _:b19 _:b20 )>>.
      (?p _:b21) (?p _:b22) ( (?p _:b23) (?p _:b24) ).
    ",
        67,
    ),
];

pub(crate) type LazyMap = LazyLock<HashMap<String, (String, usize)>>;

pub(crate) const fn nt_samples() -> LazyMap {
    LazyLock::new(|| {
        NT_SAMPLES
            .iter()
            .map(|(name, sample, count)| (name.to_string(), (sample.to_string(), *count)))
            .collect()
    })
}

pub(crate) const fn nq_samples() -> LazyMap {
    LazyLock::new(|| {
        NT_SAMPLES
            .iter()
            .chain(NQ_SAMPLES)
            .map(|(name, sample, count)| (name.to_string(), (sample.to_string(), *count)))
            .collect()
    })
}

pub(crate) const fn ttl_samples() -> LazyMap {
    LazyLock::new(|| {
        NT_SAMPLES
            .iter()
            .chain(TURTLE_SAMPLES)
            .map(|(name, sample, count)| {
                (name.to_string(), (format!("{PREFIXES}{sample}"), *count))
            })
            .collect()
    })
}

pub(crate) const fn trig_samples() -> LazyMap {
    LazyLock::new(|| {
        NT_SAMPLES
            .iter()
            .chain(TURTLE_SAMPLES)
            .chain(TRIG_SAMPLES)
            .map(|(name, sample, count)| {
                (name.to_string(), (format!("{PREFIXES}{sample}"), *count))
            })
            .chain(
                NT_SAMPLES
                    .iter()
                    .filter(|(name, ..)| *name != "version")
                    .chain(TURTLE_SAMPLES)
                    .map(|(name, sample, count)| {
                        (
                            format!("{name} in bracketed default graph"),
                            (format!("{PREFIXES}{{\n{sample}\n}}"), *count),
                        )
                    }),
            )
            .chain(
                NT_SAMPLES
                    .iter()
                    .filter(|(name, ..)| *name != "version")
                    .chain(TURTLE_SAMPLES)
                    .map(|(name, sample, count)| {
                        (
                            format!("{name} in named graph"),
                            (format!("{PREFIXES}<x:g>{{\n{sample}\n}}"), *count),
                        )
                    }),
            )
            .collect()
    })
}

pub(crate) const fn gnq_samples() -> LazyMap {
    LazyLock::new(|| {
        NT_SAMPLES
            .iter()
            .chain(NQ_SAMPLES)
            .chain(GNQ_SAMPLES)
            .map(|(name, sample, count)| (name.to_string(), (sample.to_string(), *count)))
            .collect()
    })
}

pub(crate) const fn gtrig_samples() -> LazyMap {
    LazyLock::new(|| {
        trig_samples()
            .iter()
            .map(|(name, (sample, size))| (name.clone(), (sample.clone(), *size)))
            .chain(GTRIG_SAMPLES.iter().map(|(name, sample, count)| {
                (name.to_string(), (format!("{PREFIXES}{sample}"), *count))
            }))
            .collect()
    })
}
