use super::*;

// this test is hidden behind the feature test_util because it is very long
// (and it depends on an external tool 'pyld')
#[cfg(feature = "test_util")]
#[test]
fn w3c_test_suite() {
    use crate::test_util::*;
    use std::path::Path;

    let mpath = Path::new("..")
        .join("json-ld-api")
        .join("tests")
        .join("toRdf-manifest.jsonld");
    let manifest = Manifest::new(&mpath);
    let (failed, skipped, passed) = manifest.perform_all_tests(true);
    assert_eq!(0, failed, "{}/{}", failed, failed + skipped + passed);
}

#[test]
fn node_map() -> Result<(), Box<dyn std::error::Error>> {
    for (input, expected) in [
        // simple arc
        (
            json::object! {
                "@id": "iri:s",
                "iri:p": {
                    "@id": "iri:o",
                },
            },
            json::object! {
                "@default": {
                    "iri:s": {
                        "@id": "iri:s",
                        "iri:p": [
                            { "@id": "iri:o" },
                        ],
                    },
                    "iri:o": {
                        "@id": "iri:o",
                    },
                },
            },
        ),
        // labelled bnodes
        (
            json::object! {
                "@id": "_:x",
                "iri:p": {
                    "@id": "_:y",
                },
            },
            json::object! {
                "@default": {
                    "_:b1": {
                        "@id": "_:b1",
                        "iri:p": [
                            { "@id": "_:b2" },
                        ],
                    },
                    "_:b2": {
                        "@id": "_:b2",
                    },
                },
            },
        ),
        // array of objects
        (
            json::array! [
                {
                    "@id": "iri:s1",
                    "iri:p1": {
                        "@id": "iri:o1",
                    },
                },
                {
                    "@id": "iri:s2",
                    "iri:p2": {
                        "@id": "iri:o2",
                    },
                },
            ],
            json::object! {
                "@default": {
                    "iri:s1": {
                        "@id": "iri:s1",
                        "iri:p1": [
                            { "@id": "iri:o1" },
                        ],
                    },
                    "iri:o1": {
                        "@id": "iri:o1",
                    },
                    "iri:s2": {
                        "@id": "iri:s2",
                        "iri:p2": [
                            { "@id": "iri:o2" },
                        ],
                    },
                    "iri:o2": {
                        "@id": "iri:o2",
                    },
                },
            },
        ),
        // @type is a single IRI
        (
            json::object! {
                "@id": "iri:s",
                "@type": "iri:t",
            },
            json::object! {
                "@default": {
                    "iri:s": {
                        "@id": "iri:s",
                        "@type": [
                            "iri:t",
                        ],
                    },
                },
            },
        ),
        // @type is a single bnode
        (
            json::object! {
                "@id": "iri:s",
                "@type": "_:t",
            },
            json::object! {
                "@default": {
                    "iri:s": {
                        "@id": "iri:s",
                        "@type": [
                            "_:b1",
                        ],
                    },
                },
            },
        ),
        // @type is an array
        (
            json::object! {
                "@id": "iri:s",
                "@type": [ "_:t1", "iri:t2", "_:t3", ],
            },
            json::object! {
                "@default": {
                    "iri:s": {
                        "@id": "iri:s",
                        "@type": [
                            "_:b1",
                            "iri:t2",
                            "_:b2",
                        ],
                    },
                },
            },
        ),
        /* simple value object */
        (
            json::object! {
                "@id": "iri:s",
                "iri:p": {
                    "@value": "hello",
                },
            },
            json::object! {
                "@default": {
                    "iri:s": {
                        "@id": "iri:s",
                        "iri:p": [
                            { "@value": "hello" },
                        ],
                    },
                },
            },
        ),
        /* list */
        (
            json::object! {
                "@id": "iri:s",
                "iri:p": {
                    "@list": [
                        { "@value": "item1" },
                        {
                            "@id": "iri:item2",
                            "iri:q": [
                                { "@value": "item3" },
                            ],
                        },
                    ],
                },
            },
            json::object! {
                "@default": {
                    "iri:item2": {
                        "@id": "iri:item2",
                        "iri:q": [
                            { "@value": "item3" },
                        ],
                    },
                    "iri:s": {
                        "@id": "iri:s",
                        "iri:p": [
                            { "@list": [
                                { "@value": "item1" },
                                { "@id": "iri:item2" },
                            ] },
                        ]
                    },
                },
            },
        ),
        /* list of list */
        (
            json::object! {
                "@id": "iri:s",
                "iri:p": { "@list": [
                        { "@list": [
                            { "@value": 1 },
                            { "@value": 3 },
                        ] },
                        { "@list": [
                            { "@value": 4 },
                            { "@value": 2 },
                        ] },
                ] },
            },
            json::object! {
                "@default": {
                    "iri:s": {
                        "@id": "iri:s",
                        "iri:p": [
                            { "@list": [
                                { "@list": [
                                    { "@value": 1 },
                                    { "@value": 3 },
                                ] },
                                { "@list": [
                                    { "@value": 4 },
                                    { "@value": 2 },
                                ] },
                            ] },
                        ],
                    },
                },
            },
        ),
        // unlabelled bnodes
        (
            json::object! {
                "iri:p": {},
            },
            json::object! {
                "@default": {
                    "_:b1": {
                        "@id": "_:b1",
                        "iri:p": [
                            { "@id": "_:b2" },
                        ],
                    },
                    "_:b2": {
                        "@id": "_:b2",
                    },
                },
            },
        ),
        // reverse property
        (
            json::object! {
                "@id": "iri:a",
                "@reverse": {
                    "iri:p": [{
                        "@id": "iri:b",
                        "iri:p": {
                            "@id": "iri:c",
                        },
                    }],
                },
            },
            json::object! {
                "@default": {
                    "iri:a": {
                        "@id": "iri:a",
                    },
                    "iri:b": {
                        "@id": "iri:b",
                        "iri:p": [
                            { "@id": "iri:a" },
                            { "@id": "iri:c" },
                        ]
                    },
                    "iri:c": {
                        "@id": "iri:c",
                    },
                },
            },
        ),
        // arc on self, merging types and property
        (
            json::object! {
                "@id": "iri:s",
                "@type": "iri:t1",
                "iri:p": [
                    { "@value": 42 },
                ],
                "iri:q": {
                    "@id": "iri:s",
                    "@type": "iri:t2",
                    "iri:p": [
                        { "@value": 43 },
                    ],
                },
            },
            json::object! {
                "@default": {
                    "iri:s": {
                        "@id": "iri:s",
                        "@type": ["iri:t1", "iri:t2"],
                        "iri:p": [
                            { "@value": 42 },
                            { "@value": 43 },
                        ],
                        "iri:q": [
                            { "@id": "iri:s" },
                        ],
                    },
                },
            },
        ),
        // @index
        (
            json::object! {
                "@id": "iri:s",
                "@index": 42,
                "iri:p": {
                    "@id": "iri:o",
                },
            },
            json::object! {
                "@default": {
                    "iri:s": {
                        "@id": "iri:s",
                        "@index": 42,
                        "iri:p": [
                            { "@id": "iri:o" },
                        ],
                    },
                    "iri:o": {
                        "@id": "iri:o",
                    },
                },
            },
        ),
        // @graph
        (
            json::object! {
                "@id": "iri:g",
                "@graph": [
                    {
                        "@id": "iri:s",
                        "iri:p": {
                            "@id": "iri:o",
                        },
                    },
                ],
            },
            json::object! {
                "@default": {
                    "iri:g": {
                        "@id": "iri:g",
                    },
                },
                "iri:g": {
                    "iri:s": {
                        "@id": "iri:s",
                        "iri:p": [
                            { "@id": "iri:o" },
                        ],
                    },
                    "iri:o": {
                        "@id": "iri:o",
                    },
                },
            },
        ),
        // anonymous @graph
        (
            json::object! {
                "@id": "_:g",
                "@graph": [
                    {
                        "@id": "iri:s",
                        "iri:p": {
                            "@id": "iri:o",
                        },
                    },
                ],
            },
            json::object! {
                "@default": {
                    "_:b1": {
                        "@id": "_:b1",
                    },
                },
                "_:b1": {
                    "iri:s": {
                        "@id": "iri:s",
                        "iri:p": [
                            { "@id": "iri:o" },
                        ],
                    },
                    "iri:o": {
                        "@id": "iri:o",
                    },
                },
            },
        ),
        // @included
        (
            json::object! {
                "@id": "iri:s1",
                "iri:p1": {
                    "@id": "iri:o1",
                },
                "@included": [
                    {
                        "@id": "iri:s2",
                        "iri:p2": {
                            "@id": "iri:o2",
                        },
                    },
                ],
            },
            json::object! {
                "@default": {
                    "iri:s1": {
                        "@id": "iri:s1",
                        "iri:p1": [
                            { "@id": "iri:o1" },
                        ],
                    },
                    "iri:o1": {
                        "@id": "iri:o1",
                    },
                    "iri:s2": {
                        "@id": "iri:s2",
                        "iri:p2": [
                            { "@id": "iri:o2" },
                        ],
                    },
                    "iri:o2": {
                        "@id": "iri:o2",
                    },
                },
            },
        ),
    ] {
        println!("processing {}", input);
        let bnode_gen = BNodeGen::new();
        let node_map = generate_node_map(&input, &bnode_gen)?;
        println!("node map generated");
        let got = nm_to_json(&node_map)?;
        println!("node map converted to json");
        assert_eq!(
            got, expected,
            "\ninput => {:#}\ngot => {:#}\n expected => {:#}",
            input, got, expected
        );
        println!("passed");
    }
    Ok(())
}

fn nm_to_json(nm: &NodeMap) -> Result<JsonValue, Box<dyn std::error::Error>> {
    let mut ret = json::object! {};
    for (graph_name, nodes) in nm {
        let mut nodes_json = json::object! {};
        for (node_id, node_obj) in nodes {
            let mut node_json = json::object! {
                "@id": node_obj.id,
            };
            if !node_obj.types.is_empty() {
                let types: JsonValue = (&node_obj.types[..]).into();
                node_json.insert("@type", types)?;
            }
            if let Some(index) = node_obj.index {
                node_json.insert("@index", index.clone())?;
            }
            for (property, values) in &node_obj.properties {
                let values: Vec<_> = values.iter().map(noe_to_json).collect();
                node_json.insert(property, values)?;
            }
            nodes_json.insert(node_id, node_json)?;
        }
        ret.insert(graph_name, nodes_json)?;
    }
    Ok(ret)
}

fn noe_to_json(noe: &NodeObjectEntry) -> JsonValue {
    match noe {
        NodeObjectEntry::Id(id) => json::object! {
            "@id": *id,
        },
        NodeObjectEntry::List(vals) => json::object! {
            "@list": vals.iter().map(noe_to_json).collect::<Vec<_>>(),
        },
        NodeObjectEntry::Value(obj) => (*obj).clone(),
    }
}
