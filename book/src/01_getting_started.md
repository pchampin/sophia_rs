# Getting Started

Following a short example how to build a graph, mutate it and serialize it back.


Add the sophia crate to your dependencies in `Cargo.toml`
```bash
[dependencies]
sophia = { version="0.8.0-alpha", git="https://github.com/pchampin/sophia_rs" }
```


Add these lines of code and run the programm.
```rust
use sophia::api::prelude::*;
use sophia::api::ns::Namespace;
use sophia::inmem::graph::LightGraph;
use sophia::turtle::parser::turtle;
use sophia::turtle::serializer::nt::NtSerializer;

// loading a graph
let example = r#"
    @prefix : <http://example.org/>.
    @prefix foaf: <http://xmlns.com/foaf/0.1/>.
    :alice foaf:name "Alice";
           foaf:mbox <mailto:alice@work.example> .
    :bob foaf:name "Bob".
"#;
let mut graph: LightGraph = turtle::parse_str(example).collect_triples()?;

// mutating the graph
let ex = Namespace::new("http://example.org/")?;
let foaf = Namespace::new("http://xmlns.com/foaf/0.1/")?;
graph.insert(
    ex.get("bob")?,
    foaf.get("knows")?,
    ex.get("alice")?,
)?;

// serializing the graph
let mut nt_stringifier = NtSerializer::new_stringifier();
let example2 = nt_stringifier.serialize_graph(&graph)?.as_str();
println!("The resulting graph:\n{}", example2);
# Ok::<(), Box<dyn std::error::Error>>(())
```

You should get the following output:
```text
The resulting graph:
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice".
<http://example.org/alice> <http://xmlns.com/foaf/0.1/mbox> <mailto:alice@work.example>.
<http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob".
<http://example.org/bob> <http://xmlns.com/foaf/0.1/knows> <http://example.org/alice>.
```