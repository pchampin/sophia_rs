# Getting Started

Following a short example how to build a graph, mutate it and serialize it back.


Add the sophia crate to your dependencies in `Cargo.toml`
```bash
[dependencies]
sophia = "0.6.2"
```


Add these lines of code and run the programm.
```rust
use sophia::graph::{inmem::FastGraph, *};
use sophia::ns::Namespace;
use sophia::parser::turtle;
use sophia::serializer::nt::NtSerializer;
use sophia::serializer::*;
use sophia::triple::stream::TripleSource;

fn main() {
    let example = r#"
            @prefix : <http://example.org/>.
            @prefix foaf: <http://xmlns.com/foaf/0.1/>.

            :alice foaf:name "Alice";
                foaf:mbox <mailto:alice@work.example> .

            :bob foaf:name "Bob".
            "#;
    let mut graph: FastGraph = turtle::parse_str(example).collect_triples().unwrap();

    let ex = Namespace::new("http://example.org/").unwrap();
    let foaf = Namespace::new("http://xmlns.com/foaf/0.1/").unwrap();
    graph.insert(&ex.get("bob").unwrap(), &foaf.get("knows").unwrap(), &ex.get("alice").unwrap()).unwrap();

    let mut nt_stringifier = NtSerializer::new_stringifier();
    let example2 = nt_stringifier.serialize_graph(&mut graph).unwrap().as_str();
    println!("The resulting graph\n{}", example2);
}
```

You should the following graph:
```bash
The resulting graph
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice".
<http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob".
<http://example.org/bob> <http://xmlns.com/foaf/0.1/knows> <http://example.org/alice>.
<http://example.org/alice> <http://xmlns.com/foaf/0.1/mbox> <mailto:alice@work.example>.
```