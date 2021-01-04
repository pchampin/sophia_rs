use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use sophia::triple::stream::TripleSource;

fn parse_nt(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse NT");
    group.throughput(Throughput::Bytes(NT.len() as u64));
    group.bench_with_input("parse NT", &NT, |b, &data| {
        let mut count = 0;
        b.iter(|| sophia::parser::nt::parse_str(data).for_each_triple(|_| count += 1))
    });
    group.finish();
}

fn parse_ttl(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse TTL");
    group.throughput(Throughput::Bytes(TTL.len() as u64));
    group.bench_with_input("parse TTL", &TTL, |b, &data| {
        let mut count = 0;
        b.iter(|| sophia::parser::turtle::parse_str(data).for_each_triple(|_| count += 1))
    });
    group.finish();
}

criterion_group!(benches, parse_nt, parse_ttl);
criterion_main!(benches);

const TTL: &str = r#"
@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix s: <http://schema.org/> .

:Person a owl:Class .
:Male rdfs:subClassOf :Person.
:Female rdfs:subClassOf :Person.
:Child rdfs:subClassOf :Person.
:Adult rdfs:subClassOf :Person.
:Boy rdfs:subClassOf :Male, :Child.
:Girl rdfs:subClassOf :Female, :Child.
:Man rdfs:subClassOf :Male, :Adult.
:Woman rdfs:subClassOf :Female, :Aduly.

:related a owl:TransitiveProperty, owl:SymmetricProperty ;
    rdfs:domain :Person ;
    rdfs:range :Person ;
.
:ancestor a owl:TransitiveProperty ;
    rdfs:subPropertyOf :related ;
.
:parent rdfs:subPropertyOf :ancestor ;
    owl:inverseOf :child ;
.
:father a owl:FunctionalProperty ;
    rdfs:subPropertyOf :parent ;
    rdfs:range :Male ;
.
:mother a owl:FunctionalProperty ;
    rdfs:subPropertyOf :parent ;
    rdfs:range :Female ;
.
:son rdfs:subPropertyOf :child ;
    rdfs:range :Male ;
.
:daughter rdfs:subPropertyOf :child ;
    rdfs:range :Female ;
.

:bart a :Boy ;
    :father :homer ;
    :mother :marge ;
    :age 10;
    s:name "Bart Simpson";
.
:lisa a :Girl ;
    :father :homer ;
    :mother :marge ;
    :age 8;
    s:name "Lisa Simpson"
.
:marge
    :father :clancy ;
    :mother [
        s:name "Jacqueline Bouvier née Gurney";
    ]
    s:name "Marge Simpson née Bouvier";
.
"#;

const NT: &str = r#"
<http://example.org/Person> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#Class> .
<http://example.org/Male> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Person> .
<http://example.org/Female> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Person> .
<http://example.org/Child> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Person> .
<http://example.org/Adult> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Person> .
<http://example.org/Boy> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Male> .
<http://example.org/Boy> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Child> .
<http://example.org/Girl> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Female> .
<http://example.org/Girl> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Child> .
<http://example.org/Man> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Male> .
<http://example.org/Man> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Adult> .
<http://example.org/Woman> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Female> .
<http://example.org/Woman> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://example.org/Aduly> .
<http://example.org/related> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#TransitiveProperty> .
<http://example.org/related> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#SymmetricProperty> .
<http://example.org/related> <http://www.w3.org/2000/01/rdf-schema#domain> <http://example.org/Person> .
<http://example.org/related> <http://www.w3.org/2000/01/rdf-schema#range> <http://example.org/Person> .
<http://example.org/ancestor> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#TransitiveProperty> .
<http://example.org/ancestor> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <http://example.org/related> .
<http://example.org/parent> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <http://example.org/ancestor> .
<http://example.org/parent> <http://www.w3.org/2002/07/owl#inverseOf> <http://example.org/child> .
<http://example.org/father> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#FunctionalProperty> .
<http://example.org/father> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <http://example.org/parent> .
<http://example.org/father> <http://www.w3.org/2000/01/rdf-schema#range> <http://example.org/Male> .
<http://example.org/mother> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#FunctionalProperty> .
<http://example.org/mother> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <http://example.org/parent> .
<http://example.org/mother> <http://www.w3.org/2000/01/rdf-schema#range> <http://example.org/Female> .
<http://example.org/son> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <http://example.org/child> .
<http://example.org/son> <http://www.w3.org/2000/01/rdf-schema#range> <http://example.org/Male> .
<http://example.org/daughter> <http://www.w3.org/2000/01/rdf-schema#subPropertyOf> <http://example.org/child> .
<http://example.org/daughter> <http://www.w3.org/2000/01/rdf-schema#range> <http://example.org/Female> .
<http://example.org/bart> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Boy> .
<http://example.org/bart> <http://example.org/father> <http://example.org/homer> .
<http://example.org/bart> <http://example.org/mother> <http://example.org/marge> .
<http://example.org/bart> <http://example.org/age> "10"^^<http://www.w3.org/2001/XMLSchema#integer> .
<http://example.org/bart> <http://schema.org/name> "Bart Simpson" .
<http://example.org/lisa> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Girl> .
<http://example.org/lisa> <http://example.org/father> <http://example.org/homer> .
<http://example.org/lisa> <http://example.org/mother> <http://example.org/marge> .
<http://example.org/lisa> <http://example.org/age> "8"^^<http://www.w3.org/2001/XMLSchema#integer> .
<http://example.org/lisa> <http://schema.org/name> "Lisa Simpson" .
<http://example.org/marge> <http://example.org/father> <http://example.org/clancy> .
_:Bca9fe66a6d95798f5ea498ff83306305 <http://schema.org/name> "Jacqueline Bouvier née Gurney" .
<http://example.org/marge> <http://example.org/mother> _:Bca9fe66a6d95798f5ea498ff83306305 .
"#;
