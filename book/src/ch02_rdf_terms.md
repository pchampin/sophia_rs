# RDF Terms

The [`Term`] trait defines how you interact with [RDF terms] in Sophia.

## Using terms

The first thing you usually need to know about a term is its *kind* (IRI, Literal...).
The kind is described by the [`TermKind`] enum,
and available from the [`Term::kind`] method.

```rust,noplayground
# use sophia::api::term::{SimpleTerm, Term, TermKind};
# use TermKind::*;
# let some_term: SimpleTerm = "foo".into_term();
match some_term.kind() {
    Iri => { /* ... */ }
    Literal => { /* ... */ }
    BlankNode => { /* ... */ }
    _ => { /* ... */ }
}
```

Alternatively, when only one kind is of interest, you can use [`Term::is_iri`], [`Term::is_literal`], [`Term::is_blank_node`], etc.

If you are interested in the "value" of the term, the trait provides the following methods. All of them return an `Option`, which will be `None` if the term does not have the corresponding kind.

* If the term is an IRI, [`Term::iri`] returns that IRI[^relative_iris].
* If the term is a blank node, [`Term::bnode_id`] returns its [blank node identifier].
* If the term is a literal:

  + [`Term::lexical_form`] returns its [lexical form] (the "textual value" of the literal),
  + [`Term::datatype`] returns its datatype IRI[^relative_iris],
  + [`Term::language_tag`] returns its [language tag], if any.

* If the term is a [quoted triple]:

  + [`Term::triple`] returns its 3 components in an array of terms,
  + [`Term::constituents`] iterates over all its [constituents],
  + [`Term::atoms`] iterates over all its atomic (i.e. non quoted-triple) constituents.
  + (those three methods also have a `to_X` version that destructs the original term instead of borrowing it)

* If the term is a variable[^variables], [`Term::variable`] returns its name.

Finally, the method [`Term::eq`] can be used to check whether two values implementing [`Term`] represent the same RDF term. Note that the `==` operator may give a different result than [`Term::eq`] on some types implementing the [`Term`] trait.


## Useful types implementing [`Term`]

Below is a list of useful types implementing the [`Term`] trait:

* [`Iri`]`<T>` and [`IriRef`]`<T>`, where `T: Borrow<str>`, representing IRIs
* [`BnodeId`]`<T>`, where `T: Borrow<str>`, representing blank nodes
* `str`, representing literals of type `xsd:string`,
* `i32`, `isize` and `usize` representing literals of type `xsd:integer`,
* `f64` representing literals of type `xsd:double`,
* [`SimpleTerm`](see below).

[`SimpleTerm`] is a straightforward implementation of [`Term`], that can represent any kind of term, and can either own its own underlying data or borrow it from something else.

Any term can be converted to a [`SimpleTerm`] using the [`Term::as_simple`] method.
This method borrows as much as possible from the initial term to avoid spurious memory allocation.
Alternatively, to convert any term to a self-sufficient [`SimpleTerm`], you can use [`Term::into_term`]

See also the list of [recipes](#recipes-for-constructing-terms) below.


## Borrowing terms with [`Term::borrow_term`]

In Sophia, all functions accepting terms as parameters are expecting a type `T: Term` -- not `&T`, but the type `T` itself. So what happens when you want to call such a function with a term `t`, but still want to retain ownership of `t`?

The solution is to pass [`t.borrow_term()`] to the function. This method returns *something* implementing [`Term`], representing the same RDF term as `t`, without waiving ownership. This is a very common pattern in Sophia.

More precisely, the type returned by [`t.borrow_term()`] is the associated type [`Term::BorrowTerm`]. In most cases, this is a reference a reference or a copy of `t`.


## Recipes for constructing terms

### Constructing IRIs

```rust,noplayground
# fn main() -> Result<(), Box<dyn std::error::Error>> {
#
# use sophia::{iri::IriRef, api::ns::Namespace};
# let some_text = "http://example.org";
// construct an IRI from a constant
let iri1 = IriRef::new_unchecked("http://example.org");

// construct an IRI from an untrusted string
let iri2 = IriRef::new(some_text)?;

// construct multiple IRIs from a namespace
let ns = Namespace::new_unchecked("http://example.org/ns#");
let iri3 = ns.get_unchecked("foo");
let iri4 = ns.get(some_text)?;

// standard namespaces
use sophia::api::ns::{rdf, xsd};
let iri5 = rdf::Property ;
let iri6 = xsd::string ;
#
# Ok(()) }
```

### Constructing literals
```rust,noplayground
# fn main() -> Result<(), Box<dyn std::error::Error>> {
#
# use sophia::api::{ns::xsd, term::{LanguageTag, SimpleTerm, Term}};
// use native types for xsd::string, xsd::integer, xsd::double
let lit_string = "hello world";
let lit_integer = 42;
let lit_double = 1.23;

// construct a language-tagged string
let fr = LanguageTag::new_unchecked("fr");
let lit_fr = "Bonjour le monde" * fr;

// construct a literal with an arbitrary datatype
let lit_date = "2023-11-15" * xsd::date;
#
# Ok(()) }
```

### Constructing blank nodes
```rust,noplayground
# fn main() -> Result<(), Box<dyn std::error::Error>> {
#
# use sophia::api::term::BnodeId;
let b = BnodeId::new("x");
#
# Ok(()) }
```

### Converting terms into a different type
```rust,noplayground
# use sophia::api::{ns::xsd, term::{SimpleTerm, Term}};
fn main() -> Result<(), Box<dyn std::error::Error>> {
# let some_term = "42" * xsd::integer;
let t1: SimpleTerm = "hello".into_term();
let t2: i32 = some_term.try_into_term()?;
# Ok(()) }
```

----

[^relative_iris]: Note that in Sophia's [generalized RDF] model,
IRIs can be *relative* IRI reference.

[^variables]: Note that this kind only exist in Sophia's [generalized RDF] model.


[`Term`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html
[RDF terms]: https://www.w3.org/TR/rdf-concepts/#dfn-rdf-term
[generalized RDF]: ch00_introduction.html#generalized
[`TermKind`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/enum.TermKind.html
[`Term::kind`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#tymethod.kind
[`Term::is_iri`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.is_iri
[`Term::is_blank_node`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.is_blank_node
[`Term::is_literal`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.is_literal
[`Term::iri`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.iri
[`Term::bnode_id`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.bnode_id
[`Term::bnode_id`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.bnode_id
[`Term::bnode_id`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.bnode_id
[`Term::lexical_form`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.lexical_form
[`Term::datatype`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.datatype
[`Term::language_tag`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.language_tag
[`Term::triple`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.triple
[`Term::constituents`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.constituents
[`Term::atoms`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.atoms
[`Term::variable`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.variable
[`Term::eq`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.eq
[`Term::as_simple`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.as_simple
[`Term::into_term`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.into_term
[`Term::borrow_term`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#tymethod.borrow_term
[`t.borrow_term()`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#tymethod.borrow_term
[`Term::BorrowTerm`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#associatedtype.BorrowTerm

[blank node identifier]: https://www.w3.org/TR/rdf-concepts/#dfn-blank-node-identifier
[lexical form]: https://www.w3.org/TR/rdf-concepts/#dfn-lexical-form
[datatype]: https://www.w3.org/TR/rdf-concepts/#dfn-datatype-iri
[language tag]: https://www.w3.org/TR/rdf-concepts/#dfn-language-tag
[quoted triple]: https://www.w3.org/2021/12/rdf-star.html#dfn-quoted
[constituents]: https://www.w3.org/2021/12/rdf-star.html#dfn-constituent

[`Iri`]: https://docs.rs/sophia_iri/0.8.0/sophia_iri/struct.Iri.html
[`IriRef`]: https://docs.rs/sophia_iri/0.8.0/sophia_iri/struct.IriRef.html
[`BnodeId`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/bnode_id/struct.BnodeId.html
[`SimpleTerm`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/enum.SimpleTerm.html
