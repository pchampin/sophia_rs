# Changes between version 0.9 and 0.10

Sophia has been slightly refactored between version 0.9 and 0.10.

## NTSerializer is now NTriplesSerializer

Rename sophia::turtle::serializer::nt::NTSerializer to NTriplesSerializer.

## impl Term: add base_direction

`Term::base_direction()` does provide a default implementation -- but that base implementation needs to be overridden if your implementation supports literals (it will panic when called on a literal -- like all other default implementations of `Term` methods).
If you do not support base direction at all, which would be the case for RDF 1.1 implementations, then the following workaround fixes runtime errors with literals in existing Sophia 0.9 code:

```rs
fn base_direction(&self) -> Option<BaseDirection> {
    None
}
```

## impl Graph: change triples_matching

The trait signature of `Graph::triples_matching()` it more permissive than before, so your existing method bodies should still compile.
All you need to do is to change the signature to match that of the trait:

```rs
impl Graph for MyGraph {
-    fn triples_matching<'s, S, P, O>(
+    fn triples_matching<'s, 't, S, P, O>(
         &'s self, sm: S, pm: P, om: O,
-    ) -> impl Iterator<Item = Result<Self::Triple<'s>, Self::Error>> + 's
+    ) -> impl Iterator<Item = Result<Self::Triple<'s>, Self::Error>> + 't
     where
-        S: TermMatcher + 's,
-        P: TermMatcher + 's,
-        O: TermMatcher + 's,
+        's: 't,
+        S: TermMatcher + 't,
+        P: TermMatcher + 't,
+        O: TermMatcher + 't,
     {
```

## Turtle parser now has a constructor and needs a BaseIriRef instead of an Iri

If you are creating a TurtleParser with base using Sophia 0.9 as follows...

```rs
let ttl_parser = sophia::turtle::parser::turtle::TurtleParser {
    base: Some(sophia::iri::Iri::new_unchecked("https://test.com/".to_owned())),
};
```

... in Sophia 0.10 you need to provide a `BaseIriRef<Box<str>>` instead but you can use the new constructor and `with_base` method:

```rs
let ttl_parser = sophia::turtle::parser::turtle::TurtleParser::new().with_base(
    Some(sophia::iri::IriRef::<Box<str>>::new_unchecked("https://test.com/".into()).to_base()));
```

## LiteralLanguage Tuple size increased to 3

If you don't care about the base direction and are matching on LiteralLanguage, ignore the 3rd element with `_` or `..`.
For example, you could change `SimpleTerm::LiteralLanguage(a, b)` to `SimpleTerm::LiteralLanguage(a, ,b, _)`.

## `&s` in `g.triples_matching([&s], ...)` may now be an `&IndexedTerm`

If `s` is the result of [`BasicTermIndex::get_term`](https://docs.rs/sophia_inmem/0.10.0/sophia_inmem/index/trait.TermIndex.html#tymethod.get_term), then `&s` in `g.triples_matching([&s], ...)` does not have the same type in 0.9.0 (`&SimpleTerm`) and 0.10.0 (`&IndexedTerm`).
While the former implements the trait `Term`, the latter does not!
Thus, `[&s]` is not recognized as a `TermMatcher` anymore (it needs an array of `Term`s).
The solution is to replace `&s` with `s`: `g.triples_matching([s], ...)`.
This is ok because, unlike `SimpleTerm`, `IndexedTerm` implements the trait `Copy`, so even if you need to use `s` later, this will not hurt.

## Requesting help

As migration from version 0.9 to version 0.10 can be challenging,
a [dedicated tag](https://github.com/pchampin/sophia_rs/labels/v0.10_migration)
has been added on the github repository of Sophia to mark migration issues and request assistance.
