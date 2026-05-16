# Term matchers

A number of functions in `sophia` accept parameters to select certain terms;
the most prominent example is [`Graph::triples_matching`],
which we encountered in the [previous chapter](./ch04_rdf_graphs.md).
This capacity to select, or "match", certain terms,
is captured by the trait [`TermMatcher`].
A number of types implement those traits.

(more coming soon)

<!--
TODO Section [`TermMatcher`]s illustrated
a first example uses arrays on one term and Any, and details the two
a second example uses arrays of several terms, then a slice (using a Vec whose size is not known in advance)
a fourth example uses a termkind
a fifth example uses a termkind
-->

<!--
TODO Section Other uses of [`TermMatcher`]s
Provide a few examples of other methods using term matchers.
-->

[`Graph::triples_matching`]: https://docs.rs/sophia_api/0.9.0/sophia_api/graph/trait.Graph.html#method.triples_matching
[`TermMatcher`]: https://docs.rs/sophia_api/latest/sophia_api/term/matcher/trait.TermMatcher.html
