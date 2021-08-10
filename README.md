# Sophia

A Rust toolkit for RDF and Linked Data.

[![Actions Status](https://github.com/pchampin/sophia_rs/actions/workflows/lint_and_test.yml/badge.svg)](https://github.com/pchampin/sophia_rs/actions)
[![Latest Version](https://img.shields.io/crates/v/sophia.svg)](https://crates.io/crates/sophia)
[![Documentation](https://docs.rs/sophia/badge.svg)](https://docs.rs/sophia/)

It comprises the following crates:

* [`sophia_api`] defines a generic API for RDF and linked data,
  as a set of core traits and types;
  more precisely, it provides traits for describing
  - terms, triples and quads,
  - graphs and datasets,
  - parsers and serializers
* [`sophia_iri`] provides functions, types and traits for validating and resolving IRIs.
* [`sophia_term`] defines implementations of the `TTerm` trait from `sophia_api`.
* [`sophia_inmem`] defines in-memory implementations of the `Graph` and `Dataset` traits from `sophia_api`.
* [`sophia_turtle`] provides parsers and serializers for the Turtle-family of concrete syntaxes.
* [`sophia_xml`] provides parsers and serializers for RDF/XML.
* [`sophia_jsonld`] provides preliminary support for JSON-LD.
* [`sophia_indexed`] and [`sophia_rio`] are lower-level crates, used by the ones above. 

and finally:
* [`sophia`] is the “all-inclusive” crate,
  re-exporting symbols from all the crates above.
  
## Licence

[CECILL-B] (compatible with BSD)

## Testing

The test suite depends on the [the [JSON-LD test-suite]
which is included as a `git` submodule.
In order to run all the tests, you need to execute the following commands:
```bash
$ git submodule init
$ git submodule update
```

## Citation

When using Sophia, please use the following citation:

> Champin, P.-A. (2020) ‘Sophia: A Linked Data and Semantic Web toolkit for Rust’, in Wilde, E. and Amundsen, M. (eds). The Web Conference 2020: Developers Track, Taipei, TW. Available at: https://www2020devtrack.github.io/site/schedule.

Bibtex:
```bibtex
@misc{champin_sophia_2020,
        title = {{Sophia: A Linked Data and Semantic Web toolkit for Rust},
        author = {Champin, Pierre-Antoine},
        howpublished = {{The Web Conference 2020: Developers Track}},
        address = {Taipei, TW},
        editor = {Wilde, Erik and Amundsen, Mike},
        month = apr,
        year = {2020},
        language = {en},
        url = {https://www2020devtrack.github.io/site/schedule}
}
```

## History

An outdated comparison of Sophia with other RDF libraries is still available
[here](https://github.com/pchampin/sophia_benchmark/blob/master/benchmark_results.ipynb).


[`sophia_api`]: https://crates.io/crates/sophia_api
[`sophia_iri`]: https://crates.io/crates/sophia_iri
[`sophia_term`]: https://crates.io/crates/sophia_term
[`sophia_inmem`]: https://crates.io/crates/sophia_inmem
[`sophia_turtle`]: https://crates.io/crates/sophia_turtle
[`sophia_xml`]: https://crates.io/crates/sophia_xml
[`sophia_jsonld`]: https://crates.io/crates/sophia_jsonld
[`sophia_indexed`]: https://crates.io/crates/sophia_indexed
[`sophia_rio`]: https://crates.io/crates/sophia_rio
[`sophia`]: https://crates.io/crates/sophia
[CECILL-B]: https://cecill.info/licences/Licence_CeCILL-B_V1-en.html
[RDF test-suite]: https://github.com/w3c/rdf-tests/
[JSON-LD test-suite]: https://github.com/w3c/json-ld-api/
