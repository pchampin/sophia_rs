# Sophia

A Rust toolkit for RDF and Linked Data.

[![Build Status](https://travis-ci.org/pchampin/sophia_rs.svg)](https://travis-ci.org/pchampin/sophia_rs)
[![Latest Version](https://img.shields.io/crates/v/sophia.svg)](https://crates.io/crates/sophia)
[![Coverage Status](https://coveralls.io/repos/github/pchampin/sophia_rs/badge.svg?branch=master)](https://coveralls.io/github/pchampin/sophia_rs?branch=master)

It comprises the following crates:

* [`sophia`] is currently the main crate,
  but will eventually become a "compilation" of the smaller,
  more specialized crates listed below.
  Currently, it defines
  - traits for describing RDF graphs and datasets,
  - implementation of those traits (in-memory),
  - traits for serializing and parsing various RDF formats,
  - implementation of those traits for Turtle-like syntaxes and RDF/XML.
* [`sophia_api`] currently defines the trait for RDF terms,
  with its associated types;
  in the future it will contain all the core traits and types of the Sophia API.
* [`sophia_iri`] provides functions for validating and resolving IRIs
* [`sophia_term`] defines implementations of the `TTerm` trait.
* [`sophia_jsonld`] preliminary support for JSON-LD


## Performances

A comparison of Sophia with other RDF libraries is available
[here](https://github.com/pchampin/sophia_benchmark/blob/master/benchmark_results.ipynb).

## Testing

The test suite depends on the [RDF test-suite] and the [JSON-LD test-suite]
which is included as a `git` submodule.
In order to run all the tests, you need to execude the following commands:
```
$ git submodule init
$ git submodule update
```


## Licence

* [CECILL-B] (compatible with BSD) for core reusable components
* [CECILL-C] (compatible with GNU LGPL) for other components


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


[`sophia_api`]: https://crates.io/crates/sophia_api
[`sophia_iri`]: https://crates.io/crates/sophia_iri
[`sophia_term`]: https://crates.io/crates/sophia_term
[`sophia`]: https://crates.io/crates/sophia
[`sophia_jsonld`]: https://crates.io/crates/sophia_jsonld
[CECILL-B]: https://cecill.info/licences/Licence_CeCILL-B_V1-en.html
[CECILL-C]: https://cecill.info/licences/Licence_CeCILL-C_V1-en.html
[RDF test-suite]: https://github.com/w3c/rdf-tests/
[JSON-LD test-suite]: https://github.com/w3c/json-ld-api/
