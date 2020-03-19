# Sophia

A Rust toolkit for RDF and Linked Data.

[![Build Status](https://travis-ci.org/pchampin/sophia_rs.svg?branch=master)](https://travis-ci.org/pchampin/sophia_rs)
[![Latest Version](https://img.shields.io/crates/v/sophia.svg)](https://crates.io/crates/sophia)
[![Coverage Status](https://coveralls.io/repos/github/pchampin/sophia_rs/badge.svg?branch=master)](https://coveralls.io/github/pchampin/sophia_rs?branch=master)

## Performances

A comparison of Sophia with other RDF libraries is available
[here](https://github.com/pchampin/sophia_benchmark/blob/master/benchmark_results.ipynb).

## Testing

The test suite depends on the [RDF test-suite],
which is included as a `git` submodule.
In order to run all the tests, you need to execude the following commands:
```
$ git submodule init
$ git submodule update
```


## Licence

* [CECILL-B] (compatible with BSD) for core reusable components
* [CECILL-C] (compatible with GNU LGPL) for other components



[CECILL-B]: https://cecill.info/licences/Licence_CeCILL-B_V1-en.html
[CECILL-C]: https://cecill.info/licences/Licence_CeCILL-C_V1-en.html
[RDF test-suite]: https://github.com/w3c/rdf-tests/
