# Sophia

A Rust toolkit for RDF and Linked Data.

[![Build Status](https://travis-ci.org/media-io/sophia_rs.svg?branch=master)](https://travis-ci.org/media-io/sophia_rs)
[![Latest Version](https://img.shields.io/crates/v/sophia.svg)](https://crates.io/crates/sophia)
[![Coverage Status](https://coveralls.io/repos/github/media-io/sophia_rs/badge.svg?branch=add_travis)](https://coveralls.io/github/media-io/sophia_rs?branch=add_travis)

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

[CECILL-C]

(compatible with GNU LGPL)

[CECILL-C]: http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.html
[RDF test-suite]: https://github.com/w3c/rdf-tests/
