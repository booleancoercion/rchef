# rchef: A [Chef](https://dangermouse.net/esoteric/chef.html) interpreter written in Rust.

The interpreter will almost exactly match Chef, but I might add a few of my own features, and some cases which cause ambiguity will be disallowed (instead of unspecified).

## Usage
To use, simply execute the interpreter with the name of the file you wish to run.

## Differences
rchef doesn't allow identifiers (i.e. recipe names or ingredients) that contains defined keywords, such as "from" and "Refrigerate". This might be different from other interpreters that are smarter, however these kinds of identifiers shouldn't be used in the first place.
TODO