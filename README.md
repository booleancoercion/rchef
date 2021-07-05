# rchef: A [Chef](https://dangermouse.net/esoteric/chef.html) interpreter written in Rust.

The interpreter will almost exactly match Chef, but I might add a few of my own features, and some cases which cause ambiguity will be disallowed (instead of unspecified).

## Usage
Since this crate isn't published, you'll have to build it from source. Fortunately, that's very easy with Rust:
```sh
$ git clone https://github.com/booleancoercion/rchef
$ cd rchef
$ cargo run --release -- <rchef options>
```

rchef expects to be called in the following way:
```sh
$ rchef [-s | --spaced] <filename>
```
The `-s/--spaced` options are equivalent, and will run your program in spaced mode. This mode will print numbers (dry or ambiguous ingredients) separated by spaces from other characters for easy reading.

## Differences
- rchef doesn't allow the cooking time or oven temperature segments, as they make no difference to the program and there is already a place for free-form text.  
If you'd like to include these details, please put them in the optional "Comments" section. Technically, this is equivalent to moving these sections before the "Ingredients" section.

- rchef doesn't allow ingredient names that contain keywords, i.e. fixed words that may appear in any program such as "Ingredients" or even "the".  
While this may be a difference from other interpreters, note that the language specification states that ingredient names
must be "anything reasonable", and so this interpretation is valid.

- It appears the specification lists several statement grammars incorrectly - bowls are always supposed to be referred to with a preceding 'the'.
This is apparent in the 'Hello World Souffle' example program, where a 'Put' statement is used with the word 'the'. It also makes more sense in english.  
As such, i've elected to assume *every ordinal identifier behaves this way* - the ordinal itself is optional, but the 'the' is not (unless the entire rest
of the sentence is).