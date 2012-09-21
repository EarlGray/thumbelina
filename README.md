Thumbelina Scheme
=================

My study lisp interpreter, influenced by and implemented in Haskell

Functionality:
* simplistic repl (file loading)
* read the input stream, make SExps
* special forms: *quote* (also with ' syntax), *if*, *lambda*, *def*, *begin*
* builtins: *car*, *cdr*, *eq*, *eval*, *read*, *eq*
* arithmetical functions: +/- and * (both are polymorphic: _(+ 2 2)_ => _4_, _(+ "str" "cat")_ => _"strcat"_, _( * "ah" 4)_ => _"ahahahah"_ etc), number comparison
* functions: _(def (id x) x)_, _(def sqr (fun (x) ( * x x)))_ etc
* partial application of functions: _(def add (+))_, _(def add2 (+ 2))_, etc

See example lisp file (you can load it in REPL)

Plans for the future:
* first-class map/fold/filter support
* laziness by default: lazy ()-brackets and strict evaluation within []-brackets
* functional purity, monads?
