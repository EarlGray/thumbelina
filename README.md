Thumbelina Scheme
=================

My study lisp interpreter, influenced by and implemented in Haskell

Functionality:
* simplistic repl
* read the input stream, make SExps
* special forms: *quote* (also with ' syntax), *if*, *lambda*, *def*, *begin*
* builtins: *car*, *cdr*, *eq*
* arithmetical functions: +/- and * (both are polymorphic: (+ 2 2) => 4, (+ "str" "cat") => "strcat", ( * "ah" 4) => "ahahahah" etc)
* functions: (def (f x) x), (def sqr (lambda (x) ( * x x))) etc
* partial application of functions: (def add (+)), (def add2 (+ 2)), etc

Plans for the future:
* first-class map/fold/filter support
* laziness by default: lazy ()-brackets and strict evaluation within []-brackets
* functional purity, monads?
