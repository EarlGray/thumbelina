Thumbelina Scheme
=================

My study lisp interpreter, influenced by and implemented in Haskell

Functionality:
* simplistic repl (with file loading)
* read the input stream, make SExps
* special forms: *quote* (also with ' syntax), *if*, *fun* (*lambda*), *def* (*defun*), *seq* (*begin*)
*   `(map (fun (x) (+ x 1)) list)`
* builtins: *car*, *cdr*, *eq*, *eval*, *read*, *eq*
* arithmetical functions: +/- and * (both are polymorphic: `(+ 2 2)` => `4`, `(+ "str" "cat")` => `"strcat"`, `( * "ah" 4)` => `"ahahahah"` etc), number comparison
* functions: `(def (id x) x)`, `(def sqr (fun (x) ( * x x)))` etc
* partial application of functions: `(def add (+))`, `(def add2 (+ 2))`, etc

See lisp example files (you can load it in REPL)

Plans for the future:
* first-class map/fold/filter support
* laziness by default: lazy ()-brackets and strict evaluation within []-brackets
* functional purity, monads?
