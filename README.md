thumbelina
==========

Thumbelina Scheme - my study lisp interpreter, influenced by and implemented in Haskell

Functionality:
* 0) simplistic repl
* 1) read the input stream, make SExps
* 2) evaluate self-evalutating exps (integer/float/string constants)
* 3) evaluate special forms "quote" and "if"
* 4) "car"/"cdr" builtins, special symbol "nil"
* 5) builtin functions for now: + and * (both are polymorphic: (+ 2 2) => 4, (+ "str" "cat") => "strcat", ( * "ah" 4) => "ahahahah" etc)

Plans for the future: 
* 1) first-class map/fold/filter support
* 2) laziness by default: lazy ()-brackets and strict evaluation within []-brackets
* 3) functional purity, monads?
