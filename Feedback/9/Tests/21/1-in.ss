((tmodule Foo (class foo (x) (method woo (nOne nTwo) (nOne + nTwo))) (((x Number)) ((woo (Number Number) Number))))
(tmodule Bar (import Foo) (class foo (x y) (method wee () (this --> x)) (method woo () (this --> y))) (((x Number) (y Number)) ((wee () Number))))
1.0)
