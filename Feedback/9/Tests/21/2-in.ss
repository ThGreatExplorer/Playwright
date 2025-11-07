((tmodule Foo (class foo (x) (method woo (nOne nTwo) (nOne + nTwo))) (((x Number)) ((woo (Number Number) Number))))
(tmodule Bar (import Foo) (class foo (x y)) (((x Number)) ()))
4.0)
