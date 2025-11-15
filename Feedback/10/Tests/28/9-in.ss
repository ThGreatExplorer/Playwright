((module
  Dup
  (class A (x)
    (method get () (this --> x))))
 (module
  Dup
  (class B (y)
    (method get () (this --> y))))
 (import Dup)
 (def one 1.0)
 (def a (new A (one)))
 (a --> get ()))