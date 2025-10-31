((module A (class C () (method v () 1.0)))
(module B (class C () (method v () 2.0)))
(module D (class C () (method v () 3.0)))
(import A)
(import B)
(import D)
(def o (new C ()))
(o --> v ()))

