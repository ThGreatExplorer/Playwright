((module A (class C ()))
(tmodule M (import A) (class C () (method m (x) (x isa C))) (() ()))
(import M)

1.0)