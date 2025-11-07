(
(tmodule X (class B () (method m (o) 2.0))
        (() ((m (Number) Number))))

(tmodule M (import X) (class A ()
                (method m (o) (o isa B)))
    (() ((m ((() ((m (Number) Number)))) Number))))

(import M)
(import X)
(def b (new B ()))
(def x (new A ()))
(def y 2.0)
(def r 1.0)
(r = (x --> m (b)))
r)