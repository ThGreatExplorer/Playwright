((tmodule M (class A ()
                (method m (o) 1.0))
    (() ((m (Number) Number))))

(import M)
(def x (new A ()))
(def y 2.0)
(y == x))