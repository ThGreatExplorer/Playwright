((tmodule M (class A (x y)
                (method m (o) 1.0))
    (((x Number)) ((m (Number) Number))))

(import M)
(def x (new A ()))
(def y 2.0)
y)