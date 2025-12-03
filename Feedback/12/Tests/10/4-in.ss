((module A (class A (x)))
 (module X
    (import A)
    (class X ()
        (method changeShape (obj x) (obj = (new A (x))) 0.0))
    )
 (tmodule Y
    (class Y (a b) (method c (p) (def five 5.0) (p + five)))
    (((a Number) (b Number)) ((c (Number) Number))))
 (timport X (() ((changeShape ((((a Number) (b Number)) ((c (Number) Number))) Number) Number)))) (import Y)
 (def one 1.0)
 (def two 2.0)
 (def y (new Y (one two)))
 (def x (new X ()))
 (def void (x --> changeShape (y one)))
 (y --> x))

