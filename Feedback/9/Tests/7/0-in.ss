(
    (tmodule A (class A (a)) (((a Number)) ()))
    (tmodule B (class B (a)) (((a Number)) ()))

    (import A)
    (import B)
    (def one 1.0)
    (def a (new A (one)))
    (def b (new A (one)))
    (def c (new B (one)))
    (def ret (a == b))
    
    (if0 (a == c)
        (ret = (ret + one))
        (ret = ret)
    )

    ret
)