(
    (tmodule A (class myClass (a b c)) (((a Number) (b Number) (c Number)) ()))
    (module A (class myClass (a b c)))

    (tmodule C (import A) (import B) (class myClassTwo (a b c) 
        (method makeA () (def a (this --> a)) (def b (this --> b)) (def c (this --> c)) (new myClass (a b c))))
        (((a Number) (b Number) (c Number)) ((makeA () (((a Number) (b Number) (c Number)) ()))))
        )

    (import C)
    (def one 1.0)
    (def two 2.0)
    (def three 3.0)
    (def out 0.0)
    (def myClass (new myClassTwo (one two three)))
    (def final (myClass --> makeA ()))
    (one = (final --> a))
    (two = (final --> b))
    (three = (final --> c))
    (two = (two + three))
    (out = (two + one))
    out
)